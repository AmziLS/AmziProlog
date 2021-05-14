package com.amzi.prolog.ui.views;

import amzi.ls.LogicServer;
import amzi.ls.LSException;
import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.ui.build.ProjectProperties;
import com.amzi.prolog.ui.PrologUIPlugin;
import com.amzi.prolog.ui.PrologPluginImages;
import com.amzi.prolog.ui.internal.Find;
import com.amzi.prolog.core.utils.Utils;

//import java.text.MessageFormat;
import java.util.ArrayList;
//import java.util.List;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.*;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.*;
import org.eclipse.ui.editors.text.TextEditor; 
import org.eclipse.ui.ide.*;
import org.eclipse.ui.part.ViewPart;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class XrefView extends ViewPart {
		private TreeViewer viewer;
//		private DrillDownAdapter drillDownAdapter;
		private ViewContentProvider viewContentProvider;
		private Action refreshAction = null;
//		private Action action2;
//		private Action doubleClickAction;
		private IProject project;
		
		/*
		 * The content provider class is responsible for
		 * providing objects to the view. It can wrap
		 * existing objects in adapters or simply return
		 * objects as-is. These objects may be sensitive
		 * to the current input of the view, or ignore
		 * it and always show the same content
		 * (like Task List, for example).
		 */

		class TreeObject implements IAdaptable {
			private String name;
			private TreeParent parent;
			private String file;
			private int line, line_end;
			
			public TreeObject(String name, String file, int line, int line_end) {
				this.name = name;
				if (file == null || file.length() == 0)
					this.file = null;
				else
					this.file = file;
				this.line = line;
				this.line_end = line_end;
			}
			public String getFilename() {
				return file;
			}
			public int getStartLine() {
				return line;
			}
			public int getEndLine() {
				return line_end;
			}
			public String getName() {
				return name;
			}
			public void setParent(TreeParent parent) {
				this.parent = parent;
			}
			public TreeParent getParent() {
				return parent;
			}
			public String toString() {
				return getName();
			}
			public Object getAdapter(Class key) {
				return null;
			}
		}

		class TreeParent extends TreeObject {
			private ArrayList children;
			
			public TreeParent(String name, String file, int line, int line_end) {
				super(name, file, line, line_end);
				children = new ArrayList();
			}
			public void addChild(TreeObject child) {
				children.add(child);
				child.setParent(this);
			}
			public void removeChild(TreeObject child) {
				children.remove(child);
				child.setParent(null);
			}
			public TreeObject [] getChildren() {
				return (TreeObject [])children.toArray(new TreeObject[children.size()]);
			}
			public boolean hasChildren() {
				return children.size()>0;
			}
		}

		class ViewContentProvider implements IStructuredContentProvider,
											   ITreeContentProvider {
			private LogicServer ls = null;
			private TreeParent invisibleRoot;

			public void inputChanged(Viewer v, Object oldInput, Object newInput) {
			}
			
			public void dispose() {
				if (ls != null) 
					try {
						synchronized (ls) {
							ls.Close();
							ls = null;
							System.gc();
						}
					}
					catch (LSException ex) {
						PrologUIPlugin.log(ex);
					}
			}
			
			public Object[] getElements(Object parent) {
				if (parent.equals(ResourcesPlugin.getWorkspace())) {
					if (invisibleRoot==null) initialize();
					return getChildren(invisibleRoot);
				}
				return getChildren(parent);
			}
			public Object getParent(Object child) {
				if (child instanceof TreeObject) {
					return ((TreeObject)child).getParent();
				}
				return null;
			}
			public Object [] getChildren(Object parent) {
				if (parent instanceof TreeParent) {
					return ((TreeParent)parent).getChildren();
				}
				return new Object[0];
			}
			public boolean hasChildren(Object parent) {
				if (parent instanceof TreeParent)
					return ((TreeParent)parent).hasChildren();
				return false;
			}
		   	private synchronized void initialize() {
		   		invisibleRoot = new TreeParent("", null, -1, -1);
		   	}
		   	
			private void doXref() {
				long term, term2, xrefterm, ref, warning, list, list2;
				String text;
				TreeParent pred, uses, usedby;

				project = Find.getSelectedProject();
				if (project == null) return;
				Properties buildProps = ProjectProperties.getProperties(project);

				invisibleRoot = new TreeParent("", null, -1, -1);
				try {
					String amziDir = PrologCorePlugin.getAmziDir();
					if (ls == null)
						ls = new LogicServer();
					synchronized (ls) {
						try {
							// If there is an amzi.cfg then specify it
							IFile cfgFile = project.getFile("amzi.cfg");
							if (cfgFile.exists())
								ls.Init(Utils.tiltSlashes(cfgFile.getLocation().toOSString()));
							else
								ls.Init2("control=50000, trail=50000");
								
							// Load aosutils
							ls.AddLSX("aosutils", 0);
	
							// Add the rest of the lsxs
							String lsxList = buildProps.getProperty("lsxExtensionNames");
							if (lsxList != null && lsxList.length() > 0) {
								String lsxs[] = lsxList.split(",");
								for (int i = 0 ; i < lsxs.length ; i++)
									if (lsxs[i].toLowerCase().indexOf("aosutils") < 0) {
										int dot = lsxs[i].indexOf(".");
										String filename = lsxs[i].substring(0, dot);
										ls.AddLSX(filename, 0);
									}
							}
							
	//						String s = Utils.tiltSlashes(PrologUIPlugin.getFilePath("amzijide.xpl"));
							String s = amziDir+"abin"+
								System.getProperty("file.separator")+"axrf.xpl";
							ls.Load(s);
							
							// Load the libraries
							String libs[] = buildProps.getProperty("plmLibraryNames").split(",");
							String loadList = "";
							for (int i = 0 ; i < libs.length ; i++)
								if (libs[i].length() > 0)
									loadList += "'" + libs[i] + "',";
							if (loadList.length() > 0)
								loadList = loadList.substring(0, loadList.length()-1);
							if (loadList != null && loadList.length() > 0) {
								long tf = ls.ExecStr("load([" + Utils.tiltSlashes(loadList) + "])");
								if (tf == 0) {
									MessageDialog.openError(null, "XrefView | initialize", "Unable to load Prolog libraries");
									return;
								}
							}
	
						}
						catch (LSException ex) {
							MessageDialog.openError(null, "XrefView | initialize", ex.GetMsg());
							try {
								ls.Close();
								ls = null;
							}
							catch (LSException ex2) { 
							}
							return;
						}
	
						try {
							// Eliminate old markers
							project.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
						}
						catch (CoreException ex) {
							// Ignore it
						}
	
						// Chdir to the project root
						String projectPathname = project.getLocation().toOSString();
						term = ls.ExecStr("chdir('" + Utils.tiltSlashes(projectPathname) + "')");
	
						// Get build properties
						String proExcludeNames = buildProps.getProperty("proExcludeNames", "");
						
						// Cross reference the files
						IResource members[] = project.members();
						String proNames = "[";
						for (int i = 0 ; i < members.length ; i++) {
							// Only xref pro files not excluded from build
							if (members[i].getType() == IResource.FILE &&
								members[i].exists() &&
								members[i].getFileExtension() != null &&
								members[i].getFileExtension().toLowerCase().equals("pro") &&
								proExcludeNames.indexOf(members[i].getName()) == -1 ) {
								proNames += "'" + members[i].getFullPath().removeFirstSegments(1).toOSString() +
									"',";
									
								// Delete any problems
								try {
									// Eliminate old markers
									members[i].deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
								}
								catch (CoreException ex) {
									// Ignore it
								}
	
							}
						}
						proNames = proNames.substring(0,proNames.length()-1) + "]";
	
						String s = "axrf:xref(" + proNames + ", _Warnings, _Uses)";
						xrefterm = ls.ExecStr(s);
						
						// Cross reference failed
						if (xrefterm == 0) {
							// Change out of the project directory
							ls.ExecStr("chdir(`..`)");
							return;
						}
	
						// Build the reference tree
						list = ls.GetArg(xrefterm, 3);
	
						// Check for the empty list or an atom
						long type = ls.GetTermType(list);
						if (type == LogicServer.pLIST) {
							
							// Walk the reference list
							while (list != 0) {
								// ref(M:F/A, [uses], [usedby], FILE, LINE, ENDLINE)
								ref = ls.GetHead(list);
								term = ls.GetArg(ref, 1);
								int tlen = ls.StrTermLen(term);
								pred = new TreeParent(ls.TermToStr(term, tlen+2), ls.GetStrArg(ref, 4), ls.GetIntArg(ref, 5), ls.GetIntArg(ref, 6));
								invisibleRoot.addChild(pred);
								uses = new TreeParent(PrologUIPlugin.getResourceString("XrefUses_Text"), null, -1, -1 );
								usedby = new TreeParent(PrologUIPlugin.getResourceString("XrefUsedby_Text"), null, -1, -1);
								pred.addChild(uses);
								pred.addChild(usedby);
								
								// uses list loc(M:F/A, FILE, LINE, ENDLINE)
								list2 = ls.GetArg(ref, 2);
								if (ls.GetTermType(list2) == LogicServer.pLIST) {
									while (list2 != 0) {
										term = ls.GetHead(list2);
										if (term != 0) {
											term2 = ls.GetArg(term, 1);
											tlen = ls.StrTermLen(term2);
											uses.addChild(new TreeObject(ls.TermToStr(term2, tlen+2), ls.GetStrArg(term, 2), ls.GetIntArg(term, 3), ls.GetIntArg(term, 4)));
										}
										list2 = ls.GetTail(list2);
									}
								}						
		
								// used by list loc(M:F/A, FILE, LINE, ENDLINE)
								list2 = ls.GetArg(ref, 3);
								if (ls.GetTermType(list2) == LogicServer.pLIST) {
									while (list2 != 0) {
										term = ls.GetHead(list2);
										if (term != 0) {
											term2 = ls.GetArg(term, 1);
											tlen = ls.StrTermLen(term2);
											usedby.addChild(new TreeObject(ls.TermToStr(term2, tlen+2), ls.GetStrArg(term, 2), ls.GetIntArg(term, 3), ls.GetIntArg(term, 4)));
										}
										list2 = ls.GetTail(list2);
									}
								}
		
								list = ls.GetTail(list);
							}
						}
						
						// Get the warnings list
						list = ls.GetArg(xrefterm, 2);
	
						// Check for the empty list or an atom
						type = ls.GetTermType(list);
						if (type != LogicServer.pLIST) return;
	
						// Walk the warnings list
						while (list != 0) {
							String filename, level, msg ="";
							int lineno;
							IMarker marker = null;
							
							// warning(level, [warning_message])
							term = ls.GetHead(list);
							int len = ls.StrTermLen(ls.GetArg(term,1));
							level = ls.TermToStr(ls.GetArg(term, 1), len+2);
							warning = ls.GetArg(term, 2);
							
							// Warnings are lists, with optional ['File: ', FILE, ' Line:', LINE at the beginning
							type = ls.GetTermType(warning);
							if (type == LogicServer.pLIST) {
								text = ls.GetStrHead(warning);
								
								// See if there's a file name and line number
								if (text.toLowerCase().indexOf("file") >= 0) {
									warning = ls.GetTail(warning);
									filename = ls.GetStrHead(warning);
									warning = ls.GetTail(warning);
									text = ls.GetStrHead(warning);
									warning = ls.GetTail(warning);
									lineno = ls.GetIntHead(warning);
									warning = ls.GetTail(warning);
									while (warning != 0) {
										term = ls.GetHead(warning);
										int tlen = ls.StrTermLen(term);
										msg = msg + ls.TermToStr(term, tlen+2);
										warning = ls.GetTail(warning);
									}
	
									IResource file = project.findMember(filename);
									if (file != null) {
										msg += " (xref)";
										marker = file.createMarker(IMarker.PROBLEM);
										if (marker.exists()) {
											marker.setAttribute(IMarker.MESSAGE, msg);
											marker.setAttribute(IMarker.LINE_NUMBER, lineno);
										}
									}
	
								}
								// If not just get the warning and attach it to the project
								else {
									while (warning != 0) {
										term = ls.GetHead(warning);
										int tlen = ls.StrTermLen(term);
										msg = msg + ls.TermToStr(term, tlen+2);
										warning = ls.GetTail(warning);
									}
									
									msg += " (xref)";
									marker = project.createMarker(IMarker.PROBLEM);
									if (marker.exists()) 
										marker.setAttribute(IMarker.MESSAGE, msg);
								}
	
								// Set the level of the error
								if (marker != null && marker.exists()) {
									if (level.equals("error")) {
										marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
										marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_HIGH);
									}
									else if (level.equals("warning")) {
										marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
										marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_NORMAL);
									}
									else {
										marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_INFO);
										marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_LOW);
										marker.setAttribute(IMarker.DONE, true);
									}
								}
	
							}
							
							
							list = ls.GetTail(list);
						}
						
						// Change out of the project directory
						ls.ExecStr("chdir(`..`)");
					}
				}
				catch (LSException ex) {
					// Change out of the project directory
					if (ex.GetType() == LSException.READ || ex.GetType() == LSException.EXEC)
					try {
						ls.ExecStr("chdir(`..`)");
					} 
					catch (LSException ex2) {
					}

					try {
						// Handle read errors and ide errors
						String msg="", readbuf="", readfile = "";
						int lineno=0;

						// What kind of error is it?
						int errtype = ex.GetType();

						// Get all the details about the error
						if (errtype == LSException.READ) {
							msg = ex.GetMsg();
//							rc = ex.GetRC();
							readbuf = ex.GetReadBuffer();
							lineno = ex.GetLineno();
							readfile = ex.GetReadFileName();
						}

						// Add it onto the task list
						if (errtype == LSException.READ || errtype == LSException.EXEC) {
							// Will this find files from pathnames?
							IResource file = project.findMember(readfile);
							if (file != null) {
								IMarker marker = file.createMarker(IMarker.PROBLEM);
								if (marker.exists()) {
									msg += " (xref)";
									marker.setAttribute(IMarker.MESSAGE, msg);
									if (lineno != 0) {
										marker.setAttribute(IMarker.LINE_NUMBER, lineno);
									} else if (readbuf != null && readbuf.length() > 0) {
										marker.setAttribute(IMarker.LOCATION, readbuf);
									}
									marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
									marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_HIGH);
								}
							}
							
							// Tell the user to look at the Problems View
							MessageDialog.openError(null, "Cross Reference", "Unable to create a cross reference. See the Problems View for details.");
						}
						else {
							MessageDialog.openError(null, "XrefView | loop", ex.GetMsg());
						}

					}
					catch (CoreException cex) {
					 // You need to handle the case where the marker no longer exists
					}
				}
				catch (CoreException ex) {
				}

				try {
					ls.Close();
					ls = null;
				}
				catch (LSException ex2) {
				}
			}
		}

		class ViewLabelProvider extends LabelProvider {

			public String getText(Object obj) {
				return obj.toString();
			}
			public Image getImage(Object obj) {
				if (obj.toString().equals(PrologUIPlugin.getResourceString("XrefUses_Text")))
					return PrologPluginImages.get(PrologPluginImages.IMG_OBJS_USES);
				if (obj.toString().equals(PrologUIPlugin.getResourceString("XrefUsedby_Text")))
					return PrologPluginImages.get(PrologPluginImages.IMG_OBJS_USEDBY);
				if (obj instanceof TreeParent)
					return PrologPluginImages.get(PrologPluginImages.IMG_OBJS_PREDICATE);
//				String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
//				return PlatformUI.getWorkbench().getSharedImages().getImage(imageKey);
				return PrologPluginImages.get(PrologPluginImages.IMG_OBJS_DIAMOND);
			}
		}
//		class NameSorter extends ViewerSorter {
//		}

		/**
		 * The constructor.
		 */
		public XrefView() {
			super();
		}

		/**
		 * This is a callback that will allow us
		 * to create the viewer and initialize it.
		 */
		public void createPartControl(Composite parent) {
			PlatformUI.getWorkbench().getHelpSystem().setHelp(parent, "com.amzi.prolog.ui.cross_reference");
			
			setTitleToolTip("Press Refresh Button or Menu to Cross Reference the selected Project. See Tasks View for results.");
			viewer = new TreeViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
//			drillDownAdapter = new DrillDownAdapter(viewer);
			viewContentProvider = new ViewContentProvider();
			viewer.setContentProvider(viewContentProvider);
			viewer.setLabelProvider(new ViewLabelProvider());
//			viewer.setSorter(new NameSorter());
			viewer.setInput(ResourcesPlugin.getWorkspace());
//			viewer.setInput(new TreeParent("", null, 0));
			
			makeActions();
//			hookContextMenu();
//			hookDoubleClickAction();
			contributeToActionBars();
			
			viewer.addSelectionChangedListener(new ISelectionChangedListener() {
				public void selectionChanged(SelectionChangedEvent event) {
					IDocument doc;
					TextEditor editor;
					String filename;
					int line, line_end, offset, offset_end, length;
					
					// if the selection is empty clear the label
				   	if (event.getSelection().isEmpty())
				   	   	return;

				   	if (event.getSelection() instanceof IStructuredSelection) {
					   	IStructuredSelection selection = (IStructuredSelection)event.getSelection();
//					   	for (Iterator iterator = selection.iterator(); iterator.hasNext();) {
//						   	Object domain = (Model) iterator.next();
//					   	}
						TreeObject obj = (TreeObject)selection.getFirstElement();
						IWorkbenchPage wpage = PrologUIPlugin.getDefault().getActivePage();
						filename = obj.getFilename();
						if (filename == null || filename.length() == 0) return;
						IFile file = (IFile)project.findMember(filename);
						line = obj.getStartLine();
						line_end = obj.getEndLine();
						if (line < 0) return;
						try {
							editor = (TextEditor)IDE.openEditor(wpage, file);
							doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
							try {
								offset = doc.getLineOffset(line);
								if (line_end > line)
									offset_end = doc.getLineOffset(line_end-1);
								else
									offset_end = offset;
								length = doc.getLineLength(line_end);
								editor.setHighlightRange(offset, (offset_end+length)-offset, true);
							} 
							catch (BadLocationException ex) {
								MessageDialog.openError(null, "Cross Reference", "Unable to highlight line:" + new Integer(line).toString());
							}
						} 
						catch (PartInitException ex) {
							MessageDialog.openError(null, "Cross Reference", "Unable to find and open file named:" + file);
						}
			   		}
				}
			});


		}

/*		private void hookContextMenu() {
			MenuManager menuMgr = new MenuManager("#PopupMenu");
			menuMgr.setRemoveAllWhenShown(true);
			menuMgr.addMenuListener(new IMenuListener() {
				public void menuAboutToShow(IMenuManager manager) {
					XrefView.this.fillContextMenu(manager);
				}
			});
			Menu menu = menuMgr.createContextMenu(viewer.getControl());
			viewer.getControl().setMenu(menu);
			getSite().registerContextMenu(menuMgr, viewer);
		}
*/
		private void contributeToActionBars() {
			IActionBars bars = getViewSite().getActionBars();
			fillLocalPullDown(bars.getMenuManager());
			fillLocalToolBar(bars.getToolBarManager());
		}

		private void fillLocalPullDown(IMenuManager manager) {
			manager.add(refreshAction);
//			manager.add(new Separator());
//			manager.add(action2);
		}

/*		private void fillContextMenu(IMenuManager manager) {
			manager.add(refreshAction);
//			manager.add(action2);
//			manager.add(new Separator());
//			drillDownAdapter.addNavigationActions(manager);
			// Other plug-ins can contribute there actions here
			manager.add(new Separator("Additions"));
		}
*/
		private void fillLocalToolBar(IToolBarManager manager) {
			manager.add(refreshAction);
//			manager.add(action2);
//			manager.add(new Separator());
//			drillDownAdapter.addNavigationActions(manager);
		}

		private void makeActions() {
			// For some reason this is called twice
			
			refreshAction = new Action() {
				public void run() {
					Runnable runnable = new Runnable(){
						public void run() {
							viewContentProvider.doXref();
							viewer.setContentProvider(viewContentProvider);
						}
					};
					BusyIndicator.showWhile(null, runnable);
				}
			};
			refreshAction.setText("Refresh");
			refreshAction.setToolTipText("Refresh Cross Reference using current project");
			refreshAction.setImageDescriptor(PrologPluginImages.DESC_OBJS_REFRESH);

//			action2 = new Action() {
//				public void run() {
//					showMessage("Action 2 executed");
//				}
//			};
//			action2.setText("Action 2");
//			action2.setToolTipText("Action 2 tooltip");
//			action2.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
//				getImageDescriptor(ISharedImages.IMG_OBJS_TASK_TSK));
//			doubleClickAction = new Action() {
//				public void run() {
//					ISelection selection = viewer.getSelection();
//					Object obj = ((IStructuredSelection)selection).getFirstElement();
//					showMessage("Double-click detected on "+obj.toString());
//				}
//			};
		}
/*
		private void hookDoubleClickAction() {
			viewer.addDoubleClickListener(new IDoubleClickListener() {
				public void doubleClick(DoubleClickEvent event) {
					doubleClickAction.run();
				}
			});
		}
*/
/*		private void showMessage(String message) {
			MessageDialog.openInformation(
				viewer.getControl().getShell(),
				"Sample View",
				message);
		}
*/
		/**
		 * Passing the focus request to the viewer's control.
		 */
		public void setFocus() {
			viewer.getControl().setFocus();
		}
	}
package com.amzi.prolog.ui.views;

import amzi.ls.LogicServer;
import amzi.ls.LSException;
import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.ui.PrologPluginImages;
//import com.amzi.prolog.ui.PrologUIPlugin;
import com.amzi.prolog.core.utils.Utils;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.CoreException;
//import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
//import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.model.WorkbenchViewerComparator;
import org.eclipse.ui.model.WorkbenchViewerSorter;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class PrologContentOutlinePage extends ContentOutlinePage {
	protected Object fInput;
	protected IDocumentProvider fDocumentProvider;
	protected ITextEditor fTextEditor;
	private Action sortAction;
	private TreeViewer treeViewer;
	private WorkbenchViewerSorter azSorter;
	private ViewerSorter lineSorter;

	class TreeObject implements IAdaptable {
		private String name;
		private String functor;
		private Position position;
		private TreeParent parent;
		
		public TreeObject(String name, String functor, Position position) {
			this.name = name;
			this.functor = functor;
			this.position = position;
		}
		public String getName() {
			return name;
		}
		public String getFunctor() {
			return functor;
		}
		public Position getPosition() {
			return position;
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
		public TreeParent(String name, String functor, Position position) {
			super(name, functor, position);
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

	/**
	 * 
	 */
	protected class ContentProvider implements ITreeContentProvider {

		protected final static String SEGMENTS= "__prolog_segments";
		protected IPositionUpdater fPositionUpdater= new DefaultPositionUpdater(SEGMENTS);
		protected TreeParent root = new TreeParent("", "", new Position(0, 0));
		private IEditorInput input;
		
		public ContentProvider(IEditorInput input) {
			this.input = input;
		}
		
		protected void parse(IDocument document) {
			LogicServer ls = null;
			long term, nextTerm, list;
      		int line, nextLine;
      		TreeParent pred;

			IFile file = (IFile)input.getAdapter(IFile.class);
			IProject project = file.getProject();
			if (project == null)
				return;
			
			try {
				try {
					// Eliminate old markers
					file.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
				}
				catch (CoreException ex) {
					// Ignore it
				}

				String amziDir = PrologCorePlugin.getAmziDir();
				ls = new LogicServer();

				synchronized (ls) {
					// If there is an amzi.cfg then specify it
					IFile cfgFile = project.getFile("amzi.cfg");
					if (cfgFile.exists())
						ls.Init(Utils.tiltSlashes(cfgFile.getLocation().toOSString()));
					else
						ls.Init2("heap=2000000");
					ls.AddLSX("aosutils", 0);
	//				String s = Utils.tiltSlashes(PrologUIPlugin.getFilePath("amzijide.xpl"));
					String s = amziDir+"abin"+System.getProperty("file.separator")+"acmp.xpl";
					ls.Load(s);
	
	/*				// Open the underlying file
					s = "open(`"+Utils.tiltSlashes(file.getLocation().toOSString())+
						"`, read, _ID, type(text))";
					term = ls.ExecStr(s);
	
					// It doesn't exist, we're done
					if (term == 0) {
						ls.Close();
						ls = null;
						System.gc();
						return;
					} 
											
					// Parse the file
					String projectPathname = project.getLocation().toOSString();
					id = ls.GetIntArg(term, 3);
					s = "parse:parse(`" + Utils.tiltSlashes(projectPathname) 
						+ "`, "	+ new Integer(id).toString() +  ", _List)";
					term = ls.ExecStr(s);
	*/
	
					// Parse the file
					String projectPathname = project.getLocation().toOSString();
					if (projectPathname != null) {
						if (file.isLinked())
							s = "amzi_system:outline_consult(`" + 
								Utils.tiltSlashes(projectPathname) + "`, `" +
								Utils.tiltSlashes(file.getLocation().toOSString()) + 
								"`, _LIST)";
						else
							s = "amzi_system:outline_consult(`" + 
								Utils.tiltSlashes(projectPathname) + "`, `" +
								Utils.tiltSlashes(file.getName()) + 
								"`, _LIST)";
						term = ls.ExecStr(s);
					}
					else
						term = 0;
					
					// Parsing failed
					if (term == 0) {
						ls.Close();
						ls = null;
						System.gc();
						return;
					} 
	
					// Build the outline
					list = ls.GetArg(term, 3);
											
					// Check for the empty list or an atom
	      			long type = ls.GetTermType(list);
					if (type == LogicServer.pLIST)  {
						// Walk the list
						String lastFunctor = "";
						int lastArity = -1;
						pred = root;
				      	while (list != 0)
				      	{		      		
				      		// Peek ahead to the next term
							term = ls.GetHead(list);
							list = ls.GetTail(list);
							if (list != 0)	
					      		nextTerm = ls.GetHead(list);
					      	else
					      		nextTerm = 0;
			      		
				      		// Get the data
				      		// item(Line, Functor, Arity, String)
				      		line = ls.GetIntArg(term, 1) - 1;
				      		if (nextTerm != 0)
				      			nextLine = ls.GetIntArg(nextTerm, 1) - 1;
				      		else
				      			nextLine = document.getNumberOfLines()-1;
				      			
				      		String functor = ls.GetStrArg(term, 2);
				      		int arity = ls.GetIntArg(term, 3);
				      		String desc = ls.GetStrArg(term, 4);
				      		
				      		// Add it to the tree
							int offset= document.getLineOffset(line);
							int end= document.getLineOffset(nextLine);
							int length = end - offset;
							Position p= new Position(offset, length);
	//						if (!functor.equalsIgnoreCase(":-")) {
								document.addPosition(SEGMENTS, p);
								if (!functor.equals(lastFunctor) || arity != lastArity) {
									if (!functor.equalsIgnoreCase(":-"))
										pred= new TreeParent(MessageFormat.format("{0}/{1}", 
											new Object[] { functor, new Integer(arity) }),
											functor, new Position(offset, 1));
									else
										pred= new TreeParent(MessageFormat.format("{0}", 
											new Object[] { functor, new Integer(arity) }),
											functor, new Position(offset, 1));								
									root.addChild(pred);
									lastFunctor = functor;
									lastArity = arity;
								}
								TreeObject obj = new TreeObject(desc,functor, p);
								pred.addChild(obj);
	//						}
				      	}
					}
	
					// Find the errors and warnings
					term = ls.CallStr("amzi_system:parse_message(Type, File, Line, Msg, Text)");
					if (term != 0) {
						boolean tf = true;
						while (tf) {
							boolean isError = false;
							if (ls.GetStrArg(term, 1).equalsIgnoreCase("error")) isError = true;
							addProblemMarker(file, isError, ls.GetStrArg(term, 4),
								ls.GetStrArg(term, 2), ls.GetIntArg(term, 3),
								ls.GetStrArg(term, 5));
							 tf = ls.Redo();
						}
					}
	
					ls.Close();
					ls = null;
					System.gc();
					return;
				}
			}
			catch (LSException ex) {
		    	try {
		    		// Close the file
//		    		if (id >= 0) {
//			    		String s = "close("+new Integer(id).toString()+")";
//						term = ls.ExecStr(s);
//						if (term == 0)
//							PrologUIPlugin.log(new Status(Status.ERROR, "com.amzi.prolog.ui", 301, "Unable to close outline document stream", ex));
//		    		}			

		    		synchronized (ex) {
						// Handle read errors and ide errors
						String msg="", readbuf="";
						int lineno=0;
						
						// What kind of error is it?
			      		int errtype = ex.GetType();
			      		//int rc = ex.GetRC();
	
						// Look for our special ide_errors
						if (errtype == LSException.EXEC) {
				      		msg = ex.GetMsg();
				      		int ideIdx = msg.indexOf("ide_error(");
				      		if (ideIdx >= 0) {
				      			long errterm = ls.StrToTerm(msg.substring(ideIdx));
//				      			String type = ls.GetStrArg(errterm,1);
				      			long errlist = ls.GetArg(errterm, 2);
				      			Properties errProps = Utils.prologListToProperties(ls, errlist, 10000);
				      			msg = errProps.getProperty("message");
				      			Integer.parseInt(errProps.getProperty("rc"));
				      			try {
									lineno = Integer.parseInt(errProps.getProperty("lineno"));
				      			} catch (NumberFormatException ex2) {
									try {
										lineno = Integer.parseInt(errProps.getProperty("line_number"));
									} catch (NumberFormatException ex3) {
										lineno = 0;
									}
				      			}
				      		}
				      		else {
								if (errtype != LSException.SECURITY) MessageDialog.openError(null, "Parse", ex.GetMsg());
								ls.Close();
								ls = null;
								System.gc();
								return;
				      		}								
						}
						
						// Get all the details about the error
						// This code will not run as they are wrapped into ide_error's
						if (errtype == LSException.READ) {
				      		msg = ex.GetMsg();
//				      		rc = ex.GetRC();
			    	  		readbuf = ex.GetReadBuffer();
			      			lineno = ex.GetLineno();
						}
			
						// Add it onto the task list
						if (errtype == LSException.READ || errtype == LSException.EXEC) {
							msg += " (parse)";
							IMarker marker = file.createMarker(IMarker.PROBLEM);
					 	    if (marker.exists()) {
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
						else {
							if (errtype != LSException.SECURITY) MessageDialog.openError(null, "Parse", ex.GetMsg());
							ls.Close();
							ls = null;
							System.gc();
							return;
						}
						
						ls.Close();
						ls = null;
						System.gc();	
						return;		
		    		}
			   	} 
		        catch (LSException ex2) {
		        	// Things are really bad now
		        	MessageDialog.openError(null, "Parse Error Handler", ex2.GetMsg());
					try { ls.Close(); } catch (LSException ex3) { };
					ls = null;
					System.gc();
					return;
		        }
		      	catch (CoreException cex) {
					// You need to handle the case where the marker no longer exists 
					try { ls.Close(); } catch (LSException ex3) { };
					ls = null;
					System.gc();
					return;
		        }
			}
			catch (BadPositionCategoryException ex) {
				try { ls.Close(); } catch (LSException ex2) { };
				ls = null;
				System.gc();
				return;
			} 
			catch (BadLocationException ex) {
				try { ls.Close(); } catch (LSException ex2) { };
				ls = null;
				System.gc();
				return;
			}
		}

		private void addProblemMarker(IFile file, boolean isError, String message, String filename, int lineno, String location) {
			if (file == null || !file.exists()) return;
		
			// Add it onto the task list
			try {
				IMarker markers[] = file.findMarkers(null, true, IResource.DEPTH_INFINITE);
				for (int i = 0 ; i < markers.length ; i++) {
					if (markers[i].getAttribute(IMarker.MESSAGE, "").equals(message) &&
						markers[i].getAttribute(IMarker.LINE_NUMBER, 0) == lineno)
						return;
				}
				IMarker marker = file.createMarker(IMarker.PROBLEM);
				if (marker.exists()) {
					marker.setAttribute(IMarker.MESSAGE, message + " (parse)");
					if (lineno != 0) {
						marker.setAttribute(IMarker.LINE_NUMBER, lineno);
					} else if (location != null && location.length() > 0) {
						marker.setAttribute(IMarker.LOCATION, location);
					}
				
					if (isError) {
						marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
						marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_HIGH);
					}
					else {
						marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
						marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_NORMAL);
					}
				} 
			}
			catch (CoreException e) {
			 // You need to handle the case where the marker no longer exists 
			}
		}

		/*
		 * @see IContentProvider#inputChanged(Viewer, Object, Object)
		 */
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			if (oldInput != null) {
				IDocument document= fDocumentProvider.getDocument(oldInput);
				if (document != null) {
					try {
						document.removePositionCategory(SEGMENTS);
					} 
					catch (BadPositionCategoryException x) {
					}
					document.removePositionUpdater(fPositionUpdater);
				}
			}

			// Clear out the tree
			root = new TreeParent("", "", new Position(0, 0));
//			fContent.clear();

			if (newInput != null) {
				IDocument document= fDocumentProvider.getDocument(newInput);
				if (document != null) {
					final IDocument fdocument = document;
					Runnable runnable = new Runnable(){
						public void run() {
							fdocument.addPositionCategory(SEGMENTS);
							fdocument.addPositionUpdater(fPositionUpdater);
							parse(fdocument);
						}
					};
					BusyIndicator.showWhile(null, runnable);
				}
			}
		}

		/*
		 * @see IContentProvider#dispose
		 */
		public void dispose() {
			// Clear out the tree
			if (root != null) {
				root = null;
			}			
/*			if (ls != null) {
				try {
					ls.Close();
					ls = null;
					System.gc();
				}
				catch (LSException ex) {
				}
			}
*/
		}

		/*
		 * @see IContentProvider#isDeleted(Object)
		 */
		public boolean isDeleted(Object element) {
			return false;
		}

		/*
		 * @see IStructuredContentProvider#getElements(Object)
		 */
		public Object[] getElements(Object element) {
//			return fContent.toArray();
			return root.getChildren();
		}

		/*
		 * @see ITreeContentProvider#hasChildren(Object)
		 */
		public boolean hasChildren(Object element) {
			if (element == fInput) return true;
			if (element instanceof TreeParent)
				return ((TreeParent)element).hasChildren();
			if (element instanceof TreeObject)
				return false;
			return false;
		}

		/*
		 * @see ITreeContentProvider#getParent(Object)
		 */
		public Object getParent(Object element) {
			if (element == root) return fInput;
			if (element instanceof TreeParent)
				return ((TreeParent)element).getParent();
			if (element instanceof TreeObject)
				return ((TreeObject)element).getParent();
				
			return null;
		}

		/*
		 * @see ITreeContentProvider#getChildren(Object)
		 */
		public Object[] getChildren(Object element) {
			if (element == fInput) return root.getChildren();
			if (element instanceof TreeParent)
				return ((TreeParent)element).getChildren();
			return new Object[0];
		}
	};

	// Provides a graphic with the text
	class ViewLabelProvider extends LabelProvider {

		public String getText(Object obj) {
			return obj.toString();
		}
		public Image getImage(Object obj) {
			String[] directives = PrologCorePlugin.getPrologKeywords().getDirectives();
			
			for (int i = 0 ; i < directives.length ; i++)
				if (directives[i].equals(((TreeObject)obj).getFunctor())) {
					if (obj instanceof TreeParent)	
						return PrologPluginImages.get(PrologPluginImages.IMG_OBJS_DIRECTIVES);
					else
						return PrologPluginImages.get(PrologPluginImages.IMG_OBJS_DIRECTIVE);
				}
			if (obj instanceof TreeParent) 
				return PrologPluginImages.get(PrologPluginImages.IMG_OBJS_PREDICATE);
			return PrologPluginImages.get(PrologPluginImages.IMG_OBJS_CLAUSE);
		}
	}

	public class LineSorter extends ViewerSorter {
	
		/* (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ViewerSorter#compare(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
		 */
		public int compare(Viewer viewer, Object e1, Object e2) {
//			LabelProvider labelProvider = (LabelProvider)((TreeViewer)viewer).getLabelProvider();
			int offset1 = ((TreeObject)e1).getPosition().offset;
			int offset2 = ((TreeObject)e2).getPosition().offset;
			return offset1-offset2;
		}
	
	}

	/**
	 * Creates a content outline page using the given provider and the given editor.
	 */
	public PrologContentOutlinePage(IDocumentProvider provider, ITextEditor editor) {
		super();
		fDocumentProvider= provider;
		fTextEditor= editor;
	}
	
	/* (non-Javadoc)
	 * Method declared on ContentOutlinePage
	 */
	public void createControl(Composite parent) {
		super.createControl(parent);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(), "com.amzi.prolog.ui.outline");
		
		azSorter = new WorkbenchViewerSorter();
		lineSorter = new LineSorter();
		
		treeViewer= getTreeViewer();
		treeViewer.setContentProvider(new ContentProvider((IEditorInput)fInput));
		treeViewer.setLabelProvider(new ViewLabelProvider());
		treeViewer.addSelectionChangedListener(this);

		makeActions();
		contributeToActionBars();
		
		if (fInput != null)
			treeViewer.setInput(fInput);
			
	}
	
	private void contributeToActionBars() {
		IActionBars bars = getSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}
	
	private void fillLocalPullDown(IMenuManager manager) {
		manager.add(sortAction);
	}
	
/*	private void fillContextMenu(IMenuManager manager) {
		manager.add(sortAction);
	}
*/	
	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(sortAction);
	}

	private void makeActions() {
		sortAction = new Action("Sort", Action.AS_CHECK_BOX) {
			public void run() {
				Runnable runnable = new Runnable(){
					public void run() {
						// Switch to A-Z order
						if (sortAction.isChecked())
							treeViewer.setSorter(azSorter);
						// Switch to line order
						else
							treeViewer.setSorter(lineSorter);
						treeViewer.refresh();							
					}
				};
				BusyIndicator.showWhile(null, runnable);
			}
		};
		sortAction.setToolTipText("Sort Outline Alphabetically or by Line Number");
		sortAction.setImageDescriptor(PrologPluginImages.DESC_OBJS_AZSORT);
		sortAction.setChecked(false);
	}

	/* (non-Javadoc)
	 * Method declared on ContentOutlinePage
	 */
	public void selectionChanged(SelectionChangedEvent event) {

		super.selectionChanged(event);

		ISelection selection= event.getSelection();
		if (selection.isEmpty())
			fTextEditor.resetHighlightRange();
		else {
			TreeObject element = (TreeObject) ((IStructuredSelection) selection).getFirstElement();
			int start = element.getPosition().getOffset();
			int length = element.getPosition().getLength();
			try {
				fTextEditor.setHighlightRange(start, length, true);
			} catch (IllegalArgumentException x) {
				fTextEditor.resetHighlightRange();
			}
		}
	}
	
	/**
	 * Sets the input of the outline page
	 */
	public void setInput(Object input) {
		fInput= input;
		update();
	}
	
	/**
	 * Updates the outline page.
	 */
	public void update() {
		TreeViewer viewer= getTreeViewer();

		if (viewer != null) {
			Control control= viewer.getControl();
			if (control != null && !control.isDisposed()) {
				control.setRedraw(false);
				viewer.setInput(fInput);
//				viewer.expandAll();
				control.setRedraw(true);
			}
		}
	}
}

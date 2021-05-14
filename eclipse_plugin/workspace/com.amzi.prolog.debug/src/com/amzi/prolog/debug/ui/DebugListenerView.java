package com.amzi.prolog.debug.ui;

import com.amzi.prolog.debug.core.internal.DebugListener;
import com.amzi.prolog.debug.core.internal.DebugListenerActionBuffer;
import com.amzi.prolog.debug.core.internal.DebugListenerInputBuffer;
import com.amzi.prolog.debug.core.internal.DebugListenerKeyBuffer;
import com.amzi.prolog.debug.core.model.PrologDebugTarget;

//import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
//import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.ui.AbstractDebugView;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
//import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class DebugListenerView extends AbstractDebugView /*ViewPart*/ 
	implements KeyListener, VerifyListener {
		
//	private PrologDebugTarget debugTarget;
	private Text outputText;
	private DebugListener debugListener = null;
	private String greeting = null;
	private String projectName;
	private ArrayList lineList = new ArrayList();
	private int lineIdx = -1;
	private int inputStartPosition = 0;
	private DebugListenerInputBuffer input = new DebugListenerInputBuffer();
	private DebugListenerKeyBuffer key = new DebugListenerKeyBuffer();
	private DebugListenerActionBuffer action = new DebugListenerActionBuffer();
	private boolean keyboardSingleKeyMode = false;
	private boolean changingInputLine = false;
//	private boolean running = false;
	private boolean timeToStop = false;
	private boolean timeToPause = false;
	private boolean waitingForInput = false;
	private Action copyAction, cutAction, pasteAction, selectAllAction;

	/**
	 * Constructor for ListenerView.
	 */
	public DebugListenerView() {
		super();
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	public void createPartControl(Composite parent) {
		PlatformUI.getWorkbench().getHelpSystem().setHelp(parent, "com.amzi.prolog.debug.debug_listener");

		// Create a new composite
//		Composite composite = new Composite(parent, SWT.CENTER /*| SWT.V_SCROLL*/);
		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
//		composite.setLayout(layout);
		parent.setLayout(layout);
/*		GridData data = new GridData();
		data.verticalAlignment = GridData.FILL;
		data.horizontalAlignment = GridData.FILL;
//		composite.setLayoutData(data);
		parent.setLayoutData(data);
*/
		// Create the output
//		outputText = new Text(composite, SWT.LEFT | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		outputText = new Text(parent, SWT.LEFT | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		GridData outputGridData = new GridData();
		outputGridData.grabExcessHorizontalSpace = true;
		outputGridData.grabExcessVerticalSpace = true;
		outputGridData.horizontalAlignment = GridData.FILL;
		outputGridData.verticalAlignment = GridData.FILL;
		outputText.setLayoutData(outputGridData);
		outputText.setFont(JFaceResources.getTextFont());
		outputText.setEditable(false);
		outputText.setVisible(true);
		outputText.addVerifyListener(this);		
		outputText.addKeyListener(this);

		makeActions();
	}

	public void start(PrologDebugTarget debugTarget, String projectPathname, 
		String cfgPathname, String projectName, String xplPathname,	String consultList, String loadList, 
		String lsxList) {
			
//		this.debugTarget = debugTarget;
		this.projectName = projectName;

		// Have the user finish up the old listener first
		if (debugListener != null && debugListener.isAlive())	{
			MessageDialog.openError(Display.getDefault().getActiveShell(), "Error",
				"Exit prior Debug Listener before starting a new one. (Type 'quit.' to end the listener)");
			return;
		}

		// Reset the input buffers
		key.reset();
		input.reset();
		action.reset();
		
		// Set all the flags
		keyboardSingleKeyMode = false;
		changingInputLine = false;
		setTimeToStop(false);
		setTimeToPause(false);

		// Keep a queue of our input lines
		lineList = new ArrayList();
		lineIdx = -1;
		
		// Construct a greeting
		greeting = "Debugging Project: " + projectName + "\n";
		if (lsxList == null) lsxList = "";
		greeting = greeting + "  Loading Extensions: " + lsxList;
		if (lsxList.toLowerCase().indexOf("aosutils") < 0)
			greeting = greeting + " 'aosutils' (always loaded in IDE)\n";
		else
			greeting = greeting + "\n";
		if (loadList != null && loadList.length() > 0)
			greeting = greeting + "  Loading Libraries: " + loadList + "\n";
			if (xplPathname != null)
				greeting = greeting + "  Loading: " + xplPathname + "\n";
		if (consultList != null && consultList.length() > 0)
			greeting = greeting + "  Consulting Source Files: ";
		
//		final String fgreeting = greeting;
		// Display.getCurrent() is null! so use default instead
		// NOTE!!! Must not use AbstractDebugView.syncExec does not run
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				// Let the user know we're ready for input
//				outputText.setText(fgreeting);
				outputText.setEditable(true);
				outputText.setFocus();
			}
		});

		// Create a new listener and pass it our buffers
		debugListener = new DebugListener(debugTarget, projectPathname,  
			cfgPathname, xplPathname, consultList, loadList, lsxList, 
			this, input, key, action);

		// Set the focus to the listener
		final DebugListenerView fthis = this;
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				fthis.setFocus();
			}
		});
//		setFocus();
/*		try {
			new ProgressMonitorDialog(Display.getDefault().getActiveShell()).run(true, true, debugListener);
		} catch (InterruptedException ex) {
		} catch (InvocationTargetException ex) {
		}*/
		debugListener.start();
	}

	// VerifyListener
	// synchronized?
	public void verifyText(org.eclipse.swt.events.VerifyEvent event) {

		// If were not running ignore everything
		if (!isRunning()) {
			event.doit = false;
			return;
		}
		
		// Let the editor handle it
		event.doit = true;
				
		// Skip returns (in case cursor is in middle of line)
/*		Hangs under Ubuntu GTK
 		if (event.keyCode == SWT.CR) {
			outputText.append("\n");
			event.doit = false;
			return;
		}
*/		
		// Let our arrows through
		if (event.keyCode == SWT.ARROW_UP || event.keyCode == SWT.ARROW_DOWN)
			return;
					
		//	Backspace or Left Arrow
		if (event.character == SWT.BS || event.keyCode == SWT.ARROW_LEFT) {	
			if (outputText.getCaretPosition() <= inputStartPosition)
				event.doit = false;
				return;
		}
				
		// Don't process text lines
		if (Character.getNumericValue(event.character) == -1) {
			return;
		}

		// If they moved out of the input zone, ignore them
		if (outputText.getCaretPosition() < inputStartPosition &&
			!changingInputLine) {
				outputText.setSelection(outputText.getCharCount());
				event.doit = false;
				return;
		}
	}

	// KeyListener
	public void keyPressed(KeyEvent event) { 
		// If keyb is waiting return the next key
		if (keyboardSingleKeyMode) {
			setSingleKeyMode(false);
			key.put(event.character);
			setInputStartPosition(outputText.getCaretPosition());
			return;
		}	
	}
	
	public void keyReleased(KeyEvent event) {

/*		// See if we are running
		if (!debugListener.isAlive()) {
				stopListener();
				return;
			}
*/
		if (event.character == 3) {			// Break
			setTimeToStop(false);			// No double breaks
			input.put("\003\n");
		}
			
		// Don't put lines in the input queue when the listener isn't listening
		// or we hang up waiting for them to be read
		if (event.character == SWT.CR && outputText.getEditable()) { 	// Enter
			int end = outputText.getCharCount() - 1;
			String line = outputText.getText(inputStartPosition, end);
			input.put(line);
			lineList.add(line.trim());
			lineIdx = lineList.size()-1;
		}

		// Arrows bring back prior lines
		if (event.keyCode == SWT.ARROW_UP) {	// Up
					
			// The first up arrow
			if (lineIdx < 0)
				lineIdx = lineList.size()-1;
			// Subsequent up arrows
			else if (lineIdx > 0)
				lineIdx = lineIdx - 1;
					
			changingInputLine = true;
			int end = outputText.getCharCount();
			if (end > inputStartPosition) {
				outputText.setSelection(inputStartPosition, end);
				outputText.cut();
			}
			outputText.append((String)lineList.get(lineIdx));
			changingInputLine = false;
		}

		if (event.keyCode == SWT.ARROW_DOWN) {	// Down
					
			// Going down
			if (lineIdx >= 0 && lineIdx < lineList.size()-1)
				lineIdx = lineIdx + 1;
						
			changingInputLine = true;
			int end = outputText.getCharCount();
			if (end > inputStartPosition) {
				outputText.setSelection(inputStartPosition, end);
				outputText.cut();
			}
			outputText.append((String)lineList.get(lineIdx));
			changingInputLine = false;
		}
	}

	// Called when the listener ends
	public void endListener() {
		debugListener = null;
		
/*		// See if its already done
		if (debugListener == null) return;
				
		// Tell them we're done (if we're not terminating via dispose)
			final DebugListenerView fthis = this;
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					try {
						String endText = "\n\n*** Debug Listener Finished: Select Debug|Debug to Restart ***";
						outputText.append(endText);

						// Remove the key listener, verify is needed to stop input
						outputText.removeKeyListener(fthis);
						
						outputText.setEnabled(true);
						outputText.setEditable(false);
						
						// Close all the editors we created
//						PrologDebugTarget.closeDebugEditors();
					}
					catch (Exception ex) {
					}
				}
			});

		Thread.yield();
*/	}
	
	public void dispose() {
		// Warn the user if we cannot kill the listener
		if (debugListener != null && debugListener.isAlive()) {
			MessageDialog.openError(Display.getDefault().getActiveShell(), "Warning",
				"The debug listener is still running. Closing the view does not terminate execution and will use up memory (until you exit Eclipse). We recommend that you do not do this in future unless the listener and your program have ended.");
		}

		super.dispose();
	}

	public void addProblemMarker(boolean isError, String message, String filename, int lineno, String location) {
		IProject project = (IProject)ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
		IResource file = project.findMember(filename);
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

	public void clearProblemMarkers() {
		IProject project = (IProject)ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
		
		// Remove all our special debug parse markers
		try {
			IMarker markers[] = project.findMarkers(null, true, IResource.DEPTH_INFINITE);
			for (int i = 0 ; i < markers.length ; i++) {
				if (markers[i].getAttribute(IMarker.MESSAGE, "").indexOf("(parse)") >= 0)
					markers[i].delete();
			}
		}
		catch (CoreException e) {
		 // You need to handle the case where the marker no longer exists 
		}
	}
	
	/**
	 * Passing the focus request to the viewer's control.
	 */
	public void setFocus() {
		outputText.setFocus();
	}

//	public DebugListener getListenerThread() {
//		return debugListener;
//	}
	
	/**
	 * @see org.eclipse.debug.ui.AbstractDebugView#configureToolBar(org.eclipse.jface.action.IToolBarManager)
	 */
	protected void configureToolBar(IToolBarManager tbm) {
	}

	/**
	 * @see org.eclipse.debug.ui.AbstractDebugView#createActions()
	 */
	protected void createActions() {
	}

	private void makeActions() {
		copyAction = new Action() {
			public void run() {
				outputText.copy();
			}
		};
		cutAction = new Action() {
			public void run() {
				if (outputText.getCaretPosition() >= inputStartPosition)
					outputText.cut();
			}
		};
		pasteAction = new Action() {
			public void run() {
				if (outputText.getCaretPosition() >= inputStartPosition)
					outputText.paste();
			}
		};
		selectAllAction = new Action() {
			public void run() {
				outputText.selectAll();
			}
		};

		// Retarget the main menu edit commands		
		getViewSite().getActionBars().setGlobalActionHandler(
			ActionFactory.COPY.getId(),
			copyAction);
		getViewSite().getActionBars().setGlobalActionHandler(
			ActionFactory.CUT.getId(),
			cutAction);
		getViewSite().getActionBars().setGlobalActionHandler(
			ActionFactory.PASTE.getId(),
			pasteAction);
		getViewSite().getActionBars().setGlobalActionHandler(
			ActionFactory.SELECT_ALL.getId(),
			selectAllAction);

	}

	/**
	 * @see org.eclipse.debug.ui.AbstractDebugView#createViewer(org.eclipse.swt.widgets.Composite)
	 */
	protected Viewer createViewer(Composite parent) {
		return null;
	}

	/**
	 * @see org.eclipse.debug.ui.AbstractDebugView#fillContextMenu(org.eclipse.jface.action.IMenuManager)
	 */
	protected void fillContextMenu(IMenuManager menu) {
	}

	/**
	 * @see org.eclipse.debug.ui.AbstractDebugView#getHelpContextId()
	 */
	protected String getHelpContextId() {
		return null;
	}

	public Text getText() {
		return outputText;
	}
	
	public void appendText(String text) {
		if (greeting != null) {
			outputText.setText(greeting);
			greeting = null;
		}
		outputText.append(text);
	}
	
	public void setInputEnabled(boolean flag) {
		if (flag)
			waitingForInput = true;
		else
			waitingForInput = false;
		final boolean fflag = flag;
		Display.getDefault().syncExec(new Runnable() {
			public void run() {
				try {
					if (fflag) setInputStartPosition();
					outputText.setEditable(fflag);
					if (fflag) outputText.forceFocus();
				}
				catch (Exception ex) {
				}
			}
		});
	}
	
	public boolean isInputEnabled() {
		return waitingForInput;
	}
	
	public void forceUserInput(String text) {
		input.put(text);
	}
	
	public void setSingleKeyMode(boolean mode) {
		keyboardSingleKeyMode = mode;
	}
	
	public void setInputStartPosition() {
		inputStartPosition = outputText.getCaretPosition();
	}
	public void setInputStartPosition(int pos) {
		inputStartPosition = pos;
	}

	public void actionReady(String s) {
		action.put(s);
	}
	
	public void setTimeToStop(boolean flag) {
		timeToStop = flag;
	}
	public boolean isTimeToStop() {
		return timeToStop;
	}
	public void setTimeToPause(boolean flag) {
		timeToPause = flag;
	}
	public boolean isTimeToPause() {
		return timeToPause;
	}
/*	private void setRunning(boolean running) {
		this.running = running;
	}*/
	public boolean isRunning() {
		if (debugListener != null && debugListener.isAlive())
			return true;
		else
			return false;
	}


}

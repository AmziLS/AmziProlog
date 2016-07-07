package com.amzi.prolog.ui.launch;

import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.ui.internal.Listener;
import com.amzi.prolog.ui.internal.ListenerInputBuffer;
import com.amzi.prolog.ui.internal.ListenerKeyBuffer;
import com.amzi.prolog.ui.PrologPluginImages;
import com.amzi.prolog.ui.PrologUIPlugin;

import java.util.ArrayList;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.part.ViewPart;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class ListenerView extends ViewPart implements VerifyListener, KeyListener {
	public Text outputText;
	private Listener listener = null;
//	private String projectName;
	private ArrayList lineList = new ArrayList();
	private int lineIdx = -1;
	private int inputStartPosition = 0;
	private ListenerInputBuffer input;
	private ListenerKeyBuffer key;
	private String greeting = null;
	private boolean keyboardSingleKeyMode = false;
	private boolean changingInputLine = false;
	private boolean running = false;
	private boolean timeToStop = false;
	private ILaunch launch = null;
	private Action startAction, endAction;
	private Action copyAction, cutAction, pasteAction, selectAllAction;
	private Action statusAction;
	
	/**
	 * Constructor for ListenerView.
	 */
	public ListenerView() {
		super();
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	public void createPartControl(Composite parent) {
		PlatformUI.getWorkbench().getHelpSystem().setHelp(parent, "com.amzi.prolog.ui.listener");

/*		Font font = JFaceResources.getTextFont();
		FontData fontData;
		
		// Get preferences for font
		IPreferenceStore store = PrologUIPlugin.getDefault().getPreferenceStore();
		if (store.contains(JFaceResources.TEXT_FONT)) {
			fontData= PreferenceConverter.getFontData(store, JFaceResources.TEXT_FONT);				
			if (fontData != null)
				font = new Font(Display.getDefault(), fontData);
		}
*/
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
		
		// Put us on the menu and toolbar, and create our own menu and toolbar
		makeActions();
		contributeToActionBars();

		// Create our buffers
		input = new ListenerInputBuffer();
		key = new ListenerKeyBuffer();
		
		// Not running
		setRunning(false);
	}

	public void start(ILaunch launch, String title, String projectPathname, 
		String cfgPathname, String projectName, String xplPathname, 
		String consultList, String loadList, String lsxList) {
			
		// Remove the old launch first, if it exists
		if (this.launch != null)
			DebugPlugin.getDefault().getLaunchManager().removeLaunch(this.launch);
			
		this.launch = launch;
//		this.projectName = projectName;
		
		// Have the user finish up the old listener first
		if (listener != null && listener.isAlive())	{
			MessageDialog.openError(Display.getDefault().getActiveShell(), "Error",
				"Exit prior listener or run session before starting a new one. (Type 'quit.' to end the listener)");
			return;
		}

		// Reset the input buffers
		key.reset();
		input.reset();
		
		// Set all the flags
		keyboardSingleKeyMode = false;
		changingInputLine = false;
		setTimeToStop(false);

		// Keep a queue of our input lines
		lineList = new ArrayList();
		lineIdx = -1;
		
		// Set the title on the tab and window
		setPartName(title);
		setContentDescription(title + " " + projectName);

		// Generate a greeting
		
		greeting = "";
				
		greeting += "\nInterpreting project: " + projectName + "\n";
		if (xplPathname == null || xplPathname.length() == 0) {
			if (lsxList == null) lsxList = "";
			greeting = greeting + "  Loading Extensions: " + lsxList;
			if (lsxList.toLowerCase().indexOf("aosutils") < 0)
				greeting = greeting + " aosutils.lsx (always loaded in IDE)\n";
			else
				greeting = greeting + "\n";
			if (loadList != null && loadList.length() > 0)
				greeting = greeting + "  Loading Libraries: " + loadList + "\n";
			if (consultList != null && consultList.length() > 0)
				greeting = greeting + "  Consulting Source Files: " ;
		}
		else {
			greeting = "";
		}
		

		
		
		
		
		
		
		
		
		

		// Note: Setting the greeting here by outputText.setText(greeting) does
		// not work. So it is done in appendText()
				
		// Create a new listener and pass it our buffers
		listener = new Listener(projectPathname, cfgPathname, xplPathname, consultList, 
			loadList, lsxList, this, input, key);

		// Set the focus to the listner
		final ListenerView fthis = this;
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				fthis.setFocus();
			}
		});
//		setFocus();
		
		// Let the user know we're ready for input
		outputText.setEditable(true);
		outputText.setFocus();

		// Initialize and start the listener (as a thread)
		listener.start();
		setRunning(true);
	}

	// VerifyListener
	// synchronized?
	public synchronized void verifyText(org.eclipse.swt.events.VerifyEvent event) {

		// If were not running ignore everything
		if (!running) {
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
		// if its a real key (not a shift, control, alt, etc.)
		if (keyboardSingleKeyMode) {
			if (event.character == 0) return;
			setSingleKeyMode(false);
			key.put(event.character);
			setInputStartPosition(outputText.getCaretPosition());
			return;
		}	
	}

	public synchronized void keyReleased(KeyEvent event) {
		
		// If keyb is waiting return the next key
		// if its a real key (not a shift, control, alt, etc.)
/*		if (keyboardSingleKeyMode) {
			if (event.character == 0) return;
			setSingleKeyMode(false);
			key.put(event.character);
			setInputStartPosition(outputText.getCaretPosition());
			return;
		}
*/
		if (running && event.character == 3) {			// Break
			setTimeToStop(false);			// No double breaks
			input.put("\003\n");
		}
	
		// Don't put lines in the input queue when the listener isn't listening
		// or we hang up waiting for them to be read
		if (event.character == SWT.CR && outputText.getEditable() && !keyboardSingleKeyMode) { 	// Enter
			int end = outputText.getCharCount() - 1;
			String line = outputText.getText(inputStartPosition, end);
			input.put(line);
			
			// Don't save blank lines in the queue
			if (line.trim().length() > 0) {
				lineList.add(line.trim()); 
				lineIdx = lineList.size()-1;
			}
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
			if (lineIdx >= 0)
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
			if (lineIdx >= 0)
				outputText.append((String)lineList.get(lineIdx));
			changingInputLine = false;
		}
	}
	
	// Clean up when the listener has properly ended
	public void endListener() {
		setRunning(false);
		listener = null;
	}
	
	public void dispose() {
		// Warn the user if we cannot kill the listener
		if (listener != null && listener.isAlive()) 
			MessageDialog.openError(Display.getDefault().getActiveShell(), "Warning",
				"The listener or your program is still running. Closing the view does not terminate execution and will use up memory (until you exit Eclipse). We recommend that you do not do this in future unless the listener and your program have ended.");
		super.dispose();
	}
	
	// Send a break to the Prolog engine
	private void breakListener() {
		// See if its still alive
		if (listener == null) return;
		if (!listener.isAlive()) return;
		
		setTimeToStop(true);
		
/*		// Force control-C's into the input buffers by interrupting the thread
		// See ListenerInputBuffer
		listener.interrupt();
		
		// Let others run
		Thread.yield();
		input.reset();
		input.put("\003\n");
		Thread.yield();
		key.reset();
		key.put('\003');
		Thread.yield();
		try { 
			if (listener != null && listener.isAlive())
				Thread.sleep(500);
			Thread.yield();
		}
		catch (InterruptedException ex) {
		}
*/
	}

	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());

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
		
		getViewSite().getActionBars().setGlobalActionHandler(
			"Status", statusAction);
	}

	private void fillLocalPullDown(IMenuManager manager) {
		manager.add(startAction);
		manager.add(endAction);
		manager.add(statusAction);
//		manager.add(new Separator());
//		manager.add(copyAction);
	}
	
	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(startAction);
		manager.add(endAction);
//		manager.add(copyAction);
	}

	private void makeActions() {
		startAction = new Action() {
			public void run() {
				String workspacePathname = ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString();
				start(null, "Listener", workspacePathname, "", "", "", "", "", "");
			}
		};
		startAction.setText("Start");
		startAction.setToolTipText("Starts the interpreter");
		startAction.setImageDescriptor(PrologPluginImages.DESC_OBJS_START);

		endAction = new Action() {
			public void run() {
				if (outputText.getEditable() == true) {
					setTimeToStop(false);			// No double breaks
					if (keyboardSingleKeyMode)
						key.put('\003');
					else
						input.put("\003\n");
//					MessageDialog.openInformation(Display.getDefault().getActiveShell(), "Note",
//					"The listener is waiting for input. You can type [Ctrl]-C and/or 'quit.' at the ?- prompt to end the listener.");
				}
				else
					breakListener();
			}
		};
		endAction.setText("Break");
		endAction.setToolTipText("Sends a break signal");
		endAction.setImageDescriptor(PrologPluginImages.DESC_OBJS_STOP);
		endAction.setEnabled(false);
		
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
		
		statusAction = new Action() {
			public void run() {
				getStatus();
			}
		};
		statusAction.setText("Debug Info");
	}

	public void getStatus() {
		MessageDialog.openInformation(this.getSite().getShell(), "Debug Info", listener.getStatus());
	}
	
/*	private void showMessage(String message) {
		MessageDialog.openInformation(
			outputText.getShell(),
			"Listener",
			message);
	}
*/
/*	public void addProblemMarker(String message, String filename, int lineno, String location) {
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
				marker.setAttribute(IMarker.MESSAGE, message);
				if (lineno != 0) {
					marker.setAttribute(IMarker.LINE_NUMBER, lineno);
				} else if (location != null && location.length() > 0) {
					marker.setAttribute(IMarker.LOCATION, location);
				}
				marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
				marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_HIGH);
			} 
		}
		catch (CoreException e) {
		 // You need to handle the case where the marker no longer exists 
		}
	}
*/
	/**
	 * Passing the focus request to the viewer's control.
	 */
	public void setFocus() {
		outputText.setFocus();
	}
	
	public synchronized void appendText(String text) {
		if (greeting != null) {
			outputText.setText(greeting);
			greeting = null;
		}
		outputText.append(text);
	}
	
	public void setSingleKeyMode(boolean mode) {
		keyboardSingleKeyMode = mode;
	}
	
	public void setInputEnabled(boolean flag) {
		final boolean fflag = flag;
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				try {
					if (fflag) setInputStartPosition();
//					if (Platform.getWS() == Platform.WS_GTK) {
//						outputText.setEditable(true);
//						outputText.setEnabled(true);
//						outputText.setVisible(true);
//					}
//					else
					outputText.setEditable(fflag);
					if (fflag) outputText.forceFocus();
				}
				catch (Exception ex) {
				}
			}
		});
	}

	public synchronized void setInputStartPosition() {
		inputStartPosition = outputText.getCaretPosition();
	}
	
	public synchronized void setInputStartPosition(int pos) {
		inputStartPosition = pos;
		outputText.forceFocus();
	}
	
	public synchronized void setTimeToStop(boolean flag) {
		timeToStop = flag;
	}
	
	public boolean isTimeToStop () {
		return timeToStop;
	}
	
	public synchronized void setRunning(boolean running) {
		this.running = running;
		if (running) {
			startAction.setEnabled(false);
			endAction.setEnabled(true);
		}
		else {
			startAction.setEnabled(true);
			endAction.setEnabled(false);
						
			// Tell the launch we're done
			if (launch != null)
				try {
					launch.terminate();
					// Don't do this, Eclipse doesn't clean up properly!
//					DebugPlugin.getDefault().getLaunchManager().removeLaunch(launch);
					launch = null;
				}
				catch (DebugException ex) {
					PrologUIPlugin.log(ex);
				}
		}		
	}
}

package com.amzi.prolog.debug.core.model;

import com.amzi.prolog.core.utils.Utils;
import com.amzi.prolog.debug.core.internal.DebugListenerActionBuffer;
import com.amzi.prolog.debug.core.internal.RemoteListener;
import com.amzi.prolog.debug.core.internal.SlowStep;
import com.amzi.prolog.debug.PrologDebugPlugin;
import com.amzi.prolog.debug.ui.DebugListenerView;

import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IBreakpointListener;
import org.eclipse.debug.core.IBreakpointManager;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugElement;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IMemoryBlock;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStep;
import org.eclipse.debug.core.model.IThread;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IPerspectiveDescriptor;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.swt.widgets.Display;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */

public class PrologDebugTarget extends PlatformObject
	implements IDebugElement, IDebugTarget, IStep, IDebugEventSetListener, IBreakpointListener {
		
	private ILaunchConfiguration config;
	private ILaunch launch;
	private PrologDebugProcess process = null;
	private PrologDebugThread thread = null;
	private String targetName;
	private int debugPort;
	private DebugListenerView debugListenerView;
	private RemoteListener remoteListener;
	private DebugListenerActionBuffer remoteActionBuffer;
	private boolean connected = true;
	private boolean remote = false;
	private boolean stepping = true;
	private boolean suspended = false;
	private boolean terminated = false;
	private int debugWhat;
	private SlowStep slowStepper = null;
	private int slowStepperSpeed = SlowStep.MEDIUM;
	private IAction slowStepAction = null, openListenerAction = null;
	private boolean initActions = false;
	private String binPathname;
	
	public final static int DEBUG_XPL = 1;
	public final static int DEBUG_PRO = 2;
	public final static String THREAD_SUSPEND_STEP_END = "thread_suspend_step_end";
	public final static String THREAD_SUSPEND_BREAKPOINT = "thread_suspend_breakpoint";
	public final static String PROCESS_CREATE = "process_create";
	public final static String TARGET_CREATE = "target_create";
	public final static String THREAD_CREATE = "thread_create";
	public final static String STEP_INTO = "step_into";
	public final static String STEP_OVER = "step_over";
	public final static String STEP_RETURN = "step_return";	
	public final static String TERMINATE = "stop";
	public final static String RESUME = "jump";
	public final static String SET_LEASH = "set_leash";
	public final static String CLEAR_LEASH = "clear_leash";
	public final static String SET_BREAK = "set_break";
	public final static String CLEAR_BREAK = "clear_break";
	public final static String SUSPEND = "suspend";
	public final static String OPEN_LISTENER = "open_listener";
	public final static String DCG_DISPLAY = "dcg_display";
	public final static String CUT_DISPLAY = "cut_display";
	
	/**
	 * Constructor for PrologDebugTarget.
	 */
	public PrologDebugTarget(int type, ILaunchConfiguration config, ILaunch launch) {
		super();
		this.config = config;
		this.launch = launch;
		this.debugWhat = type;
		
		// See if this is a remote session
		try {
			debugPort = config.getAttribute("debugPort", 0);
		} 
		catch (CoreException ex) {
			MessageDialog.openError(Display.getDefault().getActiveShell(), "Internal Error",
				"Unable to get values for remote port in PrologDebugTarget()");
		}

		if (debugPort != 0) 
			remote = true;
				
		// Open the debug perspective by force. Setting it in the config does
		// not open it before this code runs
		IPerspectiveDescriptor debugDescriptor = PlatformUI.getWorkbench().getPerspectiveRegistry().
			findPerspectiveWithId(IDebugUIConstants.ID_DEBUG_PERSPECTIVE);
		IWorkbenchPage page = PrologDebugPlugin.getDefault().getActivePage();
		if (page != null)
			page.setPerspective(debugDescriptor);
		
//		IWorkbench workbench = PlatformUI.getWorkbench();
//		workbench.showPerspective(IDebugUIConstants.ID_DEBUG_PERSPECTIVE);
		
		// Get our debug listener up
		showDebugView("com.amzi.prolog.debug.ui.DebugListenerView");
		
		// Find our listener view
		debugListenerView = null;
		IViewReference vr[] = page.getViewReferences();
		for (int i = 0 ; i < vr.length ; i++)
			if (vr[i].getId().equals("com.amzi.prolog.debug.ui.DebugListenerView")) {
				// Try to restore it
				debugListenerView = (DebugListenerView)vr[i].getView(true);
			}
			
		// Stop the old debug listener first
		if (debugListenerView != null && debugListenerView.isRunning()) {
			MessageDialog.openError(Display.getDefault().getActiveShell(), "Error",
				"Exit prior debug listener or run session before starting a new one. (Type 'quit.' to end the listener)");
			DebugPlugin.getDefault().getLaunchManager().removeLaunch(launch);
			return;
		}
		
		// Listen and learn
		DebugPlugin.getDefault().addDebugEventListener(this);
		DebugPlugin.getDefault().getBreakpointManager().addBreakpointListener(this);
		
		try {
			targetName = config.getAttribute("projectName", "Prolog Project");
			binPathname = config.getAttribute("binPathname", "");
		}
		catch (CoreException ex) {
			targetName = "Prolog Project";
		}
		if (targetName.trim().length() == 0)
			targetName = "Prolog Project";

		// For debug
		fireDebugAction(TARGET_CREATE);

		// Don't appear to need the process
/*		process = new PrologDebugProcess("Prolog Debug Process", this);
		try {
			launch.addProcess(process);
		} 
		catch (NullPointerException ex) {
		}
		fireDebugAction(PROCESS_CREATE);
*/		
		
		thread = new PrologDebugThread("Prolog Debug Thread", this);
		fireDebugAction(THREAD_CREATE);
		
		// Remove some views we do not use
		// org.eclipse.debug.ui.ConsoleView
		//IViewPart expView = PrologDebugPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage().findView("org.eclipse.debug.ui.ExpressionView");
	
		// Finally start it up
		if (remote) startRemote();
		else startLocal();
	}

	/**
	 * @see org.eclipse.debug.core.model.IDebugTarget#getProcess()
	 */
	public IProcess getProcess() {
		return process;
	}

	/**
	 * @see org.eclipse.debug.core.model.IDebugTarget#hasThreads()
	 */
	public boolean hasThreads() throws DebugException {
		if (thread != null)	return true;
		return false;
	}

	/**
	 * @see org.eclipse.debug.core.model.IDebugTarget#getThreads()
	 */
	public IThread[] getThreads() throws DebugException {
		IThread threads[] = { thread };
		return threads;
	}

	public PrologDebugThread getPrimaryThread() {
		return thread;
	}

	/**
	 * @see org.eclipse.debug.core.model.IDebugTarget#getName()
	 */
	public String getName() throws DebugException {
		return targetName; 
	}

	/**
	 * @see org.eclipse.debug.core.model.IDebugElement#getModelIdentifier()
	 */
	public String getModelIdentifier() {
		return PrologDebugPlugin.getModelID();
	}

	/**
	 * @see org.eclipse.debug.core.model.IDebugElement#getDebugTarget()
	 */
	public IDebugTarget getDebugTarget() {
		return this;
	}

	/**
	 * @see org.eclipse.debug.core.model.IDebugElement#getLaunch()
	 */
	public ILaunch getLaunch() {
		return launch;
	}

	private void startLocal() {
		// Start execution
		String projectPathname = "", projectName = "", 
			cfgPathname = "", xplPathname = "", 
			consultList = "", loadList = "", lsxList = "";
		try {
			projectName = config.getAttribute("projectName", "");
			projectPathname = config.getAttribute("projectPathname", "");
			cfgPathname = config.getAttribute("cfgPathname", "");
			xplPathname = config.getAttribute("xplPathname", "");
			consultList = config.getAttribute("consultList", "");
			loadList = config.getAttribute("loadList", "");
			lsxList = config.getAttribute("lsxList", "");
		}
		catch (CoreException ex) {
			PrologDebugPlugin.log(ex);
		}

		// Start the listener
		if (debugWhat == PrologDebugTarget.DEBUG_XPL)
			debugListenerView.start(this, projectPathname, cfgPathname, projectName, xplPathname, 
				null, null, lsxList);
		else
			debugListenerView.start(this, projectPathname, cfgPathname, projectName, null, 
				consultList, loadList, lsxList);
		
		// Send in the breakpoints
		try {
			IBreakpoint bps[] = getBreakpoints();
			for (int i = 0 ; i < bps.length ; i++) {
				if (bps[i] instanceof PrologLineBreakpoint) {
					PrologLineBreakpoint bp = (PrologLineBreakpoint)bps[i];
					
					// Prolog counts lines from 1
					String s = SET_BREAK + "('" + Utils.tiltSlashes(bp.getSourceName()) + "', " + 
						Integer.toString(bp.getLineNumber()) + ")";
					actionReady(s);
				}
			}
		}
		catch (CoreException ex) {
			PrologDebugPlugin.log(ex);
		}

		// Things get rolling in DebugListener
	}
	
	private void startRemote() {
		String projectPathname = "";
		
		try {
			projectPathname = config.getAttribute("projectPathname", "");
		} 
		catch (CoreException ex) {
			PrologDebugPlugin.log(ex);
		}
		
		remoteActionBuffer = new DebugListenerActionBuffer();
		remoteListener = new RemoteListener(this, debugPort, projectPathname, 
			debugListenerView, remoteActionBuffer);
		remoteListener.start();
		
		// Send in the breakpoints
		try {
			IBreakpoint bps[] = getBreakpoints();
			for (int i = 0 ; i < bps.length ; i++) {
				if (bps[i] instanceof PrologLineBreakpoint) {
					PrologLineBreakpoint bp = (PrologLineBreakpoint)bps[i];
					
					// Prolog counts lines from 1
					String s = SET_BREAK + "('" + Utils.tiltSlashes(bp.getSourceName()) + "', " + 
						Integer.toString(bp.getLineNumber()) + ")";
					remoteActionBuffer.put(s);
				}
			}
		}
		catch (CoreException ex) {
			PrologDebugPlugin.log(ex);
		}
		// Set leashing
		PrologDebugPlugin pdp = PrologDebugPlugin.getDefault();
		if (pdp.getLeashCall()) remoteActionBuffer.put(SET_LEASH + "(call)");
		else remoteActionBuffer.put(CLEAR_LEASH + "(call)");
		if (pdp.getLeashRedo()) remoteActionBuffer.put(SET_LEASH + "(redo)");
		else remoteActionBuffer.put(CLEAR_LEASH + "(redo)");
		if (pdp.getLeashFail()) remoteActionBuffer.put(SET_LEASH + "(fail)");
		else remoteActionBuffer.put(CLEAR_LEASH + "(fail)");
		if (pdp.getLeashExit()) remoteActionBuffer.put(SET_LEASH + "(exit)");
		else remoteActionBuffer.put(CLEAR_LEASH + "(exit)");
		if (pdp.getLeashInfo()) remoteActionBuffer.put(SET_LEASH + "(info)");
		else remoteActionBuffer.put(CLEAR_LEASH + "(info)");

		// Things get rolling in RemoteListener
	}
	
	private void setupDebugActions() {
		// Turn on all leashing and fire all the leashing actions
		// Also find slow step and save its instance
		// And find open listener and save its instance
		IWorkbenchPage page = PrologDebugPlugin.getDefault().getActivePage();
		IViewPart debugView = page.findView(IDebugUIConstants.ID_DEBUG_VIEW);
		if (debugView != null) {
			IToolBarManager tbm= debugView.getViewSite().getActionBars().getToolBarManager();

		if (tbm != null) {
			IContributionItem[] items = tbm.getItems();
			if (items != null) {
				for (int i = 0; i < items.length; i++) {
//					String id = items[i].getId();
					if (items[i] instanceof ActionContributionItem) {
						IAction action = ((ActionContributionItem)items[i]).getAction();
						if (action.getId().startsWith("com.amzi.prolog.debugview.leash")) {
//PrologDebugPlugin.log(new Status(Status.INFO, "com.amzi.prolog.debug", 0, "Setting leash action "+action.getId(), null));
							action.setChecked(true);
							action.run();
						}
						if (action.getId().equals("com.amzi.prolog.debugview.stepSlowly")) {
							slowStepAction = action;
						}
						if (action.getId().equals("com.amzi.prolog.debugview.openListener")) {
							openListenerAction = action;
						}
					}
				}
			}

			// Set defaults for speed, cut and dcg
			IMenuManager mm= debugView.getViewSite().getActionBars().getMenuManager();
			items = mm.getItems();
			if (items != null) {
				for (int i = 0; i < items.length; i++) {
//					String id = items[i].getId();
//					String c = items[i].getClass().getName();
					if (items[i] instanceof MenuManager) {
						IContributionItem[] mitems = ((MenuManager)items[i]).getItems();
						for (int j = 0 ; j < mitems.length ; j++ ) {
							if (mitems[j] instanceof ActionContributionItem) {
								IAction action = ((ActionContributionItem)mitems[j]).getAction();
								String mid = action.getId();
								if (mid != null && mid.endsWith("Step") && action.isChecked()) 
									action.run();
								if (mid != null && mid.endsWith("CutDetails") && action.isChecked()) 
									action.run();
								if (mid != null && mid.endsWith("DCG") && action.isChecked()) 
									action.run();
								}
							}
						}
					}
				}
			}
		}
		return;
	}
	
	/**
	 * @see org.eclipse.debug.core.model.ITerminate#canTerminate()
	 */
	public boolean canTerminate() {
		return (!terminated); /*|| (!remote && debugListenerView.isInputEnabled())*/
	}

	/**
	 * @see org.eclipse.debug.core.model.ITerminate#terminate()
	 */
	public synchronized void terminate() throws DebugException {
		// Don't overdo it
		if (terminated) return;
		
		stopSlowStepper();
		
		if (!remote) {
			// This code causes the stop button to not work!!!
/*			if (debugListenerView.isInputEnabled() == true) {
				debugListenerView.setTimeToStop(false);			// No double breaks
				debugListenerView.forceUserInput("\003\n");
				return;
			}
			else {
*/				debugListenerView.actionReady(TERMINATE);
				debugListenerView.setTimeToStop(true);
//			}
		}
		else {
			remoteActionBuffer.put(TERMINATE);
			remoteListener.setTerminating();
		}
		
		terminated = true;
		// Don't do this, Eclipse doesn't clean up properly!
//		DebugPlugin.getDefault().getLaunchManager().removeLaunch(launch);

		// Maybe remove them as they display as running
//		thread.removeAllStackFrames();
	}

	/**
	 * @see org.eclipse.debug.core.model.ITerminate#isTerminated()
	 */
	public boolean isTerminated() {
		return terminated;
	}

	/**
	 * @see org.eclipse.debug.core.model.ISuspendResume#canResume()
	 */
	public boolean canResume() {
		return(suspended);
	}

	/**
	 * @see org.eclipse.debug.core.model.ISuspendResume#resume()
	 */
	public synchronized void resume() throws DebugException {
		if (debugListenerView.isInputEnabled()) return;
		stopSlowStepper();
		stepping = false;
		suspended = false;
			
		if (remote)
			remoteActionBuffer.put("jump");
		else
			debugListenerView.actionReady("jump");
	}

	public boolean isStepping() {
		return stepping;
	}
		
	public boolean isSlowStepping() {
		if (stepping && slowStepper != null && slowStepper.isAlive() &&
			!slowStepper.isInterrupted())
			return true;
		return false;
	}
	public boolean canStepInto() {
		return(suspended);
	}
		
	public synchronized void stepInto() {
		if (!suspended) return;
		if (debugListenerView.isInputEnabled()) return;
		stepping = true;
		suspended = false;
		if (remote)
			remoteActionBuffer.put(STEP_INTO);
		else
			debugListenerView.actionReady(STEP_INTO);
	}
		
	public boolean canStepOver() {
		return(suspended);
	}
		
	public synchronized void stepOver() {
		if (!suspended) return;
		if (debugListenerView.isInputEnabled()) return;
		stepping = true;
		suspended = false;
		if (remote)
			remoteActionBuffer.put(STEP_OVER);
		else
			debugListenerView.actionReady(STEP_OVER);
	}
		
	public boolean canStepReturn() {
		return false;
	}
		
	public void stepReturn() {
/*		stepping = true;
		if (remote)
			remoteActionBuffer.put(STEP_RETURN);
		else
			debugListenerView.actionReady(STEP_RETURN);
*/	}
	
	public void setSlowStepper(SlowStep stepper) {
		this.slowStepper = stepper;
	}
	
	public void stopSlowStepper() {
		if (slowStepper != null && slowStepper.isAlive()) 
			slowStepper.interrupt();
		if (slowStepAction != null)
			slowStepAction.setChecked(false);
	}
	
	public void setSlowStepperSpeed(int speed) {
		slowStepperSpeed = speed;
	}
	
	public int getSlowStepperSpeed() {
		return slowStepperSpeed;
	}
	
	/**
	 * @see org.eclipse.debug.core.model.ISuspendResume#canSuspend()
	 */
	public boolean canSuspend() {
		if (stepping == false && suspended == true)
			return true;
		else
			return false;
	}
	
	/**
	 * @see org.eclipse.debug.core.model.ISuspendResume#suspend()
	 */
	public synchronized void suspend() throws DebugException {
		if (stepping == true || suspended == false) return;
		if (debugListenerView.isInputEnabled()) return;
		stopSlowStepper();
 
		debugListenerView.setTimeToPause(true);
		
		suspended = true;
	}
	
	/**
	 * @see org.eclipse.debug.core.model.ISuspendResume#isSuspended()
	 */
	public boolean isSuspended() {
		return (suspended);
	}

	public void fireDebugAction(String action) {
		DebugEvent event = null;
		
		if (action.equals(PROCESS_CREATE))
			event = new DebugEvent(process, DebugEvent.CREATE);	
		if (action.equals(TARGET_CREATE))	
			event = new DebugEvent(this, DebugEvent.CREATE);
		if (action.equals(THREAD_CREATE))
			event = new DebugEvent(thread, DebugEvent.CREATE);
			
		if (action.equals(THREAD_SUSPEND_STEP_END)) {
			// Must be a CLIENT_REQUEST from 3.1 to 3.2.1 to get the thread to 
			// expand in the DebugView. So this now requires 3.2.2.
			// Used to be STEP_END
			event = new DebugEvent(thread, DebugEvent.SUSPEND, DebugEvent.STEP_END);
			suspended = true;
		}
		if (action.equals(THREAD_SUSPEND_BREAKPOINT)) {
			// Used to be BREAKPOINT
			event = new DebugEvent(thread, DebugEvent.SUSPEND, DebugEvent.BREAKPOINT);
			suspended = true;
		}
		if (action.equals(TERMINATE)) {
			event = new DebugEvent(this, DebugEvent.TERMINATE, DebugEvent.CLIENT_REQUEST);
			terminated = true;
		}
		if (action.equals(STEP_INTO)) {
			event = new DebugEvent(thread, DebugEvent.RESUME, DebugEvent.STEP_INTO);
			suspended = false;
		}
		if (action.equals(STEP_OVER)) {
			event = new DebugEvent(thread, DebugEvent.RESUME, DebugEvent.STEP_OVER);
			suspended = false;
		}
		if (action.equals(STEP_RETURN)) {
			event = new DebugEvent(thread, DebugEvent.RESUME, DebugEvent.STEP_RETURN);
			suspended = false;
		}
		if (action.equals(RESUME)) {
			event = new DebugEvent(thread, DebugEvent.RESUME, DebugEvent.CLIENT_REQUEST);
			suspended = false;
		}
		if (action.equals(SUSPEND)) {
			event = new DebugEvent(thread, DebugEvent.SUSPEND, DebugEvent.CLIENT_REQUEST);
			suspended = true;
		}

		if (event != null) {
			DebugEvent events[] = { event };
			DebugPlugin.getDefault().fireDebugEventSet(events);
		}
	}

	/**
	 * @see org.eclipse.debug.core.model.IDebugTarget#supportsBreakpoint(org.
		 * eclipse.debug.core.model.IBreakpoint)
	 */
	public boolean supportsBreakpoint(IBreakpoint breakpoint) {
		if (breakpoint instanceof PrologLineBreakpoint)
			return true;
		else
			return false;
	}

	public IBreakpoint[] getBreakpoints() {
		IBreakpointManager manager = DebugPlugin.getDefault().getBreakpointManager();
		IBreakpoint[] breakpoints = manager.getBreakpoints(getModelIdentifier());
		return breakpoints;
	}

	/**
	 * @see org.eclipse.debug.core.IBreakpointListener#breakpointAdded(org.eclipse.debug.core.model.IBreakpoint)
	 */
	public void breakpointAdded(IBreakpoint breakpoint) {
		if (breakpoint instanceof PrologLineBreakpoint) { 
			try {
				PrologLineBreakpoint bp = (PrologLineBreakpoint)breakpoint;
				String s = SET_BREAK + "('" + Utils.tiltSlashes(bp.getSourceName()) + "', " + 
					Integer.toString(bp.getLineNumber()) + ")";
				if (remote)
					remoteActionBuffer.put(s);
				else
					debugListenerView.actionReady(s);
			}
			catch (CoreException ex) {
				PrologDebugPlugin.log(ex);
			}
		} 
	}

	/**
	 * @see org.eclipse.debug.core.IBreakpointListener#breakpointRemoved(org.eclipse.debug.core.model.IBreakpoint, org.eclipse.core.resources.IMarkerDelta)
	 */
	public void breakpointRemoved(IBreakpoint breakpoint, IMarkerDelta delta) {
		if (breakpoint instanceof PrologLineBreakpoint) { 
			try {
				PrologLineBreakpoint bp = (PrologLineBreakpoint)breakpoint;
				String s = CLEAR_BREAK + "('" + bp.getSourceName() + "', " + 
					Integer.toString(bp.getLineNumber()) + ")";
				if (remote)
					remoteActionBuffer.put(s);
				else
					debugListenerView.actionReady(s);
			}
			catch (CoreException ex) {
				PrologDebugPlugin.log(ex);
			}
		} 
	}

	/**
	 * @see org.eclipse.debug.core.IBreakpointListener#breakpointChanged(org.eclipse.debug.core.model.IBreakpoint, org.eclipse.core.resources.IMarkerDelta)
	 */
	public void breakpointChanged(IBreakpoint breakpoint, IMarkerDelta delta) {
//		IBreakpoint bp = breakpoint;
	}

	public void setConnected(boolean flag) {
		connected = flag;
	}
	
	/**
	 * @see org.eclipse.debug.core.model.IDisconnect#canDisconnect()
	 */
	public boolean canDisconnect() {
		if (remote && connected) return true;
		else return false;
//		return false;
	}

	/**
	 * @see org.eclipse.debug.core.model.IDisconnect#disconnect()
	 */
	public void disconnect() throws DebugException {
		remoteListener.setTerminating();
		remoteListener.interrupt();
		connected = false;
	}

	/**
	 * @see org.eclipse.debug.core.model.IDisconnect#isDisconnected()
	 */
	public boolean isDisconnected() {
//		return !connected;
		// This sets the <terminated> indicator on the launch in debug view
		if (!terminated) return false;
		else return !connected;
	}

	/**
	 * @see org.eclipse.debug.core.model.IMemoryBlockRetrieval#supportsStorageRetrieval()
	 */
	public boolean supportsStorageRetrieval() {
		return false;
	}

	/**
	 * @see org.eclipse.debug.core.model.IMemoryBlockRetrieval#getMemoryBlock(long, long)
	 */
	public IMemoryBlock getMemoryBlock(long startAddress, long length)
		throws DebugException {
		return null;
	}
	
	/**
	 * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
	 */
	public Object getAdapter(Class adapter) {
		if (adapter.equals(IDebugElement.class)) {
//		if (adapter.equals(IDebugTarget.class)) {
			return this;
		}
//		if (adapter.equals(PrologDebugTarget.class)) {
//			return this;
//		}
		return super.getAdapter(adapter);
	}

	public void handleDebugEvents(DebugEvent[] events) {
//		String s;
		if (initActions == false) {
			setupDebugActions();
			initActions = true;
		}

		for (int i = 0 ; i < events.length ; i++) {
//			s = events[i].toString();
			if (events[i].getKind() == DebugEvent.SUSPEND) {
				if (slowStepAction != null)
					slowStepAction.setEnabled(true);
				if (openListenerAction != null)
					openListenerAction.setEnabled(true);
			}
			if (events[i].getKind() == DebugEvent.TERMINATE) {
				if (slowStepAction != null)
					slowStepAction.setEnabled(false);
				if (openListenerAction != null)
					openListenerAction.setEnabled(false);
			}
		}
	}
	
	public void actionReady(String action) {
		debugListenerView.actionReady(action);
	}
	
	public void showDebugView(String viewId) {
		try {
			IWorkbenchWindow window;
			window = PrologDebugPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow();
			if (window == null)
				window = PrologDebugPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow();
			if (window != null) {
				IWorkbenchPage page = window.getActivePage();
				if (page != null) {
					try { // show the view
						IViewPart view = page.findView(viewId);
						if (view == null) {
							IWorkbenchPart activePart = page.getActivePart();
							view = page.showView(viewId);
							//restore focus 
							page.activate(activePart);
						} else {
							page.bringToTop(view);
						}
					} 
					catch (PartInitException ex) {
						PrologDebugPlugin.log(ex);
					}
				}
			}
		} 
		catch (NullPointerException ex) {
			PrologDebugPlugin.log(ex);
		}
	}

	public String getBinPathname() {
		return binPathname;
	}
	
	public void refreshProject(int level) {
	  IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
      IProject debugProject = workspaceRoot.getProject(targetName);
      try {
		debugProject.refreshLocal(level, null);
      } 
      catch (CoreException e) {
      }
	}
	
/*	public static void closeDebugEditors() {
		// Walk all the editors closing the debugging ones
		IWorkbenchWindow workWindows[] = PrologDebugPlugin.getDefault().getWorkbench().getWorkbenchWindows();
		for (int i = 0 ; i < workWindows.length ; i++) {
			IWorkbenchPage workPages[] = workWindows[i].getPages();
			for (int j = 0 ; j < workPages.length ; j++) {
				IEditorReference editRefs[] = workPages[j].getEditorReferences();
				for (int k = 0 ; k < editRefs.length ; k++) {
//					IEditorPart editor = editRefs[k].getEditor(false);
//					if (editor != null && editor.getEditorInput() instanceof PrologEditorInput)
//						workPages[j].closeEditor(editor, false);
				}
			}
		}
	}
*/
	
	public void dispose() {
//		closeDebugEditors();
		
		try {
			if (remote && connected) disconnect();
			if (!remote && !terminated) terminate();
		}
		catch (DebugException ex) {
			DebugPlugin.log(ex);
		}
	}
}

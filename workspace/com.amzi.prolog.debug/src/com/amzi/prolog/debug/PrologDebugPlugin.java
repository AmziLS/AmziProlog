package com.amzi.prolog.debug;

import java.util.*;

import com.amzi.prolog.debug.core.model.PrologLineBreakpoint;

import org.eclipse.ui.plugin.*;
import org.eclipse.core.runtime.*;
import org.eclipse.core.resources.*;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IBreakpointManager;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class PrologDebugPlugin extends AbstractUIPlugin {
	//The shared instance.
	private static PrologDebugPlugin plugin;
	//Resource bundle.
	private ResourceBundle resourceBundle;
	//Leashing
	private boolean leashCall, leashRedo, leashFail, leashExit, leashInfo;
	
	/**
	 * The constructor.
	 */
	public PrologDebugPlugin() {
		super();
		plugin = this;
	}

	/* (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);

//	System.out.println("Starting Debug");
		
		try {
			resourceBundle= ResourceBundle.getBundle("com.amzi.prolog.debug.DebugPluginResources");
		} 
		catch (MissingResourceException x) {
			resourceBundle = null;
		}
	}
	/* (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		// TODO Auto-generated method stub
		super.stop(context);
	}
	
	public static PrologLineBreakpoint lineBreakpointExists(String sourceName, int lineNumber)
		throws CoreException {
		IBreakpointManager manager = DebugPlugin.getDefault().getBreakpointManager();
		IBreakpoint[] breakpoints = manager.getBreakpoints(getModelID());
		for (int i = 0; i < breakpoints.length; i++) {
			if (!(breakpoints[i] instanceof PrologLineBreakpoint)) {
				continue;
			}
			PrologLineBreakpoint breakpoint = (PrologLineBreakpoint) breakpoints[i];
			try {
				if (breakpoint.getMarker().getType().equals(PrologLineBreakpoint.getMarkerType())) {
					if (breakpoint.getSourceName().endsWith(sourceName)) {
						if (breakpoint.getLineNumber() == lineNumber) {
							return breakpoint;
						}
					}
				}
			} 
			catch (CoreException ex) {
				PrologDebugPlugin.log(ex);
			} 
			catch (NullPointerException ex) {
				PrologDebugPlugin.log(ex);
			}
		}
		return null;
	}

	public static String getModelID() {
		return "com.amzi.prolog.debug";
	}
	
	/**
	 * Returns the shared instance.
	 */
	public static PrologDebugPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns the workspace instance.
	 */
	public static IWorkspace getWorkspace() {
		return ResourcesPlugin.getWorkspace();
	}

	/**
	 * Returns the string from the plugin's resource bundle,
	 * or 'key' if not found.
	 */
	public static String getResourceString(String key) {
		ResourceBundle bundle= PrologDebugPlugin.getDefault().getResourceBundle();
		try {
			return bundle.getString(key);
		} catch (MissingResourceException e) {
			return key;
		}
	}

	/**
	 * Returns the plugin's resource bundle,
	 */
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}

	public IWorkbenchPage getActivePage() {
		IWorkbench workbench = getWorkbench();
		IWorkbenchWindow windows[] = workbench.getWorkbenchWindows();
		if (workbench.getWorkbenchWindowCount() < 1)
			return null;
		IWorkbenchWindow win = windows[0];
		IWorkbenchPage pages[] = win.getPages();
		return pages[0];
	}
	
	public static void log(Throwable e) {
		log(new Status(IStatus.ERROR, "com.amzi.prolog.debug", IStatus.ERROR, "Error", e));
	}

	public static void log(IStatus status) {
		getDefault().getLog().log(status);
	}
	public void setLeashCall(boolean flag) {
		leashCall = flag;
	}
	public void setLeashRedo(boolean flag) {
		leashRedo = flag;
	}
	public void setLeashFail(boolean flag) {
		leashFail = flag;
	}
	public void setLeashExit(boolean flag) {
		leashExit = flag;
	}
	public void setLeashInfo(boolean flag) {
		leashInfo = flag;
	}
	public boolean getLeashCall() {
		return leashCall;
	}
	public boolean getLeashRedo() {
		return leashRedo;
	}
	public boolean getLeashFail() {
		return leashFail;
	}
	public boolean getLeashExit() {
		return leashExit;
	}
	public boolean getLeashInfo() {
		return leashInfo;
	}

}

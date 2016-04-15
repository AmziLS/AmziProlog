package com.amzi.prolog.ui.launch;

import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.core.dialogs.UpgradeDialog;
import com.amzi.prolog.debug.core.model.PrologDebugTarget;
import com.amzi.prolog.ui.PrologUIPlugin;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
//import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.swt.widgets.Display;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
 
public class ListenerLaunchConfigurationDelegate extends AbstractPrologLaunchConfigurationDelegate {
//	implements ILaunchConfigurationDelegate {

	/**
	 * Constructor for RunLaunchConfigurationDelegate.
	 */
	public ListenerLaunchConfigurationDelegate() {
		super();
	}

	/**
	 * @see org.eclipse.debug.core.model.ILaunchConfigurationDelegate#launch(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch, org.eclipse.core.runtime.IProgressMonitor)
	 */
	public void launch(ILaunchConfiguration config, String mode, ILaunch launch, IProgressMonitor monitor)
		throws CoreException {

		if (monitor == null) {
			monitor = new NullProgressMonitor();
		}

		// Is this allowed?
		if (mode.equals(ILaunchManager.DEBUG_MODE) && !PrologCorePlugin.actionAllowed(PrologCorePlugin.DEV_LOCAL_DEBUG)) {
			monitor.done();
			DebugPlugin.getDefault().getLaunchManager().removeLaunch(launch);
			Display.getDefault().asyncExec(new Runnable(){
				public void run() {
					UpgradeDialog upgradeDialog = new UpgradeDialog(Display.getDefault().getActiveShell());
					upgradeDialog.open();
				}
			});
			return;
		} 

		final String projectPathname = config.getAttribute("projectPathname", "");
		final String cfgPathname = config.getAttribute("cfgPathname", "");
		final String consultList = config.getAttribute("consultList", "");
		final String loadList = config.getAttribute("loadList", "");
		final String lsxList = config.getAttribute("lsxList", "");
		final String projectName = config.getAttribute("projectName", "");
		
		// Make sure the project is good
		IProject project = (IProject)ResourcesPlugin.getWorkspace().getRoot().getProject(new Path(projectName).lastSegment());
		if (project == null || !project.isAccessible()) {
			project = (IProject)ResourcesPlugin.getWorkspace().getRoot().findMember(new Path(projectPathname).lastSegment());
			if (project == null || !project.isAccessible()) {		
				monitor.done();
				DebugPlugin.getDefault().getLaunchManager().removeLaunch(launch);
				Display.getDefault().asyncExec(new Runnable(){
					public void run() {
						MessageDialog.openError(Display.getDefault().getActiveShell(), "Error",
							"Project is closed or not accessible");
					}
				});
				return;
			}
		}
		if (project.getNature("com.amzi.prolog.ui.PrologNature") == null) {
			monitor.done();
			DebugPlugin.getDefault().getLaunchManager().removeLaunch(launch);
			Display.getDefault().asyncExec(new Runnable(){
				public void run() {
					MessageDialog.openError(Display.getDefault().getActiveShell(), "Error",
						"Unable to run because project is not a Prolog Project");
				}
			});
			return;			
		}
		
		// Debug
		if (mode.equals(ILaunchManager.DEBUG_MODE)) {
			monitor.done();
			
			// Set source locator
//			launch.setSourceLocator(new PrologSourceLocator(project));
			
			// Launch the debugger
			final ILaunchConfiguration fConfig = config;
			final ILaunch fLaunch = launch;
			Display.getDefault().asyncExec(new Runnable(){
				public void run() {
					IDebugTarget debugTarget = new PrologDebugTarget(PrologDebugTarget.DEBUG_PRO,
						fConfig, fLaunch);
					fLaunch.addDebugTarget(debugTarget);
				}
			});
			return;
		}
		
		// Run
		if (mode.equals(ILaunchManager.RUN_MODE)) {
			monitor.done();

			// Try to start the listener
			final ILaunch fLaunch = launch;
			Display.getDefault().asyncExec(new Runnable(){
				public void run() {
					// Show our listener view
					showUIView("com.amzi.prolog.ui.launch.ListenerView");
						
					// Find our listener view
					ListenerView listenerView = null;
					IWorkbenchPage page = PrologUIPlugin.getDefault().getActivePage();
					IViewReference vr[] = page.getViewReferences();
					for (int i = 0 ; i < vr.length ; i++)
						if (vr[i].getId().equals("com.amzi.prolog.ui.launch.ListenerView")) {
							// Try to restore it (the true parameter)
							listenerView = (ListenerView)vr[i].getView(true);
							if (listenerView != null)
								page.bringToTop(listenerView);
						}
			
					// Could not find it
					if (listenerView == null) {
						DebugPlugin.getDefault().getLaunchManager().removeLaunch(fLaunch);
						MessageDialog.openError(Display.getDefault().getActiveShell(), "Internal Error",
							"Unable to find Listener View");
					}
					// We've found it, restored it, so start it
					else {
						listenerView.start(fLaunch, "Listener", projectPathname, cfgPathname, projectName, 
							"", consultList, loadList, lsxList);
					}
				}
			});
		}
	}

}

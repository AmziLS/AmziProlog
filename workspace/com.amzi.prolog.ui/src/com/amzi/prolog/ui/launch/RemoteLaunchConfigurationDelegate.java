package com.amzi.prolog.ui.launch;

import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.core.dialogs.UpgradeDialog;
import com.amzi.prolog.debug.core.model.PrologDebugTarget;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */

public class RemoteLaunchConfigurationDelegate
	extends AbstractPrologLaunchConfigurationDelegate {

	/**
	 * Constructor for RunLaunchConfigurationDelegate.
	 */
	public RemoteLaunchConfigurationDelegate() {
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
		if (!PrologCorePlugin.actionAllowed(PrologCorePlugin.DEV_LOCAL_DEBUG)) {
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
		
		final String projectName = config.getAttribute("projectName", "");
//		final String projectPathnam = config.getAttribute("projectPathname", "");
		
		// Make sure the project is good
		IProject project = (IProject)ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
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
			// Set source locator
//			launch.setSourceLocator(new PrologSourceLocator(project));
		
			monitor.done();
		
			final ILaunchConfiguration fConfig = config;
			final ILaunch fLaunch = launch;
			Display.getDefault().asyncExec(new Runnable(){
				public void run() {
					IDebugTarget debugTarget = new PrologDebugTarget(PrologDebugTarget.DEBUG_XPL, fConfig, fLaunch);
					fLaunch.addDebugTarget(debugTarget);
				}
			});
			return;
		}
	}

}

package com.amzi.prolog.ui.launch;

import com.amzi.prolog.ui.PrologUIPlugin;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.LaunchConfigurationDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
 
public class AbstractPrologLaunchConfigurationDelegate
	extends LaunchConfigurationDelegate {
//	implements ILaunchConfigurationDelegate {

	/* (non-Javadoc)
	 * @see org.eclipse.debug.core.model.ILaunchConfigurationDelegate#launch(org.eclipse.debug.core.ILaunchConfiguration, java.lang.String, org.eclipse.debug.core.ILaunch, org.eclipse.core.runtime.IProgressMonitor)
	 */
	public void launch(
		ILaunchConfiguration configuration,
		String mode,
		ILaunch launch,
		IProgressMonitor monitor)
		throws CoreException {
	}
	
	/**
	 * Shows the view or bring to the top
	 * @param viewId viewId of the view to be shown
	 */
	public void showUIView(String viewId) {
		try {
			IWorkbenchWindow window;
			window = PrologUIPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow();
			if (window == null)
				window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
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
						PrologUIPlugin.log(ex);
					}
				}
			}
		} 
		catch (NullPointerException ex) {
			PrologUIPlugin.log(ex);
		}
	}

}

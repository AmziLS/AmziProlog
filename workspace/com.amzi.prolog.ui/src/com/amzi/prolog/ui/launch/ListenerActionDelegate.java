package com.amzi.prolog.ui.launch;

import com.amzi.prolog.ui.PrologUIPlugin;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
//import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.actions.ActionDelegate;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class ListenerActionDelegate extends ActionDelegate implements IWorkbenchWindowActionDelegate {
	IWorkbenchWindow window;
	
	/**
	 * Constructor for ListenerActionDelegate.
	 */
	public ListenerActionDelegate() {
		super();
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
	 */
	public void dispose() {
	}

	/**
	 * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
	 */
	public void init(IWorkbenchWindow window) {
		this.window = window;
	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action) {
		// Run the listener from the workspace root directory
		String workspacePathname = ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString();
		
		// Look for an open listener view
		IWorkbenchPage page = PrologUIPlugin.getDefault().getActivePage();
		IViewReference vr[] = page.getViewReferences();
		for (int i = 0 ; i < vr.length ; i++)
			if (vr[i].getId().equals("com.amzi.prolog.ui.launch.ListenerView")) {
				// Try to restore it
				ListenerView listenerView = (ListenerView)vr[i].getView(true);
				if (listenerView != null) {
					page.activate(listenerView);
					listenerView.start(null, "Listener", workspacePathname, "", "", "", "", "", "");
					return;
				}
			}

		// If we didn't find it, create a new one
		try {
			ListenerView listenerView = (ListenerView)page.showView("com.amzi.prolog.ui.launch.ListenerView");
			page.activate(listenerView);
			listenerView.start(null, "Listener", workspacePathname, "", "", "", "", "", "");
		}
		catch (PartInitException ex) {
			MessageDialog.openError(window.getShell(), PrologUIPlugin.getResourceString("Listener_Name"), 
				ex.getMessage());
		}

	}

	/**
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
//	public void selectionChanged(IAction action, ISelection selection) {
//	}

}

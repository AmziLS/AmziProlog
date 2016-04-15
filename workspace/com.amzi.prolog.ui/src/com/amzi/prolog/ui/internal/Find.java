package com.amzi.prolog.ui.internal;

import com.amzi.prolog.ui.PrologUIPlugin;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class Find {

	// Find a Project from the currently selected object
	public static IProject getSelectedProject() {
		IProject project = null;
		IWorkbenchPage page = PrologUIPlugin.getDefault().getActivePage();
		if (page == null) return null;

		// See what is selected
		ISelection selection = page.getSelection();
		if (selection != null) {
			// Selected from navigator view or from other views of eclipse
			if (selection instanceof IStructuredSelection) {
				IStructuredSelection ss = (IStructuredSelection) selection;
				if (!ss.isEmpty()) {
					Object element = ss.getFirstElement();
					if (element instanceof IAdaptable) {
						try {
							IResource res = (IResource) ((IAdaptable) element).getAdapter(IResource.class);
							project = res.getProject();
							return project;
						} catch (NullPointerException ex) {
							PrologUIPlugin.log(ex);
						}
					}
				}
			}
		}
		// Selected from an editor
		IEditorPart part = page.getActiveEditor();
		if (part != null) {
			try{
				IEditorInput input = part.getEditorInput();
				IResource res = (IResource) ((IAdaptable) input).getAdapter(IResource.class);
				project = res.getProject();
				return project;
			}
			catch(NullPointerException ex){
				PrologUIPlugin.log(ex);
			}
		}
		return null;
	}

}

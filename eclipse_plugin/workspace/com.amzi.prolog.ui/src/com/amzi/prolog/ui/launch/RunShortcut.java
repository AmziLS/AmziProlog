package com.amzi.prolog.ui.launch;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import com.amzi.prolog.ui.build.ProjectProperties;
import com.amzi.prolog.ui.PrologUIPlugin;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class RunShortcut implements ILaunchShortcut {

	/**
	 * Constructor for RunShortcut.
	 */
	public RunShortcut() {
		super();
	}

	/**
	 * @see org.eclipse.debug.ui.ILaunchShortcut#launch(org.eclipse.jface.viewers.ISelection, java.lang.String)
	 */
	public void launch(ISelection selection, String mode) {
		String myName = PrologUIPlugin.getResourceString("Run_Name");
		
		// Something should be selected
		if (selection == null ) {
			MessageDialog.openError(null, myName, PrologUIPlugin.getResourceString("LaunchSelectSomething_Text"));
			return;
		}

		// See if multiple items are selected
		Object object = ((IStructuredSelection) selection).getFirstElement();
		if (object == null) {
			MessageDialog.openError(null, myName, PrologUIPlugin.getResourceString("LaunchSelectSomething_Text"));
			return;
		}

		// Make sure its a resource in a prolog project
		IResource resource = null;
		IProject project = null;
		if(object instanceof IResource){
			resource = (IResource)object;
			project = resource.getProject();
		}
		boolean isPrologProject = false;
		try {
			if (project != null)
				isPrologProject = project.hasNature(PrologUIPlugin.getResourceString("PrologNature_Name"));
		}
		catch (CoreException ex) { }
		if	(resource == null || project == null || !isPrologProject) {
			MessageDialog.openError(null, myName, PrologUIPlugin.getResourceString("LaunchSelectSomething_Text"));
			return;
		}

		// Run it		
		launch(resource.getProject(), mode);

		return;
	}

	/**
	 * @see org.eclipse.debug.ui.ILaunchShortcut#launch(org.eclipse.ui.IEditorPart, java.lang.String)
	 */
	public void launch(IEditorPart editor, String mode) {
		IEditorInput editorInput = editor.getEditorInput();
		if (editorInput instanceof IFileEditorInput) {
			IFile file = (IFile) ((IFileEditorInput) editorInput).getAdapter(IFile.class);
			IProject project = file.getProject();

			ILaunchConfiguration config = createLaunchConfiguration(project, mode);
			try {
				config.launch(mode, null);
			} 
			catch (CoreException ex) {
			}
		}
	}
	
	private void launch(IProject project, String mode) {
		ILaunchConfiguration config = createLaunchConfiguration(project, mode);
		try {
			config.launch(mode, null);
		} 
		catch (CoreException ex) {
		}
	}
	
	private ILaunchConfiguration createLaunchConfiguration(IProject project, String mode) {
		Properties buildProps = ProjectProperties.getProperties(project);
		String configName = PrologUIPlugin.getResourceString("RunCompiledLaunchConfiguration_Name");
		
		// Build a temporary launch configuration
		ILaunchConfiguration config = null;
		try {
			ILaunchConfigurationType configType = getLaunchConfigurationType();
			ILaunchConfigurationWorkingCopy wc = openConfiguration(configType, configName, mode).getWorkingCopy();

			// Set the perspective to switch to. 
			DebugUITools.setLaunchPerspective(configType, "debug", IDebugUIConstants.ID_DEBUG_PERSPECTIVE);
//			DebugUITools.setLaunchPerspective(configType, "run", IDebugUIConstants.PERSPECTIVE_DEFAULT);
			
			// Get full or build relative path to xpl file
   			String xplPathname = buildProps.getProperty("xplPathname");
   			File xplFile = new File(xplPathname);
   			if (!xplFile.isAbsolute() && !xplPathname.startsWith(System.getProperty("file.separator"))) {
				String binFoldername = buildProps.getProperty("binFolder");
				if (binFoldername != null && binFoldername.length() > 0)
	   				xplPathname = buildProps.getProperty("binFolder") +
	   					System.getProperty("file.separator") + xplPathname;
			}
   					
			// Save the information in the configuration
			// Maybe add to Run Favorites see IDebugUIConstants.ATTR_FAVORITE_GROUPS
			// and ID_RUN_GROUP and CommonTab.java in org.eclipse.debug.ui source code
			List groups = new ArrayList();
			groups.add(IDebugUIConstants.ID_RUN_LAUNCH_GROUP);
			wc.setAttribute(IDebugUIConstants.ATTR_FAVORITE_GROUPS, groups);
//			wc.setAttribute(IDebugUIConstants.ATTR_PRIVATE, true);
			wc.setAttribute("projectName", project.getName());
			wc.setAttribute("xplPathname", xplPathname);
			wc.setAttribute("projectPathname", project.getLocation().toOSString());
			wc.setAttribute("cfgPathname", "");
			wc.setAttribute("lsxList", buildProps.getProperty("lsxExtensionNames"));
			wc.setAttribute("binPathname", buildProps.getProperty("binFolder"));
					
			config = wc.doSave();
		} 
		catch (CoreException ex) {
			return null;
		}
		return config;
	}
	
	private ILaunchConfiguration openConfiguration(ILaunchConfigurationType configType, String name, String mode) {
		ILaunchConfiguration configs[] = null;
		try{
			try {
				// Get the list of all our type of configs
				configs = DebugPlugin.getDefault().getLaunchManager().getLaunchConfigurations(getLaunchConfigurationType());
			}
			catch (CoreException ex) {
				PrologUIPlugin.log(ex);
			}
			ArrayList candidates = new ArrayList();
			for (int i = 0; i < configs.length; i++) {
				try {
					if (configs[i].getAttribute("projectName", "").equals(name)) {
						candidates.add(configs[i]);
					}
				}
				catch (CoreException ex) {
					PrologUIPlugin.log(ex);
				}
			}

			if (candidates != null) {
				//Creates a configuration if none is avaible
				if (candidates.size() == 0) {
					return configType.newInstance(null, name);
//						getLaunchManager().generateUniqueLaunchConfigurationNameFrom(name));
				} else if (candidates.size() == 1) {
					//Returns the configuration if only one is avaible
					return (ILaunchConfiguration) candidates.get(0);
				} else {
					//Chooses a configuration among the avaible
					return (ILaunchConfiguration) candidates.get(0); //chooseConfiguration(candidates, mode);
				}
			}
		}
		catch (CoreException ex) {
			PrologUIPlugin.log(ex);
		}
		catch(NullPointerException ex) {
			PrologUIPlugin.log(ex);
		}
		return null;
	}

	private ILaunchConfigurationType getLaunchConfigurationType() {
		return DebugPlugin.getDefault().getLaunchManager().getLaunchConfigurationType(
			"com.amzi.prolog.ui.launch.prologRuntime");
	}

/*	private ILaunchManager getLaunchManager() {
		return DebugPlugin.getDefault().getLaunchManager();
	}
*/
}

package com.amzi.prolog.ui.launch;

import java.util.ArrayList;
import java.util.List;

import com.amzi.prolog.ui.PrologUIPlugin;
import com.amzi.prolog.ui.build.ProjectProperties;
import com.amzi.prolog.core.PrologCorePlugin;
import com.sun.xml.internal.fastinfoset.sax.Properties;

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
public class FileListenerShortcut implements ILaunchShortcut {

	/**
	 * Constructor for ListenerShortcut.
	 */
	public FileListenerShortcut() {
		super();
	}

	/**
	 * @see org.eclipse.debug.ui.ILaunchShortcut#launch(org.eclipse.jface.viewers.ISelection, java.lang.String)
	 */
	public void launch(ISelection selection, String mode) {
		String myName = PrologUIPlugin.getResourceString("Listener_Name");
		
		// Something should be selected
   		if (selection == null ) {
			MessageDialog.openError(null, myName, PrologUIPlugin.getResourceString("LaunchSelectFile_Text"));
			return;
		}

		// See if multiple items are selected
		Object object = ((IStructuredSelection) selection).getFirstElement();
		if (object == null) {
			MessageDialog.openError(null, myName, PrologUIPlugin.getResourceString("LaunchSelectFile_Text"));
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
		if	(!(resource instanceof IFile) ||resource == null || project == null || !isPrologProject) {
			MessageDialog.openError(null, myName, PrologUIPlugin.getResourceString("LaunchSelectFile_Text"));
			return;
		}
		
		// Run it
		launch((IFile)resource, mode);
	}

	/**
	 * @see org.eclipse.debug.ui.ILaunchShortcut#launch(org.eclipse.ui.IEditorPart, java.lang.String)
	 */
	public void launch(IEditorPart editor, String mode) {
		IEditorInput editorInput = editor.getEditorInput();
		if (editorInput instanceof IFileEditorInput) {
			IFile file = (IFile) ((IFileEditorInput) editorInput).getAdapter(IFile.class);

			launch(file, mode);
		}
		else {
			MessageDialog.openError(null, PrologUIPlugin.getResourceString("Listener_Name"), 
				PrologUIPlugin.getResourceString("LaunchSelectFile_Text"));
			return;
		}

	}
	
	private void launch(IFile file, String mode) {
		ILaunchConfiguration config = createLaunchConfiguration(file, mode);
		try {
			config.launch(mode, null);
		} 
		catch (CoreException ex) {
		}
	}
	
	private ILaunchConfiguration createLaunchConfiguration(IFile file, String mode) {
		IProject project = file.getProject();
		java.util.Properties buildProps = ProjectProperties.getProperties(project);
		String configName = PrologUIPlugin.getResourceString("RunFileListenerLaunchConfiguration_Name");
		
		// Build launch configuration
		ILaunchConfiguration config = null;
		try {
			// Look for the default configuration, creating it if not found
			ILaunchConfigurationType configType = getLaunchConfigurationType();
			ILaunchConfigurationWorkingCopy wc = openConfiguration(configType, configName, mode).getWorkingCopy();

			// Set the perspective to switch to. 
			DebugUITools.setLaunchPerspective(configType, "debug", IDebugUIConstants.ID_DEBUG_PERSPECTIVE);
//			DebugUITools.setLaunchPerspective(configType, "run", IDebugUIConstants.PERSPECTIVE_DEFAULT);
			
			// Build full path to xpl file
  			String xplPathname = PrologCorePlugin.getAmziDir() + "abin" + System.getProperty("file.separator") + 
				"alis.xpl"; 
   					
   			// Save the information in the configuration
			List groups = new ArrayList();
			groups.add(IDebugUIConstants.ID_RUN_LAUNCH_GROUP);
			wc.setAttribute(IDebugUIConstants.ATTR_FAVORITE_GROUPS, groups);
//			wc.setAttribute(IDebugUIConstants.ATTR_PRIVATE, true);
			wc.setAttribute("projectName", project.getName());
			wc.setAttribute("xplPathname", xplPathname);
			wc.setAttribute("projectPathname", project.getLocation().toOSString());
			wc.setAttribute("cfgPathname", "");
			wc.setAttribute("binPathname", buildProps.getProperty("binFolder"));
//			String consultList = "";
			if (file.isLinked())
				wc.setAttribute("consultList", "'"+file.getLocation().toOSString()+"'");
			else
				wc.setAttribute("consultList", "'"+file.getProjectRelativePath().toOSString()+"'");
			wc.setAttribute("loadList", "");
			wc.setAttribute("lsxList", "");

			// Save it
			config = wc.doSave();
		} 
		catch (CoreException ex) {
			PrologUIPlugin.log(ex);
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
			"com.amzi.prolog.ui.launch.prologFileListener");
	}

/*	private ILaunchManager getLaunchManager() {
		return DebugPlugin.getDefault().getLaunchManager();
	}
*/
}

package com.amzi.prolog.ui.launch;

import java.util.ArrayList;
import java.util.Properties;
import java.util.List;

import com.amzi.prolog.ui.build.ProjectProperties;
import com.amzi.prolog.ui.PrologUIPlugin;
import com.amzi.prolog.core.PrologCorePlugin;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
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
public class ListenerShortcut implements ILaunchShortcut {

	/**
	 * Constructor for ListenerShortcut.
	 */
	public ListenerShortcut() {
		super();
	}

	/**
	 * @see org.eclipse.debug.ui.ILaunchShortcut#launch(org.eclipse.jface.viewers.ISelection, java.lang.String)
	 */
	public void launch(ISelection selection, String mode) {
		String myName = PrologUIPlugin.getResourceString("Listener_Name");
		
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
	}

	/**
	 * @see org.eclipse.debug.ui.ILaunchShortcut#launch(org.eclipse.ui.IEditorPart, java.lang.String)
	 */
	public void launch(IEditorPart editor, String mode) {
		IEditorInput editorInput = editor.getEditorInput();
		if (editorInput instanceof IFileEditorInput) {
			IFile file = (IFile) ((IFileEditorInput) editorInput).getAdapter(IFile.class);

			launch(file.getProject(), mode);
		}
		else {
			MessageDialog.openError(null, PrologUIPlugin.getResourceString("Listener_Name"), 
				PrologUIPlugin.getResourceString("LaunchSelectSomething_Text"));
			return;
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
		String configName = PrologUIPlugin.getResourceString("RunListenerLaunchConfiguration_Name");
		
		// Build launch configuration
		ILaunchConfiguration config = null;
		try {
			// Look for the default configuration, creating it if not found
			ILaunchConfigurationType configType = getLaunchConfigurationType();
			ILaunchConfigurationWorkingCopy wc = openConfiguration(configType, configName, mode).getWorkingCopy();

			// Set the perspective to switch to. This doesn't do anything.
			// We force the change in PrologDebugTarget.
			DebugUITools.setLaunchPerspective(configType, "debug", IDebugUIConstants.PERSPECTIVE_DEFAULT);
//			DebugUITools.setLaunchPerspective(configType, "run", IDebugUIConstants.PERSPECTIVE_NONE);
			
			// Build full path to xpl file
  			String xplPathname = PrologCorePlugin.getAmziDir() + "abin" + System.getProperty("file.separator") + 
				"alis.xpl"; 
   					
   			// Save the information in the configuration
			List groups = new ArrayList();
			groups.add(IDebugUIConstants.ID_RUN_LAUNCH_GROUP);
			wc.setAttribute(IDebugUIConstants.ATTR_FAVORITE_GROUPS, groups);
			// If private, the default launch perspective is not opened
//			wc.setAttribute(IDebugUIConstants.ATTR_PRIVATE, true);
			wc.setAttribute(IDebugUIConstants.ATTR_LAUNCH_IN_BACKGROUND, true);
			wc.setAttribute("projectName", project.getName());
			wc.setAttribute("xplPathname", xplPathname);
			wc.setAttribute("projectPathname", project.getLocation().toOSString());
			wc.setAttribute("cfgPathname", "");
			wc.setAttribute("binPathname", buildProps.getProperty("binFolder"));
			String consultList = "";
			IResource members[] = project.members();
			String proExcludeNames = buildProps.getProperty("proExcludeNames", "");
			for (int i = 0 ; i < members.length ; i++) {
				if (members[i].getType() == IResource.FILE && 
					members[i].getFileExtension() != null &&
					members[i].getFileExtension().toLowerCase().equals("pro") &&
					members[i].exists() &&
					proExcludeNames.indexOf(members[i].getName()) == -1 ) {
					if (members[i].isLinked())
						consultList += "'" + members[i].getLocation().toOSString() + "',";
					else
						consultList += "'" + members[i].getProjectRelativePath().toOSString() + "',";
				}
			}
			if (consultList.length() > 0)
				consultList = consultList.substring(0, consultList.length()-1);
			wc.setAttribute("consultList", consultList);
			
			String libs[] = buildProps.getProperty("plmLibraryNames").split(",");
			String loadList = "";
			for (int i = 0 ; i < libs.length ; i++)
				if (libs[i].length() > 0)
					loadList += "'" + libs[i] + "',";
			List plmRefs = getReferencedPlmFiles(project, null);
			for (int i = 0 ; i < plmRefs.size() ; i++)
				loadList += "'" + plmRefs.get(i) + "',";
			if (loadList.length() > 0)
				loadList = loadList.substring(0, loadList.length()-1);
			wc.setAttribute("loadList", loadList);
			
/*			String lsxs[] = buildProps.getProperty("lsxExtensionNames").split(",");
			String lsxList = "";
			for (int i = 0 ; i < lsxs.length ; i++)
				if (lsxs[i].length() > 0)
					lsxList += "'" + lsxs[i] + "',";
			if (lsxList.length() > 0)
				lsxList = lsxList.substring(0, lsxList.length()-1);
			wc.setAttribute("lsxList", lsxList);
*/
			wc.setAttribute("lsxList", buildProps.getProperty("lsxExtensionNames"));

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
			"com.amzi.prolog.ui.launch.prologListener");
	}

/*	private ILaunchManager getLaunchManager() {
		return DebugPlugin.getDefault().getLaunchManager();
	}
*/
	public List getReferencedPlmFiles(IProject project, IProgressMonitor monitor) throws CoreException
	{
		IFolder binFolder;
		IResource newMembers[];
		IProject projRefs[] = project.getReferencedProjects();
		List plmNames = new ArrayList();
		
		for (int p = 0 ; p < projRefs.length ; p++)
		{
			if (monitor != null)
				monitor.subTask("Finding files in referenced project "+projRefs[p].getName());
			if (projRefs[p].exists() && projRefs[p].getLocation() != null) {
				Properties buildProps = ProjectProperties.getProperties(project);
				String binFoldername = buildProps.getProperty("binFolder");
				if (binFoldername.length() > 0) {
					binFolder = projRefs[p].getFolder(binFoldername);
					newMembers = binFolder.members();
				}
				else {
					newMembers = projRefs[p].members();
				}
				for (int i = 0 ; i < newMembers.length ; i++) {
					// Only link plm files
					if (newMembers[i].getType() == IResource.FILE && 
						newMembers[i].getFileExtension() != null &&
						newMembers[i].getFileExtension().toLowerCase().equals("plm") &&
						newMembers[i].exists()) {
							String filename = newMembers[i].getLocation().toOSString(); 
							if (monitor != null)
								monitor.subTask("Found "+ filename);
							plmNames.add(filename);					
					}
				}
			}
		}
		return plmNames;
	}

}

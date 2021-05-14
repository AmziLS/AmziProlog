package com.amzi.prolog.ui.wizards;

import com.amzi.prolog.core.PrologCorePlugin;
//import com.amzi.prolog.ui.PrologPluginImages;

import java.io.File;
import java.io.FileInputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
//import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Platform;
//import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;
import com.amzi.prolog.ui.build.ProjectProperties;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class NewProject	extends Wizard
	implements INewWizard, IExecutableExtension {

//	private IStructuredSelection selection;
//	private IWorkbench workbench;
	private NewProjectCreationPage projectPage;
	private IProject project;
	private IConfigurationElement config;
//	private String strPathName;
	private int state;
	
	/**
	 * Constructor for NewProject.
	 */
	public NewProject() {
		super();
//		setDefaultPageImageDescriptor(PrologPluginImages.DESC_AMZIPAW);		
	}
	
	/**
	 * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench, org.eclipse.jface.viewers.IStructuredSelection)
	 */
	public void init(IWorkbench workbench, IStructuredSelection selection) {
//		this.workbench = workbench;
//		this.selection = selection;
		setNeedsProgressMonitor(true);		
		setWindowTitle("New");	
	}

	/**
	 * @see Wizard#addPages
	 */
	public void addPages() {
		super.addPages();
		projectPage = new NewProjectCreationPage("New");		
		addPage(projectPage);
	}

	/**
	 * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement, java.lang.String, java.lang.Object)
	 */
	public void setInitializationData(
		IConfigurationElement config,
		String propertyName,
		Object data)
		throws CoreException {
			this.config = config;
	}

	/**
	 * @see org.eclipse.jface.wizard.IWizard#performFinish()
	 * To Do: Update other views (like the Project View)
	 */
	public boolean performFinish() {
		// Create the project and associate it with Prolog
		project = createProject();
		if(project == null) {
			return false;
		}
		if (setProjectNature() == false) {
			return false;
		}

		// Save our build properties
		Properties buildProps = ProjectProperties.getDefaultProperties(project);
		ProjectProperties.saveProperties(project, buildProps);
		
		// Update the perspective to ours
		BasicNewProjectResourceWizard.updatePerspective(config);
		
		return true;
	}

	private IProject createProject() {

		final IProject newProject = projectPage.getProjectHandle();

		IPath defaultPath = Platform.getLocation();		
		IPath newPath = projectPage.getLocationPath();
//		strPathName =( projectPage.getLocationPath().toOSString())+ File.separator + newProject.getName();	

		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		final IProjectDescription description = workspace.newProjectDescription(newProject.getName());
		
		if (defaultPath.equals(newPath)){
			description.setLocation(null);
		}else{
			IPath tmpPath= (IPath) newPath; //.append(newProject.getName());
			description.setLocation(tmpPath);
		}	

		// create the new project operation
		WorkspaceModifyOperation op = new WorkspaceModifyOperation() {
			protected void execute(IProgressMonitor monitor) throws CoreException {
				try {
					// Create the project
					createProject(description, newProject, monitor);
					 
					// Create the amzi.cfg file
					String amziDir = PrologCorePlugin.getAmziDir();
					File amziConfig = new File(amziDir + System.getProperty("file.separator") +
						"config" + System.getProperty("file.separator") + "amzi.cfg");
					if (amziConfig.exists()) {
						FileInputStream input = new FileInputStream(amziConfig);
						IFile projConfig = newProject.getFile("amzi.cfg");
						if (!projConfig.exists())
							projConfig.create(input, true, monitor);
					}

					// Create the bin folder (now done in builder)
//					IFolder binFolder = newProject.getFolder("bin");
//					binFolder.create(true, true, monitor);

					// Refresh all the resources in case this project is created in
					// an existing directory
					newProject.refreshLocal(IResource.DEPTH_INFINITE, monitor);
				}
				catch(Exception e){
					state = 0;
					return;
				}
			}
		};

		if(state != 0){			
			return null;
		}
		
		// run the new project creation operation
		try {
			getContainer().run(true, true, op);
			return newProject;
		}
		catch (InterruptedException e) {
			return null;
		}
		catch (InvocationTargetException e) {
		    return null;
		}
	}
	
	private void createProject(IProjectDescription description, IProject projectHandle, IProgressMonitor monitor) throws CoreException, OperationCanceledException {
		try {
			monitor.beginTask("",2000);
			projectHandle.create(description, new SubProgressMonitor(monitor,1000));
			if (monitor.isCanceled())
				throw new OperationCanceledException();
			projectHandle.open(new SubProgressMonitor(monitor,1000));
		}
		catch(CoreException e){
			state = -1;
			return;
		}
		catch(OperationCanceledException e){
			state = -1;
			return;
		}
		catch(Exception e){
			state = -1;
			return;
		}
		finally {
			monitor.done();
		}
		state = 0;
	}

	private boolean setProjectNature() {
  		try {
      		IProjectDescription description = project.getDescription();
      		String[] natures = description.getNatureIds();
      		String[] newNatures = new String[natures.length + 1];
      		System.arraycopy(natures, 0, newNatures, 0, natures.length);
      		newNatures[natures.length] = "com.amzi.prolog.ui.PrologNature";
      		description.setNatureIds(newNatures);
      		project.setDescription(description, null);
   		} catch (CoreException e) {
      		// Something went wrong
      		return false;
   		}
   		return true;
	}
	
	
}

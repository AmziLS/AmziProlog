package com.amzi.prolog.ui.build;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
//import org.eclipse.core.runtime.Status;

public class PrologProject implements IProjectNature {

	private IProject project;
	
	/**
	 * Constructor for PrologProject.
	 */
	public PrologProject() {
		super();
	}

	/**
	 * @see org.eclipse.core.resources.IProjectNature#configure()
	 */
	public void configure() throws CoreException {
		// The builder is its id and the plug-in it is part of, not the classname
		// (like Natures)
		addToBuildSpec("com.amzi.prolog.ui.PrologBuilder");
	}

	private void addToBuildSpec(String builderID) throws CoreException {
//		try{
			if(builderID != null){
				IProjectDescription description =  project.getDescription();
				ICommand command = description.newCommand();
				command.setBuilderName(builderID);
				ICommand[] newCommands = new ICommand[1];
				newCommands[0] = command;
				description.setBuildSpec(newCommands);
				project.setDescription(description,null);
			}
//		}
//		catch(Exception e){
//			return;
//		}
	}

	/**
	 * @see org.eclipse.core.resources.IProjectNature#deconfigure()
	 */
	public void deconfigure() throws CoreException {
		removeFromBuildSpec("com.amzi.prolog.ui.PrologBuilder");
	}

	private void removeFromBuildSpec(String builderID) throws CoreException {
//		try{
			if(builderID != null){
				IProjectDescription description = getProject().getDescription();
				ICommand[] commands = description.getBuildSpec();
				for (int i = 0; i < commands.length; ++i) {
					if (commands[i].getBuilderName().equals(builderID)) {
						ICommand[] newCommands = new ICommand[commands.length - 1];
						System.arraycopy(commands, 0, newCommands, 0, i);
						System.arraycopy(commands, i + 1, newCommands, i, commands.length - i - 1);
						description.setBuildSpec(newCommands);
						getProject().setDescription(description, null);
						return;
					}
				}
			}
//		}
//		catch (Exception e){
//			return;
//		}
	}

	/**
	 * @see org.eclipse.core.resources.IProjectNature#getProject()
	 */
	public IProject getProject() {
		return project;
	}

	/**
	 * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
	 */
	public void setProject(IProject value) {
		project = value;
	}

}

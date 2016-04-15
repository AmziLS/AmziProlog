package com.amzi.prolog.ui.launch;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.sourcelookup.ISourceContainer;
import org.eclipse.debug.core.sourcelookup.ISourcePathComputerDelegate;
import org.eclipse.debug.core.sourcelookup.containers.DirectorySourceContainer;
import org.eclipse.debug.core.sourcelookup.containers.FolderSourceContainer;
import org.eclipse.debug.core.sourcelookup.containers.ProjectSourceContainer;
import org.eclipse.debug.core.sourcelookup.containers.WorkspaceSourceContainer;

public class PrologSourcePathComputerDelegate implements
		ISourcePathComputerDelegate {

	public ISourceContainer[] computeSourceContainers(
			ILaunchConfiguration configuration, IProgressMonitor monitor)
			throws CoreException {
		List<ISourceContainer> array = new ArrayList<ISourceContainer>();

		String path = configuration.getAttribute("projectName", (String)null);
		ISourceContainer sourceContainer = null;
		if (path != null) {
//			IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(new Path(path).lastSegment());
			IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(path);
			if (resource != null) {
//				IContainer container = resource.getParent();
				if (resource.getType() == IResource.PROJECT) {
					sourceContainer = new ProjectSourceContainer((IProject)resource, false);
					array.add(sourceContainer);
					IProject project = (IProject) resource;
					IResource members[] = project.members();
					for (int i = 0 ; i < members.length ; i++) {
						if (members[i].getType() == IResource.FILE && 
							members[i].getFileExtension() != null &&
							members[i].getFileExtension().toLowerCase().equals("pro") &&
							members[i].isLinked()) {
							IPath mpath = members[i].getLocation();
							array.add(new DirectorySourceContainer(mpath.removeLastSegments(1), false));
						}
					}
				} 
				else if (resource.getType() == IResource.FOLDER) {
					sourceContainer = new FolderSourceContainer((IFolder)resource, false);
					array.add(sourceContainer);
				}
			}
		}
		if (sourceContainer == null) {
			sourceContainer = new WorkspaceSourceContainer();
			array.add(sourceContainer);
		}
		
		ISourceContainer[] result= new ISourceContainer[array.size()];
		array.toArray(result);
		return result;

//		return new ISourceContainer[]{sourceContainer};
	}

}

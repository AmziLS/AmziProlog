package com.amzi.prolog.ui.build;

import amzi.ls.*;
import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.core.utils.Utils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Status;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class PrologBuilder extends IncrementalProjectBuilder {
	private static String DEBUG_PLM_LIBRARY = "debug64.plm";
	
	boolean compileOK;	

	/**
	 * Constructor for PrologBuilder.
	 */
	public PrologBuilder() {
		super();
	}

	/**
	 * Builder
	 */
	protected IProject[] build(int type, Map args, IProgressMonitor monitor)
		throws CoreException {
		IFolder binFolder;
		List plmPathnames;
		String binFoldername, binPathname, linkMessage;
		int rc, debugPlmCount = 0;
		
		// Don't do autobuilds
//		if (type == AUTO_BUILD) 
//			return null;
		
		// Make sure we have a valid project
		IProject project = getProject();	
		if (project == null || !project.exists() || project.getLocation() == null || 
			!project.isOpen() || !project.hasNature("com.amzi.prolog.ui.PrologNature")) {
			return null;
		}			

		// Refresh the project to find any file/dir changes made outside eclipse
		project.refreshLocal(IResource.DEPTH_INFINITE, monitor);
		
		// Get build properties
		Properties buildProps = ProjectProperties.getProperties(project);

		// Determine output directory, make sure it exists
		binFoldername = buildProps.getProperty("binFolder");
		if (binFoldername != null && binFoldername.length() > 0) {
			binFolder = project.getFolder(binFoldername);
			if (!binFolder.exists())
				binFolder.create(false, true, monitor);
			binPathname = binFolder.getLocation().toOSString();
		}
		else {
			binFolder = null;
			binPathname = getProject().getLocation().toOSString();
		}

		// If it's a full build clean out all plms and xpls from output directory
		if (type == FULL_BUILD || type == CLEAN_BUILD) {
			// Clean out markers on the project
			// Markers are deleted in compile on a per-file basis
//			project.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_ZERO);
				
			IResource oldMembers[];
			if (binFoldername.length() > 0)
				oldMembers = project.getFolder(binFoldername).members();
			else
				oldMembers = project.members(false);

			// Delete plms and xpls
			for (int i = 0 ; i < oldMembers.length ; i++) {
				if (oldMembers[i].getType() == IResource.FILE && 
					oldMembers[i].getFileExtension() != null &&
					(oldMembers[i].getFileExtension().toLowerCase().equals("plm") ||
					oldMembers[i].getFileExtension().toLowerCase().equals("xpl"))) {
					oldMembers[i].delete(true, monitor);
				}
			}
		}
		
		// We're done if it's clean only
		if (type == CLEAN_BUILD)
			return null;
		
		// Compile the project
		plmPathnames = compileProject(type, buildProps.getProperty("buildType"), project,
			buildProps.getProperty("proExcludeNames", ""), binPathname, monitor);
		if (plmPathnames == null) compileOK = false;
		else compileOK = true;

		// If compile worked and there were source files, link
		if (compileOK && plmPathnames.size() > 0 && !monitor.isCanceled())  {
			// Refresh the project directory where the plm files were created
			if (binFolder == null)
				project.refreshLocal(IResource.DEPTH_ONE, monitor);
			else
				binFolder.refreshLocal(IResource.DEPTH_ONE, monitor);

			String xplPathname = buildProps.getProperty("xplPathname");
	   		if (compileOK && !monitor.isCanceled() && xplPathname != null &&
	   			xplPathname.trim().length() > 0)
	   		{
	   			File xplFile = new File(xplPathname);
	   			// isAbsolute does not work for Windows pathnames beginning with \
				if (!xplFile.isAbsolute() && !xplPathname.startsWith(System.getProperty("file.separator")) &&
					binPathname.length() > 0) 
					xplPathname = binPathname + System.getProperty("file.separator") + xplPathname;
	  			monitor.subTask("Linking " + xplPathname);
	
				// Convert the plm file names to a string list
				String[] plmList = new String[plmPathnames.size()];
				plmPathnames.toArray(plmList);

	
	   			// Get all the libraries
				String libNames = buildProps.getProperty("plmLibraryNames");
				String libList[] = null;
				if (libNames.length() > 0) 
					libList = libNames.split(",");
				
				// Get all the referenced plm names
				List refNames = getReferencedPlmFiles(project, monitor);
				String[] refList = new String[refNames.size()];
				refNames.toArray(refList);
	
				// Merge all the plms and libraries into one list
				// Must start with libraries for operator definitions
				String[] linkList;
				
				// Add one for debug mode
				if (buildProps.getProperty("buildType").equalsIgnoreCase("debug"))
					debugPlmCount = 1;

				if (libList == null)
					linkList = new String[plmList.length + refList.length + debugPlmCount];
				else
					linkList = new String[plmList.length + refList.length + libList.length + debugPlmCount];
	
				int i = 0, j, libListLength;
				if (libList != null) {
					for (i = 0 ; i < libList.length ; i++ )
						linkList[i] = libList[i];
					libListLength = libList.length;
				}
				else
					libListLength = 0;
				for (j = i ; j < i+plmList.length ; j++)
					linkList[j] = plmList[j - libListLength];
				for (int k = j ; k < j + refList.length ; k++)
					linkList[k] = refList[k - plmList.length];
				
				// Add on the debugger plm (if needed)
				if (buildProps.getProperty("buildType").equalsIgnoreCase("debug"))
					linkList[linkList.length-1] = DEBUG_PLM_LIBRARY;

	   			LogicServer ls = new LogicServer();
				synchronized (ls) {
					rc = ls.Link(xplPathname, linkList, "no options");
	   				linkMessage = ls.getLinkMessage().replaceAll("\n", " : ");
				}
				if (rc != 0)
	   	   			throw new CoreException(new Status(Status.ERROR, "com.amzi.prolog.ui", 200, "Linker Failed: " + new Integer(rc).toString() + " - "+ linkMessage, null));
	   		}
		} 

   		// Refresh the project directory where the XPL file was created
   		if (binFolder == null)
			project.refreshLocal(IResource.DEPTH_ONE, monitor);
		else
			binFolder.refreshLocal(IResource.DEPTH_ONE, monitor);
		IResource finalMembers[];
		if (binFoldername != null && binFoldername.length() > 0)
			finalMembers = project.getFolder(binFoldername).members(false);
		else
			finalMembers = project.members(false);

		// Mark all the plm and xpl resources as derived
		monitor.subTask("Cleaning Up");
		for (int i = 0 ; i < finalMembers.length ; i++) {
			// Only mark generated binary files
			if (finalMembers[i].getType() == IResource.FILE && 
				finalMembers[i].getFileExtension() != null &&
				(finalMembers[i].getFileExtension().toLowerCase().equals("plm") ||
				 finalMembers[i].getFileExtension().toLowerCase().equals("xpl"))) {
				finalMembers[i].setDerived(true);
			}
		}

		// We don't need deltas for other projects
		return null;
	}

	private List compileProject(int type, String buildType, IProject project, String proExcludeNames, 
		String plmPathname, IProgressMonitor monitor)
		throws CoreException {
		boolean needsLink = false;
		boolean compileError = false;
		LogicServer ls = null;
		String projectPath, proFilename, plmFilename = null;
		List plmNames = new ArrayList();

		// Load up the compiler
		ls = new LogicServer();
		try {
			synchronized (ls) {
				// If there is an amzi.cfg then specify it
				IFile cfgFile = project.getFile("amzi.cfg");
				if (cfgFile.exists())
					ls.Init(Utils.tiltSlashes(cfgFile.getLocation().toOSString()));
				else
					ls.Init("");
	
				ls.AddLSX("aosutils", 0);
				String amziDir = PrologCorePlugin.getAmziDir();
				ls.Load(amziDir+"abin"+System.getProperty("file.separator")+"acmp.xpl");
				
				// xcompile will chdir to the project directory so relative pathname
				// includes work during compile
				projectPath = project.getLocation().toOSString();
			}
		}
		catch (LSException ex) {
			// Always close the LogicServer
			try { 
				ls.Close(); 
				System.gc();
			}
			catch (LSException ex2) { }
			throw new CoreException(new Status(Status.ERROR, "com.amzi.prolog.ui", 
				100, "Compiler Init or Load Failed: " + ex.GetMsg(), null));
		}
				
		// Get all the files in the project
		IResource members[] = project.members(false);
		for (int i = 0 ; i < members.length ; i++) {
	      	try {
	      		synchronized (ls) {
	//				monitor.worked(i);
	
					// Bug out if cancelled
					if (monitor.isCanceled()) {
						try {
							// Don't stay in the project directory
							ls.ExecStr("chdir(`..`)");
							ls.Close();
							System.gc();
						}
						catch (LSException ex2) { }
						return null;
					}
					
					// Only rebuild pro files not on exclude list
					if (members[i].getType() == IResource.FILE && 
						members[i].getFileExtension() != null &&
						members[i].exists() &&
						members[i].getFileExtension().toLowerCase().equals("pro") &&
						proExcludeNames.toLowerCase().indexOf(members[i].getName().toLowerCase()) == -1 ) {
						if (members[i].isLinked())
							proFilename = members[i].getLocation().toOSString();
						else
							proFilename = members[i].getProjectRelativePath().toOSString();
						IPath tempPath = members[i].getLocation().removeFileExtension().addFileExtension("plm"); 
						plmFilename = plmPathname + System.getProperty("file.separator") +
							tempPath.lastSegment();						
	
						// Check if the pro file is newer, or if there is no plm file
						long proTime = 0;
						long plmTime = 0;
						if (type != FULL_BUILD) {
							proTime = new File(projectPath + System.getProperty("file.separator") +
								proFilename).lastModified();
							File plmFile = new File(plmFilename);
							if (plmFile.exists()) {
								plmTime = plmFile.lastModified();
							}
						}
						
						// Save the names of all our plm files
						plmNames.add(plmFilename);
						
						// Recompile everything on a full build, 
						// only modified on INCREMENTAL or AUTO
						if (type == FULL_BUILD || proTime > plmTime || proTime == 0) {
							// Delete the problem markers before building
							try {
	      						members[i].deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_ZERO);
	   						} catch (CoreException e) {
	      						// something went wrong
	   						}
	
							// Compile the file
							// Tell the user what we are building
							monitor.subTask("Compiling "+members[i].getName());
		          			String  s = "amzi_compiler:";
		          			if (buildType.equalsIgnoreCase("release"))
		          				s += "xcompile('";
		          			else
		          				s += "debug_compile('";
		          				
		          			s += Utils.tiltSlashes(projectPath)+"', '"+
		          				Utils.tiltSlashes(proFilename)+"', '"+
		          				Utils.tiltSlashes(plmFilename)+"', null)";
							long term = ls.ExecStr(s);
		    	 			if (term == 0) {
								try {
									// Don't stay in the project directory
									ls.ExecStr("chdir(`..`)");
									ls.Close();
									System.gc();
								}
								catch (LSException ex2) { }
	
		        	   			throw new CoreException(new Status(Status.ERROR, "com.amzi.prolog.ui", 101, "Compiler Failed", null));
							}
							else
								needsLink = true;
		    	 			
		    	 			if (buildType.equalsIgnoreCase("debug")) {
			    				term = ls.CallStr("amzi_system:parse_message(Type, File, Line, Msg, Text)");
			    				if (term != 0) {
			    					boolean tf = true;
			    					while (tf) {
			    						boolean isError = false;
			    						if (ls.GetStrArg(term, 1).equalsIgnoreCase("error")) isError = true;
			    						addProblemMarker(isError, ls.GetStrArg(term, 4),
			    							ls.GetStrArg(term, 2), ls.GetIntArg(term, 3),
			    							ls.GetStrArg(term, 5));
			    						 tf = ls.Redo();
			    					}
			    				}
		    	 			}
		     			}
					} 					
	      		}
	      	}
	      	catch (LSException ex) {
	      		compileError = true;
	      		
	      		// Try to delete the plm file
	      		if (plmFilename != null) {
	      			new File(plmFilename).delete();
	      		}
	      		
	      		// Get all the details about the error
//	      		synchronized (ex) {
		      		String msg = ex.GetMsg();
//		      		int rc = ex.GetRC();
//		      		int errtype = ex.GetType();
		      		String readbuf = ex.GetReadBuffer();
		      		int lineno = ex.GetLineno();
					String stack = ex.GetCallStack();
	
					// Add it onto the task list
					String location = "";
					if (readbuf != null && readbuf.length() > 0) location = readbuf;
					if (stack != null && stack.length() > 0) location = stack;
					
					addProblemMarker(true, msg, members[i].getName(), lineno, location);

/*					IMarker marker = members[i].createMarker(IMarker.PROBLEM);
			 	    if (marker.exists()) {
				    	try {
				    		msg += " (compile)";
				         	marker.setAttribute(IMarker.MESSAGE, msg);
				         	if (lineno != 0) {
					         	marker.setAttribute(IMarker.LINE_NUMBER, lineno);
				         	} else if (readbuf != null && readbuf.length() > 0) {
				         		marker.setAttribute(IMarker.LOCATION, readbuf);
				         	} else if (stack != null && stack.length() > 0) {
				         		marker.setAttribute(IMarker.LOCATION, stack);
				         	}
				         	marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
				         	marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_HIGH);
				      	} 
				      	catch (CoreException e) {
				         // You need to handle the case where the marker no longer exists 
				        }
				   	} */
//	      		}
     		}
		}
		
		// Always close the LogicServer
		if (ls != null) {
			try {
				synchronized (ls) {
					// Don't stay in the project directory
					ls.ExecStr("chdir(`..`)");
					ls.Close();
					System.gc();
				}
			}
			catch (LSException ex2) {
			}
		}

		if (!needsLink || compileError) return null;
		else return plmNames;
	}
	
	private void addProblemMarker(boolean isError, String message, String filename, int lineno, String location) {
		IProject project = getProject();
		IResource file = project.findMember(filename);
		if (file == null || !file.exists()) return;
		
		// Add it onto the task list
		try {
			IMarker markers[] = file.findMarkers(null, true, IResource.DEPTH_INFINITE);
			for (int i = 0 ; i < markers.length ; i++) {
				if (markers[i].getAttribute(IMarker.MESSAGE, "").equals(message) &&
					markers[i].getAttribute(IMarker.LINE_NUMBER, 0) == lineno)
					return;
			}
			IMarker marker = file.createMarker(IMarker.PROBLEM);
			if (marker.exists()) {
				marker.setAttribute(IMarker.MESSAGE, message + " (compile)");
				if (lineno != 0) {
					marker.setAttribute(IMarker.LINE_NUMBER, lineno);
				} else if (location != null && location.length() > 0) {
					marker.setAttribute(IMarker.LOCATION, location);
				}
				
				if (isError) {
					marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
					marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_HIGH);
				}
				else {
					marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
					marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_NORMAL);
				}
			} 
		}
		catch (CoreException e) {
		 // You need to handle the case where the marker no longer exists 
		}
	}


	private List getReferencedPlmFiles(IProject project, IProgressMonitor monitor) throws CoreException
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

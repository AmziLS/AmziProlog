package com.amzi.prolog.ui.build;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.Properties;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
 
public class ProjectProperties {

	public static Properties getDefaultProperties(IProject project) {
		Properties buildProps = new Properties();

		buildProps.setProperty("buildType", "Release");
		buildProps.setProperty("xplPathname", project.getName()+".xpl");
		buildProps.setProperty("binFolder", "bin");
		buildProps.setProperty("plmLibraryNames", ""); 
		buildProps.setProperty("lsxExtensionNames", "");
		buildProps.setProperty("proExcludeNames", "");
		
		return(buildProps);
	}
	
	public static Properties getProperties(IProject project) {
		// Create and load default properties
		Properties defaultProps = getDefaultProperties(project);

		// Create program properties with default
		Properties buildProps = new Properties(defaultProps);
		
		// Now load properties from disk
		try {
			String propPath = project.getLocation().toOSString()+System.getProperty("file.separator")+"build.properties";
			FileInputStream in = new FileInputStream(propPath);
			buildProps.load(in);
			in.close();
		}
		catch (Exception ex) {	// FileNotFoundException or IOException
		}
		
		return(buildProps);
	}
	
	public static boolean saveProperties(IProject project, Properties buildProps) {
		try {
			String propPath = project.getLocation().toOSString()+System.getProperty("file.separator")+"build.properties";
			FileOutputStream out = new FileOutputStream(propPath);
			buildProps.store(out, "Prolog Project Build Properties");
			out.close();
			
			// This doesn't refresh the editor
			project.refreshLocal(IResource.DEPTH_ONE, null);
			IResource buildResource = project.findMember("build.properties");
			if (buildResource != null) 
				buildResource.refreshLocal(IResource.DEPTH_ZERO, null);
		}
		catch (Exception ex) {	// FileNotFoundException, IOException or CoreException
			return false;
		}
		
		return true;
	}

}

package com.amzi.prolog.core;

import com.amzi.prolog.core.utils.Utils;
import amzi.ls.LogicServer;
import amzi.ls.LSException;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.*;

//import org.eclipse.core.boot.BootLoader;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
 
/**
 * The main plugin class to be used in the desktop.
 */
public class PrologCorePlugin extends AbstractUIPlugin {
	public static int RUN_DISTRIBUTE 	= 0x01;
	public static int RUN_EXPIRES       = 0x02;
	public static int RUN_MANY_ENGINES  = 0x04;
	public static int RUN_UNLOAD      	= 0x08;
	public static int RUN_SERVER        = (RUN_MANY_ENGINES | RUN_UNLOAD);
	public static int DEV_LOCAL_DEBUG   = 0x01;
	public static int DEV_REMOTE_DEBUG  = 0x02;

	//The shared instance.
	private static PrologCorePlugin plugin;
	//Resource bundle.
	private ResourceBundle resourceBundle;
	// Amzi! Directory
	private static String amziDir = null;
	// Home Directory
	private static String homeDir = null;
	// Keywords
	private static PrologKeywordsAndActions prologKeywords = null;
	// Actions Need Reloading
	private static boolean voidActions = false;
	
	/**
	 * The constructor.
	 */
	public PrologCorePlugin() throws CoreException {
		super();
		plugin = this;
	}

	/* (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);

//	System.out.println("Starting Core");
	
		// Get a shell
		Shell shell = null;
		Display display = Display.getCurrent();
		if (display != null)
			shell = Display.getCurrent().getActiveShell();
		
		// Get our dialog settings
		loadDialogSettings();
		
		// Get our resources
		try {
			resourceBundle= ResourceBundle.getBundle("com.amzi.prolog.core.PrologCorePluginResources");
		} 
		catch (MissingResourceException x) {
			resourceBundle = null;
		}
		
		String userdir = System.getProperty("user.dir");
		String slash = System.getProperty("file.separator");
		boolean winos;
		if (System.getProperty("os.name").contains("Windows")) winos = true; else winos = false;
		boolean macos;
		if (System.getProperty("os.name").contains("Mac")) macos = true; else macos = false;
		// we want userdir to be the directory where eclipse started from.
		// on the mac, it actually starts at Eclipse.app/Contents/MacOS, so we need to back up two.
		if (macos) userdir = userdir + slash + ".." + slash + ".." + slash + "..";
		

		// Get the value of amziDir if not already set

		if (amziDir == null) {
			amziDir = System.getenv("AMZI_DIR");
			if (amziDir == null) {
				// the directory for the full ide is at the same level,
				// amzi/eclipse, as the amzi/core directory
				amziDir = userdir + slash + ".." + slash + "apls" + slash;			
			}
			else {
				amziDir += slash;
			}
		}
		
		// os.name helps, Windows versions all seem to start with 'Windows'
		// Linux with 'Linux' and Mac with 'Mac OS'.
		String amzivm;
		
		if (winos) {
			amzivm = amziDir + "bin" + slash + "amzi.dll";
		}
		else {
			amzivm = amziDir + "lib" + slash + "libamzi.so";
		}
		
		// I'm not sure we need to do this, the Java Logic Server will load it...
		//String dllpath = amziDir + "bin" + slash + "amzi.dll";
		//String dllpath = "/Users/dennis/Documents/amzi/release/mac/apls/lib/libamzi.so";
		
		//MessageDialog.openError(null,"My Info", "First 1 try load: " + amzivm);
		try
		{
			System.load(amzivm);
			//MessageDialog.openError(null, "Found it",
			//		"PrologCorePlugin Found libamzi using AMZI_DIR: " + amzivm);
		}
		catch (Error ex){
				//System.out.println("ex Unable to load Amzi! DLL");
				MessageDialog.openWarning(shell,"Amzi! Error", "Unable to load Amzi! dynamic library from: \n" + amzivm
						+ "\nSet the system environment variable AMZI_DIR to the (install directory)/amzi/apls/");
		}

		//MessageDialog.openError(null,"My Info", "Making New LogicServer");
		LogicServer ls = new LogicServer();

		// Get the value of HOME (used for Unix licensing)
		if (homeDir == null)  {
			try {
				homeDir = ls.getEnvironmentVariable("HOME");
				if (homeDir.length() > 0) homeDir += System.getProperty("file.separator");
				else homeDir = null;
			}
			catch (Exception ex) {	
			}
		}
		//MessageDialog.openError(null,"My Info", "Calling prologKeywords");

		// Get our keywords and privileges
		if (prologKeywords == null) {
			prologKeywords = new PrologKeywordsAndActions(getAmziDir());
		}
		
		IPreferenceStore prefs = getPreferenceStore();

/*
		// Check if our documentation is installed

		// Make sure our docs are configured
		if (Platform.getBundle("com.amzi.prolog.help") == null) {
			prefs.setValue("AMZI_DIR", amziDir);

			// Let the user know how to do it
			if (!makeDocLink()) {
				if (shell != null)
					MessageDialog.openInformation(shell, "Warning",
						"Unable to install Amzi! Documentation.\r\n   Select Help | Update Manager\r\n   In the 'Feature Updates' view, open your amzi/docs install directory \r\n   Right-click on 'eclipse' and select 'Link Product Extension'.");
			}
			else
				if (shell != null)
					MessageDialog.openInformation(shell, "Install",
						"Successfully installed Amzi! Documentation.\r\nRestart Eclipse to view the documentation in Help.");
		}
		
		// Check if a new version of Amzi! was installed
		// If so, reconfigure the docs
		String pAmziDir = prefs.getString("AMZI_DIR");
		if (!pAmziDir.equals(amziDir)) {
			prefs.setValue("AMZI_DIR", amziDir);
			
			// Let the user know how to do it
			if (!makeDocLink()) {
				if (shell != null)
					MessageDialog.openInformation(shell, "Warning",
						"Unable to update Amzi! Documentation.\r\n   Select Help | Update Manager\r\n   In the 'Feature Updates' view, open your amzi/docs install directory \r\n   Right-click on 'eclipse' and select 'Link Product Extension'.");
			}
			else
				if (shell != null)
					MessageDialog.openInformation(shell, "Install",
						"Successfully re-installed Amzi! Documentation.\r\nRestart Eclipse to view the documentation in Help.");	
		}
*/
		
	}

	/* (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
	}
	
	private boolean makeDocLink() {
		boolean installFailed = false;
			
/*
		 String sep = System.getProperty("file.separator");
		 
		URL installURL = Platform.getInstallLocation().getURL();
		String filepath = installURL.getFile();
			
		// Create the links subdirectory under eclipse
		String linkPath;
		if (filepath.endsWith(sep))
			linkPath = filepath + "links";
		else
			linkPath = filepath + sep + "links";
		File linkDir = new File(linkPath);
		if (!linkDir.exists())
			linkDir.mkdir();
				
		// Create the .link file for our doc plugin
		File linkFile = new File(linkPath + sep + "com.amzi.prolog.help.link");
		if (!linkFile.exists())
			try {
				if (!linkFile.createNewFile())	{
					installFailed = true;
				}
			}
			catch (IOException ex) {
				installFailed = true;
			}
			
		// Write the AMZI_DIR path to the file adding the docs subdirectory
		if (!installFailed) {
			try {
				FileWriter fileWriter = new FileWriter(linkFile);
				String amziDir = PrologCorePlugin.getAmziDir();
				String amziPlugPath;
				if (amziDir.endsWith(sep))
					amziPlugPath = amziDir + "docs";
				else
					amziPlugPath = amziDir + sep + "docs";
				fileWriter.write("path=" + Utils.doubleSlashes(amziPlugPath) + "\r\n");
				fileWriter.close();
			}
			catch (IOException ex) {	
				installFailed = true;		
			}
		}
*/
		return !installFailed;
	}
		
	/**
	 * Returns the shared instance.
	 */
	public static PrologCorePlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns the workspace instance.
	 */
//	public static IWorkspace getWorkspace() {
//		return ResourcesPlugin.getWorkspace();
//	}

	/**
	 * Returns the string from the plugin's resource bundle,
	 * or 'key' if not found.
	 */
	public static String getResourceString(String key) {
		ResourceBundle bundle= PrologCorePlugin.getDefault().getResourceBundle();
		try {
			return bundle.getString(key);
		} catch (MissingResourceException e) {
			return key;
		}
	}

	/**
	 * Returns the plugin's resource bundle,
	 */
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}
	
	/*
		Return our Logic Server
	 */
	public static LogicServer getLogicServer() throws LSException {
		return prologKeywords.getLogicServer();
	}
	
	/**
	 * Returns the setting of AMZI_DIR
	 */
	public static String getAmziDir() {
		return amziDir;
	}

	public static String getHomeDir() {
		return homeDir;
	}
	
	/**
 	* Returns all the Prolog Keywords
 	*/
	public static PrologKeywordsAndActions getPrologKeywords() {
		// Get the Prolog keywords
		if (prologKeywords == null)
			updateKeywordsAndActions();
		return prologKeywords;
	}

	public static String getPredicateInfo(String predicate) {
		// Get the Prolog keywords
		if (prologKeywords == null)
			updateKeywordsAndActions();
		return prologKeywords.getPredicateInfo(predicate);
	}

	/**
	 * Returns what actions are allowed
	 */

	
	public static void updateKeywordsAndActions() {
		prologKeywords = new PrologKeywordsAndActions(getAmziDir());
		voidActions = false;
	}
	
	public static void voidActions() {
		voidActions = true;
	}
	
	public void doReminders() {
		// Get a shell
		Shell shell = null;
		Display display = Display.getCurrent();
		if (display != null)
			shell = Display.getCurrent().getActiveShell();
		
		// See if we need any reminders
		IDialogSettings choices = getDialogSettings();
		}
	
}

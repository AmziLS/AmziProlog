package com.amzi.prolog.ui;

import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.ui.actions.LicenseActionDelegate;

import java.text.MessageFormat;
import java.util.*;

import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.plugin.*;
import org.eclipse.core.runtime.*;
import org.eclipse.core.resources.*;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class PrologUIPlugin extends AbstractUIPlugin {
	//The shared instance.
	private static PrologUIPlugin plugin;
	//Resource bundle.
	private static ResourceBundle resourceBundle;
	
	/**
	 * The constructor.
	 */
	public PrologUIPlugin() {
		super();
		plugin = this;
	}

	/* (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);

//	System.out.println("Starting UI");
		
		try {
			resourceBundle= ResourceBundle.getBundle("com.amzi.prolog.ui.PrologUIPluginResources");
		} catch (MissingResourceException x) {
			resourceBundle = null;
		}
		
		// If we're the free edition, put up our buy box
		// no, let's not for now.
		//String product = PrologCorePlugin.getProductType();
		//if (product == null || product.charAt(2) == '_') {
		//	LicenseActionDelegate licenseAction = new LicenseActionDelegate();
		//	licenseAction.run(null);
		//}
		
		// Initialize our preferences
//		IPreferenceStore store = getPreferenceStore();
//		PrologMainPreferencePage.initDefaults(store);
	}
	
	/* (non-Javadoc)
	 * @see org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
	}
	
	public static String getModelID() {
		return "com.amzi.prolog.ui";
	}
	
	/**
	 * Returns the shared instance.
	 */
	public static PrologUIPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns the workspace instance.
	 */
	public static IWorkspace getWorkspace() {
		return ResourcesPlugin.getWorkspace();
	}

	/**
	 * Returns the string from the plugin's resource bundle,
	 * or 'key' if not found.
	 */
	public static String getResourceString(String key) {
		try {
			return resourceBundle.getString(key);
		} catch (MissingResourceException e) {
			return key;
		}
	}

	public static String getFormattedResourceString(String key, String arg) {
		return getFormattedResourceString(key, new String[] { arg });
	}
	
	public static String getFormattedResourceString(String key, String[] args) {
		return MessageFormat.format(getResourceString(key), args);	
	}	

	/**
	 * Returns the plugin's resource bundle,
	 */
	public static ResourceBundle getResourceBundle() {
		return resourceBundle;
	}
	
	public IWorkbenchPage getActivePage() {
		IWorkbenchWindow window= getWorkbench().getActiveWorkbenchWindow();
		if (window == null) {
			IWorkbenchWindow windows[] = getWorkbench().getWorkbenchWindows();
			for (int i = 0 ; i < windows.length ; i++)
				if (windows[i].getActivePage() != null)
					return windows[0].getActivePage();
		}
		return getWorkbench().getActiveWorkbenchWindow().getActivePage();
	}
	
	public static void log(Throwable e) {
		log(new Status(IStatus.ERROR, "com.amzi.prolog.ui", IStatus.ERROR, "Error", e));
	}

	public static void log(IStatus status) {
		getDefault().getLog().log(status);
	}
	
/*	public static String getFilePath(String fileName) {
		String path = null;
		
		IPluginDescriptor desc = getDefault().getDescriptor();
		URL xplURL = desc.find(new Path(fileName));
		
		try {
			URL xplLocal = Platform.asLocalURL(xplURL);
			path = xplLocal.getFile();
		}
		catch (IOException ex) {
		}

		return path;
	}
*/	
}

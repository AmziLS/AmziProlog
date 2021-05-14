package com.amzi.prolog.ui.actions;

import amzi.ls.*;
import com.amzi.prolog.core.PrologCorePlugin;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.actions.ActionDelegate;
import org.eclipse.swt.widgets.Shell;

public class AboutActionDelegate extends ActionDelegate
	implements IWorkbenchWindowActionDelegate {

	private Shell shell;
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
	 */
	public void init(IWorkbenchWindow window) {
		shell = window.getShell();
	}
	
	public void run(IAction action) {
		String edition, license, version, amziDir;
		
		LogicServer ls = new LogicServer();
		try {
			synchronized (ls) {
				ls.Init("");
	
	/*//			InetAddress hostname[] = InetAddress.getAllByName( InetAddress.getLocalHost().getHostAddress() );
	//			String computer = hostname[0].getHostName();
				String computer = "Computer Name: ";
				try {
					computer += InetAddress.getLocalHost().getHostName();
				} catch (UnknownHostException ex) {
					computer = "unknown";
				} */
				
				amziDir = PrologCorePlugin.getAmziDir();
				ls.Load(amziDir+"abin"+System.getProperty("file.separator")+"acmp.xpl");
				version = "Version: " + ls.GetVersion();
				ls.Close();
			}
			
			license = "Open Source Edition   ";


			MessageDialog.openInformation(shell, "Amzi! Prolog + Logic Server",
				version + "\n" + "Running from AMZI_DIR="+amziDir + "\n\n" +
				license);
		}
		catch (LSException ex) {
			// Always close the LogicServer
			try { 
				ls.Close();
				System.gc();
			}
			catch (LSException ex2) { }
			MessageDialog.openError(shell, "Error",
				"AMZI_DIR is not set correctly or is not on your system PATH environment variable.");
		}

	}

}

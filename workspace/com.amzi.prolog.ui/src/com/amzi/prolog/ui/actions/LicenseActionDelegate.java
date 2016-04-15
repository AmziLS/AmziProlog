package com.amzi.prolog.ui.actions;

import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.core.utils.BrowserControl;
import com.amzi.prolog.ui.dialogs.UnixBuyDialog;

import java.io.IOException;

import org.eclipse.jface.action.IAction;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.actions.ActionDelegate;

public class LicenseActionDelegate extends ActionDelegate
	implements IWorkbenchWindowActionDelegate {

	private Shell shell;

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
	 */
	 public void init(IWorkbenchWindow window) {
		 shell = window.getShell();
	 }
	
	 public void run(IAction action) {

		// Run unlock.exe under Windows
	 	if (BrowserControl.isWindowsPlatform()) {
			try {
				Process process = Runtime.getRuntime().exec(PrologCorePlugin.getAmziDir() + "bin" + 
					System.getProperty("file.separator") + "activate.exe");
				process.waitFor();
				
				// Update the new license privileges (if any)
				PrologCorePlugin.updateKeywordsAndActions();
			} 
			catch (IOException ex) {
			} 
			catch (InterruptedException ex2) {
			}
	 	}
	 	else {
	 		UnixBuyDialog bd = new UnixBuyDialog(shell);
	 		bd.setBlockOnOpen(true);
	 		bd.open();

			// Update the new license privileges (if any)
	 	}
	
	 }

}

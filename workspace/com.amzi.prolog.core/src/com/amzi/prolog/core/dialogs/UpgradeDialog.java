package com.amzi.prolog.core.dialogs;

import com.amzi.prolog.core.utils.BrowserControl;

//import java.io.IOException;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
//import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */

/**
 * The Upgrade Dialog
 */
public class UpgradeDialog extends Dialog {

//	private Shell shell;

	public UpgradeDialog(Shell parentShell) {
		super(parentShell);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
//		shell = newShell;
		newShell.setText("Upgrade Required");
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite)super.createDialogArea(parent);
		Label label = new Label(composite, SWT.LEFT);
		label.setText("This feature is not available in the edition you are running.\n\nYou can upgrade by pressing the 'Yes' button below,\n"+
			"then enter your license key using File | Buy/Activate License Key.\nDo you want to purchase now?");		
		return composite;
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.YES_ID, IDialogConstants.YES_LABEL, true);
		createButton(parent, IDialogConstants.NO_ID, IDialogConstants.NO_LABEL, false);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	protected void buttonPressed(int buttonId) {
		super.buttonPressed(buttonId);
		if (buttonId == IDialogConstants.YES_ID) {
			BrowserControl.displayURL("http://www.amzi.com/download");
			super.okPressed();
		}
		else
			super.cancelPressed();

		/*		try {
					BrowserLauncher.openURL("http://www.amzi.com/download");
				}
				catch (IOException ex) {
					MessageDialog.openError(shell, "Error", "Unable to open browser to view: http://www.amzi.com/download/buy_prolog_maintenance.htm");
				}
		*/
	}

}

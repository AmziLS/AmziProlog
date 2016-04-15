package com.amzi.prolog.core.dialogs;

import com.amzi.prolog.core.utils.BrowserControl;

//import java.io.IOException;
import java.util.Date;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogSettings;
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
 

public class MaintenanceDialog extends Dialog {
	private static String BUY_LABEL = "Renew Now!";
	private static String REMIND_DAY_LABEL = "Remind in a Day";
	private static String REMIND_WEEK_LABEL = "Remind in a Week";
	private static String REMIND_NEVER_LABEL = "Never Remind";
	
//	private Shell shell;
	private IDialogSettings choices;
	private int daysLeft;

	public MaintenanceDialog(Shell parentShell, int daysLeft, IDialogSettings choices) {
		super(parentShell);
//		shell = parentShell;
		this.daysLeft = daysLeft;
		this.choices = choices;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Maintenance Renewal");
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite)super.createDialogArea(parent);
		Label label = new Label(composite, SWT.LEFT);
		label.setText("Software maintenance and technical support will expire for this product in "+
			new Integer(daysLeft).toString()+" days.\nPlease renew by visiting our Website, www.amzi.com");		
		return composite;
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.CLIENT_ID+1, REMIND_DAY_LABEL, false);
		createButton(parent, IDialogConstants.CLIENT_ID+2, REMIND_WEEK_LABEL, false);
		createButton(parent, IDialogConstants.CLIENT_ID+3, REMIND_NEVER_LABEL, false);
		createButton(parent, IDialogConstants.CANCEL_ID, BUY_LABEL, true);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	protected void buttonPressed(int buttonId) {
		long increment = 0;
		long now = new Date().getTime();

		switch (buttonId) {
			case IDialogConstants.CLIENT_ID+1:	// Day
				increment = 24*60*60*1000;
				break;
			case IDialogConstants.CLIENT_ID+2:	// Week
				increment = 7*24*60*60*1000;
				break;
			case IDialogConstants.CLIENT_ID+3:	// Never
				increment = -1;
				break;
		}
		
		// Save next reminder date
		if (increment >= 0)
			choices.put("maintenance_reminder_date", now+increment);
		else
			choices.put("maintenance_reminder_date", increment);
			
		super.buttonPressed(buttonId);
		close();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
	 */
	protected void cancelPressed() {
		super.cancelPressed();
		BrowserControl.displayURL("http://www.amzi.com/download");

/*		try {
			BrowserLauncher.openURL("http://www.amzi.com/download/buy_prolog_maintenance.htm");
		}
		catch (IOException ex) {
			MessageDialog.openError(shell, "Error", "Unable to open browser to view: http://www.amzi.com/download/buy_prolog_maintenance.htm");
		} */
	}


}

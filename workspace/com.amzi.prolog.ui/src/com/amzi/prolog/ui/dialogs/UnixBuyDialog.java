package com.amzi.prolog.ui.dialogs;

import java.io.File;
import java.io.FileWriter;

import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.core.utils.BrowserControl;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

public class UnixBuyDialog extends Dialog {

	private static String BUY_STUDENT_LABEL = "Buy Student Edition";
	private static String BUY_OTHER_LABEL = "Buy Another Edition";
	private static String ACTIVATE_KEY_LABEL = "Activate License Key";
	private static String CLOSE_LABEL = "Close";
	
	private Text username, key, fingerprint;

	public UnixBuyDialog(Shell parentShell) {
		super(parentShell);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText("Buy/Activate License");
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	protected Control createDialogArea(Composite parent) {
		Composite composite = (Composite)super.createDialogArea(parent);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(parent, "com.amzi.prolog.ui.license");
		
		GridLayout introLayout = new GridLayout();
		introLayout.numColumns = 1;
		introLayout.marginHeight = 10;
		introLayout.marginWidth = 10;
		composite.setLayout(introLayout);

		Label intro = new Label(composite, SWT.TOP | SWT.LEFT | SWT.WRAP);
		intro.setText("Thanks for using Amzi! Prolog + Logic Server.\n\n" +
			"Click the Close button to continue running the Free Edition\n");
		
		Group group1 = new Group(composite, SWT.CENTER);			
		group1.setText("First, Buy License");		

		GridLayout layout1 = new GridLayout();
		layout1.numColumns = 1;
		layout1.marginHeight = 10;
		layout1.marginWidth = 10;
		group1.setLayout(layout1);

/*		GridData data1 = new GridData();
		data1.verticalAlignment = GridData.FILL;
		data1.horizontalAlignment = GridData.FILL;
		group1.setLayoutData(data1); */
		
		Label student = new Label(group1, SWT.LEFT | SWT.WRAP);
		student.setText("To buy the Student Edition press the button (or visit www.amzi.com/download)" +
		"\nand copy this hardware fingerprint to the order page:");
		fingerprint = new Text(group1, SWT.LEFT);
		fingerprint.setText(PrologCorePlugin.getFingerprint());
		Label other = new Label(group1, SWT.LEFT | SWT.WRAP);
		other.setText("To buy any other Edition press the button (or visit www.amzi.com/download).");
		
		Group group2 = new Group(composite, SWT.CENTER);			
		group2.setText("Second, Activate License");		

		GridLayout layout2 = new GridLayout();
		layout2.numColumns = 1;
		layout2.marginHeight = 10;
		layout2.marginWidth = 10;
		group2.setLayout(layout2);

		GridData group2data = new GridData();
		group2data.verticalAlignment = GridData.FILL;
		group2data.horizontalAlignment = GridData.FILL;
		group2.setLayoutData(group2data);

		Label license = new Label(group2, SWT.LEFT | SWT.WRAP);
		license.setText("Check your e-mail and enter the following:");

		Label userLabel = new Label(group2, SWT.LEFT);
		userLabel.setText("User Name:");
		username = new Text(group2, SWT.LEFT);
		GridData userdata = new GridData();
		userdata.verticalAlignment = GridData.FILL;
		userdata.horizontalAlignment = GridData.FILL;
		username.setLayoutData(userdata);
		username.setFocus();

		Label keyLabel = new Label(group2, SWT.LEFT);
		keyLabel.setText("License Key:");
		key = new Text(group2, SWT.LEFT);
		GridData keydata = new GridData();
		keydata.verticalAlignment = GridData.FILL;
		keydata.horizontalAlignment = GridData.FILL;
		key.setLayoutData(keydata);

		return composite;
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.CLIENT_ID+1, BUY_STUDENT_LABEL, false);
		createButton(parent, IDialogConstants.CLIENT_ID+2, BUY_OTHER_LABEL, false);
		createButton(parent, IDialogConstants.OK_ID, ACTIVATE_KEY_LABEL, true);
		createButton(parent, IDialogConstants.CANCEL_ID, CLOSE_LABEL, false);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
	 */
	protected void cancelPressed() {
		super.cancelPressed();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#okPressed()
	 */
	protected void okPressed() {
		String homeDir = PrologCorePlugin.getHomeDir();
		
		if (homeDir == null) {
			MessageDialog.openError(null, "Error Activating License", "Cannot find your HOME environment variable.\nPlease set it and try again.");
			return;
		}

		try {
			File licenseFile = new File(homeDir + ".alicense");
			FileWriter out = new FileWriter(licenseFile);
			out.write(username.getText() + "\n");
			out.write(key.getText() + "\n");
			out.write(fingerprint.getText() + "\n");
			out.close();
		}
		catch (Exception ex) {
			MessageDialog.openError(null, "Error Activating License", "Cannot create license file: " + ex.getMessage());
			return;
		}

		super.okPressed();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	protected void buttonPressed(int buttonId) {
		super.buttonPressed(buttonId);

		String product="";
		
		// Mac Student=1633695, Linux Student=1633685
		if (PrologCorePlugin.getProductType().endsWith("mx"))
			product = "1633695";
		if (PrologCorePlugin.getProductType().endsWith("lx"))
			product = "1633685";

		// Buy Student
		if (buttonId == IDialogConstants.CLIENT_ID+1)
			BrowserControl.displayURL("https://www.plimus.com/jsp/buynow.jsp?contractId="+
				product+"&custom1="+fingerprint.getText());

		// Buy other
		if (buttonId == IDialogConstants.CLIENT_ID+2)
			BrowserControl.displayURL("http://www.amzi.com/download");
	}

}

package com.amzi.prolog.ui.wizards;

import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
//import org.eclipse.swt.widgets.Text;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class NewProjectCreationPage extends WizardNewProjectCreationPage {

	/**
	 * Constructor for NewProjectCreationPage.
	 * @param pageName
	 */
	public NewProjectCreationPage(String pageName) {
		super(pageName);
		this.setTitle("Prolog Project");
		this.setDescription("Create a new Prolog Project");
	}

	/**
	 * Method declared on IDialogPage.
	 * @param parent a Composite parent
	 */
	public void createControl(Composite parent) {
		super.createControl(parent);
		
		Composite composite = (Composite)getControl();
		
		// Create a group composite
		Group group = new Group(composite,SWT.CENTER);			

		GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		layout.marginHeight = 10;
		layout.marginWidth = 10;
		group.setLayout(layout);

		GridData data = new GridData();
		data.verticalAlignment = GridData.FILL;
		data.horizontalAlignment = GridData.FILL;
		group.setLayoutData(data);
		group.setText("Notes:");
		
		// Create a label
		Label label1 = new Label(group, SWT.LEFT);
		label1.setText("[1] This creates a new project in the 'workspace' directory or it creates a new project in\nthe directory path you specify.");		
		Label label2 = new Label(group, SWT.LEFT);
		label2.setText("[2] Right-click on the new project to set 'Properties' such as libraries, lsxs, xpl name, etc.");

	}
	
/*
	public void createControl(Composite parent) {
		super.createControl(parent);
		Composite composite = (Composite)getControl();

		// Create a group composite
		Group group = new Group(composite,SWT.CENTER);			

		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		layout.marginHeight = 10;
		layout.marginWidth = 10;
		group.setLayout(layout);

		GridData data = new GridData();
		data.verticalAlignment = GridData.FILL;
		data.horizontalAlignment = GridData.FILL;
		group.setLayoutData(data);
		group.setText("Executable File:");
		
		// Create a label
		Label label = new Label(group, SWT.LEFT);
		label.setText("XPL File or Path Name:");		
		data = new GridData();
		data.horizontalAlignment = GridData.FILL;
		data.verticalAlignment = GridData.CENTER;
		label.setLayoutData(data);

		// And a text field
		xplPathname = new Text(group, SWT.SINGLE | SWT.BORDER);
		data = new GridData();
		data.horizontalAlignment = GridData.FILL;
		data.verticalAlignment = GridData.CENTER;
		data.grabExcessHorizontalSpace = true;
		data.widthHint = 200;
		xplPathname.setLayoutData(data);
	}

	public String getXPLPathname(){
		return xplPathname.getText();
	}

	public void setXPLPathname(String str){
		if(str!=null){
			xplPathname.setText(str);
		}
	}
*/
}

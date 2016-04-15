package com.amzi.prolog.ui.build;

import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.core.utils.BrowserControl;

import java.io.File;
import java.util.Properties;
import java.util.Vector;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPropertyPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PropertyPage;

import com.amzi.prolog.core.utils.ExtFilenameFilter;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */

public class ProjectPropertyPage extends PropertyPage
	implements IWorkbenchPropertyPage {

	private IProject project;
	private Properties buildProps;
	private Text xplPathname, binPathname, proExclude;
	private Vector libButtons, lsxButtons;
	private String libs, lsxs;
	private List typeList;
	
	/**
	 * Constructor for ProjectPropertyPage.
	 */
	public ProjectPropertyPage() {
		super();
	}

	/**
	 * @see org.eclipse.jface.preference.PreferencePage#createContents(org.eclipse.swt.widgets.Composite)
	 */
	protected Control createContents(Composite composite) {
		PlatformUI.getWorkbench().getHelpSystem().setHelp(composite, "com.amzi.prolog.ui.project_properties");
		
		project = getProject();
     	String amziDir = PrologCorePlugin.getAmziDir();
		
		// Get the current settings
		buildProps = ProjectProperties.getProperties(project);
		
		// XPL File
		Group xplGroup = new Group(composite, SWT.CENTER | SWT.SHADOW_ETCHED_OUT);
		xplGroup.setText("Build Output");
		GridLayout xplLayout = new GridLayout(2, false);
		xplLayout.marginHeight = 10;
		xplLayout.marginWidth = 10;
		xplGroup.setLayout(xplLayout);
		GridData xplData = new GridData();
//		xplData.verticalAlignment = GridData.FILL;
		xplData.horizontalAlignment = GridData.FILL;
		xplData.grabExcessHorizontalSpace = true;
		xplGroup.setLayoutData(xplData);

		Label binLabel = new Label(xplGroup, SWT.LEFT);
		binLabel.setText("Default Output Folder:");
		GridData binLabelData = new GridData();
		binLabelData.horizontalAlignment = GridData.FILL;
		binLabelData.grabExcessHorizontalSpace = true;
		binLabelData.horizontalSpan = 2;
		binLabel.setLayoutData(binLabelData);
		binPathname = new Text(xplGroup, SWT.LEFT);
		binPathname.setText(buildProps.getProperty("binFolder"));
		GridData binPathData = new GridData();
		binPathData.widthHint = 300;
		binPathData.horizontalAlignment = GridData.FILL;
		binPathData.grabExcessHorizontalSpace = true;
		binPathData.horizontalSpan = 2;		// was 1 with button
		binPathname.setLayoutData(binPathData);
/*		Button binButton = new Button(xplGroup, SWT.PUSH | SWT.CENTER);
		binButton.setText("Browse...");
		GridData binButtonData = new GridData();
		binButtonData.grabExcessHorizontalSpace = false;
		binButtonData.horizontalSpan = 1;
		binButton.setLayoutData(binButtonData);
		binButton.addSelectionListener(new SelectionListener(){
			public void widgetDefaultSelected(SelectionEvent event) {
				ResourceSelectionDialog dialog =
						new ResourceSelectionDialog(getShell(), rootResource, msg);
					dialog.setInitialSelections(selectedResources);
					dialog.open();
					return dialog.getResult();
			}
			public void widgetSelected(SelectionEvent event) {

			}
		});
*/
		Label typeLabel = new Label(xplGroup, SWT.LEFT);
		typeLabel.setText("Executable (xpl) File Type:");
		GridData typeLabelData = new GridData();
		typeLabelData.horizontalAlignment = GridData.HORIZONTAL_ALIGN_BEGINNING;
		typeLabelData.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
		typeLabelData.horizontalSpan = 2;
		typeLabelData.grabExcessHorizontalSpace = false;
		typeLabel.setLayoutData(typeLabelData);
		typeList = new List(xplGroup, SWT.SINGLE);
		typeList.add("Debug");
		typeList.add("Release");
		typeList.select(typeList.indexOf(buildProps.getProperty("buildType")));
		GridData typeListData = new GridData();
		typeListData.horizontalAlignment = GridData.HORIZONTAL_ALIGN_BEGINNING;
		typeListData.verticalAlignment = GridData.VERTICAL_ALIGN_BEGINNING;
		typeListData.horizontalSpan = 2;
		typeListData.grabExcessHorizontalSpace = false;
		typeList.setLayoutData(typeListData);
		
		Label xplLabel = new Label(xplGroup, SWT.LEFT);
		xplLabel.setText("Executable (xpl) File or Path Name:");		
		xplPathname = new Text(xplGroup, SWT.LEFT);
		GridData xplPathData = new GridData();
		xplPathData.horizontalAlignment = GridData.FILL;
		xplPathData.verticalAlignment = GridData.CENTER;
		xplPathData.grabExcessHorizontalSpace = true;
		xplPathData.horizontalSpan = 2;
		xplPathData.widthHint = 200;
		xplPathname.setLayoutData(xplPathData);
		xplPathname.setText(buildProps.getProperty("xplPathname"));

		Label proLabel = new Label(xplGroup, SWT.LEFT);
		proLabel.setText("Comma-Separated List of Source Files (pro)\nto Exclude from Build (xpl), Cross Reference and Listener Consult:");
		proExclude = new Text(xplGroup, SWT.LEFT);
		GridData proListData = new GridData();
		proListData.horizontalAlignment = GridData.FILL;
		proListData.verticalAlignment = GridData.CENTER;
		proListData.grabExcessHorizontalSpace = true;
		proListData.horizontalSpan = 2;
		proListData.widthHint = 200;
		proExclude.setLayoutData(proListData);
		proExclude.setText(buildProps.getProperty("proExcludeNames"));

		// Libraries
		Group libGroup = new Group(composite, SWT.SHADOW_ETCHED_OUT);
		libGroup.setText("Prolog Libraries");
		GridLayout libLayout = new GridLayout(2, true);
		libLayout.marginHeight = 10;
		libLayout.marginWidth = 10;
		libGroup.setLayout(libLayout);
		GridData libData = new GridData();
		libData.verticalAlignment = GridData.FILL;
		libData.horizontalAlignment = GridData.FILL;
		libData.grabExcessHorizontalSpace = true;
		libGroup.setLayoutData(libData);

		Label libLabel = new Label(libGroup, SWT.LEFT);
		libLabel.setText("Select the (plm) libraries to be linked into your executable (xpl) file:");
		GridData libLabelData = new GridData();
		libLabelData.horizontalSpan = 2;
		libLabel.setLayoutData(libLabelData);
		
		libButtons = new Vector(20);
		libs = buildProps.getProperty("plmLibraryNames");
		if (libs == null) libs = "";
		
		File libDir = new File(amziDir + "abin");
        ExtFilenameFilter libFilter = new ExtFilenameFilter("plm");
        File libFiles[] = libDir.listFiles(libFilter);
        if (libFiles != null) {
           	for (int i = 0 ; i < libFiles.length ; i++) {
           		if (!libFiles[i].getName().equalsIgnoreCase("alib.plm") && 
           			!libFiles[i].getName().equalsIgnoreCase("debug64.plm")) {
					Button button = new Button(libGroup, SWT.CHECK | SWT.LEFT);
					button.setText(libFiles[i].getName());
					
					if (libs.indexOf(libFiles[i].getName()) >= 0) {
						button.setSelection(true);
					}
					libButtons.add(button);
           		}
           	}
        }
		
		// LSXs
		Group lsxGroup = new Group(composite, SWT.SHADOW_ETCHED_OUT);
		lsxGroup.setText("Runtime Logic Server Extensions");
		GridLayout lsxLayout = new GridLayout(2, true);
		lsxLayout.marginHeight = 10;
		lsxLayout.marginWidth = 10;
		lsxGroup.setLayout(lsxLayout);
		GridData lsxData = new GridData();
		lsxData.verticalAlignment = GridData.FILL;
		lsxData.horizontalAlignment = GridData.FILL;
		lsxData.grabExcessHorizontalSpace = true;
		lsxGroup.setLayoutData(lsxData);

		Label lsxLabel = new Label(lsxGroup, SWT.LEFT);
		lsxLabel.setText("Select the Logic Server Extensions (lsx) to load at runtime:");
		GridData lsxLabelData = new GridData();
		lsxLabelData.horizontalSpan = 2;
		lsxLabel.setLayoutData(lsxLabelData);
		
		lsxButtons = new Vector(20);
		lsxs = buildProps.getProperty("lsxExtensionNames");
		if (lsxs == null) lsxs = "";
		
		File lsxDir = null;
		if (BrowserControl.isWindowsPlatform())
			lsxDir = new File(amziDir + "bin");
		else
			lsxDir = new File(amziDir + "lib");
        ExtFilenameFilter lsxFilter = new ExtFilenameFilter("lsx");
        File lsxFiles[] = lsxDir.listFiles(lsxFilter);
        if (lsxFiles != null) {
           	for (int i = 0 ; i < lsxFiles.length ; i++) {
				if (!lsxFiles[i].getName().equalsIgnoreCase("adebug.lsx")) {
					Button button = new Button(lsxGroup, SWT.CHECK | SWT.LEFT);
					if (PrologCorePlugin.isEvaluationExpired() &&
						!lsxFiles[i].getName().equalsIgnoreCase("aosutils.lsx"))
						button.setEnabled(false);
					button.setText(lsxFiles[i].getName());
					button.setAlignment(SWT.RIGHT);
				
					if (lsxs.indexOf(lsxFiles[i].getName()) >= 0) {
						button.setSelection(true);
					}
					lsxButtons.add(button);
				}
           	}
        }

		return null;
	}

	public boolean performOk () {
		// type
		buildProps.setProperty("buildType", typeList.getItem(typeList.getSelectionIndex()));
		
		// xpl name
		buildProps.setProperty("xplPathname", xplPathname.getText());
		
		// bin folder
		buildProps.setProperty("binFolder", binPathname.getText());
		
		// pro exclude list
		buildProps.setProperty("proExcludeNames", proExclude.getText());
		
		// plm libraries
		String libs = "";
		for (int i = 0 ; i < libButtons.size() ; i++) {
			if (((Button)libButtons.elementAt(i)).getSelection()) {
				if (libs.equals(""))
					libs = libs + ((Button)libButtons.elementAt(i)).getText();
				else
					libs = libs + ","+((Button)libButtons.elementAt(i)).getText();
			}
		}
		buildProps.setProperty("plmLibraryNames", libs);
		
		// lsx extensions
		String lsxs = "";
		for (int i = 0 ; i < lsxButtons.size() ; i++) {
			if (((Button)lsxButtons.elementAt(i)).getSelection()) {
				if (lsxs.equals(""))
					lsxs = lsxs + ((Button)lsxButtons.elementAt(i)).getText();
				else
					lsxs = lsxs + ","+((Button)lsxButtons.elementAt(i)).getText();
			}
		}
		buildProps.setProperty("lsxExtensionNames", lsxs);		
		
		ProjectProperties.saveProperties(project, buildProps);
						
		// Clean out the project so it rebuilds
		project = getProject();
		try {
			project.build(IncrementalProjectBuilder.CLEAN_BUILD, new NullProgressMonitor());
		} 
		catch (CoreException e) {
		}
		
		return true;
	}
	
	public boolean performCancel() {
		return true;
	}
	
	public IProject getProject() {

		try {
			IAdaptable adaptable= getElement();
			IResource resource= (IResource) adaptable.getAdapter(IResource.class);
			IProject proj= resource.getProject();
			return proj;
			} 
		catch (NullPointerException nex) {
			return null;
		} 
		catch (Exception ex){
			return null;
		}

	}

}

package com.amzi.prolog.ui.launch;

import java.io.File;
import java.util.Properties;

import com.amzi.prolog.ui.build.ProjectProperties;
import com.amzi.prolog.ui.PrologUIPlugin;
import com.amzi.prolog.ui.PrologPluginImages;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */

public class RunTab extends AbstractPrologTab {

	private String projectName, xplPathname, projectPathname, cfgPathname, lsxList;
	private Text projectNameText, xplPathnameText, projectPathnameText, cfgPathnameText, lsxListText;
	
	/**
	 * Constructor for PrologRunTab.
	 */
	public RunTab() {
		super();
	}

	/**
	 * @see ILaunchConfigurationTab#createControl(Composite)
	 */
	public void createControl(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout gridLayout =	new GridLayout();
		gridLayout.numColumns = 1;
		composite.setLayout(gridLayout);
		composite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		new Label(composite, SWT.NONE).setText("Project Name:");
		projectNameText = new Text(composite, SWT.LEFT);
		GridData projectGridData = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
		projectGridData.widthHint = 100;
		projectNameText.setLayoutData(projectGridData);
		if (projectName != null)
			projectNameText.setText(projectName);
		projectNameText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				updateLaunchConfigurationDialog();
			}
		});

		new Label(composite, SWT.NONE).setText("Compiled (.xpl) Relative or Full Pathname to Run:");
		xplPathnameText = new Text(composite, SWT.LEFT);
		GridData xplGridData = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
		xplGridData.widthHint = 100;
		xplPathnameText.setLayoutData(xplGridData);
		if (xplPathname != null)
			xplPathnameText.setText(xplPathname);
		xplPathnameText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				updateLaunchConfigurationDialog();
			}
		});

		new Label(composite, SWT.NONE).setText("Directory to Run From:");
		projectPathnameText = new Text(composite, SWT.LEFT | SWT.H_SCROLL);
		GridData pathnameGridData = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
		pathnameGridData.widthHint = 100;
		projectPathnameText.setLayoutData(pathnameGridData);
		if (projectPathname != null)
			projectPathnameText.setText(projectPathname);
		projectPathnameText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				updateLaunchConfigurationDialog();
			}
		});

		new Label(composite, SWT.NONE).setText("Optional Full Pathname for Config (.cfg) File (defaults to amzi.cfg in the directory above):");
		cfgPathnameText = new Text(composite, SWT.LEFT | SWT.H_SCROLL);
		GridData cfgGridData = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
		cfgGridData.widthHint = 100;
		cfgPathnameText.setLayoutData(pathnameGridData);
		if (cfgPathname != null)
			cfgPathnameText.setText(projectPathname);
		cfgPathnameText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				updateLaunchConfigurationDialog();
			}
		});

		new Label(composite, SWT.NONE).setText("List of Extensions (.lsx) Files to Load:");
		lsxListText = new Text(composite, SWT.LEFT | SWT.H_SCROLL);
		GridData lsxGridData = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
		lsxGridData.widthHint = 100;
		lsxListText.setLayoutData(lsxGridData);
		if (lsxList != null)
			lsxListText.setText(lsxList);
		lsxListText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				updateLaunchConfigurationDialog();
			}
		});

		// Actually display the fields
		setControl(composite);
	}

	/**
	 * @see ILaunchConfigurationTab#initializeFrom(ILaunchConfiguration)
	 */
	public void initializeFrom(ILaunchConfiguration config) {
		try {
			xplPathname = config.getAttribute("xplPathname", "");
			projectPathname = config.getAttribute("projectPathname", "");
			cfgPathname = config.getAttribute("cfgPathname", "");
			lsxList = config.getAttribute("lsxList", "");
			projectName = config.getAttribute("projectName", "");
		}
		catch (CoreException ex) {
			PrologUIPlugin.log(ex);
		}
		xplPathnameText.setText(xplPathname);
		projectPathnameText.setText(projectPathname);
		cfgPathnameText.setText(cfgPathname);
		lsxListText.setText(lsxList);
		projectNameText.setText(projectName);
	}
	
	/**
	 * @see ILaunchConfigurationTab#performApply(ILaunchConfigurationWorkingCopy)
	 */
	public void performApply(ILaunchConfigurationWorkingCopy config) {
		config.setAttribute("xplPathname", xplPathnameText.getText());
		config.setAttribute("projectPathname", projectPathnameText.getText());
		config.setAttribute("cfgPathname", cfgPathnameText.getText());
		config.setAttribute("lsxList", lsxListText.getText());
		config.setAttribute("projectName", projectNameText.getText());
	}

	/**
	 * @see ILaunchConfigurationTab#setDefaults(ILaunchConfigurationWorkingCopy)
	 */
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		IProject project = getSelectedProject();
		if (project == null) return;

		config.setAttribute("projectPathname", project.getLocation().toOSString());
		config.setAttribute("cfgPathname", "");
		config.setAttribute("projectName", project.getName());

		Properties buildProps = ProjectProperties.getProperties(project);

		// Get full or build relative path to xpl file
		String xplPathname = buildProps.getProperty("xplPathname");
		File xplFile = new File(xplPathname);
		if (!xplFile.isAbsolute() && !xplPathname.startsWith(System.getProperty("file.separator"))) {
			String binFoldername = buildProps.getProperty("binFolder");
			if (binFoldername != null && binFoldername.length() > 0)
				xplPathname = buildProps.getProperty("binFolder") +
					System.getProperty("file.separator") + xplPathname;
		}
		config.setAttribute("xplPathname", xplPathname);
		
/*		String lsxs[] = buildProps.getProperty("lsxExtensionNames").split(",");
		String lsxList = "";
		for (int i = 0 ; i < lsxs.length ; i++)
			if (lsxs[i].length() > 0)
				lsxList += "'" + lsxs[i] + "',";
		if (lsxList.length() > 0)
			lsxList = lsxList.substring(0, lsxList.length()-1);
		config.setAttribute("lsxList", lsxList);
*/
		config.setAttribute("lsxList", buildProps.getProperty("lsxExtensionNames"));

		String configName = getLaunchManager().generateUniqueLaunchConfigurationNameFrom(project.getName());
		config.rename(configName);
	}
	
	/**
	 * @see ILaunchConfigurationTab#getName()
	 */
	public String getName() {
		return("Main");
	}

	/**
	 * @see ILaunchConfigurationTab#getImage()
	 */
	public Image getImage() {
		return PrologPluginImages.get(PrologPluginImages.IMG_MONITOR);
	}

	public boolean canSave() {
		return true;
	}

	public boolean isValid(ILaunchConfiguration launchConfig) {
		return true;
	}

}

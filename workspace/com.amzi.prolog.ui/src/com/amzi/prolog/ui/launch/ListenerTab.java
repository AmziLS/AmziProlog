package com.amzi.prolog.ui.launch;

import java.util.Properties;

import com.amzi.prolog.ui.build.ProjectProperties;
import com.amzi.prolog.ui.PrologUIPlugin;
import com.amzi.prolog.ui.PrologPluginImages;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */

public class ListenerTab extends AbstractPrologTab {

	private String projectName, projectPathname, cfgPathname, consultList, loadList, lsxList;
	private Text projectNameText, projectPathnameText, cfgPathnameText, consultListText, loadListText, lsxListText;

	/**
	 * Constructor for PrologRunTab.
	 */
	public ListenerTab() {
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

		new Label(composite, SWT.NONE).setText("Directory to Run From (full pathname):");
		projectPathnameText = new Text(composite, SWT.LEFT);
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
		cfgPathnameText = new Text(composite, SWT.LEFT);
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

		new Label(composite, SWT.NONE).setText("List of Source (.pro) Files to Consult (in single quotes, comma delimited):");
		consultListText = new Text(composite, SWT.LEFT | SWT.H_SCROLL);
		GridData consultGridData = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
		consultGridData.widthHint = 100;
		consultListText.setLayoutData(consultGridData);
		if (consultList != null)
			consultListText.setText(consultList);
		consultListText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				updateLaunchConfigurationDialog();
			}
		});

		new Label(composite, SWT.NONE).setText("List of Libraries (.plm) Files to Load (in single quotes, comma delimited):");
		loadListText = new Text(composite, SWT.LEFT | SWT.H_SCROLL);
		GridData loadGridData = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
		loadGridData.widthHint = 100;
		loadListText.setLayoutData(loadGridData);
		if (loadList != null)
			loadListText.setText(consultList);
		loadListText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				updateLaunchConfigurationDialog();
			}
		});

		new Label(composite, SWT.NONE).setText("List of Extensions (.lsx) Files to Load (in single quotes, comma delimited):");
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
			projectPathname = config.getAttribute("projectPathname", "");
			cfgPathname = config.getAttribute("cfgPathname", "");
			consultList = config.getAttribute("consultList", "");
			loadList = config.getAttribute("loadList", "");
			lsxList = config.getAttribute("lsxList", "");
			projectName = config.getAttribute("projectName", "");
		}
		catch (CoreException ex) {
			PrologUIPlugin.log(ex);
		}
		projectPathnameText.setText(projectPathname);
		cfgPathnameText.setText(cfgPathname);
		consultListText.setText(consultList);
		loadListText.setText(loadList);
		lsxListText.setText(lsxList);
		projectNameText.setText(projectName);
	}

	/**
	 * @see ILaunchConfigurationTab#performApply(ILaunchConfigurationWorkingCopy)
	 */
	public void performApply(ILaunchConfigurationWorkingCopy config) {
		config.setAttribute("projectPathname", projectPathnameText.getText());
		config.setAttribute("cfgPathname", cfgPathnameText.getText());
		config.setAttribute("consultList", consultListText.getText());
		config.setAttribute("loadList", loadListText.getText());
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
		try {
			String consultList = "";
			IResource members[] = project.members();
			String proExcludeNames = buildProps.getProperty("proExcludeNames", "");
			for (int i = 0 ; i < members.length ; i++) {
				if (members[i].getType() == IResource.FILE &&
					members[i].getFileExtension() != null &&
					members[i].getFileExtension().toLowerCase().equals("pro") &&
					members[i].exists() &&
					proExcludeNames.indexOf(members[i].getName()) == -1 ) {
					if (members[i].isLinked())
						consultList += "'" + members[i].getLocation().toOSString()+"',";
					else
						consultList += "'" + members[i].getProjectRelativePath().toOSString()+"',";
				}
			}
			consultList = consultList.substring(0, consultList.length()-1);
			config.setAttribute("consultList", consultList);
		} 
		catch (CoreException ex) {
			PrologUIPlugin.log(ex);
		}
		
		String libs[] = buildProps.getProperty("plmLibraryNames").split(",");
		String loadList = "";
		for (int i = 0 ; i < libs.length ; i++)
			if (libs[i].length() > 0)
				loadList += "'" + libs[i] + "',";
		if (loadList.length() > 0)
			loadList = loadList.substring(0, loadList.length()-1);
		config.setAttribute("loadList", loadList);

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

	public Image getImage() {
		return PrologPluginImages.get(PrologPluginImages.IMG_QUESTIONDASH);
	}

	public boolean canSave() {
		return true;
	}

	public boolean isValid(ILaunchConfiguration launchConfig) {
		return true;
	}
}

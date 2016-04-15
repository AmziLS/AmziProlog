package com.amzi.prolog.ui.launch;

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

public class RemoteTab extends AbstractPrologTab {
	private String projectName;
	private int debugPort;
	private Text projectNameText, debugPortText;

	public RemoteTab() {
		super();
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#createControl(org.eclipse.swt.widgets.Composite)
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

/*		new Label(composite, SWT.NONE).setText("Remote Host Name:");
		debugHostText = new Text(composite, SWT.LEFT);
		GridData xplGridData = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
		xplGridData.widthHint = 100;
		debugHostText.setLayoutData(xplGridData);
		if (debugHost != null)
			debugHostText.setText(debugHost);
		debugHostText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				updateLaunchConfigurationDialog();
			}
		});
*/
		new Label(composite, SWT.NONE).setText("Debug Port Number:");
		debugPortText = new Text(composite, SWT.LEFT);
		GridData pathnameGridData = new GridData(GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL);
		pathnameGridData.widthHint = 100;
		debugPortText.setLayoutData(pathnameGridData);
		if (debugPort != 0)
			debugPortText.setText(new Integer(debugPort).toString());
		debugPortText.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent evt) {
				updateLaunchConfigurationDialog();
			}
		});

		// Actually display the fields
		setControl(composite);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#setDefaults(org.eclipse.debug.core.ILaunchConfigurationWorkingCopy)
	 */
	public void setDefaults(ILaunchConfigurationWorkingCopy config) {
		IProject project = getSelectedProject();
		if (project == null) return;

		config.setAttribute("projectPathname", project.getLocation().toOSString());
		config.setAttribute("projectName", project.getName());
		config.setAttribute("debugPort",8000);

		String configName = getLaunchManager().generateUniqueLaunchConfigurationNameFrom(project.getName());
		config.rename(configName);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#initializeFrom(org.eclipse.debug.core.ILaunchConfiguration)
	 */
	public void initializeFrom(ILaunchConfiguration config) {
		try {
			projectName = config.getAttribute("projectName", "");
			debugPort = config.getAttribute("debugPort", 8000);
		}
		catch (CoreException ex) {
			PrologUIPlugin.log(ex);
		}
		projectNameText.setText(projectName);
//		debugHostText.setText(debugHost);
		debugPortText.setText(new Integer(debugPort).toString());
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#performApply(org.eclipse.debug.core.ILaunchConfigurationWorkingCopy)
	 */
	public void performApply(ILaunchConfigurationWorkingCopy config) {
		config.setAttribute("projectName", projectNameText.getText());
		config.setAttribute("debugPort", Integer.parseInt(debugPortText.getText()));
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTab#getName()
	 */
	public String getName() {
		return "Main";
	}

	/**
	 * @see ILaunchConfigurationTab#getImage()
	 */
	public Image getImage() {
		return PrologPluginImages.get(PrologPluginImages.IMG_REMOTE);
	}

	public boolean canSave() {
		return true;
	}

	public boolean isValid(ILaunchConfiguration launchConfig) {
		return true;
	}

}

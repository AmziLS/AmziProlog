package com.amzi.prolog.ui.launch;

import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.CommonTab;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
 
public class RemoteConfigurationTabGroup
	extends AbstractLaunchConfigurationTabGroup {

	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.ILaunchConfigurationTabGroup#createTabs(org.eclipse.debug.ui.ILaunchConfigurationDialog, java.lang.String)
	 */
	public void createTabs(ILaunchConfigurationDialog arg0, String arg1) {
		ILaunchConfigurationTab[] tabs = new ILaunchConfigurationTab[] {
			new RemoteTab(),
			new CommonTab()
		};
		setTabs(tabs);
	}

}

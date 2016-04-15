package com.amzi.prolog.ui.launch;

import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup;
import org.eclipse.debug.ui.CommonTab;
import org.eclipse.debug.ui.ILaunchConfigurationDialog;
import org.eclipse.debug.ui.ILaunchConfigurationTab;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */

public class ListenerConfigurationTabGroup extends AbstractLaunchConfigurationTabGroup {

	/**
	 * Insert the method's description here.
	 * @see AbstractLaunchConfigurationTabGroup#createTabs
	 */
	public void createTabs(ILaunchConfigurationDialog dialog, String mode)  {
		ILaunchConfigurationTab[] tabs = new ILaunchConfigurationTab[] {
			new ListenerTab(),
			new CommonTab()
		};
		setTabs(tabs);
	}

}

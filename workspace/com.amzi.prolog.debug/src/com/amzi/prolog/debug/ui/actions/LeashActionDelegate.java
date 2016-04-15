package com.amzi.prolog.debug.ui.actions;

import com.amzi.prolog.debug.PrologDebugPlugin;
import com.amzi.prolog.debug.core.model.PrologDebugTarget;

import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.model.IDebugTarget;
//import org.eclipse.debug.ui.IDebugView;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
//import org.eclipse.ui.actions.ActionDelegate;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class LeashActionDelegate implements IViewActionDelegate {
//	private static int initCount = 0;
	
	/**
	 * 
	 */
	public LeashActionDelegate() {
		super();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
	 */
	public void init(IViewPart view) {
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate2#init(org.eclipse.jface.action.IAction)
	 */
	public void init(IAction action) {
//		super.init(action);
		// This code executes when IActionDelegate2 is implemented,
		// but it is overriden later
		action.setChecked(true);
		
/*		// The id is not set, so we have to initialize by position
		initCount++;
		
		// They are initialized last to first same order as in plugin.xml
		switch (initCount) {
			case 1:
				action.setChecked(PrologDebugPlugin.getDefault().getLeashInfo());
				break;
			case 2:
				action.setChecked(PrologDebugPlugin.getDefault().getLeashExit());
				break;
			case 3:
				action.setChecked(PrologDebugPlugin.getDefault().getLeashFail());
				break;
			case 4:
				action.setChecked(PrologDebugPlugin.getDefault().getLeashRedo());
				break;
			case 5:
				action.setChecked(PrologDebugPlugin.getDefault().getLeashCall());
				break;
		}*/
	}


	/* (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action) {
		PrologDebugTarget prologDebugTarget = null;
		
		IDebugTarget targets[] = DebugPlugin.getDefault().getLaunchManager().getDebugTargets();
		if (targets != null)
			for (int i = 0 ; i < targets.length ; i++) {
				if (targets[i] instanceof PrologDebugTarget && !targets[i].isTerminated()) {
					prologDebugTarget = (PrologDebugTarget)targets[i];
				}
			}

		String mid = action.getId();
		if (mid != null && mid.endsWith("leashCall")) {
			if (prologDebugTarget != null) {
				if (action.isChecked()) prologDebugTarget.actionReady(PrologDebugTarget.SET_LEASH + "(call)");
				else prologDebugTarget.actionReady(PrologDebugTarget.CLEAR_LEASH + "(call)");
			}
			PrologDebugPlugin.getDefault().setLeashCall(action.isChecked());
		}
		if (mid != null && mid.endsWith("leashRedo")) {
			if (prologDebugTarget != null) {
				if (action.isChecked()) prologDebugTarget.actionReady(PrologDebugTarget.SET_LEASH + "(redo)");
				else prologDebugTarget.actionReady(PrologDebugTarget.CLEAR_LEASH + "(redo)");
			}
			PrologDebugPlugin.getDefault().setLeashRedo(action.isChecked());
		}
		if (mid != null && mid.endsWith("leashFail")) {
			if (prologDebugTarget != null) {
				if (action.isChecked()) prologDebugTarget.actionReady(PrologDebugTarget.SET_LEASH + "(fail)");
				else prologDebugTarget.actionReady(PrologDebugTarget.CLEAR_LEASH + "(fail)");
			}
			PrologDebugPlugin.getDefault().setLeashFail(action.isChecked());
		}
		if (mid != null && mid.endsWith("leashExit")) {
			if (prologDebugTarget != null) {
				if (action.isChecked()) prologDebugTarget.actionReady(PrologDebugTarget.SET_LEASH + "(exit)");
				else prologDebugTarget.actionReady(PrologDebugTarget.CLEAR_LEASH + "(exit)");
			}
			PrologDebugPlugin.getDefault().setLeashExit(action.isChecked());
		}
		if (mid != null && mid.endsWith("leashInfo")) {
			if (prologDebugTarget != null) {
				if (action.isChecked()) prologDebugTarget.actionReady(PrologDebugTarget.SET_LEASH + "(info)");
				else prologDebugTarget.actionReady(PrologDebugTarget.CLEAR_LEASH + "(info)");
			}
			PrologDebugPlugin.getDefault().setLeashInfo(action.isChecked());
		}
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
	}

}

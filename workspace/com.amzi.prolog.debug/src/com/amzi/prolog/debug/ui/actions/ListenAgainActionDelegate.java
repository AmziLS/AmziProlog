package com.amzi.prolog.debug.ui.actions;

import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.jface.action.IAction;
//import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
//import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;

import com.amzi.prolog.debug.core.model.PrologDebugTarget;

public class ListenAgainActionDelegate implements IViewActionDelegate {
	private boolean firstTime = true;
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
	 */
	public void init(IViewPart view) {
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action) {
		PrologDebugTarget target = null;
		
		IDebugTarget targets[] = DebugPlugin.getDefault().getLaunchManager().getDebugTargets();
		for (int i = 0 ; i < targets.length ; i++) {
			if (targets[i] instanceof PrologDebugTarget && !targets[i].isTerminated()) {
				target = (PrologDebugTarget)targets[i];
			}
		}
		if (target != null) {
			target.actionReady(PrologDebugTarget.OPEN_LISTENER);
//			MessageDialog.openInformation(Display.getDefault().getActiveShell(), "Prolog Debugger",
//				"Command not implemented yet");
		}
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		// Only way I know how to do this-- ugly!
		if (firstTime) {
			action.setEnabled(false);
			firstTime = false;
		}
	}

}

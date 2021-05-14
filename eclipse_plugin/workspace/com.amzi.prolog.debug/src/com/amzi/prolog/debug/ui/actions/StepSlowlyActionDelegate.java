package com.amzi.prolog.debug.ui.actions;

import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;

import com.amzi.prolog.debug.core.internal.SlowStep;
import com.amzi.prolog.debug.core.model.PrologDebugTarget;

public class StepSlowlyActionDelegate implements IViewActionDelegate {
	private SlowStep stepper = null;
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
		
		if (action.isChecked()) {
			IDebugTarget targets[] = DebugPlugin.getDefault().getLaunchManager().getDebugTargets();
			for (int i = 0 ; i < targets.length ; i++) {
				if (targets[i] instanceof PrologDebugTarget && !targets[i].isTerminated()) {
					target = (PrologDebugTarget)targets[i];
				}
			}
			if (target != null) {
				stepper = new SlowStep(target, target.getSlowStepperSpeed());
				stepper.start();
			}
		}
		else {
			if (stepper != null && stepper.isAlive() && !stepper.isInterrupted()) {
				stepper.interrupt();
			}
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

	/* (non-Javadoc)
	 * @see java.lang.Object#finalize()
	 */
	protected void finalize() throws Throwable {
		super.finalize();
		if (stepper != null && stepper.isAlive() && !stepper.isInterrupted())
			stepper.interrupt();
	}

}

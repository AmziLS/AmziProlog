package com.amzi.prolog.debug.ui.actions;

import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;

import com.amzi.prolog.debug.core.internal.SlowStep;
import com.amzi.prolog.debug.core.model.PrologDebugTarget;

public class StepSpeedActionDelegate implements IViewActionDelegate {
	private IViewPart debugView;
	private IAction verySlow = null;
	private IAction slow = null;
	private IAction medium = null;
	private IAction fast = null;
	private IAction veryFast = null;

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
	 */
	public void init(IViewPart view) {
		debugView = view;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action) {
		PrologDebugTarget target = null;
		
		if (verySlow == null || slow == null || medium == null || fast == null || veryFast == null)
			findActions();

		boolean vs = verySlow.isChecked();
		boolean s = slow.isChecked();
		boolean m = medium.isChecked();
		boolean f = fast.isChecked();
		boolean vf = veryFast.isChecked();
		if (!action.isChecked()) {
			if (!vs && !s && !m && !f && !vf) {
				action.setChecked(true);
				action.run();
			}
			return;
		} 
				
		int count = 0;
		if (vs) count++;
		if (s) count++;
		if (m) count++;
		if (f) count++;
		if (vf) count++;
		if (count > 1 && action.getId() != null) {
			if (action.getId().equals(verySlow.getId())){
				slow.setChecked(false); 
				medium.setChecked(false); 
				fast.setChecked(false); 
				veryFast.setChecked(false); 
			   }
			if (action.getId().equals(slow.getId())){
				verySlow.setChecked(false); 
				medium.setChecked(false); 
				fast.setChecked(false); 
				veryFast.setChecked(false); 
			   }
			if (action.getId().equals(medium.getId())){
				verySlow.setChecked(false); 
				slow.setChecked(false); 
				fast.setChecked(false); 
				veryFast.setChecked(false); 
			   }
			if (action.getId().equals(fast.getId())){
				verySlow.setChecked(false); 
				slow.setChecked(false); 
				medium.setChecked(false); 
				veryFast.setChecked(false); 
			   }
			if (action.getId().equals(veryFast.getId())){
				verySlow.setChecked(false); 
				slow.setChecked(false); 
				medium.setChecked(false); 
				fast.setChecked(false); 
			   }
		}
			
		IDebugTarget targets[] = DebugPlugin.getDefault().getLaunchManager().getDebugTargets();
		if (targets != null)
			for (int i = 0 ; i < targets.length ; i++) {
				if (targets[i] instanceof PrologDebugTarget && !targets[i].isTerminated()) {
					target = (PrologDebugTarget)targets[i];
				}
			}
		if (target != null && action.getId() != null) {
			if (action.getId().endsWith("verySlowStep"))
				target.setSlowStepperSpeed(SlowStep.VERY_SLOW);
			if (action.getId().endsWith("slowStep"))
				target.setSlowStepperSpeed(SlowStep.SLOW);
			if (action.getId().endsWith("mediumStep"))
				target.setSlowStepperSpeed(SlowStep.MEDIUM);
			if (action.getId().endsWith("fastStep"))
				target.setSlowStepperSpeed(SlowStep.FAST);
			if (action.getId().endsWith("veryFastStep"))
				target.setSlowStepperSpeed(SlowStep.VERY_FAST);
		}
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
//		String s = action.getId();
//		String s2 = selection.toString();
//		String s3 = "";
	}

	private void findActions() {
		IMenuManager mm= debugView.getViewSite().getActionBars().getMenuManager();
		IContributionItem[] items = mm.getItems();
		if (items != null) {
			for (int i = 0; i < items.length; i++) {
//				String id = items[i].getId();
//				String c = items[i].getClass().getName();
				if (items[i] instanceof MenuManager) {
					IContributionItem[] mitems = ((MenuManager)items[i]).getItems();
					for (int j = 0 ; j < mitems.length ; j++ ) {
						if (mitems[j] instanceof ActionContributionItem) {
							IAction action = ((ActionContributionItem)mitems[j]).getAction();
							String mid = action.getId();
							if (mid != null && mid.endsWith("verySlowStep")) 
								verySlow = action;
							if (mid != null && mid.endsWith("slowStep")) 
								slow = action;
							if (mid != null && mid.endsWith("mediumStep")) 
								medium = action;
							if (mid != null && mid.endsWith("fastStep")) 
								fast = action;
							if (mid != null && mid.endsWith("veryFastStep")) 
								veryFast = action;
						}
					}
				}
			}
		}
	}

}

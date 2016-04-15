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

import com.amzi.prolog.debug.core.model.PrologDebugTarget;

public class DCGVariablesActionDelegate implements IViewActionDelegate {
	private IViewPart debugView;
	private IAction all = null;
	private IAction none = null;
	private IAction first = null;
	
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
		
		if (all == null || none == null || first == null)
			findActions();
			
		boolean a = all.isChecked();
		boolean n = none.isChecked();
		boolean f = first.isChecked();
		if (!action.isChecked()) {
			if (!a && !n && !f) {
				action.setChecked(true);
				action.run();
			}
			return;
		} 
		
		int count = 0;
		if (a) count++;
		if (n) count++;
		if (f) count++;
		if (count > 1 && action.getId() != null) {
			if (action.getId().equals(all.getId())){
				none.setChecked(false); 
				first.setChecked(false); 
			   }
			if (action.getId().equals(none.getId())){
				all.setChecked(false); 
				first.setChecked(false); 
			   }
			if (action.getId().equals(first.getId())){
				all.setChecked(false); 
				none.setChecked(false); 
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
			if (action.getId().endsWith("allDCG"))
				target.actionReady(PrologDebugTarget.DCG_DISPLAY + "(all)");
			if (action.getId().endsWith("noneDCG"))
				target.actionReady(PrologDebugTarget.DCG_DISPLAY + "(none)");
			if (action.getId().endsWith("firstLastDCG"))
				target.actionReady(PrologDebugTarget.DCG_DISPLAY + "(first_last)");
		}
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
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
							if (mid != null && mid.endsWith("allDCG")) 
								all = action;
							if (mid != null && mid.endsWith("noneDCG")) 
								none = action;
							if (mid != null && mid.endsWith("firstLastDCG")) 
								first = action;
						}
					}
				}
			}
		}
	}

}

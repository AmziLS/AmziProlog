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

public class CutDisplayActionDelegate implements IViewActionDelegate {
	private IViewPart debugView;
	private IAction omit = null;
	private IAction show = null;
	
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

		if (omit == null || show == null)
			findActions();

		boolean o = omit.isChecked();
		boolean s = show.isChecked();
		if (!action.isChecked()) {
			if (!o && !s) {
				action.setChecked(true);
				action.run();
			}
			return;
		} 
		
		if (o && s && action.getId() != null) {
			if (action.getId().equals(omit.getId())) show.setChecked(false);
			if (action.getId().equals(show.getId())) omit.setChecked(false);
		}
			
		IDebugTarget targets[] = DebugPlugin.getDefault().getLaunchManager().getDebugTargets();
		if (targets != null)
			for (int i = 0 ; i < targets.length ; i++) {
				if (targets[i] instanceof PrologDebugTarget && !targets[i].isTerminated()) {
					target = (PrologDebugTarget)targets[i];
				}
			}
//		boolean checked = action.isChecked();
		if (target != null && action.getId() != null) {
			if (action.getId().endsWith("omitCutDetails"))
				target.actionReady(PrologDebugTarget.CUT_DISPLAY + "(off)");
			if (action.getId().endsWith("showCutDetails"))
				target.actionReady(PrologDebugTarget.CUT_DISPLAY + "(on)");
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
							if (mid != null && mid.endsWith("omitCutDetails")) 
								omit = action;
							if (mid != null && mid.endsWith("showCutDetails")) 
								show = action;
						}
					}
				}
			}
		}
	}

}
package com.amzi.prolog.ui;

import org.eclipse.debug.ui.IDebugUIConstants;
//cannot find, yet it exists: import org.eclipse.search.ui.SearchUI;
import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.IViewLayout;

/*
 * NOTE!!!  This must be in the com.amzi.prolog.ui plug-in not in core or
 * Eclipse cannot find it on start-up.
 * 
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class PrologPerspective implements IPerspectiveFactory {

	/**
	 * Constructor for PrologPerspective.
	 */
	public PrologPerspective() {
		super();
	}

	/**
	 * @see org.eclipse.ui.IPerspectiveFactory#createInitialLayout(org.eclipse.ui.IPageLayout)
	 */
	public void createInitialLayout(IPageLayout layout) {
		defineActions(layout);
		defineLayout(layout);
	}
	/**
	 * Defines the initial actions for a page.  
	 */
	public void defineActions(IPageLayout layout) {
		// Add "new wizards".
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.folder");
		layout.addNewWizardShortcut("org.eclipse.ui.wizards.new.file");
	
		// Add "show views".
		layout.addShowViewShortcut(IPageLayout.ID_RES_NAV);
		layout.addShowViewShortcut(IPageLayout.ID_BOOKMARKS);
		layout.addShowViewShortcut(IPageLayout.ID_OUTLINE);
		layout.addShowViewShortcut(IPageLayout.ID_PROP_SHEET);
		layout.addShowViewShortcut(IPageLayout.ID_PROBLEM_VIEW);
		
		//This will show the debug and run as icons in the toolbar.
		layout.addActionSet(IDebugUIConstants.LAUNCH_ACTION_SET);
	}
	
	/**
	 * Defines the initial layout for a page.  
	 */
	public void defineLayout(IPageLayout layout) {
		// Editors are placed for free.
		String editorArea = layout.getEditorArea();
	
		// Left
		IFolderLayout navigator = layout.createFolder("navigation", IPageLayout.LEFT, (float)0.15, editorArea);
		navigator.addView(IPageLayout.ID_RES_NAV);
	
		// Bottom
		IFolderLayout status = layout.createFolder("status", IPageLayout.BOTTOM, (float)0.65, editorArea);
		status.addView("com.amzi.prolog.ui.launch.ListenerView");
		IViewLayout listenerLayout = layout.getViewLayout("com.amzi.prolog.ui.launch.ListenerView");
		listenerLayout.setCloseable(false);
		status.addView(IPageLayout.ID_PROBLEM_VIEW);
//		status.addPlaceholder(SearchUI.SEARCH_RESULT_VIEW_ID);
		status.addPlaceholder("com.amzi.prolog.debug.ui.DebugListenerView");
		IViewLayout debugListenerLayout = layout.getViewLayout("com.amzi.prolog.debug.ui.DebugListenerView");
		debugListenerLayout.setCloseable(false);

		// Right
		IFolderLayout outline = layout.createFolder("outline", IPageLayout.RIGHT, (float)0.75, editorArea);
		outline.addView(IPageLayout.ID_OUTLINE);
		outline.addView("com.amzi.prolog.ui.views.XrefView");
//		outline.addPlaceholder(IPageLayout.ID_BOOKMARKS);
	}
}

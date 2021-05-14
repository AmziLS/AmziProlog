package com.amzi.prolog.ui.editor.actions;

import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;

import com.amzi.prolog.ui.PrologPluginImages;
import com.amzi.prolog.ui.PrologUIPlugin;

/*
 * Copyright (c) 2002-2004 Amzi! inc. All Rights Reserved.
 */

/**
 * A toolbar action which toggles the presentation model of the
 * connected text editor. The editor shows either the highlight range
 * only or always the whole document.
 */
public class PresentationAction extends TextEditorAction {

	/**
	 * Constructs and updates the action.
	 */
	public PresentationAction() {
		super(PrologUIPlugin.getResourceBundle(), "TogglePresentation_", null); //$NON-NLS-1$
		setImageDescriptor(PrologPluginImages.DESC_SEGMENTEDIT);
		update();
	}
	
	/* (non-Javadoc)
	 * Method declared on IAction
	 */
	public void run() {

		ITextEditor editor= getTextEditor();

		editor.resetHighlightRange();
		boolean show= editor.showsHighlightRangeOnly();
		setChecked(!show);
		editor.showHighlightRangeOnly(!show);
	}
	
	/* (non-Javadoc)
	 * Method declared on TextEditorAction
	 */
	public void update() {
		setChecked(getTextEditor() != null && getTextEditor().showsHighlightRangeOnly());
		setEnabled(true);
	}
}

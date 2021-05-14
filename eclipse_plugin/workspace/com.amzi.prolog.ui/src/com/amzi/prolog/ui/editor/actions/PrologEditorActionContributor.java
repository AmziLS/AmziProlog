package com.amzi.prolog.ui.editor.actions;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.editors.text.TextEditorActionContributor;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.RetargetTextEditorAction;

import com.amzi.prolog.ui.PrologUIPlugin;

/*
 * Copyright (c) 2002-2004 Amzi! inc. All Rights Reserved.
 */

public class PrologEditorActionContributor extends TextEditorActionContributor {

	protected RetargetTextEditorAction fContentAssistProposal;
	protected RetargetTextEditorAction fContentAssistTip;
//	protected TextEditorAction fTogglePresentation;

	/**
	 * Default constructor.
	 */
	public PrologEditorActionContributor() {
		super();
		fContentAssistProposal= new RetargetTextEditorAction(PrologUIPlugin.getResourceBundle(), "ContentAssistProposal_"); //$NON-NLS-1$
		fContentAssistProposal.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS); 
		fContentAssistTip= new RetargetTextEditorAction(PrologUIPlugin.getResourceBundle(), "ContentAssistTip_"); //$NON-NLS-1$
		fContentAssistTip.setActionDefinitionId(ITextEditorActionDefinitionIds.CONTENT_ASSIST_CONTEXT_INFORMATION);
//		fTogglePresentation= new PresentationAction();
	}
	
	/*
	 * @see IEditorActionBarContributor#init(IActionBars)
	 */
	public void init(IActionBars bars) {
		super.init(bars);
		
		IMenuManager menuManager= bars.getMenuManager();
		IMenuManager editMenu= menuManager.findMenuUsingPath(IWorkbenchActionConstants.M_EDIT);
		if (editMenu != null) {
			editMenu.add(new Separator());
			editMenu.add(fContentAssistProposal);
//			editMenu.add(fContentAssistTip);
		}	
		
		IToolBarManager toolBarManager= bars.getToolBarManager();
		if (toolBarManager != null) {
			toolBarManager.add(new Separator());
//			toolBarManager.add(fTogglePresentation);
		}
	}
	
	private void doSetActiveEditor(IEditorPart part) {
		super.setActiveEditor(part);

		ITextEditor editor= null;
		if (part instanceof ITextEditor)
			editor= (ITextEditor) part;

		fContentAssistProposal.setAction(getAction(editor, "ContentAssistProposal"));
		fContentAssistTip.setAction(getAction(editor, "ContentAssistTip"));
//		fTogglePresentation.setEditor(editor);
//		fTogglePresentation.update();
	}
	
	/*
	 * @see IEditorActionBarContributor#setActiveEditor(IEditorPart)
	 */
	public void setActiveEditor(IEditorPart part) {
		super.setActiveEditor(part);
		doSetActiveEditor(part);
	}
	
	/*
	 * @see IEditorActionBarContributor#dispose()
	 */
	public void dispose() {
		doSetActiveEditor(null);
		super.dispose();
	}

}

package com.amzi.prolog.ui.editor;

import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.ui.PrologUIPlugin;
import com.amzi.prolog.ui.editor.internal.*;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.graphics.RGB;
//import org.eclipse.jface.text.DefaultAutoIndentStrategy;
//import org.eclipse.jface.text.IAutoIndentStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.BufferedRuleBasedScanner;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;

/*
 * Copyright (c) 2002-2004 Amzi! inc. All Rights Reserved.
 */

public class PrologSourceViewerConfiguration extends SourceViewerConfiguration {
	private PrologDoubleClickStrategy doubleClickStrategy;
	private PrologCodeScanner scanner;
	private ColorManager colorManager;
	private IPreferenceStore store;
	private SingleTokenScanner commentScanner;
	
	/**
	 * Single token scanner. Used for multi-line comments.
	 */
	static class SingleTokenScanner extends BufferedRuleBasedScanner {
		public SingleTokenScanner(TextAttribute attribute) {
			setDefaultReturnToken(new Token(attribute));
		}
	};
		
	public PrologSourceViewerConfiguration(ColorManager colorManager) {
		this.colorManager = colorManager;
		store = PrologUIPlugin.getDefault().getPreferenceStore();
	}
	
	public String[] getConfiguredContentTypes(ISourceViewer sourceViewer) {
		return new String[] {
			IDocument.DEFAULT_CONTENT_TYPE,
			PrologPartitionScanner.PROLOG_COMMENT };
	}
	
	public ITextDoubleClickStrategy getDoubleClickStrategy(ISourceViewer sourceViewer, String contentType) {
		if (doubleClickStrategy == null)
			doubleClickStrategy = new PrologDoubleClickStrategy();
		return doubleClickStrategy;
	}

	protected PrologCodeScanner getPrologScanner() {
		if (scanner == null) {
			scanner = new PrologCodeScanner(colorManager);
//			scanner.setDefaultReturnToken(new Token(new TextAttribute(colorManager.getColor(PreferenceConverter.getColor(getPreferenceStore(), IPrologColorConstants.CODE)))));
			scanner.setDefaultReturnToken(scanner.getDefaultToken());
		}
		return scanner;
	}

	public IPresentationReconciler getPresentationReconciler(ISourceViewer sourceViewer) {
		PresentationReconciler reconciler = new PresentationReconciler();

		DefaultDamagerRepairer dr = new DefaultDamagerRepairer(getPrologScanner());
		reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
		reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

		commentScanner = new SingleTokenScanner(new TextAttribute(colorManager.getColor(PreferenceConverter.getColor(getPreferenceStore(), IPrologColorConstants.COMMENT))));
		dr= new DefaultDamagerRepairer(commentScanner);
		reconciler.setDamager(dr, PrologPartitionScanner.PROLOG_COMMENT);
		reconciler.setRepairer(dr, PrologPartitionScanner.PROLOG_COMMENT);

		return reconciler;
	}

//	public IAutoIndentStrategy getAutoIndentStrategy(ISourceViewer sourceViewer, String contentType) {
//		return (new DefaultAutoIndentStrategy());
//	}

	public int getTabWidth(ISourceViewer sourceViewer) {
		// Fix getIndentPrefixes if this is changed
		int tabsize = store.getInt(IPrologColorConstants.TABSIZE_KEY);
		return tabsize;
	}
	
	public String[] getIndentPrefixes(ISourceViewer sourceViewer, String contentType) {
		int tabsize = store.getInt(IPrologColorConstants.TABSIZE_KEY);
		StringBuffer sb = new StringBuffer("");
		for (int i = 0 ; i < tabsize ; i++)
			sb.append(" ");
		String spaces = new String(sb);
		
		return new String[] { "\t", spaces };

/*		Vector vector= new Vector();

		// prefix[0] is either '\t' or ' ' x tabWidth, depending on useSpaces
		int tabsize = getPreferenceStore().getInt(PrologEditorPreferencePage.TABSIZE_KEY);
		boolean useSpaces= getPreferenceStore().getBoolean(PrologEditorPreferencePage.SPACES_FOR_TABS);

		for (int i= 0; i <= tabsize; i++) {
			StringBuffer prefix= new StringBuffer();

			if (useSpaces) {
				for (int j= 0; j + i < tabsize; j++)
					prefix.append(' ');

				if (i != 0)
					prefix.append('\t');
			} 
			else {
				for (int j= 0; j < i; j++)
					prefix.append(' ');

				if (i != tabsize)
					prefix.append('\t');
			}

			vector.add(prefix.toString());
		}

		vector.add("");

		return (String[]) vector.toArray(new String[vector.size()]); */
	}

	protected IPreferenceStore getPreferenceStore() {
		return PrologUIPlugin.getDefault().getPreferenceStore();
	}

	public void updateColor(String colorKey, RGB rgb) {
		scanner.updateColor(colorKey, rgb);
		commentScanner.setDefaultReturnToken(new Token(new TextAttribute(colorManager.getColor(PreferenceConverter.getColor(getPreferenceStore(), IPrologColorConstants.COMMENT)))));
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getTextHover(org.eclipse.jface.text.source.ISourceViewer, java.lang.String)
	 */
	public ITextHover getTextHover(ISourceViewer sourceViewer, String contentType) {
					return new PrologTextHover();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getContentAssistant(org.eclipse.jface.text.source.ISourceViewer)
	 */
	public IContentAssistant getContentAssistant(ISourceViewer sourceViewer) {
		ContentAssistant assistant= new ContentAssistant();
		// 2nd param PrologPartitionScanner.PROLOG_DEFAULT does not work, wonder why?
		assistant.setContentAssistProcessor(new PrologContentAssistProcessor(), IDocument.DEFAULT_CONTENT_TYPE);
//		assistant.setContentAssistProcessor(new JavaDocCompletionProcessor(), JavaPartitionScanner.JAVA_DOC);

		assistant.enableAutoActivation(true);
		assistant.setAutoActivationDelay(500);
		assistant.setProposalPopupOrientation(IContentAssistant.PROPOSAL_OVERLAY);
		assistant.setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_ABOVE);

		return assistant;
	}

}
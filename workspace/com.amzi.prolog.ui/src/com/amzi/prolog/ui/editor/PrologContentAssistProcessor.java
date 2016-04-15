package com.amzi.prolog.ui.editor;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;

import amzi.ls.LSException;
import amzi.ls.LogicServer;

import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.core.PrologKeywordsAndActions;
import com.amzi.prolog.core.dialogs.UpgradeDialog;
import com.amzi.prolog.ui.PrologPluginImages;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.ITextViewer;
//import org.eclipse.jface.text.TextPresentation;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ContextInformation;
import org.eclipse.jface.text.contentassist.ContextInformationValidator;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
//import org.eclipse.jface.text.contentassist.IContextInformationPresenter;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;

/*
 * Copyright (c) 2002-2004 Amzi! inc. All Rights Reserved.
 */

public class PrologContentAssistProcessor implements IContentAssistProcessor {
	protected IContextInformationValidator contextValidator= new ContextInformationValidator(this);
	private PrologWordDetector word;
	private PrologWhitespaceDetector whitespace;
	private String keywords[];
	private String lastPredicate = null;
	private int lastIndex = -1, lastLine = -1, commaCount = -1;
	
	public PrologContentAssistProcessor() {
		int i;
		
		// Build all the possible completion proposals
		word = new PrologWordDetector();
		whitespace = new PrologWhitespaceDetector();
		
		String allwords[] = PrologCorePlugin.getPrologKeywords().getKeywords();
		List wordList = new ArrayList();
		for (i = 0 ; i < allwords.length ; i++) {
			if (word.isWordPart(allwords[i].charAt(0))) {
				if (PrologCorePlugin.getPredicateInfo(allwords[i]) != null)
					wordList.add(allwords[i]);
			}
			// Always add operators
			else
				wordList.add(allwords[i]);
		}
		
		String directives[] = PrologCorePlugin.getPrologKeywords().getDirectives();
		for (i = 0 ; i < directives.length ; i++)
			wordList.add(directives[i]);
			
		String constants[] = PrologCorePlugin.getPrologKeywords().getConstants();
		for (i = 0 ; i < constants.length ; i++)
			wordList.add(constants[i]);
		
		String maths[] = PrologCorePlugin.getPrologKeywords().getMaths();
		for (i = 0 ; i < maths.length ; i++)
			wordList.add(maths[i]);
		
		TreeSet sortedSet = new TreeSet(wordList);
		keywords = new String[sortedSet.size()];
		sortedSet.toArray(keywords);
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#computeCompletionProposals(org.eclipse.jface.text.ITextViewer, int)
	 */
	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer, int documentOffset) {
		String prefix = "";
		
		if (PrologCorePlugin.isEvaluationExpired()) {
			Display.getDefault().asyncExec(new Runnable(){
				public void run() {
					UpgradeDialog upgradeDialog = new UpgradeDialog(Display.getDefault().getActiveShell());
					upgradeDialog.open();
				}
			});
			return null;
		}
		
		// See if there is a word part we want to complete 
		try {
			if (!whitespace.isWhitespace(viewer.getDocument().getChar(documentOffset-1))) {
				int idx = documentOffset-1;
				while (word.isWordPart(viewer.getDocument().get(idx, 1).charAt(0)))
					idx--;
				idx++;
				int len = (documentOffset - idx);
				prefix = viewer.getDocument().get(idx, len);
			}
		} 
		catch (BadLocationException e) {
		}

		List propList = new ArrayList();
		
		for (int i= 0; i < keywords.length; i++) {
			if (prefix.length() == 0 || keywords[i].startsWith(prefix)) {
				Image image = null;
				switch (PrologCorePlugin.getPrologKeywords().getKeywordType(keywords[i])){
					case PrologKeywordsAndActions.CONSTANT:
						image = PrologPluginImages.get(PrologPluginImages.IMG_OBJS_CONSTWORD);
						break;
					case PrologKeywordsAndActions.DIRECTIVE:
						image = PrologPluginImages.get(PrologPluginImages.IMG_OBJS_DIRECTWORD);
						break;
					case PrologKeywordsAndActions.MATHFN:
						image = PrologPluginImages.get(PrologPluginImages.IMG_OBJS_MATHWORD);
						break;
					case PrologKeywordsAndActions.PREDICATE:
						image = PrologPluginImages.get(PrologPluginImages.IMG_OBJS_PREDWORD);
						break;
				}
				// Could pass a context information if desired
//				IContextInformation info= new ContextInformation(keywords[i], MessageFormat.format(JavaEditorMessages.getString("CompletionProcessor.Proposal.ContextInfo.pattern"), new Object[] { fgProposals[i] })); //$NON-NLS-1$

				propList.add(new CompletionProposal(keywords[i], 
					documentOffset-prefix.length(), prefix.length(),
					keywords[i].length(), image, keywords[i], null, null));
			}
		}

		ICompletionProposal[] result = new ICompletionProposal[propList.size()];
		propList.toArray(result);
		return result;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#computeContextInformation(org.eclipse.jface.text.ITextViewer, int)
	 */
	public IContextInformation[] computeContextInformation(ITextViewer viewer, int documentOffset) {
		String predicate;
		int idx, len, typed;
		
		try {
			typed = viewer.getDocument().get(documentOffset-1, 1).charAt(0);
			
			// If the user typed (, put up all the possible predicates
			if (typed == '(') {
				// Back up and get predicate name
				idx = documentOffset-2;
				len = 0;
			
				while (word.isWordPart(viewer.getDocument().get(idx, 1).charAt(0)))
					idx--;
				idx++;
				len = (documentOffset - idx) - 1;
				predicate = viewer.getDocument().get(idx, len);
				
				// Package up all the arities
				IContextInformation[] result = getPredicateChoices(predicate);

				// Save our location if we found something
				if (result != null) {
					lastPredicate = predicate;
					lastIndex = documentOffset;
					lastLine = viewer.getDocument().getLineOfOffset(lastIndex);
					commaCount = 0;
					return result;
				}
			}
			else if (typed == ',') {
				if (documentOffset < lastIndex || lastPredicate == null ||
					viewer.getDocument().getLineOfOffset(documentOffset) != lastLine) {
					lastPredicate = null;
					lastIndex = -1;
					lastLine = -1;
					commaCount = -1;
					return null;
					}
					
				// See if we have a term
				try {
					LogicServer ls = PrologCorePlugin.getLogicServer();
					synchronized (ls) {
						if (ls != null) {
							String newText = viewer.getDocument().get(lastIndex, (documentOffset-lastIndex)-1).trim();
							if (ls.StrToTerm(newText) == 0) 
								return null;
						}
					}
				}
				catch (LSException ex) {
					return null;
				}
				
				// Get all the arities
				IContextInformation[] choices = getPredicateChoices(lastPredicate);
				
				// They are in order, we have to figure out which one we are on
				commaCount++;
				lastIndex = documentOffset;
				int pos = -1;
				int choiceIdx = 0;
				String args = "";
				while (pos < 0 && choiceIdx < choices.length) {
					args = choices[choiceIdx].getInformationDisplayString();
					for (int i = 1 ; i <= commaCount ; i++)
						pos = args.indexOf(',', pos+1);
					choiceIdx++;
				}
				
				// Ran out of options
				if (pos < 0 || choiceIdx > choices.length) {
					lastPredicate = null;
					lastIndex = -1;
					lastLine = -1;
					commaCount = -1;
					return null;
				}
				
				// Display the next argument
				pos += 2;
				int pos2 = args.indexOf(',', pos+2);
				if (pos2 < 0) pos2 = args.length() - 1;
				String nextArg = args.substring(pos, pos2);
				
				IContextInformation[] info = new IContextInformation[1];
				info[0] = new ContextInformation(null, null, nextArg);
				return info;
			}
		} 
		catch (BadLocationException e) {
			return null;
		}
		
		return null;
	}

	private IContextInformation[] getPredicateChoices(String predicate) {
		String info;
		int idx, nidx;

		// We've got a predicate name now
		info = PrologCorePlugin.getPredicateInfo(predicate);
		if (info == null) return null;
		
		Image image = null;
		switch (PrologCorePlugin.getPrologKeywords().getKeywordType(predicate)){
			case PrologKeywordsAndActions.CONSTANT:
				image = PrologPluginImages.get(PrologPluginImages.IMG_OBJS_CONSTWORD);
				break;
			case PrologKeywordsAndActions.DIRECTIVE:
				image = PrologPluginImages.get(PrologPluginImages.IMG_OBJS_DIRECTWORD);
				break;
			case PrologKeywordsAndActions.MATHFN:
				image = PrologPluginImages.get(PrologPluginImages.IMG_OBJS_MATHWORD);
				break;
			case PrologKeywordsAndActions.PREDICATE:
				image = PrologPluginImages.get(PrologPluginImages.IMG_OBJS_PREDWORD);
				break;
		}

		List infoList = new ArrayList();
		idx = 0;
		while (idx < info.length())	{
			String args = info.substring(idx, info.indexOf('\n', idx));
			idx = info.indexOf('\n', idx) + 1;
			nidx = info.indexOf('\n', idx);
			if (nidx < 0) nidx = info.length();
			String desc = info.substring(idx, nidx);
			idx = nidx;

			infoList.add(new ContextInformation(image, desc, args));

			if (idx < info.length()) {
				idx = info.indexOf('\n', idx) + 2;
			}
		}
		
		IContextInformation[] result= new IContextInformation[infoList.size()];
		infoList.toArray(result);

		return result;		
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getCompletionProposalAutoActivationCharacters()
	 */
	public char[] getCompletionProposalAutoActivationCharacters() {
		return null;
//		return new char[] { ':', '#' };
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getContextInformationAutoActivationCharacters()
	 */
	public char[] getContextInformationAutoActivationCharacters() {
		if (PrologCorePlugin.isEvaluationExpired())
			return null;
		else
			return new char[] { '(', ',' };
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getErrorMessage()
	 */
	public String getErrorMessage() {
		return null;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.contentassist.IContentAssistProcessor#getContextInformationValidator()
	 */
	public IContextInformationValidator getContextInformationValidator() {
		return contextValidator;
	}

}

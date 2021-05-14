package com.amzi.prolog.ui.editor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.ui.PrologUIPlugin;
import com.amzi.prolog.ui.editor.internal.*;

import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.jface.text.rules.*;
import org.eclipse.jface.text.*;

/*
 * Copyright (c) 2002-2004 Amzi! inc. All Rights Reserved.
 */
 
public class PrologCodeScanner extends RuleBasedScanner {
	private Map tokenMap = new HashMap(10);
	private ColorManager colorManager;
	
	private static String[] fgKeywords = PrologCorePlugin.getPrologKeywords().getKeywords();

	private static String[] fgDirectives= PrologCorePlugin.getPrologKeywords().getDirectives();
	
	private static String[] fgConstants= PrologCorePlugin.getPrologKeywords().getConstants();

	private static String[] fgMaths= PrologCorePlugin.getPrologKeywords().getMaths();
	
	private Token string, keyword, other, comment, math;

	public PrologCodeScanner(ColorManager manager) {
		createRules(manager);	
	}
	
	public void createRules(ColorManager colorManager) {
		this.colorManager = colorManager;
		
		IPreferenceStore store = PrologUIPlugin.getDefault().getPreferenceStore();
		
		string= new Token(new TextAttribute(colorManager.getColor(PreferenceConverter.getColor(store, IPrologColorConstants.STRING_CONSTANT))));
		tokenMap.put(IPrologColorConstants.STRING_CONSTANT, string);
		keyword= new Token(new TextAttribute(colorManager.getColor(PreferenceConverter.getColor(store,IPrologColorConstants.SYSTEM_PREDICATE))));
		tokenMap.put(IPrologColorConstants.SYSTEM_PREDICATE, keyword);
		other= new Token(new TextAttribute(colorManager.getColor(PreferenceConverter.getColor(store,IPrologColorConstants.CODE))));
		tokenMap.put(IPrologColorConstants.CODE, other);
		comment= new Token(new TextAttribute(colorManager.getColor(PreferenceConverter.getColor(store,IPrologColorConstants.COMMENT))));
		tokenMap.put(IPrologColorConstants.COMMENT, comment);
		math= new Token(new TextAttribute(colorManager.getColor(PreferenceConverter.getColor(store,IPrologColorConstants.MATH_SYMBOL_FUNCTION))));
		tokenMap.put(IPrologColorConstants.MATH_SYMBOL_FUNCTION, math);

		List rules = new ArrayList();

		// Add rule for single line comments.
		rules.add(new EndOfLineRule("%", comment));
		// Add rule for multi-line comments
		rules.add(new MultiLineRule("/*", "*/", comment));

		// Add generic whitespace rule.
		rules.add(new WhitespaceRule(new PrologWhitespaceDetector()));

		// Add rule for double quotes with escape character
		rules.add(new MultiLineRule("\"", "\"", string, '\\'));
		// Add a rule for back quotes with escape character
		rules.add(new MultiLineRule("`", "`", string, '\\'));
		// Add a rule for single quotes
		rules.add(new SingleLineRule("'", "'", string, '\\'));

		// Add word rule for keywords, types, and constants.
		WordRule wordRule = new WordRule(new PrologWordDetector(), other);
		WordRule opRule = new WordRule(new PrologOperatorDetector(), other);
		for (int i= 0; i < fgConstants.length; i++)
			wordRule.addWord(fgConstants[i], keyword);
		for (int i= 0; i < fgDirectives.length; i++)
			wordRule.addWord(fgDirectives[i], keyword);
		for (int i= 0; i < fgMaths.length; i++) {
			if (Character.isLetter(fgMaths[i].charAt(0)))
				wordRule.addWord(fgMaths[i], math);
			else
				opRule.addWord(fgMaths[i], math);
		}
		// Last to avoid dups of above
		for (int i= 0; i < fgKeywords.length; i++) {
			if (Character.isLetter(fgKeywords[i].charAt(0)))
				wordRule.addWord(fgKeywords[i], keyword);
			else
				opRule.addWord(fgKeywords[i], keyword);
		}
			
		rules.add(wordRule);
		rules.add(opRule);
				
		IRule[] result= new IRule[rules.size()];
		rules.toArray(result);
		setRules(result);
	}
	
	public Token getDefaultToken() {
		return other;
	}
	
	public void updateColor(String colorKey, RGB rgb) {
		Color newColor = colorManager.getColor(rgb);
		Token token = (Token)tokenMap.get(colorKey);
	 	TextAttribute oldAttr = (TextAttribute) token.getData();
 		token.setData(new TextAttribute(newColor, oldAttr.getBackground(), oldAttr.getStyle()));
	}
		
}

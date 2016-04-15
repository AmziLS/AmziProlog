package com.amzi.prolog.ui.editor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.jface.text.rules.*;
import org.eclipse.jface.text.*;

public class PrologPartitionScanner extends RuleBasedPartitionScanner {
	private Map tokenMap = new HashMap(10);
	private ColorManager colorManager;

	private Token comment;

	// Also add to PrologDocumentProvider
//	public final static String PROLOG_DEFAULT = IDocument.DEFAULT_CONTENT_TYPE;
	public final static String PROLOG_COMMENT = "__comment";
//	public final static String PROLOG_LITERAL = "__literal";

	public PrologPartitionScanner() {

//		other = new Token(PROLOG_DEFAULT);
		comment = new Token(PROLOG_COMMENT);
//		literal = new Token(PROLOG_LITERAL);

		List rules= new ArrayList();

		// Add rule for multi-line comments
		rules.add(new MultiLineRule("/*", "*/", comment));

		// Add rule for single line comments.
		rules.add(new EndOfLineRule("%", Token.UNDEFINED));

		// Add rule for strings and character constants.
		rules.add(new SingleLineRule("\"", "\"", Token.UNDEFINED, '\\'));
		rules.add(new SingleLineRule("`", "`", Token.UNDEFINED, '\\'));

		// Add rule for quoted atoms.
		rules.add(new SingleLineRule("'", "'", Token.UNDEFINED, '\\'));
		
		IPredicateRule[] result= new IPredicateRule[rules.size()];
		rules.toArray(result);
		setPredicateRules(result);
	}
	
	public void updateColor(String colorKey, RGB rgb) {
		Color newColor = colorManager.getColor(rgb);
		Token token = (Token)tokenMap.get(colorKey);
		TextAttribute oldAttr = (TextAttribute) token.getData();
		token.setData(new TextAttribute(newColor, oldAttr.getBackground(), oldAttr.getStyle()));
	}

}

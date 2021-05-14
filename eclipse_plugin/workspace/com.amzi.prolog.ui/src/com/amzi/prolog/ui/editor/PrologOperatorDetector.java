package com.amzi.prolog.ui.editor;

import org.eclipse.jface.text.rules.IWordDetector;

public class PrologOperatorDetector implements IWordDetector {

	private String agraphics = "#&*+-./:<=>?@\\^~";

	/**
	 * Constructor for PrologOperatorDetector.
	 */
	public PrologOperatorDetector() {
		super();
	}

	/**
	 * @see org.eclipse.jface.text.rules.IWordDetector#isWordStart(char)
	 * 
	 * 00A1 - 00BF (symbols for copyright, fractions, some currency, etc.) 
	 * 2010 - 303F (symbols for punctuation, math, arrows, box drawing, currency, etc.) 
	 */
	public boolean isWordStart(char ch) {
		if (agraphics.indexOf(ch) >= 0 ||
			(ch >= '\u00A1' && ch <= '\u00BF') ||
			(ch >= '\u2010' && ch <= '\u303F') ) 
			return true;
		return false;
	}

	/**
	 * @see org.eclipse.jface.text.rules.IWordDetector#isWordPart(char)
	 */
	public boolean isWordPart(char ch) {
		if (agraphics.indexOf(ch) >= 0 ||
			(ch >= '\u00A1' && ch <= '\u00BF') ||
			(ch >= '\u2010' && ch <= '\u303F') ) 
			return true;
		return false;
	}

}

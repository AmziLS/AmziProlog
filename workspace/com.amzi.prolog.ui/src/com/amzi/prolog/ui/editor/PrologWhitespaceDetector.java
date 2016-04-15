package com.amzi.prolog.ui.editor;

import org.eclipse.jface.text.rules.IWhitespaceDetector;

/*
 * Copyright (c) 2002-2004 Amzi! inc. All Rights Reserved.
 */

public class PrologWhitespaceDetector implements IWhitespaceDetector {

	/* Prolog whitespace
	 *  (non-Javadoc)
	 * @see org.eclipse.jface.text.rules.IWhitespaceDetector#isWhitespace(char)
	 * 
	 * 2000 - 200F (special spacing symbols) 
	 * FFF0 - FFFE and FFEF (reserved symbols) 
	 */
	public boolean isWhitespace(char ch) {
		if (Character.isWhitespace(ch) ||
			ch <= '\u0020' ||
			(ch >= '\u2000' && ch <= '\u200F') ||
			(ch >= '\uFFF0' && ch <= '\uFFFE') ) 
			return true;
		return false;
	}
}

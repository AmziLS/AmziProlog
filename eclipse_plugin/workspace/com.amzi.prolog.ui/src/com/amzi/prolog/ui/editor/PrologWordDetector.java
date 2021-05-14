package com.amzi.prolog.ui.editor;

import org.eclipse.jface.text.rules.IWordDetector;

/*
 * Copyright (c) 2002-2004 Amzi! inc. All Rights Reserved.
 */

public class PrologWordDetector implements IWordDetector {

	/**
	 * Constructor for PrologWordDetector.
	 */
	public PrologWordDetector() {
		super();
	}

	/**
	 * @see org.eclipse.jface.text.rules.IWordDetector#isWordStart(char)
	 * 
	 * 00C0 - 1FFF (many phonetic language symbols) 
	 * 3040 - D7FF (Chinese, Japanese, Korean (CJK) ideographs) 
	 * E000 - FFEF (except FEFF) (Private use, additional CJK and Arabic symbols) 
	 */
	public boolean isWordStart(char ch) {
		if (ch == '\uFEFF') return false;
		if (Character.isLetterOrDigit(ch) ||
			ch == '_' ||
			(ch >= '\u00C0' && ch <= '\u1FFF') ||
			(ch >= '\u3040' && ch <= '\uD7FF') ||
			(ch >= '\uE000' && ch <= '\uFFEF') ) 
			return true;
		return false;
	}

	/**
	 * @see org.eclipse.jface.text.rules.IWordDetector#isWordPart(char)
	 */
	public boolean isWordPart(char ch) {
		if (ch == '\uFEFF') return false;
		if (Character.isLetterOrDigit(ch) ||
			ch == '_' ||
			(ch >= '\u00C0' && ch <= '\u1FFF') ||
			(ch >= '\u3040' && ch <= '\uD7FF') ||
			(ch >= '\uE000' && ch <= '\uFFEF') ) 
			return true;
		return false;
	}

}

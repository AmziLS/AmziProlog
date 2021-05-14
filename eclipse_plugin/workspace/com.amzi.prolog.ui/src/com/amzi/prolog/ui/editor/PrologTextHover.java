package com.amzi.prolog.ui.editor;

import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.ui.editor.PrologWordDetector;
import com.amzi.prolog.ui.editor.PrologOperatorDetector;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Region;
import org.eclipse.swt.graphics.Point;

/*
 * Copyright (c) 2002-2004 Amzi! inc. All Rights Reserved.
 */

public class PrologTextHover implements ITextHover {
	private PrologWordDetector word;
	private PrologOperatorDetector oper;
	
	public PrologTextHover() {
		word = new PrologWordDetector();
		oper = new PrologOperatorDetector();	
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.ITextHover#getHoverInfo(org.eclipse.jface.text.ITextViewer, org.eclipse.jface.text.IRegion)
	 */
	public String getHoverInfo(ITextViewer textViewer, IRegion hoverRegion) {
		if (hoverRegion != null) {
			try {
				if (hoverRegion.getLength() > -1) {
					String predicate = textViewer.getDocument().get(hoverRegion.getOffset(), hoverRegion.getLength());
					return PrologCorePlugin.getPredicateInfo(predicate);
				}
			} 
			catch (BadLocationException x) {
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.text.ITextHover#getHoverRegion(org.eclipse.jface.text.ITextViewer, int)
	 */
	public IRegion getHoverRegion(ITextViewer textViewer, int offset) {
		String s;
		int idx, len;
		
		// See if the user has made a selection
		Point selection= textViewer.getSelectedRange();
		if (selection.x <= offset && offset < selection.x + selection.y)
			return new Region(selection.x, selection.y);

		// Otherwise determine if we are over a keyword or an operator
		try {
			s = textViewer.getDocument().get(offset, 1);
			idx = offset;
			len = 0;
			
			// Operator
			if (oper.isWordPart(s.charAt(0))) {
				while (oper.isWordPart(textViewer.getDocument().get(idx-1, 1).charAt(0)))
					idx--;
				len = offset - idx;
				while (oper.isWordPart(textViewer.getDocument().get(idx+len, 1).charAt(0)))
					len++;
			}
			// Keyword
			else {
				while (word.isWordPart(textViewer.getDocument().get(idx-1, 1).charAt(0)))
					idx--;
				len = offset - idx;
				while (word.isWordPart(textViewer.getDocument().get(idx+len, 1).charAt(0)))
					len++;
			}
		} 
		catch (BadLocationException e) {
			return null;
		}
		
		return new Region(idx, len);		
	}

}

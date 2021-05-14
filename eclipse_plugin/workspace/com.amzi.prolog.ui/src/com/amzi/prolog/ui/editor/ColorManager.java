package com.amzi.prolog.ui.editor;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.jface.text.source.ISharedTextColors;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

/*
 * Copyright (c) 2002-2004 Amzi! inc. All Rights Reserved.
 */

public class ColorManager implements ISharedTextColors {

	protected Map colorRGBTable = new HashMap(10);
	
	public void dispose() {
		Iterator e = colorRGBTable.values().iterator();
		while (e.hasNext())
			 ((Color) e.next()).dispose();
	}
	
	public Color getColor(RGB rgb) {
		Color color = (Color) colorRGBTable.get(rgb);
		if (color == null) {
			color = new Color(Display.getCurrent(), rgb);
			colorRGBTable.put(rgb, color);
		}
		return color;
	}
	
}

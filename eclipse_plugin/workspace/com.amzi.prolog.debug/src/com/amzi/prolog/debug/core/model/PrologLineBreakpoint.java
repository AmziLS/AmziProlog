package com.amzi.prolog.debug.core.model;

//import java.util.HashMap;
//import java.util.Map;

import com.amzi.prolog.debug.PrologDebugPlugin;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.LineBreakpoint;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */

public class PrologLineBreakpoint extends LineBreakpoint {
	
	private static final String PROLOG_LINE_BREAKPOINT_MARKER = 
		"com.amzi.prolog.debug.prologLineBreakpointMarker";
	private static final String BREAKPOINT_SOURCE_NAME =
		"com.amzi.prolog.debug.sourceName";
		
	/**
	 * Constructor for PrologLineBreakpoint.
	 */
	public PrologLineBreakpoint() {
		super();
	}

	public PrologLineBreakpoint(IResource file, int line, int charStart, int charEnd) {
		super();
		
		try {
			// Create the marker
			setMarker(file.createMarker(PROLOG_LINE_BREAKPOINT_MARKER));
			IMarker marker = ensureMarker();			
			marker.setAttribute(IBreakpoint.ID, getModelIdentifier());
			marker.setAttribute(IBreakpoint.ENABLED, true);
			marker.setAttribute(IMarker.LINE_NUMBER, line);
			marker.setAttribute(IMarker.CHAR_START, charStart);
			marker.setAttribute(IMarker.CHAR_END, charEnd);
			marker.setAttribute(IBreakpoint.PERSISTED, true);
			String filename = "";
			if (file.isLinked())
				filename = file.getLocation().toOSString();
			else
				filename = file.getProjectRelativePath().toOSString();
			marker.setAttribute(BREAKPOINT_SOURCE_NAME, filename);
			String msg = file.getName() + "[line: " + Integer.toString(line) + "]";
			marker.setAttribute(IMarker.MESSAGE, msg);
			setPersisted(true);
			
			// Add to breakpoint manager
			DebugPlugin debugPlugin = DebugPlugin.getDefault();
			if(debugPlugin != null && debugPlugin.getBreakpointManager()!=null) {
				debugPlugin.getBreakpointManager().addBreakpoint(this);
				setRegistered(true);
			}
			else
				setRegistered(false);
		}
		catch (CoreException ex) {
			PrologDebugPlugin.log(ex);			
		}
		catch (NullPointerException ex) {
			PrologDebugPlugin.log(ex);
		}
	}

	public String getSourceName() {
		try {
			if (ensureMarker() != null) {
				return (String) ensureMarker().getAttribute(BREAKPOINT_SOURCE_NAME);
			}
		} catch (Exception ex) {
//			PrologDebugPlugin.log(ex);
		}
		return "";
	}

	/**
	 * Returns the type of marker associated with this type of breakpoints
	 */
	public static String getMarkerType() {
		return PROLOG_LINE_BREAKPOINT_MARKER;
	}

	/**
	 * @see org.eclipse.debug.core.model.IBreakpoint#getModelIdentifier()
	 */
	public String getModelIdentifier() {
		return PrologDebugPlugin.getModelID();
	}

}

package com.amzi.prolog.ui.editor.actions;

import java.util.ResourceBundle;

import com.amzi.prolog.debug.core.model.PrologLineBreakpoint;
import com.amzi.prolog.debug.PrologDebugPlugin;
import com.amzi.prolog.ui.PrologUIPlugin;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;


/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */

public class BreakpointRulerEditorAction extends TextEditorAction {
	private IVerticalRuler ruler;

	/**
	 * Constructor for BreakpointRulerEditorAction.
	 * @param bundle
	 * @param prefix
	 * @param editor
	 */
	public BreakpointRulerEditorAction(ResourceBundle bundle, String prefix, 
		ITextEditor editor, IVerticalRuler ruler) {
		super(bundle, prefix, editor);
		this.ruler = ruler;
	}

	/**
	 * Ths will be executed when the user clicks on the Breakpoint item in the context menu
	 * @see Action#run()
	 */
	public void run() {
		if (getTextEditor() != null) {
			try {
//				IDocument document =
//					getTextEditor().getDocumentProvider().getDocument(
//						getTextEditor().getEditorInput());
	
				// See if the breakpoint exists. The editor counts lines from 0 internally.
				// Everything else counts from 1.
				PrologLineBreakpoint breakpoint = PrologDebugPlugin.lineBreakpointExists(getResource().getProjectRelativePath().toOSString(), 
					ruler.getLineOfLastMouseButtonActivity() + 1);
					
				// If it doesn't exist, create it
				if (breakpoint == null) {
					// Must create it this way, or the marker does not appear
					final IEditorInput fEditorInput = getTextEditor().getEditorInput();
					IWorkspaceRunnable body = new IWorkspaceRunnable() {
						public void run(IProgressMonitor monitor) throws CoreException {
							createBreakpoint(fEditorInput);
						}
					};			
					try {
						ResourcesPlugin.getWorkspace().run(body, null);
					} 
					catch (CoreException ex) {
						PrologUIPlugin.log(ex);
					}
				}
				// Delete the marker and the breakpoint
				else
					breakpoint.delete();
			}
			catch (CoreException ex) {
				PrologUIPlugin.log(ex);
			}
		}
	}
	
	/**
	 * Creates a breakpoint to the current line.
	 * @param editorInput - current editor input
	 * @return breakpoint - returns the created COBOLLinebreakpoint 
	 * 				returns <code> null <code>if beakpoint creation fails
	 */
	protected IBreakpoint createBreakpoint(IEditorInput editorInput) {

		int lineStart = -1;
		int lineEnd = -1;
//		int length = 0;
		
		// Internally the editor line numbers start with 0
		int lineNumber = ruler.getLineOfLastMouseButtonActivity();

		IDocument document =
			getTextEditor().getDocumentProvider().getDocument(
				getTextEditor().getEditorInput());
		try {

			lineStart = document.getLineOffset(lineNumber);
			int lineLength = 0;
			lineLength = document.getLineLength(lineNumber);
			lineEnd = lineStart + document.getLineLength(lineNumber);
			if ((document.getNumberOfLines() != lineNumber)
				&& (lineLength != 1)
				&& (lineLength != 0))
				lineEnd = lineEnd - 2;
		} 
		catch (BadLocationException ex) {
			PrologUIPlugin.log(ex);
		}

		// But our breakpoints use line numbers starting with 1
		PrologLineBreakpoint bp = new PrologLineBreakpoint(getResource(),
			lineNumber + 1, lineStart, lineEnd);
			
		return bp;
	}
	
	/**
	 * Store the current line number in breakLineNumber variable
	 * @return IResource - the current file opened in the editor
	 */
	protected IResource getResource() {
		IEditorInput input = getTextEditor().getEditorInput();
		return (IResource) ((IAdaptable) input).getAdapter(IResource.class);
	}

}



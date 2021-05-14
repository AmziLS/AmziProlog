package com.amzi.prolog.debug.ui;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.ILineBreakpoint;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IVariable;
import org.eclipse.debug.core.model.LineBreakpoint;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugEditorPresentation;
import org.eclipse.debug.ui.IDebugModelPresentation;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.debug.ui.IValueDetailListener;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
//import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.IDocumentProvider;

import com.amzi.prolog.debug.PrologDebugPlugin;
import com.amzi.prolog.debug.core.model.PrologLineBreakpoint;
import com.amzi.prolog.debug.core.model.PrologStackFrame;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
 
public class PrologDebugModelPresentation extends LabelProvider implements IDebugModelPresentation, IDebugEditorPresentation {
//	private boolean showType = false;
	Annotation ip;
	IAnnotationModel annotationModel;
	List annotations = new ArrayList();
	
	/**
	 * Constructor for PrologDebugModelPresentation.
	 */
	public PrologDebugModelPresentation() {
		super();
	}

	/**
	 * @see org.eclipse.debug.ui.IDebugModelPresentation#setAttribute(java.lang.String, java.lang.Object)
	 */
	public void setAttribute(String attribute, Object value) {
/*		try {
			if (attribute.equals(IDebugModelPresentation.DISPLAY_VARIABLE_TYPE_NAMES)){
				showType = ((Boolean) value).booleanValue();
			}
		} 
		catch (ClassCastException ex) {
			PrologDebugPlugin.log(ex);
		}
*/
	}

	/**
	 * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
	 */
	public Image getImage(Object element) {
		try {
			if (element instanceof IMarker) {
				IBreakpoint bp = DebugPlugin.getDefault().getBreakpointManager().getBreakpoint((IMarker)element);
				if (bp != null && bp instanceof IBreakpoint) {
					return getBreakpointImage((IBreakpoint) bp);
				}
			}
			if (element instanceof IBreakpoint) {
				return getBreakpointImage((IBreakpoint) element);
			}
			if (element instanceof IVariable) {
				return DebugUITools.getImage(IDebugUIConstants.IMG_OBJS_VARIABLE);
			}
		} catch (CoreException ex) {
			PrologDebugPlugin.log(ex);
		}
		return null;
	}

	protected Image getBreakpointImage(IBreakpoint breakpoint)
		throws CoreException {
		try {
			if (breakpoint.isEnabled())
				return DebugUITools.getImage(IDebugUIConstants.IMG_OBJS_BREAKPOINT);
			else 
				return DebugUITools.getImage(IDebugUIConstants.IMG_OBJS_BREAKPOINT_DISABLED);
		} 
		catch (NullPointerException ex) {
			DebugPlugin.log(ex);
			return null;
		}
	}

	/**
	 * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
	 */
	public String getText(Object element) {
		try {
			if (element instanceof IMarker) {
				IBreakpoint breakpoint = DebugPlugin.getDefault().getBreakpointManager().getBreakpoint((IMarker)element);
				if (breakpoint != null) {
					return getBreakpointText(breakpoint);
				}
				return "<unknown breakpoint>";		// Perhaps stop crash for null pointer
			}
			if (element instanceof PrologLineBreakpoint) {
				return getBreakpointText((IBreakpoint) element);
			}
/*			if (element instanceof IVariable) {
				return getVariableText((IVariable) item);
			}
			if (element instanceof IValue) {
				return getValueText(((IValue) item));
			}
*/			if (element instanceof PrologStackFrame) {
				return ((PrologStackFrame)element).getName();
			}
		}
		catch (CoreException ex) {
			PrologDebugPlugin.log(ex);
			return ex.getMessage();
		} 
		catch (NullPointerException ex) {
			PrologDebugPlugin.log(ex);
			return ex.getMessage();
		}
		return null;
	}

	protected String getBreakpointText(IBreakpoint breakpoint)
		throws CoreException {
		StringBuffer label = new StringBuffer();
		if (breakpoint instanceof PrologLineBreakpoint) {
			try {
				final String fileName = ((PrologLineBreakpoint) breakpoint).getMarker().getResource().getProjectRelativePath().toOSString();
				if (fileName != null) {
					label.append(fileName);
				}
				final int lineNumber = ((PrologLineBreakpoint) breakpoint).getLineNumber();
				if (lineNumber > 0) {
					label.append(" [");
					label.append("line:");
					label.append(' ');
					label.append(lineNumber);
					label.append(']');
					return label.toString();
				}
			} 
			catch (NullPointerException ex) {
				PrologDebugPlugin.log(ex);
			}
		}
		return "<unknown breakpoint>";
	}


	/**
	 * @see org.eclipse.debug.ui.IDebugModelPresentation#computeDetail(org.eclipse.debug.core.model.IValue, org.eclipse.debug.ui.IValueDetailListener)
	 */
	public void computeDetail(IValue value, IValueDetailListener listener) {
	}

	/**
	 * @see org.eclipse.debug.ui.ISourcePresentation#getEditorInput(java.lang.Object)
	 */
	public IEditorInput getEditorInput(Object element) {
		if (element instanceof IStackFrame) {
			IResource resource = ((PrologStackFrame)element).getResource();
//			IDebugTarget debugTarget = ((IStackFrame)element).getDebugTarget();
//			return new PrologEditorInput(new FileEditorInput((IFile)resource), 
//				(PrologDebugTarget)debugTarget, (PrologStackFrame)element);
			return new FileEditorInput((IFile) resource);
		}
		if (element instanceof ILineBreakpoint) {
			LineBreakpoint s = (LineBreakpoint) element;
			IResource resource = s.getMarker().getResource();
			return new FileEditorInput((IFile) resource);
		} 
		if (element instanceof IFile) {
			return new FileEditorInput((IFile) element);
		}
		return null;
	}

	/**
	 * @see org.eclipse.debug.ui.ISourcePresentation#getEditorId(org.eclipse.ui.IEditorInput, java.lang.Object)
	 */
	public String getEditorId(IEditorInput input, Object element) {
		try {
			IEditorRegistry registry = PlatformUI.getWorkbench().getEditorRegistry();
		 	IEditorDescriptor descriptor = registry.getDefaultEditor(input.getName());
		 	if (descriptor != null) {
			 	return descriptor.getId();
			}
		} 
		catch (NullPointerException ex) {
		 	PrologDebugPlugin.log(ex);
	 	}
	 	return null;
	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
	 */
//	public void addListener(ILabelProviderListener listener) {
//		ILabelProviderListener l = listener;
//	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
	 */
	public void dispose() {
	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
	 */
	public boolean isLabelProperty(Object element, String property) {
		return true;
	}

	/**
	 * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
	 */
//	public void removeListener(ILabelProviderListener listener) {
//		ILabelProviderListener l = listener;
//	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.IDebugEditorPresentation#addAnnotations(org.eclipse.ui.IEditorPart, org.eclipse.debug.core.model.IStackFrame)
	 */
	public boolean addAnnotations(IEditorPart editorPart, IStackFrame stackFrame) {
		int charStart, length;
		Position position;
		PrologStackFrame prologFrame;
		
		if (!(editorPart instanceof TextEditor)) return false;
		if (stackFrame instanceof PrologStackFrame) {
			prologFrame = (PrologStackFrame)stackFrame;
			TextEditor textEditor = (TextEditor)editorPart;
			IDocumentProvider docProvider = textEditor.getDocumentProvider();
			IEditorInput editorInput = textEditor.getEditorInput();

			// Create the Position object that specifies a location for the annotation
			IDocument doc = docProvider.getDocument(editorInput);
			if (doc == null) {
				return false;
			}
			try {
				int lineNumber = stackFrame.getLineNumber() - 1;
				IRegion region = doc.getLineInformation(lineNumber);
				charStart = region.getOffset();
				length = region.getLength();
			} catch (BadLocationException ble) {
				return false;
			} catch (DebugException de) {
				return false;
			}
			position = new Position(charStart, length);
			textEditor.selectAndReveal(charStart, 0);
//			textEditor.getSourceViewer().revealRange(charStart, length);
				
			// If there is no annotation model, there's nothing more to do
			annotationModel = docProvider.getAnnotationModel(editorInput);
			if (annotationModel == null) {
				return false;
			}
			
			// Create the annotation object
			String port = prologFrame.getPort().toLowerCase().trim();
			ip = new Annotation("com.amzi.prolog.debug.ui."+port+"IP", false, "call");

			// Add the annotation at the position to the editor's annotation model.
			annotationModel.addAnnotation(ip, position);
			annotations.add(ip);
			return true;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.ui.IDebugEditorPresentation#removeAnnotations(org.eclipse.ui.IEditorPart, org.eclipse.debug.core.model.IThread)
	 */
	public void removeAnnotations(IEditorPart editorPart, IThread thread) {
	    for (Iterator i = annotations.iterator(); i.hasNext(); ) {
			annotationModel.removeAnnotation((Annotation) i.next());
	    	i.remove();
	    }
	}


}

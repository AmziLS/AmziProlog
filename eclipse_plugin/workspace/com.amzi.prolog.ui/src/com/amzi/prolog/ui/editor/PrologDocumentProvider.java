package com.amzi.prolog.ui.editor;

import com.amzi.prolog.ui.editor.PrologMarkerAnnotationModel;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.editors.text.FileDocumentProvider;
import org.eclipse.ui.IFileEditorInput;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */

public class PrologDocumentProvider extends FileDocumentProvider {
//	private ColorManager colorManager;
	private PrologPartitionScanner prologPartitionScanner;
	private final static String[] TYPES= new String[] { PrologPartitionScanner.PROLOG_COMMENT };

	public PrologDocumentProvider(ColorManager colorManager) {
//		this.colorManager = colorManager;
	}

	protected IDocument createDocument(Object element) throws CoreException {
		prologPartitionScanner = new PrologPartitionScanner();
		
		IDocument document = super.createDocument(element);
		if (document != null) {
			IDocumentPartitioner partitioner = 
				new FastPartitioner(prologPartitionScanner, TYPES);
			document.setDocumentPartitioner(partitioner);
			partitioner.connect(document);
		}
		return document;
	}
	
	protected IAnnotationModel createAnnotationModel(Object element)throws CoreException {
		if (element instanceof IFileEditorInput) {
			IFileEditorInput input= (IFileEditorInput) element;
			return new PrologMarkerAnnotationModel(input.getFile());
		}
		return super.createAnnotationModel(element);		
	}

	public void updateColor(String colorKey, RGB rgb) {
		prologPartitionScanner.updateColor(colorKey, rgb);
	}

}
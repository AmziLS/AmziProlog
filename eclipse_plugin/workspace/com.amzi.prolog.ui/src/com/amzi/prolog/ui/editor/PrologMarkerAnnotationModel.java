package com.amzi.prolog.ui.editor;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.ui.texteditor.MarkerAnnotation;
import org.eclipse.ui.texteditor.ResourceMarkerAnnotationModel;


/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */

public class PrologMarkerAnnotationModel
	extends ResourceMarkerAnnotationModel {

	/**
	 * Constructor for PrologMarkerAnnotationModel.
	 * @param resource
	 */
	public PrologMarkerAnnotationModel(IResource resource) {
		super(resource);
	}

	/**
	 * creates the marker annnotation model
	 * @param marker
	 */
//	protected MarkerAnnotation createMarkerAnnotation(IMarker marker) {
//		return new PrologMarkerAnnotation(marker);
//	}

	/**
	 * @see AbstractMarkerAnnotationModel#modifyMarkerAnnotation(IMarker)
	 */
	protected void modifyMarkerAnnotation(IMarker marker) {
		MarkerAnnotation a= getMarkerAnnotation(marker);
		if (a == null) {
			// It might not have been good enough before, but now it 
			// is, try adding this marker into the model again.
			addMarkerAnnotation(marker);
		}
		super.modifyMarkerAnnotation(marker);
	}

}

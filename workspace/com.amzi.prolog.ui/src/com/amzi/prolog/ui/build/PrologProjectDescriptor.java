package com.amzi.prolog.ui.build;

import org.eclipse.core.resources.IProjectNatureDescriptor;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class PrologProjectDescriptor implements IProjectNatureDescriptor {

	/**
	 * Constructor for PrologProjectDescriptor.
	 */
	public PrologProjectDescriptor() {
		super();
	}

	/**
	 * @see org.eclipse.core.resources.IProjectNatureDescriptor#getNatureId()
	 */
	public String getNatureId() {
		return "PrologNature";
	}

	/**
	 * @see org.eclipse.core.resources.IProjectNatureDescriptor#getLabel()
	 */
	public String getLabel() {
		return "Prolog Project";
	}

	/**
	 * @see org.eclipse.core.resources.IProjectNatureDescriptor#getRequiredNatureIds()
	 */
	public String[] getRequiredNatureIds() {
		return null;
	}

	/**
	 * @see org.eclipse.core.resources.IProjectNatureDescriptor#getNatureSetIds()
	 */
	public String[] getNatureSetIds() {
		return null;
	}

	public boolean isLinkingAllowed() {
		return true;
	}
}

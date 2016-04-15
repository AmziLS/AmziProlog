package com.amzi.prolog.ui.launch;

import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupDirector;
import org.eclipse.debug.core.sourcelookup.ISourceLookupParticipant;

public class PrologSourceLookupDirector extends AbstractSourceLookupDirector {

	public void initializeParticipants() {
		addParticipants(new ISourceLookupParticipant[]{new PrologSourceLookupParticipant()});
	}

}

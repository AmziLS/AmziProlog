package com.amzi.prolog.ui.launch;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupParticipant;

import com.amzi.prolog.debug.core.model.PrologStackFrame;

public class PrologSourceLookupParticipant extends
		AbstractSourceLookupParticipant {

	public String getSourceName(Object object) throws CoreException {
		if (object instanceof PrologStackFrame) {
			PrologStackFrame stack = (PrologStackFrame) object;
			String filename = stack.getFilename();
			if (filename == null)
				return null;
			// Must check for forward slash because Prolog always allows it
			if (filename.indexOf(System.getProperty("file.separator")) >= 0 || filename.indexOf("/") >= 0) {
				Path path = new Path(filename);
				String name = path.lastSegment();
//				String name = path.toOSString();
				return name;
			}
			else
				return filename;
		}
		return null;
	}

}

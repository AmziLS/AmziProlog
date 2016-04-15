package com.amzi.prolog.debug.core.model;

import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IStreamMonitor;

public class NullStreamMonitor implements IStreamMonitor {

	/**
	 * 
	 */
	public NullStreamMonitor() {
		super();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.core.model.IStreamMonitor#addListener(org.eclipse.debug.core.IStreamListener)
	 */
	public void addListener(IStreamListener listener) {
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.core.model.IStreamMonitor#getContents()
	 */
	public String getContents() {
		return "";
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.core.model.IStreamMonitor#removeListener(org.eclipse.debug.core.IStreamListener)
	 */
	public void removeListener(IStreamListener listener) {
	}

}

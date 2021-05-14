package com.amzi.prolog.debug.core.model;

import java.io.IOException;

import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.core.model.IStreamsProxy;

public class NullStreamsProxy implements IStreamsProxy {

	private IStreamMonitor streamMonitor;
	/**
	 * 
	 */
	public NullStreamsProxy() {
		super();
		streamMonitor = new NullStreamMonitor();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.core.model.IStreamsProxy#getErrorStreamMonitor()
	 */
	public IStreamMonitor getErrorStreamMonitor() {
		return streamMonitor;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.core.model.IStreamsProxy#getOutputStreamMonitor()
	 */
	public IStreamMonitor getOutputStreamMonitor() {
		return streamMonitor;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.debug.core.model.IStreamsProxy#write(java.lang.String)
	 */
	public void write(String input) throws IOException {
		return;
	}

}

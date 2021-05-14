package com.amzi.prolog.debug.core.model;

import java.util.Properties;

import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamsProxy;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class PrologDebugProcess extends PlatformObject implements IProcess {
	private String label;
	private PrologDebugTarget debugTarget;
	private Properties attributes = new Properties();
	private NullStreamsProxy nullStreamsProxy;
	
	/**
	 * Constructor for PrologDebugProcess.
	 */
	public PrologDebugProcess(String label, PrologDebugTarget debugTarget) {
		super();
		this.label = label;
		this.debugTarget = debugTarget;
		this.nullStreamsProxy = new NullStreamsProxy();
	}

	/**
	 * @see org.eclipse.debug.core.model.IProcess#getLabel()
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * @see org.eclipse.debug.core.model.IProcess#getLaunch()
	 */
	public ILaunch getLaunch() {
		return debugTarget.getLaunch();
	}

	/**
	 * @see org.eclipse.debug.core.model.IProcess#getStreamsProxy()
	 */
	public IStreamsProxy getStreamsProxy() {
		return nullStreamsProxy;
	}

	/**
	 * @see org.eclipse.debug.core.model.IProcess#setAttribute(java.lang.String, java.lang.String)
	 */
	public void setAttribute(String key, String value) {
		attributes.setProperty(key, value);
	}

	/**
	 * @see org.eclipse.debug.core.model.IProcess#getAttribute(java.lang.String)
	 */
	public String getAttribute(String key) {
		return attributes.getProperty(key);
	}

	/**
	 * @see org.eclipse.debug.core.model.IProcess#getExitValue()
	 */
	public int getExitValue() throws DebugException {
		return 0;
	}

	/**
	 * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
	 */
	public Object getAdapter(Class adapter) {
//		if (adapter == IProcess.class) {
//			return this;
//		}
//		return debugTarget.getAdapter(adapter);
		if (adapter.equals(IProcess.class)) {
			return this;
		}
		if (adapter.equals(IDebugTarget.class)) 
			return debugTarget;
			/*{
			ILaunch launch = getLaunch();
			IDebugTarget[] targets = launch.getDebugTargets();
			for (int i = 0; i < targets.length; i++) {
				if (this.equals(targets[i].getProcess())) {
					return targets[i];
				}
			}
			return null;
		} */
		return super.getAdapter(adapter);
	}

	/**
	 * @see org.eclipse.debug.core.model.ITerminate#canTerminate()
	 */
	public boolean canTerminate() {
		return debugTarget.canTerminate();
	}

	/**
	 * @see org.eclipse.debug.core.model.ITerminate#isTerminated()
	 */
	public boolean isTerminated() {
		return debugTarget.isTerminated();
	}

	/**
	 * @see org.eclipse.debug.core.model.ITerminate#terminate()
	 */
	public void terminate() throws DebugException {
		debugTarget.terminate();
	}

}

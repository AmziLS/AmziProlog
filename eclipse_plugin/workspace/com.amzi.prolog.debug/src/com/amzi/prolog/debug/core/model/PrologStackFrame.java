package com.amzi.prolog.debug.core.model;

import com.amzi.prolog.debug.core.model.PrologVariable;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugElement;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IRegisterGroup;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;
import org.eclipse.debug.core.model.IVariable;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class PrologStackFrame extends PlatformObject implements IDebugElement, IStackFrame {
	private String port;
	private String goal;
	private String filename;
	private IResource resource = null;
	private int line;
	private PrologDebugTarget debugTarget;
	private PrologVariable[] prologVars;
	private PrologVariable[] emptyVars = {};

	public PrologStackFrame(PrologDebugTarget target, String filename, int line,
		String port, String goal, PrologVariable[] prologVars) {
		this.debugTarget = target;
//		this.invocation = invocation;
		this.port = port;
		this.goal = goal;
		this.filename = filename;
		this.line = line;
		this.prologVars = prologVars;
		if (this.prologVars == null)
			this.prologVars = emptyVars;
	}
	
	/**
	 * @see org.eclipse.debug.core.model.IStackFrame#getThread()
	 */
	public IThread getThread() {
		return debugTarget.getPrimaryThread();
	}

	/**
	 * @see org.eclipse.debug.core.model.IStackFrame#hasVariables()
	 */
	public boolean hasVariables() throws DebugException {
		if (prologVars == null)
			return false;
		else
			return true;
	}

	/**
	 * @see org.eclipse.debug.core.model.IStackFrame#getVariables()
	 */
	public IVariable[] getVariables() throws DebugException {
		return prologVars;
	}

	/**
	 * @see org.eclipse.debug.core.model.IStackFrame#hasRegisterGroups()
	 */
	public boolean hasRegisterGroups() throws DebugException {
		return false;
	}

	/**
	 * @see org.eclipse.debug.core.model.IStackFrame#getRegisterGroups()
	 */
	public IRegisterGroup[] getRegisterGroups() throws DebugException {
		return null;
	}

	/**
	 * @see org.eclipse.debug.core.model.IStackFrame#getLineNumber()
	 */
	public int getLineNumber() throws DebugException {
		return line;
	}

	/**
	 * @see org.eclipse.debug.core.model.IStackFrame#getCharStart()
	 */
	public int getCharStart() throws DebugException {
		return -1;
	}

	/**
	 * @see org.eclipse.debug.core.model.IStackFrame#getCharEnd()
	 */
	public int getCharEnd() throws DebugException {
		return -1;
	}

	/**
	 * @see org.eclipse.debug.core.model.IStackFrame#getName()
	 */
	public String getName() throws DebugException {
		String s = "";
//		for (int i = 0 ; i < invocation ; i++)
//			s += " ";
		s += port + " " + goal;
		return s;
	}

	public IResource getResource() {
		return resource;
	}
	
	public void setResource(IResource resource) {
		this.resource = resource;
	}
	
	public String getFilename() {
		return filename;
	}

	public int getLine() {
		return line;
	}
	
	public String getPort() {
		return port;
	}
	
	/**
	 * @see org.eclipse.debug.core.model.IDebugElement#getModelIdentifier()
	 */
	public String getModelIdentifier() {
		return debugTarget.getModelIdentifier();
	}

	/**
	 * @see org.eclipse.debug.core.model.IDebugElement#getDebugTarget()
	 */
	public IDebugTarget getDebugTarget() {
		return debugTarget;
	}

	/**
	 * @see org.eclipse.debug.core.model.IDebugElement#getLaunch()
	 */
	public ILaunch getLaunch() {
		return debugTarget.getLaunch();
	}

	/**
	 * @see org.eclipse.debug.core.model.IStep#canStepInto()
	 */
	public boolean canStepInto() {
		boolean canStepInto = debugTarget.canStepInto();
		return canStepInto;
	}

	/**
	 * @see org.eclipse.debug.core.model.IStep#canStepOver()
	 */
	public boolean canStepOver() {
		boolean canStepOver = debugTarget.canStepOver();
		return canStepOver;
	}

	/**
	 * @see org.eclipse.debug.core.model.IStep#canStepReturn()
	 */
	public boolean canStepReturn() {
		boolean canStepReturn = debugTarget.canStepReturn();
		return canStepReturn;
	}

	/**
	 * @see org.eclipse.debug.core.model.IStep#isStepping()
	 */
	public boolean isStepping() {
		boolean stepping = debugTarget.isStepping();
		return stepping;
	}

	/**
	 * @see org.eclipse.debug.core.model.IStep#stepInto()
	 */
	public void stepInto() throws DebugException {
		debugTarget.stepInto();
	}

	/**
	 * @see org.eclipse.debug.core.model.IStep#stepOver()
	 */
	public void stepOver() throws DebugException {
		debugTarget.stepOver();
	}

	/**
	 * @see org.eclipse.debug.core.model.IStep#stepReturn()
	 */
	public void stepReturn() throws DebugException {
		debugTarget.stepReturn();
	}

	/**
	 * @see org.eclipse.debug.core.model.ISuspendResume#canResume()
	 */
	public boolean canResume() {
		boolean canResume = debugTarget.canResume();
		return canResume;
	}

	/**
	 * @see org.eclipse.debug.core.model.ISuspendResume#canSuspend()
	 */
	public boolean canSuspend() {
		boolean canSuspend = debugTarget.canSuspend();
		return canSuspend;
	}

	/**
	 * @see org.eclipse.debug.core.model.ISuspendResume#isSuspended()
	 */
	public boolean isSuspended() {
		boolean suspended = debugTarget.isSuspended();
		return suspended;
	}

	/**
	 * @see org.eclipse.debug.core.model.ISuspendResume#resume()
	 */
	public void resume() throws DebugException {
		debugTarget.resume();
	}

	/**
	 * @see org.eclipse.debug.core.model.ISuspendResume#suspend()
	 */
	public void suspend() throws DebugException {
		debugTarget.suspend();
	}

	/**
	 * @see org.eclipse.debug.core.model.ITerminate#canTerminate()
	 */
	public boolean canTerminate() {
		boolean canTerminate = debugTarget.canTerminate();
		return canTerminate;
	}

	/**
	 * @see org.eclipse.debug.core.model.ITerminate#isTerminated()
	 */
	public boolean isTerminated() {
		boolean terminated = debugTarget.isTerminated();
		return terminated;
	}

	/**
	 * @see org.eclipse.debug.core.model.ITerminate#terminate()
	 */
	public void terminate() throws DebugException {
		debugTarget.terminate();
	}

	/**
	 * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
	 */
	public Object getAdapter(Class adapter) {
//		if (adapter == IStackFrame.class) {
//			return this;
//		}
//		return debugTarget.getAdapter(adapter);
		if (adapter == IDebugElement.class) {
			return this;
		}
		return super.getAdapter(adapter);
	}

}

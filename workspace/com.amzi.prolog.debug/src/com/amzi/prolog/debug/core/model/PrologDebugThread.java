package com.amzi.prolog.debug.core.model;

//import com.amzi.prolog.debug.core.model.*;

import java.util.Vector;

import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IDebugElement;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
public class PrologDebugThread extends PlatformObject implements IDebugElement, IThread {
	private String name;
	private PrologDebugTarget debugTarget;
	private Vector stackFrames;
	private IStackFrame[] currentFrames;
	
	/**
	 * Constructor for PrologDebugThread.
	 */
	public PrologDebugThread(String name, PrologDebugTarget debugTarget) {
		super();
		this.name = name;
		this.debugTarget = debugTarget;
		stackFrames = new Vector(0);
		stackFrames.clear();
		currentFrames = null;
	}

	/**
	 * @see org.eclipse.debug.core.model.IThread#getStackFrames()
	 */
	public synchronized IStackFrame[] getStackFrames() throws DebugException {
//		if (stackFrames.size() == 0)
//			return null;
//		if (isSuspended()) {
//		if (currentFrames == null) {
			PrologStackFrame[] frames = new PrologStackFrame[stackFrames.size()];
			stackFrames.toArray(frames);
			currentFrames = frames;
//		}
			return (currentFrames);
//		}
//		else 
//			return new IStackFrame[0];
	}

	/**
	 * @see org.eclipse.debug.core.model.IThread#hasStackFrames()
	 */
	public boolean hasStackFrames() throws DebugException {
		return true;
//		return isSuspended();
	}
	
	public synchronized void removeAllStackFrames() {
		currentFrames = null;
		stackFrames.clear();
	}

	public void addStackFrame(PrologStackFrame frame) {
		stackFrames.add(frame);
	}
	
	/**
	 * @see org.eclipse.debug.core.model.IThread#getPriority()
	 */
	public int getPriority() throws DebugException {
		return 0;
	}

	/**
	 * @see org.eclipse.debug.core.model.IThread#getTopStackFrame()
	 */
	public IStackFrame getTopStackFrame() throws DebugException {
		if (stackFrames.size() == 0)
			return null;
		PrologStackFrame psf = (PrologStackFrame)stackFrames.lastElement();
		return psf;
	}

	/**
	 * @see org.eclipse.debug.core.model.IThread#getName()
	 */
	public String getName() throws DebugException {
		return name;
	}

	/**
	 * @see org.eclipse.debug.core.model.IThread#getBreakpoints()
	 */
	public IBreakpoint[] getBreakpoints() {
		return debugTarget.getBreakpoints();
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
	 * @see org.eclipse.debug.core.model.ISuspendResume#canResume()
	 */
	public boolean canResume() {
		return debugTarget.canResume();
	}

	/**
	 * @see org.eclipse.debug.core.model.ISuspendResume#canSuspend()
	 */
	public boolean canSuspend() {
		return debugTarget.canSuspend();
	}

	/**
	 * @see org.eclipse.debug.core.model.ISuspendResume#isSuspended()
	 */
	public boolean isSuspended() {
		return debugTarget.isSuspended();
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
	 * @see org.eclipse.debug.core.model.IStep#canStepInto()
	 */
	public boolean canStepInto() {
		return debugTarget.canStepInto();
	}

	/**
	 * @see org.eclipse.debug.core.model.IStep#canStepOver()
	 */
	public boolean canStepOver() {
		return debugTarget.canStepOver();
	}

	/**
	 * @see org.eclipse.debug.core.model.IStep#canStepReturn()
	 */
	public boolean canStepReturn() {
		return debugTarget.canStepReturn();
	}

	/**
	 * @see org.eclipse.debug.core.model.IStep#isStepping()
	 */
	public boolean isStepping() {
		return debugTarget.isStepping();
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

	/**
	 * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
	 */
	public Object getAdapter(Class adapter) {
//		if (adapter == IThread.class) {
//			return this;
//		} 
		if (adapter == IStackFrame.class) {
			try {
				return (IStackFrame)getTopStackFrame();
			} catch (DebugException e) {
				// do nothing if not able to get frame
			} 
		}

//		return debugTarget.getAdapter(adapter);

		if (adapter == IDebugElement.class) {
			return this;
		}
		return super.getAdapter(adapter);
	}

}

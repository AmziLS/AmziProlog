package com.amzi.prolog.debug.core.internal;

import java.util.Vector;

import com.amzi.prolog.debug.core.model.PrologDebugTarget;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
 
public class DebugListenerActionBuffer {
	static int count = 0;
	int id;
	boolean available;
	Vector buffer = new Vector();
	
	public DebugListenerActionBuffer()
	{
		available = false;
		count++;
		id = count;
	}
	
	public synchronized String get()
	{
		String action;
		Thread.currentThread().setPriority(Thread.MIN_PRIORITY);
		while (available == false) {
			try {
				// wait for Producer to put value
				//System.out.println("string get waiting " + id);
				wait();
			} catch (InterruptedException e) {
				buffer.add(0, PrologDebugTarget.TERMINATE);
			}
		}
		Thread.currentThread().setPriority(Thread.NORM_PRIORITY);

		//System.out.println("string get got " + id);
		// notify Producer that value has been retrieved
		notifyAll();
		//System.out.println("string get returning " + id);

		action = (String)buffer.elementAt(0);
		buffer.removeElementAt(0);
		if (buffer.size() > 0)
			available = true;
		else
			available = false;
		return action;
	}
	
	public synchronized void put(String s)
	{
		buffer.add(s);
		available = true;
		//System.out.println("string put put " + id);
		// notify Consumer that value has been set
		notifyAll();
		//System.out.println("string put returning " + id);
	}

	public synchronized void reset() {
		buffer.clear();
		available = false;
		notifyAll();
	}
}

package com.amzi.prolog.debug.core.internal;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
 
public class DebugListenerInputBuffer {

	static int count = 0;
	int id;
	boolean available;
	String   buffer;
	
	public DebugListenerInputBuffer() {
		available = false;
		count++;
		id = count;
	}
	
	public synchronized String get()	{
		Thread.currentThread().setPriority(Thread.MIN_PRIORITY);
		while (available == false) {
			 try {
				  // wait for Producer to put value
				  //System.out.println("string get waiting " + id);
				  wait();
			 } 
			 catch (InterruptedException e) {
				// If we're interrupted we need to break
				buffer = "\003\n";
			 }
		}
		Thread.currentThread().setPriority(Thread.NORM_PRIORITY);
		available = false;
		//System.out.println("string get got " + id);
		// notify Producer that value has been retrieved
		notifyAll();
		//System.out.println("string get returning " + id);
		return buffer;
		
	}
	
	public synchronized void put(String s) {
		while (available == true) {
			 try {
				  // wait for Consumer to get value
				  //System.out.println("string put waiting " + id);
				  wait();
			 } 
			 catch (InterruptedException e) {
				// If we're interrupted go on now
				s = "\003\n";
				break;
			 }
		}
		buffer = s;
		available = true;
		//System.out.println("string put put " + id);
		// notify Consumer that value has been set
		notifyAll();
		//System.out.println("string put returning " + id);
	}

	public synchronized boolean isAvailable() {
		return available;
	}
	
	public synchronized void reset() {
		available = false;
		notifyAll();
	}
}

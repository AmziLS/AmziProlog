package com.amzi.prolog.debug.core.internal;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
 
public class DebugListenerKeyBuffer {
	static int count = 0;
	int id;
	boolean available;
	char   buffer;
	
	public DebugListenerKeyBuffer()
	{
		available = false;
		count++;
		id = count;
	}
	
	public synchronized char get() {
		Thread.currentThread().setPriority(Thread.MIN_PRIORITY);
		while (available == false) {
			 try {
				  // wait for Producer to put value
				  //System.out.println("key get waiting " + id);
				  wait();
			 } 
			 catch (InterruptedException e) {
				// If we're interrupted we need to break
				buffer = '\003';
			 }
		}
		Thread.currentThread().setPriority(Thread.NORM_PRIORITY);
		available = false;
		// notify Producer that value has been retrieved
		notifyAll();
		return buffer;
		
	}
	
	public synchronized void put(char s) {
		while (available == true) {
			 try {
				  // wait for Consumer to get value
				  //System.out.println("key put waiting " + id);
				  wait();
			 } 
			 catch (InterruptedException e) { 
				// If we're interrupted go on now
				s = '\003';
				break;
			 }
		}
		buffer = s;
		available = true;
		// notify Consumer that value has been set
		notifyAll();
	}
	
	public synchronized boolean isAvailable() {
		return available;
	}
	
	public synchronized void reset() {
		available = false;
		notifyAll();
	}
}

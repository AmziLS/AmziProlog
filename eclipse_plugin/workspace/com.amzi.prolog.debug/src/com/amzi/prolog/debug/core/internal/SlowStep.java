package com.amzi.prolog.debug.core.internal;

import com.amzi.prolog.debug.core.model.PrologDebugTarget;

public class SlowStep extends Thread {
	public static int VERY_SLOW = 5000;
	public static int SLOW = 3000;
	public static int MEDIUM = 1500;
	public static int FAST = 500;
	public static int VERY_FAST = 0;
	
	private PrologDebugTarget target;
	private int delay;

	public SlowStep(PrologDebugTarget target, int delay) {
		this.target = target;
		this.delay = delay;
		setPriority(Thread.MIN_PRIORITY);
		setName("Slow Stepper");
	}
	
	public void run() {
		target.setSlowStepper(this);
		
		while (!target.isTerminated()) {

			// Queue up another step into
			target.stepInto();
			
			// We sleep while things happen
			while (!target.isSuspended()) {
				try {
					Thread.yield();
					Thread.sleep(100);
				} 
				catch (InterruptedException e) {
					return;
				}
			}
			
			// Now execution is suspended, so sleep some more for the user 
			// to read the screen
			try {
				Thread.yield();
				Thread.sleep(delay);
			} 
			catch (InterruptedException e) {
				return;
			}
			
		}
	}	

	/* (non-Javadoc)
	 * @see java.lang.Thread#finalize()
	 */
	public void finalize() {
		target.setSlowStepper(null);
	}

}

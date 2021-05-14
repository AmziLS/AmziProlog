package com.amzi.prolog.ui.internal;

import amzi.ls.*;

import java.io.File;

import com.amzi.prolog.ui.launch.ListenerView;
import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.core.utils.Utils;

import org.eclipse.swt.widgets.Display;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
 
public class Listener extends Thread {
	private LogicServer ls = null;
	private ListenerView listenerView;
	private boolean runningListener = false;
	private ListenerInputBuffer input;
	private ListenerKeyBuffer key;
	private String projectPathname, cfgPathname, xplPathname, consultList, loadList, lsxList;
	private String status;
		
	public Listener(String projectPathname, String cfgPathname,
		String xplPathname, String consultList, 
		String loadList, String lsxList, ListenerView listenerView,
		ListenerInputBuffer input, ListenerKeyBuffer key) {
		super("Listener");

		this.projectPathname = projectPathname;
		this.cfgPathname = cfgPathname;
		this.xplPathname = xplPathname;
		this.consultList = consultList;
		this.loadList = loadList;
		this.lsxList = lsxList;
		this.listenerView = listenerView;
		this.input = input;
		this.key = key;
	}

	public void init() {
		// Count how many files we will be consulting
		String proFiles[] = null;
		if (consultList != null && consultList.length() > 0) {
			proFiles = consultList.split(",");
		} 

		if (ls != null)
			close(false);
			
		ls = new LogicServer();
		try {
			synchronized (ls) {
				// If there is an amzi.cfg then specify it
				if (cfgPathname == null || cfgPathname.length() == 0)
					cfgPathname = projectPathname + System.getProperty("file.separator") +
						"amzi.cfg";
				File cfg = new File(cfgPathname);
				if (cfg.exists())
					ls.Init(cfgPathname);
				else
					ls.Init("");
				
				// Need aosutils for chdir
				ls.AddLSX("aosutils", 0);
				
				// Add the rest of the lsxs
				if (lsxList != null && lsxList.length() > 0) {
					String lsxs[] = lsxList.split(",");
					for (int i = 0 ; i < lsxs.length ; i++)
						if (lsxs[i].toLowerCase().indexOf("aosutils") < 0) {
							int dot = lsxs[i].indexOf(".");
							String filename = lsxs[i].substring(0, dot);
							ls.AddLSX(filename, 0);
						}
				}
	
				ls.AddPred("keyb", 1, "com.amzi.prolog.ui.internal.Listener", "p_keyb", this, LogicServer.ECLIPSE);
				ls.AddPred("user_break", 0, "com.amzi.prolog.ui.internal.Listener", "p_user_break", this, LogicServer.ECLIPSE);
	
				// Load this AFTER AddPred's
				String amziDir = PrologCorePlugin.getAmziDir();
				if (xplPathname != null && xplPathname.length() > 0) {
					File xplFile = new File(xplPathname);
					// isAbsolute does not work for Windows pathnames beginning with \
					if (!xplFile.isAbsolute() && !xplPathname.startsWith(System.getProperty("file.separator")))
						ls.Load(Utils.tiltSlashes(projectPathname + System.getProperty("file.separator") + 
							xplPathname));
					else
						ls.Load(Utils.tiltSlashes(xplPathname));
					runningListener = false;
				} 
				else {
					ls.Load(amziDir + "abin" + System.getProperty("file.separator") + "aidl.xpl");
					runningListener = true;
				}
	
				int in = ls.OpenInputStream("user_in", "com.amzi.prolog.ui.internal.Listener", "user_get_line", this);
				int out = ls.OpenOutputStream("user_out","com.amzi.prolog.ui.internal.Listener", "user_put_string", this);
				ls.SetStream(LogicServer.CUR_IN, in);
				ls.SetStream(LogicServer.CUR_OUT, out);
				ls.SetStream(LogicServer.CUR_ERR, out);
				ls.SetStream(LogicServer.USER_IN, in);
				ls.SetStream(LogicServer.USER_OUT, out);
				ls.SetStream(LogicServer.USER_ERR, out);
	
				// Change directory before loading
				ls.ExecStr("chdir(`" + Utils.tiltSlashes(amziDir + "abin") + "`)");
				if (loadList != null && loadList.length() > 0) {
					long tf = ls.ExecStr("load([" + Utils.tiltSlashes(loadList) + "])");
					if (tf == 0) {
						displayError("Load failed");
						return;
					}
				}
	
				// Change directory before consulting
				if (ls.ExecStr("chdir(`" + Utils.tiltSlashes(projectPathname) + "`)") == 0)
					displayError("Unable to change directory to project directory");
	
				// Consult the files one at a time 
				if (proFiles != null) {
					for (int i = 0 ; i < proFiles.length ; i++) {
						if (i > 0) user_put_string(", ");
						user_put_string(proFiles[i]);
						
						// Consult the file
						long tf = ls.ExecStr("consult([" + Utils.tiltSlashes(proFiles[i]) + "])");
						if (tf == 0) {
							displayError("Consult failed");
							return;
						}
					}
				}
				
				// Tell the user we're ready to go
				if (runningListener)
					user_put_string("\nType 'quit.' to end and [Ctrl]-C to stop user input.\n");
			}
		} 
		catch(LSException ex) {
			displayError(ex);
			listenerView.setInputEnabled(false);
			user_put_string("\n\n*** Listener Failed to Start ***");

			if (ex.GetType() == LSException.READ || 
				ex.GetType() == LSException.EXEC ||
				ex.GetType() == LSException.FATAL)
				close(true);
			else
				close(false);
		}
	} 
	
	public void run() {
		boolean done = false;
		boolean chdir = true;
		
		// Initialize
		init();
		if (ls == null) return;

		if (runningListener) {
			while (!done) {
				try {
					synchronized (ls) {
						ls.ExecStr("amzi_listener:eclipse_listen");
						done = true;
					}
				}
				catch(LSException ex) {
					if (ex.GetRC() == 1029) 	// break
						done = true;
					else
						displayError(ex);
					
					// We can go on with read and exec errors
					if (ex.GetType() != LSException.EXEC && 
						ex.GetType() != LSException.READ &&
						ex.GetType() != LSException.FATAL) {
						done = true;
						chdir = false;
					}
				}
			}
		}
		else  {
			try {
				synchronized (ls) {
					// May not start with input
					listenerView.setInputEnabled(false);
					ls.Main();
				}
			}
			catch (LSException ex) {
				if (ex.GetRC() != 1029) 	// break
					displayError(ex);
				
				if (ex.GetType() != LSException.EXEC && 
					ex.GetType() != LSException.READ &&
					ex.GetType() != LSException.FATAL) {
					chdir = false;
				}
			}
		}

		if (runningListener)
			user_put_string("\n\n*** Listener Finished: Press Play or Run|Listener or Run|Run to Restart ***");
		else
			user_put_string("\n\n*** Run Finished: Press Run|Run to Restart ***");

		// Clean everything up
		close(chdir);
	}
	
	// Extended predicate: put a line in the output
	public void user_put_string(String str) {
		status = "user_put_string";
		final String fstr = str;
		// Display.getCurrent() is null! so use this instead
		Display.getDefault().syncExec(new Runnable() {
			public void run() {
				listenerView.appendText(fstr);
				listenerView.setInputStartPosition();
			}
		});
	}
	
	// Extended predicate: get a line from the user
	// Don't synchronize this or the above will not run!
	public String user_get_line() {
		status = "user_get_line";
		// Get the string
		listenerView.setInputEnabled(true);
		String s = input.get();
		listenerView.setInputEnabled(false);
		status = "user_got_line: " + s;
		
		// Prolog no likie carriage returns
		s.replaceAll("\n", "");
		return s.replaceAll("\r", "");
	}

	// Extended predicate: more solutions
	public boolean p_keyb() {
		status = "get keyb";
		listenerView.setSingleKeyMode(true);
		listenerView.setInputEnabled(true);
		char c = key.get();
		status = "got keyb: " + c;
		int i = c;
		listenerView.setInputEnabled(false);
		boolean tf = false;
		try {
			synchronized (ls) {
				tf = ls.UnifyIntParm(1, i);
			}
		}
		catch (LSException ex) {
			displayError(ex);
		}
		return tf;
	}
	
	// Extended predicate: user break
	public boolean p_user_break()
	{
		if (listenerView.isTimeToStop()) {
			// Only send it once, we crash if we interrupt the interrupt
			listenerView.setTimeToStop(false);
			return true;
		}
		else
			return false;
	}

	public void displayError(LSException ex) {
//		if (terminating) return;
		
		String message, location;
		int rc;
		
//		synchronized (ex) {
			if (ex.GetType() == LSException.READ) {
				message = "*** Logic Server Exception ***\n";
				message += ex.getMessage();
				message += "\nline: " + ex.GetLineno();
				message += "\nfile: " + ex.GetReadFileName();
				location = ex.GetReadBuffer();
				message += "\nread buffer: " + location;
				message += "\n";

				// Can't do this, might not be a file, besides outline does this
	//			listenerView.addProblemMarker(ex.getMessage(), ex.GetReadFileName(),
	//				ex.GetLineno(), location);
			}
			else {
				message = "*** Logic Server Exception ***\n";
				message += ex.getMessage();
				rc = ex.GetRC();
				message += "\nerror code: " + Integer.toString(rc);
				location = ex.GetCallStack();
				message += "\ncall stack: " + location;
				message += "\n";
			}
//		}

		final String fmessage = message;
		
		// Display.getCurrent() is null! so use this instead
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				listenerView.appendText(fmessage);
			}
		});
	}

	public void displayError(String errText) {
//		if (terminating) return;
		
		String message;
		
		message = "*** Error ***\n";
		message += errText;
		message += "\n";
		
		final String fmessage = message;
		
		// Display.getCurrent() is null! so use this instead
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				listenerView.appendText(fmessage);
			}
		});
	}

	public void close(boolean chdir) {
		// Close things down
		try {
			synchronized (ls) {
				if (ls != null) {
					// Change directory up to workspace
					if (chdir) ls.ExecStr("chdir(`..`)");
					ls.Close();
					ls = null;
					System.gc();
				}
			}
		} 
    	catch(LSException ex) {
    	}
    	
    	// Tell them we're done
   		listenerView.endListener();
	}

	public String getStatus() {
		return status;
	}
	
	protected void finalize() throws Throwable {
		close(false);
		super.finalize();
	}

}

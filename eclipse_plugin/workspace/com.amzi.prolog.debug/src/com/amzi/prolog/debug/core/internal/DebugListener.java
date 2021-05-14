package com.amzi.prolog.debug.core.internal;

import amzi.ls.*;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Properties;
import java.util.ArrayList;
import java.util.List;

import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.core.utils.Utils;
import com.amzi.prolog.debug.core.internal.DebugListenerActionBuffer;
import com.amzi.prolog.debug.core.internal.DebugListenerInputBuffer;
import com.amzi.prolog.debug.core.internal.DebugListenerKeyBuffer;
import com.amzi.prolog.debug.core.model.PrologDebugTarget;
import com.amzi.prolog.debug.core.model.PrologStackFrame;
import com.amzi.prolog.debug.core.model.PrologVariable;
import com.amzi.prolog.debug.core.model.PrologValue;
import com.amzi.prolog.debug.ui.DebugListenerView;

//import org.eclipse.debug.core.DebugException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;


/*
 * Copyright (c) 2002-2003 Amzi! inc. All Rights Reserved.
 * 
 * The DebugListener interacts with the PrologDebugTarget to initiate and
 * notify debugger actions. It interacts with the DebugListenerView to manage
 * user input and output. And extended predicates and i/o redirection are
 * used for the Amzi! Logic Server.
 */
 
public class DebugListener extends Thread {
	// Must have a .txt extension
	private static String LISTENER_FILENAME = "listener_ouput.txt";
	private LogicServer ls = null;
	private PrologDebugTarget debugTarget;
	private DebugListenerView debugListenerView;
	private DebugListenerActionBuffer action;
	private DebugListenerInputBuffer input;
	private DebugListenerKeyBuffer key;
	private String projectPathname, cfgPathname, xplPathname, consultList, loadList, lsxList;
	private boolean respondToAction;
	private File listenFile = null;
	
	public DebugListener(PrologDebugTarget debugTarget, String projectPathname, 
		String cfgPathname, String xplPathname, String consultList, String loadList, String lsxList,  
		DebugListenerView listenerView, DebugListenerInputBuffer input, 
		DebugListenerKeyBuffer key, DebugListenerActionBuffer action) {
			
		super("Debug Listener");

		this.debugTarget = debugTarget;
		this.projectPathname = projectPathname;
		this.cfgPathname = cfgPathname;
		this.xplPathname = xplPathname;
		this.consultList = consultList;
		this.loadList = loadList;
		this.lsxList = lsxList;
		this.debugListenerView = listenerView;
		this.input = input;
		this.key = key;
		this.action = action;
		this.respondToAction = true;
	}

	private void init() {
		// Count how many files we will be consulting
		String proFiles[] = null;
		if (consultList != null && consultList.length() > 0) {
			proFiles = consultList.split(",");
//			monitor.beginTask("Initializing Debug Listener", proFiles.length+1);
		} 
//		else
//			monitor.beginTask("Initializing Debug Listener", IProgressMonitor.UNKNOWN);

		if (ls != null)
			close(false);
		
		ls = new LogicServer();
		try {
//			synchronized (ls) {
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
	
				ls.AddPred("keyb", 1, "com.amzi.prolog.debug.core.internal.DebugListener", "p_keyb", this, LogicServer.ECLIPSE);
				ls.AddPred("user_break", 0, "com.amzi.prolog.core.internal.DebugListener", "p_user_break", this, LogicServer.ECLIPSE);
				ls.AddPred("user_pause", 0, "com.amzi.prolog.core.internal.DebugListener", "p_user_pause", this, LogicServer.ECLIPSE);
				ls.AddPred("user_debug_action", 1, "com.amzi.prolog.debug.core.internal.DebugListener", "p_debug_action", this, LogicServer.ECLIPSE);
				ls.AddPred("user_debug_stack", 5, "com.amzi.prolog.debug.core.internal.DebugListener", "p_debug_stack", this, LogicServer.ECLIPSE);
				ls.AddPred("user_debug_error", 1, "com.amzi.prolog.debug.core.internal.DebugListener", "p_debug_error", this, LogicServer.ECLIPSE);
	
				// Load this AFTER AddPred's
				String amziDir = PrologCorePlugin.getAmziDir();
				if (xplPathname == null || xplPathname.length() == 0)
					ls.Load(amziDir+"abin"+System.getProperty("file.separator")+"aidl.xpl");
				else {
					File xplFile = new File(xplPathname);
					// isAbsolute does not work for Windows pathnames beginning with \
					if (!xplFile.isAbsolute() && !xplPathname.startsWith(System.getProperty("file.separator")))
						ls.Load(projectPathname + System.getProperty("file.separator") + xplPathname);
					else
						ls.Load(xplPathname);
				}
				
				// Redirect the streams
				int in = ls.OpenInputStream("user_in", "com.amzi.prolog.debug.core.internal.DebugListener", "user_get_line", this);
				int out = ls.OpenOutputStream("user_out","com.amzi.prolog.debug.core.internal.DebugListener", "user_put_string", this);
				ls.SetStream(LogicServer.CUR_IN, in);
				ls.SetStream(LogicServer.CUR_OUT, out);
				ls.SetStream(LogicServer.CUR_ERR, out);
				ls.SetStream(LogicServer.USER_IN, in);
				ls.SetStream(LogicServer.USER_OUT, out);
				ls.SetStream(LogicServer.USER_ERR, out);
	
				// Initialize the debugger
	//			if (ls.ExecStr("debug_init") == 0)
	//				displayError("Unable to initialize debugger");
	
				// Change directory before loading
				if (ls.ExecStr("chdir(`" + Utils.tiltSlashes(amziDir + "abin") + "`)") == 0)
					displayError("Unable to change directory to abin");
				if (loadList != null && loadList.length() > 0) {
					long tf = ls.ExecStr("load([" + Utils.tiltSlashes(loadList) + "])");
					if (tf == 0) {
						displayError("Load failed");
						return;
					}
				}
	
				// Change directory before consulting
	//			monitor.worked(1);
				if (ls.ExecStr("chdir(`" + Utils.tiltSlashes(projectPathname) + "`)") == 0)
					displayError("Unable to change directory to project directory");
	
				// Consult the files one at a time with a progress monitor
				if (proFiles != null) {
					for (int i = 0 ; i < proFiles.length ; i++) {
						if (i > 0) user_put_string(", ");
						user_put_string(proFiles[i]);
	//					monitor.setTaskName("Consulting "+proFiles[i]);
	//					if (monitor.isCanceled()) return;
						
						// Consult the file
						long tf = ls.ExecStr("debug_consult([" + Utils.tiltSlashes(proFiles[i]) + "])");
						if (tf == 0) {
							displayError("Debug Consult failed");
							return;
						}
	//					monitor.worked(i+2);
					}
				}
				
				// Pick up any errors: parse_message(Type, File, Line, Msg, Text)
				debugListenerView.clearProblemMarkers();
				long term = ls.CallStr("amzi_system:parse_message(Type, File, Line, Msg, Text)");
				if (term != 0) {
					user_put_string("\nSee the Problems view for errors.");
					boolean tf = true;
					while (tf) {
						boolean isError = false;
						if (ls.GetStrArg(term, 1).equalsIgnoreCase("error")) isError = true;
						debugListenerView.addProblemMarker(isError, ls.GetStrArg(term, 4),
							ls.GetStrArg(term, 2), ls.GetIntArg(term, 3),
							ls.GetStrArg(term, 5));
						 tf = ls.Redo();
					}
				}
	
				// Turn on debugging (throws an error on failure)
				if (ls.ExecStr("amzi_system:debug64_init") == 0)
					user_put_string("\nUnable to initialize debugger");
//				debugTarget.fireDebugAction(PrologDebugTarget.STEP_INTO);
				
				// Tell the we're ready to go
				if (xplPathname == null || xplPathname.length() == 0)
					user_put_string("\n\nType 'quit.' to end and [Ctrl]-C to break user input.\n");
				else
					user_put_string("\n");
				
				user_put_string("At breakpoints, click on the last stack frame to enable run, step and stop buttons\n");
//			}
		} 
		catch(LSException ex) {
			debugListenerView.setInputEnabled(false);
			user_put_string("\n\n*** Debug Listener Failed to Start ***");
			displayError(ex);
			if (ex.GetType() == LSException.READ || 
				ex.GetType() == LSException.EXEC ||
				ex.GetType() == LSException.FATAL)
				close(true);
			else
				close(false);
		}
	} 
	
	public void run() {
		boolean chdir = true;
		
		// Initialize
		init();
		if (ls == null) return;
		
		if (xplPathname == null || xplPathname.length() == 0) {
			try {
//				synchronized (ls) {
					ls.ExecStr("amzi_listener:eclipse_listen");
//				}
			}
			catch(LSException ex) {
				if (ex.GetRC() != 1029 && ex.GetRC() != 1001) 	// break or abort
					displayError(ex);	
				
				// We can go on with read and exec errors
				if (ex.GetType() != LSException.EXEC && 
					ex.GetType() != LSException.READ &&
					ex.GetType() != LSException.FATAL) {
					chdir = false;
				}
			}
		} 
		else {
			try {
//				synchronized (ls) {
					debugListenerView.setInputEnabled(false);
					ls.Main();
//				}
			}
			catch(LSException ex) {
				if (ex.GetRC() != 1029 && ex.GetRC() != 1001) 	// break or abort
					displayError(ex);	
				
				// We can go on with read and exec errors
				if (ex.GetType() != LSException.EXEC && 
					ex.GetType() != LSException.READ &&
					ex.GetType() != LSException.FATAL) {
					chdir = false;
				}
			}
		}

		user_put_string("\n\n*** Debug Listener Finished: Select Debug|Debug to Restart ***");

		// Clean everything up
		close(chdir);
	}
	
	// Extended predicate: put a line in the output
	public void user_put_string(String str) {
		final String fstr = str;
		// Display.getCurrent() is null! so use this instead
		Display.getDefault().syncExec(new Runnable() {
			public void run() {
				debugListenerView.appendText(fstr);
				debugListenerView.setInputStartPosition();
			}
		});
	}
	
	// Extended predicate: get a line from the user
	// Don't synchronize this or the above will not run!
	public String user_get_line() {
		debugListenerView.setInputEnabled(true);
		String s = input.get();
		debugListenerView.setInputEnabled(false);

		// Prolog no likie carriage returns
		s.replaceAll("\n", "");
		return s.replaceAll("\r", "");
	}

	// Extended predicate: more solutions
	public boolean p_keyb() {
		debugListenerView.setInputEnabled(true);
		debugListenerView.setSingleKeyMode(true);
		char c = key.get();
		int i = c;
		debugListenerView.setInputEnabled(false);
		boolean tf = false;
		try {
//			synchronized (ls) {
				tf = ls.UnifyIntParm(1, i);
//			}
		}
		catch (LSException ex) {
			displayError(ex);
		}
		return tf;
	}
	
	// Extended predicate: user break
	public boolean p_user_break()
	{
//		boolean stop = debugListenerView.isTimeToStop();
//		boolean pause = debugListenerView.isTimeToPause();
		
		if (debugListenerView.isTimeToStop() || debugListenerView.isTimeToPause()) {
			debugListenerView.setTimeToStop(false);
			return true;
		}
		else
			return false;
	}

	// Extended predicate: user pause
	public boolean p_user_pause()
	{
		if (debugListenerView.isTimeToPause()) {
//			debugListenerView.setTimeToPause(false);
			return true;
		}
		else
			return false;
	}

	// debug_action(Action) gets next action from user
	//   [step, jump, set_breakpoint(File,Line)]
	public boolean p_debug_action()
	{
		// We suspended
		if (debugListenerView.isTimeToPause()) {
			// Reset the flag
			debugListenerView.setTimeToPause(false);
			
			// Tell everyone we've paused due to suspend
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					debugTarget.fireDebugAction(PrologDebugTarget.SUSPEND);
				}
			});
		}
		else {
			// Tell everyone we've stopped and why
			// This is true the first time through
			if (respondToAction) {
				Display.getDefault().asyncExec(new Runnable() {
					public void run() {
						if (debugTarget.isStepping())
							debugTarget.fireDebugAction(PrologDebugTarget.THREAD_SUSPEND_STEP_END);
						else
							debugTarget.fireDebugAction(PrologDebugTarget.THREAD_SUSPEND_BREAKPOINT);
					}
				});
			}
		}

		String s = action.get();		
		if (s.equalsIgnoreCase(PrologDebugTarget.TERMINATE))
			debugListenerView.setTimeToStop(false);
		if (s.equals(PrologDebugTarget.RESUME) || s.equals(PrologDebugTarget.STEP_INTO) ||
			s.equals(PrologDebugTarget.STEP_OVER) || s.equals(PrologDebugTarget.STEP_RETURN))
			respondToAction = true;
		else
			respondToAction = false;
		try {
//			synchronized (ls) {
				ls.UnifyStrParm(1, s);
//			}
			final String fstr = s;
			Display.getDefault().asyncExec(new Runnable() {
				public void run() {
					debugTarget.fireDebugAction(fstr);
				}
			});
			return true;
		}
		catch (LSException ex) {
			return false;
		}
	}

	// debug_stack(Port, Goal, File, Line, Stack) reports the entire stack and its vars
	public boolean p_debug_stack()
	{
		try {
//			synchronized (ls) {
//				String port = ls.GetStrParm(1);
//				int tlen = ls.StrTermLen(ls.GetParm(2));
//				String goal = ls.TermToStrQ(ls.GetParm(2), tlen+2);
//				String file = ls.GetStrParm(3);
//				int line = ls.GetIntParm(4);
//	
				long stack = ls.GetParm(5);
				buildStack(stack);
//			}
		}
		catch (LSException ex) {
			displayError(ex);
		}
		return true;
	}

	private synchronized void buildStack(long stack)
	{
		PrologVariable[] prologVars = null;
		long frame, head, vars;
		String type, headStr, varsStr, fileName, port;
		int line, cut;
		
		// Clear the stack first		
		debugTarget.getPrimaryThread().removeAllStackFrames();
		
		try
		{
//			synchronized (ls) {
				while (stack != 0 && ls.GetTermType(stack) != LogicServer.pATOM)
				{
					frame = ls.GetHead(stack);
					type = ls.GetFunctor(frame);
					stack = ls.GetTail(stack);
	
					// clause(Head,File,Line,Port,Vars,I)
					if (type.equals("clause")) {
						head = ls.GetArg(frame,1);
//						len = ls.StrTermLen(head);
//						headStr = ls.TermToStrQ(head, len+2);
						headStr = ls.TermToStrQ(head, 500);
						fileName = ls.GetStrArg(frame,2);
						line = ls.GetIntArg(frame,3);
						port = ls.GetStrArg(frame,4);
						vars = ls.GetArg(frame,5);
						// arg 6 is the integer indent
//						len = ls.StrTermLen(vars);
//						varsStr = ls.TermToStrQ(vars, len+2);
						varsStr = ls.TermToStrQ(vars, 500);
						prologVars = buildPrologVariables(vars);
						if (fileName.equals("listener")) {
							if (stack == 0) {
//								debugTarget.stopSlowStepper();
//								debugTarget.actionReady(PrologDebugTarget.STEP_INTO);
							}
							try {
								listenFile = new File(projectPathname + System.getProperty("file.separator") +
//									debugTarget.getBinPathname() + System.getProperty("file.separator") + 
									LISTENER_FILENAME);
								if (listenFile.exists())
									listenFile.delete();
								fileName = listenFile.getAbsolutePath();
								FileWriter fw = new FileWriter(fileName);
								fw.write(headStr);
								fw.close();
								debugTarget.refreshProject(IResource.DEPTH_ONE);
							} 
							catch (IOException e) {
								fileName = null;
							}
							line = 1;
//							port = "?-";
						}
						if (port.equalsIgnoreCase("INFO"))
							port = "     ";
						if (varsStr.trim().indexOf("[]") != 1)
							headStr += "   " + varsStr.substring(1, varsStr.length()-1);				
	
						cut = ls.GetIntArg(frame,7);
						if (cut != 0)
							headStr = "! " + headStr;
							
						debugTarget.getPrimaryThread().addStackFrame(new PrologStackFrame(debugTarget, fileName, line, port, headStr, prologVars));
					}
					// goal(Goal,File,Line,Port,I)
					if (type.equals("goal")) {
						head = ls.GetArg(frame,1);
//						len = ls.StrTermLen(head);
//						headStr = ls.TermToStrQ(head, len+2);
						headStr = ls.TermToStrQ(head, 500);
						fileName = ls.GetStrArg(frame,2);
						line = ls.GetIntArg(frame,3);
						port = ls.GetStrArg(frame,4);
						if (fileName.equals("listener")) {
//							if (stack == 0) {
//								debugTarget.stopSlowStepper();
//								try {
//									debugTarget.getPrimaryThread().stepInto();
//								} catch (DebugException e) {
//								}
//								debugTarget.actionReady(PrologDebugTarget.STEP_INTO);
//							}
							try {
								listenFile = new File(projectPathname + System.getProperty("file.separator") +
//									debugTarget.getBinPathname() + System.getProperty("file.separator") + 
									LISTENER_FILENAME);
								if (listenFile.exists())
									listenFile.delete();
								fileName = listenFile.getAbsolutePath();
								FileWriter fw = new FileWriter(fileName);
								fw.write(headStr);
								fw.close();
								debugTarget.refreshProject(IResource.DEPTH_ONE);
							} 
							catch (IOException e) {
								fileName = null;
							}
							line = 1;
						}
						if (port.equalsIgnoreCase("INFO"))
							port = "     ";
	
						if (port.equalsIgnoreCase("CALL") || port.equalsIgnoreCase("FAIL") || 
							port.equalsIgnoreCase("EXIT"))
							debugTarget.getPrimaryThread().addStackFrame(new PrologStackFrame(debugTarget, fileName, line, port, headStr, prologVars));
						else
							debugTarget.getPrimaryThread().addStackFrame(new PrologStackFrame(debugTarget, fileName, line, port, headStr, null));					
					}
				}
//			}
		}
		catch(LSException ex) {
			displayError(ex);
		}
		return;
	}

	private synchronized PrologVariable[] buildPrologVariables(long pList)
	{
		PrologVariable[] prologVars = null;
		long element, name, value;
		List varList = new ArrayList();

		try
		{
//			synchronized (ls) {
				// Check for the empty list or an atom
				long type = ls.GetTermType(pList);
				if (type != LogicServer.pLIST) return null;
	
				while (pList != 0)
				{
				  element = ls.GetHead(pList);
				  name = ls.GetArg(element, 1);
				  value = ls.GetArg(element, 2);
				  int nlen = ls.StrTermLen(name);
//				  int vlen = ls.StrTermLen(value);
//				  String vstr = ls.TermToStrQ(value, vlen+100);
				  String vstr = ls.TermToStrQ(value, 500);
				  String nstr = ls.TermToStr(name, nlen+100);
				  PrologValue pValue = new PrologValue(debugTarget, vstr);
				  PrologVariable pVar = new PrologVariable(debugTarget, nstr, pValue);
				  varList.add(pVar);
	
				  pList = ls.GetTail(pList);
				}
//			}
		}
		catch (LSException ex) {
			displayError(ex);
			return null;
		}
		
		prologVars = new PrologVariable[varList.size()];
		varList.toArray(prologVars);
		
		return prologVars;
	}
	
/*	private String buildStackString(long stack)
	{
//		Properties varsProps;
		long frame, head, vars;
		String str, type, headStr, varsStr, fileName, port;
		int len;

		str = "";
	
		try {
			synchronized (ls) {
				while (stack != 0 && ls.GetTermType(stack) != LogicServer.pATOM)
				{
					headStr = "";
					frame = ls.GetHead(stack);
					type = ls.GetFunctor(frame);
					stack = ls.GetTail(stack);
	
					// clause(Head,File,Line,Port,Vars,I)
					if (type.equals("clause")) {
						head = ls.GetArg(frame,1);
						len = ls.StrTermLen(head);
						headStr = ls.TermToStrQ(head, len+2);
						fileName = ls.GetStrArg(frame,2);
//						line = ls.GetIntArg(frame,3);
						port = ls.GetStrArg(frame,4);
						vars = ls.GetArg(frame,5);
						len = ls.StrTermLen(vars);
						varsStr = ls.TermToStrQ(vars, len+2);
//						varsProps = Utils.prologListToPropertiesQ(ls, vars, 100000);
						if (fileName.equals("listener")) {
							fileName = null;
//							line = -1;
							port = "?-";
						}
						if (port.equalsIgnoreCase("INFO"))
							port = "     ";
						if (varsStr.indexOf("[]") < 0)
							headStr += "   " + varsStr.substring(1, varsStr.length()-1);				
					}
					// goal(Goal,File,Line,Port,I)
					if (type.equals("goal")) {
						head = ls.GetArg(frame,1);
						len = ls.StrTermLen(head);
						headStr = ls.TermToStrQ(head, len+2);
						fileName = ls.GetStrArg(frame,2);
//						line = ls.GetIntArg(frame,3);
						port = ls.GetStrArg(frame,4);
						if (fileName.equals("listener")) {
							fileName = null;
//							line = -1;
						}
						if (port.equalsIgnoreCase("INFO"))
							port = "     ";
					}
					
					str = str + headStr + "\n";
				}
			}
		}
		catch(LSException ex) {
			return null;
		}
		
		return str;
	}
*/
	public boolean p_debug_error() {
		long list;
		Properties errProps;
		String message, lineno, file;
		
		try {
//			synchronized (ls) {
				list = ls.GetParm(1);
				errProps = Utils.prologListToProperties(ls, list, 100000);
//			}
		}
		catch (LSException ex) {
			return false;		
		}
	
		message = errProps.getProperty("message");
		message += "\nPredicate: " + errProps.getProperty("predicate");
		message += "\nError Code: " + errProps.getProperty("rc");
		file = errProps.getProperty("file");
		if (file != null)
			message += "\nFile: " + file;
		lineno = errProps.getProperty("line");
		if (lineno != null)
			message += "\nLine #: " + lineno;
		
		final String fmessage = message;
		
		// Display.getCurrent() is null! so use this instead
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				MessageDialog.openError(Display.getDefault().getActiveShell(), "Error",
					fmessage);
			}
		});
		
		return true;
	}
	
	private void displayError(LSException ex) {
		String message, location;
		int rc;
		
		// Don't display anything for breaks or if we're disposing
		synchronized (ex) {
			if (ex.GetRC() == 1029) return;
			if (ex.GetRC() == 1002 && ex.GetMsg().equals("User reset")) return;
			
			if (ex.GetType() == LSException.READ) {
				message = "\n*** Logic Server Exception ***\n";
				message += ex.getMessage();
				message += "\nline: " + ex.GetLineno();
				message += "\nfile: " + ex.GetReadFileName();
				location = ex.GetReadBuffer();
				message += "\nread buffer: " + location;
				message += "\n";
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
			// amzi_system:get_debug_stack(Term) same as p_debug_stack()
			// not for aborts
/*		if (ex.GetType() != LSException.ABORT && ls != null) {
				try {
					long term = ls.ExecStr("amzi_system:get_debug_stack(_STACKTERM)");
					if (term != 0) {
						message += "Call Stack:\n" + buildStackString(ls.GetArg(term, 1));
					}
				}
				catch (LSException ex2) {			
				}
			}
*/
		}

		final String fmessage = message;
		
		// Display.getCurrent() is null! so use this instead
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				debugListenerView.appendText(fmessage);
			}
		});
	}

	private void displayError(String errText) {
		String message;
		
		message = "\n*** Error ***\n";
		message += errText;
		message += "\n";
		
		final String fmessage = message;
		
		// Display.getCurrent() is null! so use this instead
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				debugListenerView.appendText(fmessage);
			}
		});
	}
	
	private void close(boolean chdir) {
		if (listenFile != null && listenFile.exists()) {
			listenFile.delete();
			debugTarget.refreshProject(IResource.DEPTH_ONE);
		}
		// Close things down
		try {
//			synchronized (ls) {
				if (ls != null) {
					// Change directory up to workspace
					if (chdir) ls.ExecStr("chdir(`..`)");
					ls.Close();
					ls = null;
					System.gc();
				}
//			}
		} 
		catch(LSException ex) {
			displayError(ex);
		}

		// Tell them we're done if we didn't terminate
		if (!debugTarget.isTerminated())
			debugTarget.fireDebugAction(PrologDebugTarget.TERMINATE);
		debugListenerView.endListener();
	}

	protected void finalize() throws Throwable {
		close(false);
		super.finalize();
	}

}

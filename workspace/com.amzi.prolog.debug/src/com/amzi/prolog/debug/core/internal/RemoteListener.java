package com.amzi.prolog.debug.core.internal;


import amzi.ls.*;

import java.io.EOFException;
import java.io.IOException;
import java.util.Properties;
import java.util.ArrayList;
import java.util.List;

import com.amzi.prolog.debug.core.model.PrologVariable;
import com.amzi.prolog.debug.core.model.PrologValue;
import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.core.utils.Utils;
import com.amzi.prolog.debug.core.model.PrologDebugTarget;
import com.amzi.prolog.debug.core.model.PrologStackFrame;
//import com.amzi.prolog.debug.PrologDebugPlugin;
import com.amzi.prolog.debug.remote.SocketConnection;
//import com.sun.net.ssl.SSLContext;
import com.amzi.prolog.debug.ui.DebugListenerView;

//import org.eclipse.debug.core.DebugException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;

/*
 * Copyright (c) 2002-2005 Amzi! inc. All Rights Reserved.
 */
 
public class RemoteListener extends Thread {
	private LogicServer ls = null;
	private PrologDebugTarget debugTarget;
	private DebugListenerView debugListenerView;
	private DebugListenerActionBuffer action;
	private SocketConnection sock = null;
//	private String projectPathname;
	private int debugPort;
	private boolean terminating = false;

	
	public RemoteListener(PrologDebugTarget debugTarget, int debugPort,
		String projectPathname,	DebugListenerView debugListenerView, DebugListenerActionBuffer action) {		
		super("Remote Prolog Application");

		this.debugTarget = debugTarget;
		this.debugPort = debugPort;
//		this.projectPathname = projectPathname;
		this.debugListenerView = debugListenerView;
		this.action = action;
	}

	public void init() {
		if (ls != null)
			close();
			
		ls = new LogicServer();
		try {
			synchronized (ls) {
				ls.Init("");
				
				// Need aosutils for chdir
	//			ls.AddLSX("aosutils", 0);
				
				ls.AddPred("user_break", 0, "com.amzi.prolog.core.internal.RemoteListener", "p_user_break", this, LogicServer.ECLIPSE);
				ls.AddPred("user_pause", 0, "com.amzi.prolog.core.internal.RemoteListener", "p_user_pause", this, LogicServer.ECLIPSE);
				ls.AddPred("user_debug_action", 1, "com.amzi.prolog.debug.core.internal.RemoteListener", "p_debug_action", this, LogicServer.ECLIPSE);
				ls.AddPred("user_debug_stack", 5, "com.amzi.prolog.debug.core.internal.RemoteListener", "p_debug_stack", this, LogicServer.ECLIPSE);
				ls.AddPred("user_debug_error", 1, "com.amzi.prolog.debug.core.internal.RemoteListener", "p_debug_error", this, LogicServer.ECLIPSE);
	
				// Load this AFTER AddPred's
				String amziDir = PrologCorePlugin.getAmziDir();
				ls.Load(amziDir+"abin"+System.getProperty("file.separator")+"aidl.xpl");
				
				// Change directory to project path
	//			if (ls.ExecStr("chdir(`" + Utils.tiltSlashes(projectPathname) + "`)") == 0)
	//				displayError("Unable to change directory to project directory");
			}
		} 
		catch(LSException ex) {
			displayError(ex);
			close();
		}
	} 
	
	public void run() {
		String in, out;
		long term;
//		boolean caughtException = false;
		
		init();
		if (ls == null) return;

		// Connect (can be cancelled by user)
		sock = new SocketConnection(debugPort);
		try {
			if (!sock.connect()) {
				displayError("Unable to connect to remote Prolog application on port "+new Integer(debugPort).toString()
					+ ": See log for details.\nCheck that adebug.lsx is loaded in the remote LogicServer.");
				close();
				return;
			}
			debugTarget.setConnected(true);
			in = sock.readLine();
			if (in.equalsIgnoreCase("connect_01"))
				sock.writeLine("ok");
			else
				displayError("Incompatible remote Prolog application version on port "+new Integer(debugPort).toString()
					+ "\r\nRecompile and relink with latest release.");
		}
		catch (IOException ex) {
			displayError(ex);
			close();
			return;
		}

		// Increment the counter of debug runs
		try {
			synchronized (ls) {
				ls.ExecStr("amzi_system:license$action(debugger_runs, _)");
			}
		}
		catch (LSException ex) {
			displayError(ex);
			terminating = true;
//			caughtException = true;
		}

		// Command loop
		while (!terminating && !isInterrupted()) {
			try {
				in = sock.readLine();
				if (in.equals("done"))
					break;
				term = ls.ExecStr(in);
				if (in.startsWith("user_debug_action")) {
					out = ls.GetStrArg(term, 1);
					sock.writeLine(out);
				}
				else
					if (term != 0) sock.writeLine("true");
					else sock.writeLine("false");
			}
			catch(EOFException ex) {
				terminating = true;
				
				displayError("Remote Prolog application has disconnected");
				debugTarget.setConnected(false);
				
/*				try {
					debugTarget.terminate();
				}
				catch (DebugException ex2) {
					PrologDebugPlugin.log(ex2);
				}
*/				
//				caughtException = true;
				break;
			}
			catch(Exception ex) {
				displayError(ex);
//				caughtException = true;
				break;
			}
		}

		// Disconnect
/*		if (!debugTarget.isDisconnected()) {
			try {
				in = sock.readLine();
				sock.writeLine("disconnect");
			}
			catch (Exception ex) {
				if (!caughtException)
					displayError(ex);
			}
		}
*/
		close();
	}
	
	// Extended predicate: user break
	public boolean p_user_break()
	{
		if (debugListenerView.isTimeToPause() || terminating)
			return true;
		else
			return false;
	}

	// Extended predicate: user pause
	public boolean p_user_pause()
	{
		if (debugListenerView.isTimeToPause()) {
			debugListenerView.setTimeToPause(false);
			return true;
		}
		else
			return false;
	}


/*	public boolean p_output()
	{
		try {
			sock.debugOutput(ls.GetStrParm(1));
		}
		catch (LSException ex) {
			displayError(ex);
		}
		return true;
	}
*/
	
	// debug_action(Action) gets next action from user
	//   [step, jump, set_breakpoint(File,Line)]
	public boolean p_debug_action()
	{
		// Tell everyone we've stopped
		debugTarget.fireDebugAction(PrologDebugTarget.THREAD_SUSPEND_STEP_END);

		String s = action.get();		
		try {
			synchronized (ls) {
				ls.UnifyStrParm(1, s);
			}
			debugTarget.fireDebugAction(s);
			return true;
		}
		catch (LSException ex) {
			displayError(ex);
			return false;
		}
	}

	// debug_stack(Port, Goal, File, Line, Stack) reports the entire stack and its vars
	public boolean p_debug_stack()
	{
		try {
			synchronized (ls) {
/*				String port = ls.GetStrParm(1);
				int len = ls.StrTermLen(ls.GetParm(2));
				String goal = ls.TermToStrQ(ls.GetParm(2), len+2);
				String file = ls.GetStrParm(3);
				int line = ls.GetIntParm(4);
*/	
				long stack = ls.GetParm(5);
				buildStack(stack);
			}
		}
		catch (LSException ex) {
			displayError(ex);
		}
		return true;
	}

	void buildStack(long stack)
	{
		PrologVariable[] prologVars = null;
		long frame, head, vars;
		String type, headStr, varsStr, fileName, port;
		int line, cut, len;

		// Clear the stack first		
		debugTarget.getPrimaryThread().removeAllStackFrames();
		
		try
		{
			synchronized (ls) {
				while (stack != 0 && ls.GetTermType(stack) != LogicServer.pATOM)
				{
					frame = ls.GetHead(stack);
					type = ls.GetFunctor(frame);
					stack = ls.GetTail(stack);
	
					// clause(Head,File,Line,Port,Vars,I)
					if (type.equals("clause")) {
						head = ls.GetArg(frame,1);
						len = ls.StrTermLen(head);
						headStr = ls.TermToStrQ(head, len+2);
						fileName = ls.GetStrArg(frame,2);
						line = ls.GetIntArg(frame,3);
						port = ls.GetStrArg(frame,4);
						vars = ls.GetArg(frame,5);
						// arg 6 is the integer indent
						len = ls.StrTermLen(vars);
						varsStr = ls.TermToStrQ(vars, len+2);
						prologVars = buildPrologVariables(vars);
						if (fileName.equals("listener")) {
							if (stack == 0) {
								action.put(PrologDebugTarget.STEP_OVER);
							}
							fileName = null;
							line = -1;
							port = "?-";
						}
						if (port.equalsIgnoreCase("INFO"))
							port = "     ";
						if (varsStr.indexOf("[]") < 0)
							headStr += "   " + varsStr.substring(1, varsStr.length()-1);				
	
						cut = ls.GetIntArg(frame,7);
						if (cut != 0)
							headStr = "! " + headStr;
	
						debugTarget.getPrimaryThread().addStackFrame(new PrologStackFrame(debugTarget, fileName, line, port, headStr, prologVars));
					}
					// goal(Goal,File,Line,Port,I)
					if (type.equals("goal")) {
						head = ls.GetArg(frame,1);
						len = ls.StrTermLen(head);
						headStr = ls.TermToStrQ(head, len+2);
						fileName = ls.GetStrArg(frame,2);
						line = ls.GetIntArg(frame,3);
						port = ls.GetStrArg(frame,4);
						if (fileName.equals("listener")) {
							if (stack == 0) {
								action.put(PrologDebugTarget.STEP_OVER);
							}
							fileName = null;
							line = -1;
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
			}
		}
		catch(LSException ex) {
			displayError(ex);
		}
	}

	private PrologVariable[] buildPrologVariables(long pList)
	{
		PrologVariable[] prologVars = null;
		long element, name, value;
		List varList = new ArrayList();
	
		try
		{
			synchronized (ls) {
				// Check for the empty list or an atom
				long type = ls.GetTermType(pList);
				if (type != LogicServer.pLIST) return null;
		
				while (pList != 0)
				{
				  element = ls.GetHead(pList);
				  name = ls.GetArg(element, 1);
				  value = ls.GetArg(element, 2);
				  int nlen = ls.StrTermLen(name);
				  int vlen = ls.StrTermLen(value);
				  String vstr = ls.TermToStrQ(value, vlen+100);
				  String nstr = ls.TermToStr(name, nlen+100);
				  PrologValue pValue = new PrologValue(debugTarget, vstr);
				  PrologVariable pVar = new PrologVariable(debugTarget, nstr, pValue);
				  varList.add(pVar);
		
				  pList = ls.GetTail(pList);
				}
			}
		}
		catch (LSException ex) {
			displayError(ex);
			return null;
		}
			
		prologVars = new PrologVariable[varList.size()];
		varList.toArray(prologVars);
			
		return prologVars;
	}
	
	public boolean p_debug_error() {
		long list;
		Properties errProps;
		String message, lineno, file;
			
		try {
			synchronized (ls) {
				list = ls.GetParm(1);
				errProps = Utils.prologListToProperties(ls, list, 100000);
			}
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
	
	public void displayError(Exception ex) {
//		if (terminating) return;
		
		String message, location;
		
		if (ex instanceof LSException) {
			synchronized (ex) {
				LSException lex = (LSException)ex;
				if (lex.GetType() == LSException.READ) {
					message = "LogicServer Read Error\n";
					message += lex.getMessage();
					message += "\nline: " + lex.GetLineno();
					message += "\nfile: " + lex.GetReadFileName();
					location = lex.GetReadBuffer();
					message += "\nread buffer: " + location;
				}
				else {
					message = "LogicServer Exception\n";
					message += lex.getMessage();
					location = lex.GetCallStack();
				}
			}
		}
		else {
			message = ex.getClass().getName() + " Exception\n";
			message += ex.getMessage();
		}
		
		final String fmessage = message;
		
		// Display.getCurrent() is null! so use this instead
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				MessageDialog.openError(Display.getDefault().getActiveShell(), "Error",
					fmessage);
			}
		});
	}

	public void displayError(String errText) {
//		if (terminating) return;
		
		String message;
		
		message = errText;
		
		final String fmessage = message;
		
		// Display.getCurrent() is null! so use this instead
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				MessageDialog.openError(Display.getDefault().getActiveShell(), "Error",
					fmessage);
			}
		});
	}
	
	public void close() {
		if (sock != null) {
			sock.dispose();
			sock = null;
		}
		
		// Close things down
		try {
			if (ls != null) {
				synchronized (ls) {
					// Change directory up to workspace
	//				ls.ExecStr("chdir(`..`)");
					ls.Close();
					ls = null;
				}
				System.gc();
			}
		} 
		catch(LSException ex) {
			displayError(ex);
		}

		// Display.getCurrent() is null! so use this instead
//		Display.getDefault().asyncExec(new Runnable() {
//			public void run() {
//				PrologDebugTarget.closeDebugEditors();
//			}
//		});
		
		// Tell them we're done
		debugTarget.fireDebugAction(PrologDebugTarget.TERMINATE);
	}

	public void setTerminating() {
		terminating = true;
	}

	protected void finalize() throws Throwable {
		close();
		super.finalize();
	}

}

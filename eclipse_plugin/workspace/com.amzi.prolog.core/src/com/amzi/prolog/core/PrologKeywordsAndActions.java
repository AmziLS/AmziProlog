package com.amzi.prolog.core;

import amzi.ls.*;
//import com.amzi.prolog.core.utils.Utils;

import java.util.*;

import org.eclipse.jface.dialogs.MessageDialog;

/*
 * Copyright (c) 2002-2009 Amzi! inc. All Rights Reserved.
 */
public class PrologKeywordsAndActions {
	private LogicServer ls;
	
	private final static String MIN_AMZI_VERSION_STR = "7.1.0";
	private final static int MIN_AMZI_VERSION = 7;
	private final static int MIN_AMZI_SUBVERSION = 1;
//	private final static int MIN_AMZI_BUILD = 0;
	
	public final static int DIRECTIVE = 1;
	public final static int CONSTANT = 2;
	public final static int MATHFN = 3;
	public final static int PREDICATE = 4;

	private static String[] pKeywords;

	private static String[] pDirectives= { "discontiguous", "dynamic", "multifile", 
		"ensure_loaded", "op", "set_prolog_flag", "module", "end_module", "body", 
		"end_body", "import", "export", "metapredicate", "include", "sorted",
		"indexed", "nonTerminal", "noNonTerminals", "dcg_terminal" };
	
	private static String[] pConstants= { "end_of_file" };

	private static String[] pMaths= { "cpuclock", "cputime", "e", "pi", "degtorad", 
		"radtodeg", "random", "-", "+", "*", "/", "//", "**", 
		"mod", "mods", "modu", "divs", "divu", "xor",
		"min", "max", "cos", "acos", "tan", "atan", "sin", "asin",
		"abs", "ceiling", "exp", "float", "floor", "integer", "ln",
		"log", "log10", "real", "round", "sign", "sqrt", 
		"/\\", "\\", "\\/", "<<", ">>" };
	
	private static HashMap<String, String> infoMap;
//	private static int devActions;
//	private static int maintenanceDaysLeft;
//	private static int evaluationDaysLeft;
//	private static boolean evaluationExpired;
//	private static boolean evaluation;
//	private static boolean free;
//	private static boolean versionGtRenewDate;
//	private static boolean lanEdition;
//	private static boolean connectedToServer;
//	private static String productType;
//	private static String productName;
//	private static String userName;
//	private static String fingerprint;
	private String amziDir;
	
	/**
	 * Constructor for PrologKeywordsAndActions.
	 */
	public PrologKeywordsAndActions(String amziDir) {
		super();
		ls = null;
		initializeProlog(amziDir);
	}
	
	private synchronized void initializeProlog(String amziDir) {
		long term;
		
  		this.amziDir = amziDir;
  		try {
			if (ls != null) 
				ls.Close();
			//MessageDialog.openError(null, "My Info", "calling getLogicServer");
  	  		ls = getLogicServer();
			//MessageDialog.openError(null, "My Info", "called getLogicServer");
			// Check the version
			String aver = ls.GetVersion();
			String sver = aver.substring(0, aver.indexOf("."));
			int ver = Integer.valueOf(sver).intValue();
			int subver = Integer.valueOf(aver.substring(aver.indexOf(".")+1, aver.lastIndexOf("."))).intValue();
			//int buildver = Integer.valueOf(aver.substring(aver.lastIndexOf(".")+1, aver.indexOf(" "))).intValue();
			if (ver < MIN_AMZI_VERSION || (ver == MIN_AMZI_VERSION && subver < MIN_AMZI_SUBVERSION) )
				throw new Exception("Incompatible Amzi! version: " + aver + 
				". Requires version " + MIN_AMZI_VERSION_STR + "." + String.valueOf(MIN_AMZI_SUBVERSION) + " or later.");
			
			term = ls.ExecStr("findall(_F, (current_predicate(amzi_system:_F/_A),predicate_property(_F/_A, exported)), _LIST)");
     		long list = ls.GetArg(term, 3);
     		
     		// Using Utils generates a LinkageError
     		pKeywords = prologListToStringArrayNoDups(ls, list, 1000);
     		
     		// Get all the predicate info
     		infoMap = buildPredicateInfo(ls);

  		}
  		catch (LSException lsex) {
  			MessageDialog.openError(null, "Amzi! Initialization Error",	lsex.getMessage());
  			pKeywords = new String[0];
  	  			try {
  	  				ls.Close();
  	  				ls = null;
  	  				System.gc();
  	  			}
  	  			catch (LSException ex2) {
  	  				ls = null;
  	  			}  			
  		}
  		catch (Exception ex) {
  			MessageDialog.openError(null, "Amzi! Initialization Error", ex.getMessage());
  			pKeywords = new String[0];
  			try {
  				ls.Close();
  				ls = null;
  				System.gc();
  			}
  			catch (LSException ex2) {
  				ls = null;
  			}
  		}
	}
	
	private synchronized HashMap<String, String> buildPredicateInfo(LogicServer ls) throws LSException {
		HashMap<String, String> infoMap = new HashMap<String, String>(pKeywords.length);
		long term;
		String s, desc;
		
		for (int i = 0 ; i < pKeywords.length ; i++) {
			if (!infoMap.containsKey(pKeywords[i])) {
				// Concatenate the descriptions for all the arities into one string
				s = "amzi_system:predicate_info(amzi_system:(" + pKeywords[i] + ")/_A , _P, _D)";
				term = ls.CallStr(s);
				if (term != 0) {
					do {
						if (term != 0) {
							if (!infoMap.containsKey(pKeywords[i])) {
								String args = ls.GetStrArg(term, 2);
								String info = ls.GetStrArg(term, 3);
								if (args != null && args.length() > 0 &&
									info != null && info.length() > 0) {
									desc = args + "\n" + info;
									infoMap.put(pKeywords[i], desc);
								}
							}
							else {
								String args = ls.GetStrArg(term, 2);
								String info = ls.GetStrArg(term, 3);
								if (args != null && args.length() > 0 &&
									info != null && info.length() > 0) {
									desc = infoMap.get(pKeywords[i]);
									desc = desc + "\n\n" + args + "\n" + info;
									infoMap.put(pKeywords[i], desc);
								}
							}
						}
					}
					while (ls.Redo());
				}
			}
		}
		
		for (int i = 0 ; i < pDirectives.length ; i++) {
			if (!infoMap.containsKey(pDirectives[i])) {
				// Concatenate the descriptions for all the arities into one string
				s = "amzi_system:predicate_info(amzi_system:(" + pDirectives[i] + ")/_A , _P, _D)";
				term = ls.CallStr(s);
				if (term != 0) {
					do {
						if (term != 0) {
							if (!infoMap.containsKey(pDirectives[i])) {
								String args = ls.GetStrArg(term, 2);
								String info = ls.GetStrArg(term, 3);
								if (args != null && args.length() > 0 &&
									info != null && info.length() > 0) {
									desc = args + "\n" + info;
									infoMap.put(pDirectives[i], desc);
								}
							}
							else {
								String args = ls.GetStrArg(term, 2);
								String info = ls.GetStrArg(term, 3);
								if (args != null && args.length() > 0 &&
									info != null && info.length() > 0) {
									desc = infoMap.get(pDirectives[i]);
									desc = desc + "\n\n" + args + "\n" + info;
									infoMap.put(pDirectives[i], desc);
								}
							}
						}
					}
					while (ls.Redo());
				}
			}
		}

		for (int i = 0 ; i < pConstants.length ; i++) {
			if (!infoMap.containsKey(pConstants[i])) {
				// Concatenate the descriptions for all the arities into one string
				s = "amzi_system:predicate_info(amzi_system:(" + pConstants[i] + ")/_A , _P, _D)";
				term = ls.CallStr(s);
				if (term != 0) {
					do {
						if (term != 0) {
							if (!infoMap.containsKey(pConstants[i])) {
								String args = ls.GetStrArg(term, 2);
								String info = ls.GetStrArg(term, 3);
								if (args != null && args.length() > 0 &&
									info != null && info.length() > 0) {
									desc = args + "\n" + info;
									infoMap.put(pConstants[i], desc);
								}
							}
							else {
								String args = ls.GetStrArg(term, 2);
								String info = ls.GetStrArg(term, 3);
								if (args != null && args.length() > 0 &&
									info != null && info.length() > 0) {
									desc = infoMap.get(pConstants[i]);
									desc = desc + "\n\n" + args + "\n" + info;
									infoMap.put(pConstants[i], desc);
								}
							}
						}
					}
					while (ls.Redo());
				}
			}
		}

		for (int i = 0 ; i < pMaths.length ; i++) {
			if (!infoMap.containsKey(pMaths[i])) {
				// Concatenate the descriptions for all the arities into one string
				s = "amzi_system:predicate_info(amzi_system:(" + pMaths[i] + ")/_A , _P, _D)";
				term = ls.CallStr(s);
				if (term != 0) {
					do {
						if (term != 0) {
							if (!infoMap.containsKey(pMaths[i])) {
								String args = ls.GetStrArg(term, 2);
								String info = ls.GetStrArg(term, 3);
								if (args != null && args.length() > 0 &&
									info != null && info.length() > 0) {
									desc = args + "\n" + info;
									infoMap.put(pMaths[i], desc);
								}
							}
							else {
								String args = ls.GetStrArg(term, 2);
								String info = ls.GetStrArg(term, 3);
								if (args != null && args.length() > 0 &&
									info != null && info.length() > 0) {
									desc = infoMap.get(pMaths[i]);
									desc = desc + "\n\n" + args + "\n" + info;
									infoMap.put(pMaths[i], desc);
								}
							}
						}
					}
					while (ls.Redo());
				}
			}
		}

		return infoMap;
	}
	
	public String getPredicateInfo(String predicate) {
		return infoMap.get(predicate);
	}
	public String[] getKeywords() {
		return pKeywords;
	}
	public String[] getDirectives() {
		return pDirectives;
	}
	public String[] getConstants() {
		return pConstants;
	}
	public String[] getMaths() {
		return pMaths;
	}
	
	public int getKeywordType(String word) {
		int i;
		
		for (i = 0 ; i < pConstants.length ; i++)
			if (pConstants[i].equalsIgnoreCase(word))
				return CONSTANT;
		for (i = 0 ; i < pDirectives.length ; i++)
			if (pDirectives[i].equalsIgnoreCase(word))
				return DIRECTIVE;
		for (i = 0 ; i < pMaths.length ; i++)
			if (pMaths[i].equalsIgnoreCase(word))
				return MATHFN;
		
		return PREDICATE;
	}
	
	public synchronized LogicServer getLogicServer() throws LSException{
		if (ls == null) {
			ls = new LogicServer();
	     	ls.Init("");
			String slash = System.getProperty("file.separator");
			//String subdir;
			//if (System.getProperty("os.name").contains("Windows")) subdir = "bin"; else subdir = "lib";
	     	ls.AddLSX("aosutils", 0);
	     	//ls.AddLSX(amziDir + slash + subdir + slash + "aosutils.lsx", 0);
	     	//MessageDialog.openError(null, "getLogicServer", "Loading: " + amziDir + "abin" + slash + "aidl.xpl");
     		ls.Load(amziDir + "abin" + slash + "aidl.xpl");
	     	//MessageDialog.openError(null, "getLogicServer", "Loading PLM libraries 3");
			ls.ExecStr("load(date_time)");
			ls.ExecStr("load(list)");
			ls.ExecStr("load(misc)");
			try {
				ls.ExecStr("load(acgi)");
			}
			catch (LSException ex0) {
			}
			try {
				ls.ExecStr("load(aodbc)");
			}
			catch (LSException ex0) {
			}
			try {
				ls.ExecStr("load(asock)");
			}
			catch (LSException ex0) {
			}
		}
		return ls;
	}
	
	
	private synchronized String[] prologListToStringArrayNoDups(LogicServer ls, long list, int size) throws LSException
	{
		List<String> array = new ArrayList<String>();

		// Check for the empty list or an atom
		long type = ls.GetTermType(list);

		if (type != LogicServer.pLIST) return new String[0];

		while (list != 0)
		{
			long head = ls.GetHead(list);
			int len = ls.StrTermLen(head);
			String item = ls.TermToStr(head, len+2); 
			if (!array.contains(item))
				array.add(item);
			//System.out.println(ls.TermToStr(ls.GetHead(list), size));
			list = ls.GetTail(list);
		}

		String[] result= new String[array.size()];
		array.toArray(result);
		return result;
   }

	/* (non-Javadoc)
	 * @see java.lang.Object#finalize()
	 */
	protected synchronized void finalize() throws Throwable {
		if (ls != null) {
			ls.Close();
			ls = null;
		}
	}

}

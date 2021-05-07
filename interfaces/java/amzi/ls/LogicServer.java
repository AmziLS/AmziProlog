/**
    Amzi! Logic Server Java 2.0 Interface
    Copyright 1996-2007 Amzi! inc. All Rights Reserved
*/

package amzi.ls;

import java.net.URL;
import java.io.InputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;



public class LogicServer
{
	private long curEng;

   /* Prolog types */
   public static final int pERR = -1, pATOM = 0, pINT = 1, pSTR = 2, pFLOAT = 3, pSTRUCT = 4,
      pLIST = 5, pTERM = 6, pADDR = 7, pVAR = 8, pWSTR = 9, pWATOM = 10, pREAL = 11;

   /* Java types */
   public static final int jAATOM = 0, jASTR = 1, jINT = 2, jLONG = 3, jSHORT = 4,
      jFLOAT = 5, jDOUBLE = 6, jADDR = 7, jTERM = 8, jWSTR = 9, jWATOM = 10, jMOD = 11, jGOAL = 12;
   
   /* Standard streams */
   public static final int CUR_IN = 0, CUR_OUT = 1, CUR_ERR = 2,
   	USER_IN = 3, USER_OUT = 4, USER_ERR = 5;

	 /* JEnvPtr types */
	 public static final int ADD_PRED = 1, EXEC_OR_CALL = 2, CURRENT_THREAD = 3, ECLIPSE = 4;
	
	/* Main entry points to set up Prolog environment */

	public native void Init(String ININame) throws LSException;
	public native void Init2(String INIParms) throws LSException;
	public native void InitLSX(long Arg) throws LSException;
	public native void AddLSX(String LSXName, long Arg) throws LSException;
	public void AddJPred(String PredName, int Arity,
		String Class, String Method, Object Obj, long eng) throws LSException
	{ AddJPred(PredName, Arity, Class, Method, Obj, ADD_PRED, curEng); }
	public native void AddJPred(String PredName, int Arity,
		String Class, String Method, Object Obj, int JEnvType, long eng) throws LSException;
	public void AddPred(String PredName, int Arity,
		String Class, String Method, Object Obj) throws LSException
	{ AddJPred(PredName, Arity, Class, Method, Obj, curEng); }
	public void AddPred(String PredName, int Arity,
		String Class, String Method, Object Obj, int JEnvType) throws LSException
	{ AddJPred(PredName, Arity, Class, Method, Obj, JEnvType, curEng); }
	public native void Load(String XPLName) throws LSException;
	public native void LoadFromMemory(String XPLName, int Length, byte[] Code) throws LSException;
	public native boolean Main() throws LSException;
	public native void Reset() throws LSException;
	public native void JClose(long eng) throws LSException;
	public void Close() throws LSException
	{ JClose(curEng);	}
	
	// Defining User Streams
	
	public native int OpenJStream(int inout, String alias, String clazz,
		String method, Object instance, long eng);
	public int OpenInputStream(String alias, String clazz,
		String method, Object instance)
	{ return OpenJStream(0, alias, clazz, method, instance, curEng); }
	public int OpenOutputStream(String alias, String clazz,
		String method, Object instance)
	{ return OpenJStream(1, alias, clazz, method, instance, curEng); }
	
	public native void CloseJStream(int stream, long eng);
	public void CloseStream(int stream)
	{ CloseJStream(stream, curEng); }
	
	public native void SetStream(int std_stream, int stream);

	/* Calling Prolog from Java */

	public native long Exec(long Term) throws LSException;
	public native long ExecStr(String Query) throws LSException;
   public native long Call(long Term) throws LSException;
   public native long CallStr(String Query) throws LSException;
   public native boolean Redo() throws LSException;
	public native void ClearCall() throws LSException;

	/* Asserting and retracting */

	public native void Asserta(long Term) throws LSException;
	public native void Assertz(long Term) throws LSException;
	public native long Retract(long Term) throws LSException;
	public native void AssertaStr(String TermStr) throws LSException;
	public native void AssertzStr(String TermStr) throws LSException;
	public native boolean RetractStr(String TermStr) throws LSException;

	/* String/Term conversion functions */

	public native String TermToStr(long Term, int Len) throws LSException;
	public native String TermToStrQ(long Term, int Len) throws LSException;
   public native long StrToTerm(String TermStr) throws LSException;
   public native int  StrTermLen(long Term) throws LSException;

	/* Making Prolog types */

	public native long MakeAtom(String AtomStr) throws LSException;
	public native long MakeStr(String Str) throws LSException;
	public native long MakeInt(int Num) throws LSException;
	public native long MakeFloat(double Num) throws LSException;

	/* Getting Java values from Prolog terms */

	public native int GetTermType(long Term) throws LSException;
	public native String GetStrTerm(long Term) throws LSException;
	public native int GetIntTerm(long Term) throws LSException;
	public native double GetFloatTerm(long Term) throws LSException;

   /* Get Parameters for Extended Predicates */

   public native int GetParmType(int iarg) throws LSException;
   public native long GetParm(int iarg) throws LSException;
   public native String GetStrParm(int iarg) throws LSException;
   public native int GetIntParm(int iarg) throws LSException;
   public native double GetFloatParm(int iarg) throws LSException;
   public native boolean UnifyParm(int iarg, long Term) throws LSException;
   public native boolean UnifyStrParm(int iarg, String s) throws LSException;
   public native boolean UnifyAtomParm(int iarg, String s) throws LSException;
   public native boolean UnifyIntParm(int iarg, int i) throws LSException;
   public native boolean UnifyFloatParm(int iarg, double f) throws LSException;

	/* Structure hacking functions */

	public native String GetFunctor(long Term) throws LSException;
	public native int GetArity(long Term) throws LSException;
	public native long MakeFA(String Functor, int Arity) throws LSException;
	public native long GetArg(long Term, int Num) throws LSException;
	public native String GetStrArg(long Term, int Num) throws LSException;
	public native int GetIntArg(long Term, int Num) throws LSException;
	public native double GetFloatArg(long Term, int Num) throws LSException;
	public native long UnifyAtomArg(long Term, int Num, String Str) throws LSException;
	public native long UnifyStrArg(long Term, int Num, String Str) throws LSException;
	public native long UnifyIntArg(long Term, int Num, int Val) throws LSException;
	public native long UnifyFloatArg(long Term, int Num, double Val) throws LSException;
	public native int GetArgType(long Term, int Num) throws LSException;
	public native int StrArgLen(long Term, int Num) throws LSException;

	/* List hacking functions */

	public native long MakeList() throws LSException;
	public native long PushList(long ListTerm, long Term) throws LSException;
	public native long GetHead(long ListTerm) throws LSException;
	public native String GetStrHead(long ListTerm) throws LSException;
	public native int GetIntHead(long ListTerm) throws LSException;
	public native double GetFloatHead(long ListTerm) throws LSException;
	public native long GetTail(long ListTerm) throws LSException;

	/* Miscellaneous functions */

	public native String GetVersion() throws LSException;

	/* Java Specific LSAPI Extensions */
	
	public void LoadFromURL(String XPLName, URL url) throws LSException, IOException {
		InputStream stream = url.openStream();
		ByteArrayOutputStream oStream = new ByteArrayOutputStream();
		int c;
		while ((c = stream.read()) != -1)
			oStream.write(c);
		stream.close();
		LoadFromMemory(XPLName, oStream.size() , oStream.toByteArray() );
	}
		
	/* Linker functions. Amzi! must be installed. */
	
	public native int Link(String xplFile, String[] plmFiles, String options);
	public native String getLinkMessage();
	public native String getEnvironmentVariable(String envvar);

	static
	{
	   try
           {
           	// note the file must be called libamzijni.jnilib on the Mac (and other unix?)
           	// but is amzijni.dll in the bin directory on Windows
			//String userdir = System.getProperty("user.dir");
			//System.out.println(userdir);
			//String slash = System.getProperty("file.separator");
			//String amziDir = userdir + slash + "amzi" + slash;
			//String jnipath = amziDir + "bin" + slash + "amzijni.dll";
			//System.load(jnipath);
							//System.load("/Users/dennis/Documents/amzi/release/mac/apls/lib/libamzijni.so");
              //System.err.println("Loading amzijni");
              System.loadLibrary("amzijni");
           }
           catch (UnsatisfiedLinkError ex2)
           {
              System.err.println("Unable to load (lib)amzijni library: " + ex2.getMessage());
           }
	}
}

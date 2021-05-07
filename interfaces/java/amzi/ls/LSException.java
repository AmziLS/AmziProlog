/*
    LSException.java -- The Amzi! Logic Server Exception Class
       for use with JDK 1.1

    Copyright (c) 1996-2001 Amzi! inc.  All Rights Reserved.
*/

package amzi.ls;

public class LSException extends Exception
{
    private long curEng;

	// Error Types
	public static final int BADENG = 0;
	public static final int ABORT = 1;
	public static final int INTERNAL = 2;
	public static final int FATAL = 3;
	public static final int INIT = 4;
	public static final int API = 5;
	public static final int LOAD = 6;
	public static final int EXEC = 7;
	public static final int READ = 8;
	public static final int UNKNOWN = 9;

    public LSException(String msg)
    {
        super(msg);
    }
    public native int GetType();
    public native int GetRC();
    public native int GetLineno();
    public native String GetMsg();
    public native String GetReadFileName();
    public native String GetReadBuffer();
    public native String GetCallStack();

    public String getMessage()
    {
    	return GetMsg();
    }
    public String toString()
    {
		if (GetType() != READ)
			return "Error #" + new Integer(GetRC()).toString() + ": " + GetMsg(); // + " while calling " + GetCallStack();
		else
			return "Read Error #" + new Integer(GetRC()).toString() + ": " + GetMsg() +
				" at line " + new Integer(GetLineno()).toString() + " in file " + GetReadFileName() +
				" near " + GetReadBuffer(); // + " while calling " + GetCallStack();
    }
}

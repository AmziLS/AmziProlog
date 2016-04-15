package com.amzi.prolog.core.utils;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

/**
* A simple, static class to display a URL in the system browser.
*
* Under Unix, the system browser is hard-coded to be 'netscape'.
* Netscape must be in your PATH for this to work.  This has been
* tested with the following platforms: AIX, HP-UX and Solaris.
*
* Under Windows, this will bring up the default browser under windows,
* usually either Netscape or Microsoft IE.  The default browser is
* determined by the OS.  This has been tested under Windows 95/98/NT.
*
* Examples:
* BrowserControl.displayURL("http://www.javaworld.com")
* BrowserControl.displayURL("file://c:\\docs\\index.html")
* BrowserContorl.displayURL("file:///user/joe/index.html");
* 
* NOTE!!! Microsoft does not allow a filename, e.g. www.amzi.com/foo.htm
* 
* Note - you must include the url type -- either "http://" or
* "file://".
*/
public class BrowserControl
{
   // Used to identify the windows platform.
   private static final String WIN_ID = "Windows";
   // The default system browser under windows.
   private static final String WIN_PATH = "rundll32";
   // The flag to display a url.
   private static final String WIN_FLAG = "url.dll,FileProtocolHandler";
   // The default browser under unix.
   private static final String UNIX_PATH = "netscape";
   // The flag to display a url.
   private static final String UNIX_FLAG = "-remote openURL";

   /**
     * Display a file in the system browser.  If you want to display a
     * file, you must include the absolute path name.
     *
     * @param url the file's url (the url must start with either "http://" or
     * "file://").
     */
    public static void displayURL(String url)
    {
        boolean windows = isWindowsPlatform();
        String cmd = null;
        try
        {
            if (windows)
            {
                // cmd = 'rundll32 url.dll,FileProtocolHandler http://...'
                cmd = WIN_PATH + " " + WIN_FLAG + " " + url;
                Runtime.getRuntime().exec(cmd);
            }
            else
            {
                // Under Unix, Netscape has to be running for the "-remote"
                // command to work.  So, we try sending the command and
                // check for an exit value.  If the exit command is 0,
                // it worked, otherwise we need to start the browser.
                // cmd = 'netscape -remote openURL(http://www.javaworld.com)'
                cmd = UNIX_PATH + " " + UNIX_FLAG + "(" + url + ")";
                Process p = Runtime.getRuntime().exec(cmd);
                try
                {
                    // wait for exit code -- if it's 0, command might have worked,
                    // if there's something in the error stream, it did not work,
                    // otherwise we need to start the browser up.
                    int exitCode = p.waitFor();
                    
					BufferedReader in = 
						new BufferedReader( new InputStreamReader( p.getInputStream() ) );
					BufferedReader err = 
						new BufferedReader   ( new InputStreamReader( p.getErrorStream() ) );
//					String line = null;
//					while( (line = in.readLine()) != null ) {  
					while( (in.readLine()) != null ) {  
						//System.out.println( line );
						exitCode = -1;;
					}
//					while( (line = err.readLine()) != null ) {  
					while( (err.readLine()) != null ) {  
						//System.out.println( line );
						exitCode = -1;
					}
                    
                    if (exitCode != 0)
                    {
                        // Command failed, start up the browser
                        // cmd = 'netscape http://www.javaworld.com'
                        cmd = UNIX_PATH + " "  + url;
                        p = Runtime.getRuntime().exec(cmd);
                    }
                }
                catch(InterruptedException x)
                {
//                    System.err.println("Error bringing up browser, cmd='" +
//                                       cmd + "'");
//                    System.err.println("Caught: " + x);
                }
            }
        }
        catch(IOException x)
        {
            // couldn't exec browser
//            System.err.println("Could not invoke browser, command=" + cmd);
//            System.err.println("Caught: " + x);
        }
    }
    /**
     * Try to determine whether this application is running under Windows
     * or some other platform by examing the "os.name" property.
     *
     * @return true if this application is running under a Windows OS
     */
    public static boolean isWindowsPlatform()
    {
        String os = System.getProperty("os.name");
        if ( os != null && os.startsWith(WIN_ID))
            return true;
        else
            return false;

    }
}

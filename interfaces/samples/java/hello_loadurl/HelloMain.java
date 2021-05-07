/*
 Hello Load URL
 Copyright (c) 2004 Amzi! inc. All Rights Reserved.
*/

import amzi.ls.*;
import java.net.URL;
import java.io.IOException;

public class HelloMain {
  public static void main(String[] args) {
    long term;
    String result;

    // Load the XPL file from our Executable JAR that contains this class
    // and HelloLoadURL
    HelloLoadURL hello = new HelloLoadURL();
    URL url = hello.GetURL("hello.xpl");

    // Alternative method: Load the XPL file into a byte array
    // and call LoadFromMemory
//    byte xplfile[] = hello.LoadXPL();

    LogicServer ls = new LogicServer();
    try {
      ls.Init("");

      // Alternative method: Load the XPL file into a byte array
      // and call LoadFromMemory
//      ls.LoadFromMemory("hello.xpl", xplfile.length, xplfile);

      ls.LoadFromURL("hello.xpl", url);

      // Call the XPL file
      term = ls.ExecStr("hello($Java Programmer$, Response)");
      if (term == 0) {
        System.out.println("Hello Failed");
      }
      else {
        result = ls.GetStrArg(term, 2);
        System.out.println(result);
      }
      ls.Close();
    }
    catch (LSException ex) {
      System.out.println(ex.getMessage());
    }
    catch (IOException ioex) {
      System.out.println(ioex.getMessage());
    }

  }
}

/*
 Hello Load URL
 Copyright (c) 2004 Amzi! inc. All Rights Reserved.
*/

import java.net.URL;
import java.io.InputStream;
import java.io.IOException;

public class HelloLoadURL {

  //  LoadXPL reads in the XPL file from the JAR file into a byte array
  public byte[] LoadXPL() {
    byte code[] = null;

    URL url = getClass().getClassLoader().getResource("hello.xpl");
    InputStream stream = null;
    try {
      stream = url.openStream();
      int len = stream.available();
      code = new byte[len];
      int offset = 0;
      int count = 0;
      while (offset != -1) {
        count = stream.read(code, offset, len);
        offset = offset + count;
        len = len - count;
      }
      stream.close();
    }
    catch (IOException ex) {
    }
    return code;
  }

  // GetURL returns the URL to the specified resource in the JAR file
  // This is in a separate class because you cannot call getClass()
  // from the static main() method
  public URL GetURL(String filename) {
    return getClass().getClassLoader().getResource(filename);
  }
}

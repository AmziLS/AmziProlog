package com.amzi.prolog.debug.remote;

import com.amzi.prolog.core.PrologCorePlugin;
import com.amzi.prolog.debug.PrologDebugPlugin;
import com.amzi.prolog.debug.ui.RemoteConnectWaiter;
import com.amzi.prolog.core.dialogs.UpgradeDialog;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.net.*;

import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Display;

/*
When you create a TCP socket you need to set the option TCP_NODELAY.  Otherwise, an
unanswered write to TCP socket waits for 50 to 200 ms before you can write again.
Solaris uses the minimum delay of 50 ms, and IRIX uses the maximum of 200 ms. See Stevens
"Unix Network Programming" volume 1, 2nd ed., page 203.

This delay is so a TCP acknowledgment of a write can be bundled with a response from the
other direction.  Historically this behavior was justified for clients like telnet and
rlogin, to which a server echos every character.  The client gets the
acknowledgment of the write bundled with the echoed character, instead of two separate
responses.

In C or C++, the fix looks like

<pre>
  #include <netinet/tcp.h>
  int socketFileDescriptor;
  int m = 1;
  socketFileDescriptor = socket(AF_INET,...);
  if(-1 == setsockopt(socketFileDescriptor ,
     IPPROTO_TCP,TCP_NODELAY,(char*)&m,sizeof(int))) {
           // print and ignore error 
  }
</pre>
In Java, the fix is

<pre>
   Socket socket;
   socket.setTcpNoDelay(true);
</pre>

Both sides of a socket connection must set this option.
*/

public class SocketConnection {
	public DataInputStream dis = null;
	public DataOutputStream dos = null; 
	protected Socket socket = null;
	protected ServerSocket ss = null;
  	private int portNumber;
  	private static int BUFFER_SIZE = 100000;	// Set same value in debug.lsx
  	
	public SocketConnection(int portNumber) {
    	this.portNumber = portNumber;
	}
	
	public boolean connect() throws IOException {
		// Get a server socket in ss
  		ss = new ServerSocket(portNumber, 1);
//		InetAddress inetAddr = InetAddress.getByName(hostName);
//		InetSocketAddress inetSocketAddr = new InetSocketAddress(inetAddr, portNumber);
// 		ss.bind(inetSocketAddr);

		// Following moved to RemoteConnectWaiter
//		ss.setSoTimeout(100000);
//		socket = ss.accept();

		// Connect to the client with a progress bar and cancel button
		// Display.getCurrent() is null! so use this instead
		final SocketConnection fthis = this;
		final ServerSocket fss = ss;
		Display.getDefault().syncExec(new Runnable() {
			public void run() {
				try {
				   	IRunnableWithProgress op = new RemoteConnectWaiter(fthis, fss);
				   	new ProgressMonitorDialog(Display.getDefault().getActiveShell()).run(true, true, op);
				} 
				catch (InterruptedException ex) {
					// User pressed cancel
				}
				catch (InvocationTargetException ex) {
					PrologDebugPlugin.log(ex.getTargetException());
				} 		
			}
		});
		
		// See if this is allowed
		if (socket != null) {
			InetAddress remote = socket.getInetAddress();
			InetAddress local = InetAddress.getByName("127.0.0.1");
			
			if (!remote.equals(local) && !PrologCorePlugin.actionAllowed(PrologCorePlugin.DEV_REMOTE_DEBUG)) {
				Display.getDefault().asyncExec(new Runnable(){
					public void run() {
						UpgradeDialog upgradeDialog = new UpgradeDialog(Display.getDefault().getActiveShell());
						upgradeDialog.open();
					}
				});

				try {
					socket.close();
					socket = null;
					ss.close();
					ss = null;
				}
				catch (IOException ex) {
					PrologDebugPlugin.log(ex);
				}
				return false;
			}
		}
			
		// If we connected finish initialization
		if (socket != null) {
			// We can wait forever now
			ss.setSoTimeout(0);

			// And no delays in transmission
			socket.setTcpNoDelay(true);

			// Experiments lead to no discernable changes
//			socket.setKeepAlive(true);
//			socket.setSoTimeout(0);
//			socket.setTrafficClass(0x04);	// IPTOS_RELIABILITY
			
			// Open streams
			dis = new DataInputStream(new BufferedInputStream(socket.getInputStream(), BUFFER_SIZE));
			dos = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream(), BUFFER_SIZE));
		}
		else {
			try {
				ss.close();
				ss = null;
			}
			catch (IOException ex) {
				PrologDebugPlugin.log(ex);
			}
			return false;
		}
		
		return true;
	}

	public void setSocket(Socket socket) {
		this.socket = socket;
	}
	
	public void debugOutput(String s) {
		System.out.println(s); 
		System.out.flush();
	}

	public void dispose() {
		try {if (ss != null) ss.close();} catch (IOException e){}
		try {if (dis != null) dis.close();} catch (IOException e){}
		try {if (dos != null) dos.flush();} catch (IOException e){}
		try {if (dos != null) dos.close();} catch (IOException e){}
		try {if (socket != null) socket.close();} catch (IOException e){}
		ss = null; dis = null; dos = null; socket = null;
	}
	
	public String readLine() throws java.io.IOException {
		char b[] = new char[100000];
		int i = 0;
		do {
			b[i] = dis.readChar();
//			char c = b[i];
//			int ci = (int)c;
			i++;
		} while (b[i-1] != '\n' && i < 100000);
		String line = new String(b);
		return line.substring(0, i-1);
	}
	
	public void writeLine(String s) throws java.io.IOException {
		dos.writeChars(s);
		dos.writeChar('\n');
		dos.flush();
	}

/*	
  public String readString() throws java.io.IOException {
      int stringLength = dis.readInt();
      byte b[] = new byte[stringLength];
      dis.read(b);
      String s = new String(b);
      return s;
  }
  
  public void writeString(String s) throws java.io.IOException {
      dos.writeInt(s.length());
      dos.writeBytes(s);
      dos.flush();
  }
  
  public void writeFloat(float f) throws java.io.IOException {
    dos.writeFloat(f); dos.flush();
  }
  
  public void writeInt(int i) throws java.io.IOException {
    dos.writeInt(i); dos.flush();
  }

  public float readFloat() throws java.io.IOException {return dis.readFloat();}

  public int readInt() throws java.io.IOException {return dis.readInt();}
*/
/*  public static void main (String [] args) {
    SocketConnection ss = new SocketConnection(args[0]);
    // ready for data
    try{
      float fo = (float) 201.5; 
      int io = 209;
      ss.writeFloat(fo);
      ss.writeInt(io);
      ss.writeString("string from java");

      float fi = (float) 0.; int ii = 0;
      fi = ss.readFloat();
      ii = ss.readInt();
      String s = ss.readString();
      comment("java read 101.5=" + fi + " 109=" + ii);
      comment("java read = " + s );
    } catch (IOException e) {System.err.println(e);}
    ss.dispose();
  }
*/
}

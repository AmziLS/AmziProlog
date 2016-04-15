package com.amzi.prolog.debug.ui;

import com.amzi.prolog.debug.remote.SocketConnection;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;

public class RemoteConnectWaiter implements IRunnableWithProgress {
	private SocketConnection socketConnection;
	private ServerSocket serverSocket;
	private Socket socket = null;
	
	public RemoteConnectWaiter(SocketConnection socketConnection, ServerSocket serverSocket) {
		super();
		this.socketConnection = socketConnection;
		this.serverSocket = serverSocket;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.operation.IRunnableWithProgress#run(org.eclipse.core.runtime.IProgressMonitor)
	 */
	public void run(IProgressMonitor monitor)
		throws InvocationTargetException, InterruptedException {
			monitor.beginTask("Waiting for remote Prolog application. Start it now.", IProgressMonitor.UNKNOWN);
			try {
				// Wait a second at a time
				serverSocket.setSoTimeout(1000);
				while (!monitor.isCanceled() && socket == null)
					try {
						socket = serverSocket.accept();
					}
					catch (SocketTimeoutException ex) {
						// Do nothing we will try again, serverSocket is valid
					}
					
				// If cancelled must throw InterruptedException
				if (monitor.isCanceled())
					throw new InterruptedException();
			}
			catch (IOException ex) {
				throw new InvocationTargetException(ex);
			}
			
			// Save the socket back in the caller's object
			socketConnection.setSocket(socket);
			
			monitor.done();
	}

}

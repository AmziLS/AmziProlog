/*
   This client connects to a server running sserver.pro and sends it a string,
   then gets a string and closes the socket.

   To run, change the internet address used in inet_addr to the address of the
   computer running sserver.pro (both programs can run on the same machine).
   You must have TCP/IP running. If you are using a standalone machine, you 
   may have to dial-up to Internet to load your TCP/IP stack.

   Note that the wsaX predicates are not necessary for Unix, and simply
   return true in that environment.
*/

% NOTE!!! Uncomment the appropriate sdefine for your environment
% Linux & Windows
sdefine('SOCK_STREAM', 1).
% Solaris
%sdefine('SOCK_STREAM', 2).

main :-
   wsaStartup(0x0101, Ver, HighVer, Desc, Status, MaxSockets, MaxUdpDg),
   inet_addr($127.0.0.1$, NetAddr),
   sdefine('SOCK_STREAM', SOCK_STREAM),
   socket(SOCK_STREAM, 0, Sock),
   connect(Sock, NetAddr, 4001),
   write($Send what? $), read_string(Str),
   send(Sock, Str, 0, NumSent),
   recv(Sock, Buf, 4096, 0, NumRecv),
   write(Buf),
   closesocket(Sock),
   wsaCleanup.

/*
*   finger.pro - a simple finger client
*/

% Note that the wsaX predicates are not necessary under Unix,
% and simply return true.

% NOTE!!! Uncomment the appropriate sdefine for your environment
% Linux & Windows
sdefine('SOCK_STREAM', 1).
% Solaris
%sdefine('SOCK_STREAM', 2).

main :-
   wsaStartup(0x0101, Ver, HighVer, Desc, Status, MaxSockets, MaxUdpDg),
   write($Host name to finger? $), read_string(HostName),
   gethostbyname(HostName, AliasList, [NetAddr | AddrList]),
   inet_ntoa(NetAddr, DotAddr),
   write(DotAddr), nl,
   sdefine('SOCK_STREAM', SOCK_STREAM),
   socket(SOCK_STREAM, 0, Sock),
   connect(Sock, NetAddr, 79),
   write($Sock=$), write(Sock), nl,
   write($Login id name? $), read_string(LoginName),
   strcat(LoginName, $\x0d\x0a$, SendBuf),
   write($Sending request$), nl,
   send(Sock, SendBuf, 0, NumSent),
   write($Waiting for response$), nl,
   recv(Sock, Buf, 4096, 0, NumRecv),
   write(Buf), nl,
   closesocket(Sock),
   wsaCleanup.

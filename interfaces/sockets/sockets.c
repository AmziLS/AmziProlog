/*
*   sockets.c -- Extended Predicates for the Sockets APIL
*
*   Copyright (c) 1996 Amzi! inc.  All Rights Reserved.
*
* $Log: sockets.c,v $
* Revision 1.1.1.1  2003/09/11 02:15:12  dennis
* Starting release 7.0
*
* Revision 1.2  2002/12/06 18:33:21  dennis
* updates for running on HPUX
*
* Revision 1.1.1.1  2000/12/29 02:18:07  dennis
* moved to a6
*
* Revision 1.7  2000/08/19 05:43:17  mary
* Removed C from InitPreds
*
* Revision 1.6  2000/06/06 23:31:11  mary
* solaris, current sockets
*
* Revision 1.5  2000/06/04 02:19:59  mary
* Re-converted to Linux
*
* Revision 1.4  2000/06/02 21:55:28  mary
* Updated for Solaris
*
* Revision 1.3  2000/01/27 10:00:51  dennis
* Put perror() check in bind, need to add more general
* purpose error handling someday, throwing strerror() strings.
*
* Revision 1.2  2000/01/26 23:38:16  dennis
* Ported sockets to Linux. Note that the the p_wsaX routines simply
* return true for Linux. Also, the function p_ioctlsocket just returns
* true. There might be an Unix equivalent, but I don't know it as
* I put in this update.
*
*
*
*/

/*
   Differences from the Winsock API:
      getsockopt return 0 if linger is disabled and the amount of time to wait if enabled
	  setsockopt disables linger if the value is 0 and enables it for non-zero using the
		value as the amount of time to wait
	  sa_family in sockaddr is always assumed to be AF_INET
	  the type parameter for get__by__ and socket are always PF_INET

   socket.pro contains the 'defines' to make these calls

   Network addresses are passed always in network byte order
   Protocol and port numbers are passed always in host byte order
   This means you do not have to do any conversions on these values if you pass exactly
   what you receive from one function to another.

   This implementation assumes ints are 32-bits

   This implementation does not include the Windows async calls (not needed from Prolog)

   MSVC will issue warning C4133 for each use of sockaddr_in
     '=' : incompatible types - from 'struct sockaddr_in *' to 'struct sockaddr *'
*/

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "amzi.h"

#if defined(MSWIN)    // from amzi.h
#include <winsock.h>
#define S_ADDR S_un.S_addr   // different sockaddr_in on Windows

#elif defined(UNIX)
#include <sys/socket.h>
#include <netinet/in.h>  // sockaddr_in and friends
#include <netdb.h>       // gethostbyname and friends
#include <arpa/inet.h>   // inet_ntoa and friends
#include <sys/time.h>    // timeval
#include <unistd.h>      // gethostname
//#include <bits/types.h>  // __fd_set ??? shouldn't be needed...
#if !defined(HPUX)
#include <sys/select.h>  // fd_set and friends
#endif
typedef int SOCKET;
typedef long _int32;
#define SOCKET_ERROR (-1)
#define INVALID_SOCKET (-1)
#define S_ADDR s_addr
#define BOOL int
#endif
#ifdef SOLARIS
#define INADDR_NONE 0xffffffff
#endif

/* built-in predicates, callable from Prolog */
/* ----------------------------------------- */

/* function prototypes */

/* SDK functions */
TF EXPFUNC p_wsaStartup(ENGid);
TF EXPFUNC p_wsaCleanup(ENGid);
TF EXPFUNC p_wsaGetLastError(ENGid);
TF EXPFUNC p_ioctlsocket(ENGid);

TF EXPFUNC p_accept(ENGid);
TF EXPFUNC p_bind(ENGid);
TF EXPFUNC p_closesocket(ENGid);
TF EXPFUNC p_connect(ENGid);
TF EXPFUNC p_gethostbyaddr(ENGid);
TF EXPFUNC p_gethostbyname(ENGid);
TF EXPFUNC p_gethostname(ENGid);
TF EXPFUNC p_getpeername(ENGid);
TF EXPFUNC p_getprotobyname(ENGid);
TF EXPFUNC p_getprotobynumber(ENGid);
TF EXPFUNC p_getservbyname(ENGid);
TF EXPFUNC p_getservbyport(ENGid);
TF EXPFUNC p_getsockopt(ENGid);
TF EXPFUNC p_getsockname(ENGid);
TF EXPFUNC p_inet_addr(ENGid);
TF EXPFUNC p_inet_ntoa(ENGid);
TF EXPFUNC p_listen(ENGid);
TF EXPFUNC p_recv(ENGid);
TF EXPFUNC p_recvfrom(ENGid);
TF EXPFUNC p_select(ENGid);
TF EXPFUNC p_send(ENGid);
TF EXPFUNC p_sendto(ENGid);
TF EXPFUNC p_setsockopt(ENGid);
TF EXPFUNC p_shutdown(ENGid);
TF EXPFUNC p_socket(ENGid);


/* extended predicate table definitions */

PRED_INIT aSocketsPreds[] = 
{
	/* SDK functions */
	{"wsaCleanup", 0, p_wsaCleanup},
	{"wsaGetLastError", 1, p_wsaGetLastError},
	{"wsaStartup", 7, p_wsaStartup},
	{"ioctlsocket", 4, p_ioctlsocket},

	{"accept", 4, p_accept},
	{"bind", 3, p_bind},
	{"closesocket", 1, p_closesocket},
	{"connect", 3, p_connect},
	{"gethostbyaddr", 3, p_gethostbyaddr},
	{"gethostbyname", 3, p_gethostbyname},
	{"gethostname", 1, p_gethostname},
	{"getpeername", 3, p_getpeername},
	{"getprotobyname", 3, p_getprotobyname},
	{"getprotobynumber", 3, p_getprotobynumber},
	{"getservbyname", 4, p_getservbyname},
	{"getservbyport", 4, p_getservbyport},
	{"getsockopt", 4, p_getsockopt},
	{"getsockname", 3, p_getsockname},
	{"inet_addr", 2, p_inet_addr},
	{"inet_ntoa", 2, p_inet_ntoa},
	{"listen", 2, p_listen},
	{"recv", 5, p_recv},
	{"recvfrom", 7, p_recvfrom},
	{"select", 8, p_select},
	{"send", 4, p_send},
	{"sendto", 6, p_sendto},
	{"setsockopt", 4, p_setsockopt},
	{"shutdown", 2, p_shutdown},
	{"socket", 3, p_socket},

	{NULL, 0, NULL}
};


/* Global Variables */
/* ---------------- */

#define	MAX_HOSTNAME	1024
#define	MAX_PROTONAME	64
#define MAX_SERVNAME	64


/* Initialization Functions */
/* ------------------------ */

#if defined(MSWIN)

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD dwReason, LPVOID lpRes)
{
	switch(dwReason)
	{
		case DLL_PROCESS_ATTACH:
		case DLL_THREAD_ATTACH:
		case DLL_THREAD_DETACH:
		case DLL_PROCESS_DETACH:
			break;
	}
	return TRUE;
}

__declspec(dllexport) RC EXPFUNC InitPreds(ENGid eid, void* p)
 {
	RC rc;
	char buf[80];

	rc = lsInitPreds(eid, aSocketsPreds);
	if (rc)
	{
#ifdef	_WIN32
		wsprintf(buf, "Error #%d Loading Sockets Predicates", rc);
		MessageBox(NULL, buf, "CGI LSX", MB_OK);
#else
		printf("Error #%d Loading CGI Predicates", rc);
#endif
	}

	return 0;
}

#elif defined(UNIX) // from amzi.h
extern RC EXPFUNC InitPreds(ENGid eid, void* p)
{
	RC rc;
	char buf[80];

	rc = lsInitPreds(eid, aSocketsPreds);
	if (rc)
		printf("Error #%d loading sockets predicates", rc);
//	else
//		printf("sockets predicates loaded");
	return 0;
}
#endif

#if defined(MSWIN)
/* Windows Sockets API Routines */

/*
	wsaStartup(VersionReqI, VersionV, HighVersionV, DescriptionStrV, StatusStrV,
		MaxSocketsV, MaxUdpDgV)
	*/
TF EXPFUNC p_wsaStartup(ENGid eid)
{
	WSADATA	sockdata;
	WORD	verReq;
	RC		rc, ecode;
	TF		tf;
	int		MaxSockets, MaxUdpDg;

	rc = lsGetParm(eid, 1, cINT, &verReq);
	if (rc != 0) lsErrRaise(eid, "invalid VersionRequested");

	ecode = WSAStartup(verReq, &sockdata);
	if (ecode != 0) return(FALSE);

	rc = lsUnifyParm(eid, 2, cINT, &sockdata.wVersion);
	rc = lsUnifyParm(eid, 3, cINT, &sockdata.wHighVersion);
	rc = lsUnifyParm(eid, 4, cSTR, &sockdata.szDescription);
	rc = lsUnifyParm(eid, 5, cSTR, &sockdata.szSystemStatus);
	MaxSockets = (int)sockdata.iMaxSockets;
	MaxUdpDg = (int)sockdata.iMaxUdpDg;
	tf = lsUnifyParm(eid, 6, cINT, &MaxSockets);
	tf = lsUnifyParm(eid, 7, cINT, &MaxUdpDg);
	return(TRUE);
}

/*
	wsaCleanup
*/
TF EXPFUNC p_wsaCleanup(ENGid eid)
{
	int		ecode;

	ecode = WSACleanup();
	if (ecode != 0) return(FALSE);
	return(TRUE);
}

/*
	wsaGetLastError(ErrorNumV)
*/
TF EXPFUNC p_wsaGetLastError(ENGid eid)
{
	int		ecode;
	TF		tf;

	ecode = WSAGetLastError();
	tf = lsUnifyParm(eid, 1, cINT, &ecode);
	if (tf != TRUE) return(FALSE);
	return(TRUE);
}
#elif defined(UNIX)  // end MSWIN
TF EXPFUNC p_wsaStartup(ENGid eid) {return TRUE;}
TF EXPFUNC p_wsaCleanup(ENGid eid) {return TRUE;}
TF EXPFUNC p_wsaGetLastError(ENGid eid) {return TRUE;}
#endif

/* Sockets API Routines */

/*
	accept(SocketI, RemoteHostAddrNumV, RemoteHostPortNumV,NewSocketNumV)
*/
TF EXPFUNC p_accept(ENGid eid)
{
	SOCKET	sock, newsock;
	_int32	rhostaddr;
	int		rhostport, sockdesclen;
	struct sockaddr_in	sockdesc;
	struct sockaddr		*sdp;
	RC		rc;
	TF		tf;
	
	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	sdp = (struct sockaddr*)&sockdesc;
	sockdesclen = sizeof(sockdesc);
	newsock = accept(sock, sdp, &sockdesclen);
	if (newsock == INVALID_SOCKET) return(FALSE);

	//rhostaddr = sockdesc.sin_addr.S_un.S_addr;
	rhostaddr = sockdesc.sin_addr.S_ADDR;
	tf = lsUnifyParm(eid, 2, cINT, &rhostaddr);
	if (tf != TRUE) return(FALSE);

	rhostport = (_int32) ntohs(sockdesc.sin_port);
	tf = lsUnifyParm(eid, 3, cINT, &rhostport);
	if (tf != TRUE) return(FALSE);

	tf = lsUnifyParm(eid, 4, cINT, &newsock);
	if (tf != TRUE) return(FALSE);

	return(TRUE);
}

/*
	bind(SocketI, LocalHostAddrI, LocalPortI)
*/
TF EXPFUNC p_bind(ENGid eid)
{
	SOCKET	sock;
	_int32	lhostaddr;
	int		lhostport, ecode;
	struct sockaddr_in	sockdesc;
	struct sockaddr		*sdp;
	RC		rc;
	
	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 2, cINT, &lhostaddr);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 3, cINT, &lhostport);
	if (rc != 0) return(FALSE);

	sockdesc.sin_family = AF_INET;
	sockdesc.sin_port = htons((unsigned short) lhostport);
	//sockdesc.sin_addr.S_un.S_addr = (unsigned long) lhostaddr;
	sockdesc.sin_addr.S_ADDR = (unsigned long) lhostaddr;
	sdp = (struct sockaddr*)&sockdesc;
	ecode = bind(sock, sdp, sizeof(sockdesc));
	if (ecode != 0)
	{
#ifdef UNIX
		perror("asock lsx error during bind");
#endif
		return(FALSE);
	}
	return(TRUE);
}

/*
	closesocket(SocketI)
*/
TF EXPFUNC p_closesocket(ENGid eid)
{
	SOCKET	sock;
	RC		rc;
	int		ecode;

	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
#if defined(MSWIN)
	ecode = closesocket(sock);
#elif defined(UNIX)
	ecode = close(sock);
#endif
	if (ecode != 0) return(FALSE);
	return(TRUE);
}

/*
	connect(SocketI, RemoteHostAddrI, RemoteHostPortI)
*/
TF EXPFUNC p_connect(ENGid eid)
{
	SOCKET	sock;
	_int32	rhostaddr;
	int		rhostport, ecode;
	struct sockaddr_in	sockdesc;
	struct sockaddr		*sdp;
	RC		rc;
	
	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 2, cINT, &rhostaddr);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 3, cINT, &rhostport);
	if (rc != 0) return(FALSE);

	sockdesc.sin_family = AF_INET;
	sockdesc.sin_port = htons((unsigned short) rhostport);
	//sockdesc.sin_addr.S_un.S_addr = (unsigned long) rhostaddr;
	sockdesc.sin_addr.S_ADDR = (unsigned long) rhostaddr;
	sdp = (struct sockaddr*)&sockdesc;
	ecode = connect(sock, sdp, sizeof(sockdesc));
	if (ecode != 0) return(FALSE);
	return(TRUE);
}

/*
	gethostbyaddr(HostAddrI, AliasListV, AddrListV)
*/
TF EXPFUNC p_gethostbyaddr(ENGid eid)
{
	_int32	hostaddr;
	struct hostent	*hp;
	TERM	term, item;
	RC		rc;
	TF		tf;
	int		i;

	rc = lsGetParm(eid, 1, cINT, &hostaddr);
	if (rc != 0) return(FALSE);

	hp = gethostbyaddr((char *)&hostaddr, 4, PF_INET);
	if (hp == NULL) return(FALSE);

	rc = lsMakeList(eid, &term);
	if (rc != 0) return(FALSE);
	i = 0;
	while (hp->h_aliases[i] != 0)
	{
		rc = lsMakeStr(eid, &item, hp->h_aliases[i]);
		if (rc != 0) return(FALSE);
		rc = lsPushList(eid, &term, item);
		if (rc != 0) return(FALSE);
		i++;
	}
	tf = lsUnifyParm(eid, 2, cTERM, &term);
	if (tf != TRUE) return(FALSE);

	rc = lsMakeList(eid, &term);
	if (rc != 0) return(FALSE);
	i = 0;
	while (hp->h_addr_list[i] != 0)
	{
		rc = lsMakeInt(eid, &item, *(_int32*)(hp->h_addr_list[i]));
		if (rc != 0) return(FALSE);
		rc = lsPushList(eid, &term, item);
		if (rc != 0) return(FALSE);
		i++;
	}
	tf = lsUnifyParm(eid, 3, cTERM, &term);
	if (tf != TRUE) return(FALSE);

	return(TRUE);
}

/*
	gethostbyname(HostNameS, AliasListV, AddrListV)
*/
TF EXPFUNC p_gethostbyname(ENGid eid)
{
	char	hostname[MAX_HOSTNAME];
	struct hostent	*hp;
	TERM	term, item;
	RC		rc;
	TF		tf;
	int		i;

	if (MAX_HOSTNAME <= lsStrParmLen(eid, 1) )
		lsErrRaise(eid, "host name too long");
	rc = lsGetParm(eid, 1, cSTR, hostname);
	if (rc != 0) return(FALSE);

	hp = gethostbyname(hostname);
	if (hp == NULL) return(FALSE);

	rc = lsMakeList(eid, &term);
	if (rc != 0) return(FALSE);
	i = 0;
	while (hp->h_aliases[i] != 0)
	{
		rc = lsMakeStr(eid, &item, hp->h_aliases[i]);
		if (rc != 0) return(FALSE);
		rc = lsPushList(eid, &term, item);
		if (rc != 0) return(FALSE);
		i++;
	}
	tf = lsUnifyParm(eid, 2, cTERM, &term);
	if (tf != TRUE) return(FALSE);

	rc = lsMakeList(eid, &term);
	if (rc != 0) return(FALSE);
	i = 0;
	while (hp->h_addr_list[i] != 0)
	{
		rc = lsMakeInt(eid, &item, *(_int32*)(hp->h_addr_list[i]));
		if (rc != 0) return(FALSE);
		rc = lsPushList(eid, &term, item);
		if (rc != 0) return(FALSE);
		i++;
	}
	tf = lsUnifyParm(eid, 3, cTERM, &term);
	if (tf != TRUE) return(FALSE);

	return(TRUE);
}

/*
	gethostname(HostNameStrV)
*/
TF EXPFUNC p_gethostname(ENGid eid)
{
	char	hostname[MAX_HOSTNAME];
	int		ecode;
	TF		tf;

	ecode = gethostname(hostname, MAX_HOSTNAME);
	if (ecode != 0) return(FALSE);
	tf = lsUnifyParm(eid, 1, cSTR, hostname);
	if (tf != TRUE) return(FALSE);
	return(TRUE);
}

/*
	getpeername(SocketI, RemoteHostAddrV, RemoteHostPortV)
*/
TF EXPFUNC p_getpeername(ENGid eid)
{
	SOCKET	sock;
	_int32	rhostaddr;
	int		rhostport, sockdesclen, ecode;
	struct sockaddr_in	sockdesc;
	struct sockaddr		*sdp;
	RC		rc;
	TF		tf;
	
	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	sdp = (struct sockaddr*)&sockdesc;
	ecode = getpeername(sock, sdp, &sockdesclen);
	if (ecode != 0) return(FALSE);

	//rhostaddr = sockdesc.sin_addr.S_un.S_addr;
	rhostaddr = sockdesc.sin_addr.S_ADDR;
	tf = lsUnifyParm(eid, 2, cINT, &rhostaddr);
	if (tf != TRUE) return(FALSE);

	rhostport = (_int32) ntohs(sockdesc.sin_port);
	tf = lsUnifyParm(eid, 3, cINT, &rhostport);
	if (tf != TRUE) return(FALSE);

	return(TRUE);
}

/*
	getprotobyname(ProtoS, ProtoNumV, AliasListV)
*/
TF EXPFUNC p_getprotobyname(ENGid eid)
{
	char	protoname[MAX_PROTONAME];
	struct protoent	*pp;
	TERM	term, item;
	RC		rc;
	TF		tf;
	int		i, proto;

	if (MAX_PROTONAME <= lsStrParmLen(eid, 1) )
		lsErrRaise(eid, "protocol name too long");
	rc = lsGetParm(eid, 1, cSTR, protoname);
	if (rc != 0) return(FALSE);

	pp = getprotobyname(protoname);
	if (pp == NULL) return(FALSE);

	proto = pp->p_proto;
	tf = lsUnifyParm(eid, 2, cINT, &proto);
	if (tf != TRUE) return(FALSE);

	rc = lsMakeList(eid, &term);
	if (rc != 0) return(FALSE);
	i = 0;
	while (pp->p_aliases[i] != 0)
	{
		rc = lsMakeStr(eid, &item, pp->p_aliases[i]);
		if (rc != 0) return(FALSE);
		rc = lsPushList(eid, &term, item);
		if (rc != 0) return(FALSE);
		i++;
	}
	tf = lsUnifyParm(eid, 3, cTERM, &term);
	if (tf != TRUE) return(FALSE);

	return(TRUE);
}

/*
	getprotobynumber(ProtoI, ProtoNameV, AliasListV)
*/
TF EXPFUNC p_getprotobynumber(ENGid eid)
{
	struct protoent	*pp;
	TERM	term, item;
	RC		rc;
	TF		tf;
	int		i, proto;

	rc = lsGetParm(eid, 1, cINT, &proto);
	if (rc != 0) return(FALSE);

	pp = getprotobynumber(proto);
	if (pp == NULL) return(FALSE);

	tf = lsUnifyParm(eid, 2, cSTR, pp->p_name);
	if (tf != TRUE) return(FALSE);

	rc = lsMakeList(eid, &term);
	if (rc != 0) return(FALSE);
	i = 0;
	while (pp->p_aliases[i] != 0)
	{
		rc = lsMakeStr(eid, &item, pp->p_aliases[i]);
		if (rc != 0) return(FALSE);
		rc = lsPushList(eid, &term, item);
		if (rc != 0) return(FALSE);
		i++;
	}
	tf = lsUnifyParm(eid, 3, cTERM, &term);
	if (tf != TRUE) return(FALSE);

	return(TRUE);
}

/*
	getservbyname(ServNameS, ProtoNameS, ServPortNumV, ServAliasListV)
*/
TF EXPFUNC p_getservbyname(ENGid eid)
{
	char	servname[MAX_SERVNAME], protoname[MAX_PROTONAME];
	struct servent	*sp;
	TERM	term, item;
	RC		rc;
	TF		tf;
	int		i, port;

	if (MAX_SERVNAME <= lsStrParmLen(eid, 1) )
		lsErrRaise(eid, "service name too long");
	rc = lsGetParm(eid, 1, cSTR, servname);
	if (rc != 0) return(FALSE);
	if (MAX_PROTONAME <= lsStrParmLen(eid, 2) )
		lsErrRaise(eid, "protocol name too long");
	rc = lsGetParm(eid, 2, cSTR, protoname);
	if (rc != 0) return(FALSE);

	if (strlen(protoname) == 0)
		sp = getservbyname(servname, NULL);
	else
		sp = getservbyname(servname, protoname);
	if (sp == NULL) return(FALSE);

	port = ntohs(sp->s_port);
	tf = lsUnifyParm(eid, 3, cINT, &port);
	if (tf != TRUE) return(FALSE);

	rc = lsMakeList(eid, &term);
	if (rc != 0) return(FALSE);
	i = 0;
	while (sp->s_aliases[i] != 0)
	{
		rc = lsMakeStr(eid, &item, sp->s_aliases[i]);
		if (rc != 0) return(FALSE);
		rc = lsPushList(eid, &term, item);
		if (rc != 0) return(FALSE);
		i++;
	}
	tf = lsUnifyParm(eid, 4, cTERM, &term);
	if (tf != TRUE) return(FALSE);

	return(TRUE);
}

/*
	getservbyport(PortnumI, ProtoNameS, ServNameV, ServAliasListV)
*/
TF EXPFUNC p_getservbyport(ENGid eid)
{
	char	protoname[MAX_PROTONAME];
	struct servent	*sp;
	TERM	term, item;
	RC		rc;
	TF		tf;
	int		i, port;

	rc = lsGetParm(eid, 1, cINT, &port);
	if (rc != 0) return(FALSE);
	if (MAX_PROTONAME <= lsStrParmLen(eid, 2) )
		lsErrRaise(eid, "protocol name too long");
	rc = lsGetParm(eid, 2, cSTR, protoname);
	if (rc != 0) return(FALSE);

	if (strlen(protoname) == 0)
		sp = getservbyport(htons((short) port), NULL);
	else
		sp = getservbyport(htons((short) port), protoname);
	if (sp == NULL) return(FALSE);

	tf = lsUnifyParm(eid, 3, cSTR, sp->s_name);
	if (tf != TRUE) return(FALSE);

	rc = lsMakeList(eid, &term);
	if (rc != 0) return(FALSE);
	i = 0;
	while (sp->s_aliases[i] != 0)
	{
		rc = lsMakeStr(eid, &item, sp->s_aliases[i]);
		if (rc != 0) return(FALSE);
		rc = lsPushList(eid, &term, item);
		if (rc != 0) return(FALSE);
		i++;
	}
	tf = lsUnifyParm(eid, 4, cTERM, &term);
	if (tf != TRUE) return(FALSE);

	return(TRUE);
}

/*
	getsockname(SocketI, LocalHostAddrV, LocalHostPortV)
*/
TF EXPFUNC p_getsockname(ENGid eid)
{
	SOCKET	sock;
	_int32	lhostaddr;
	int		lhostport, sockdesclen, ecode;
	struct sockaddr_in	sockdesc;
	struct sockaddr		*sdp;
	RC		rc;
	TF		tf;
	
	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	sdp = (struct sockaddr*)&sockdesc;
	ecode = getsockname(sock, sdp, &sockdesclen);
	if (ecode != 0) return(FALSE);

	//lhostaddr = sockdesc.sin_addr.S_un.S_addr;
	lhostaddr = sockdesc.sin_addr.S_ADDR;
	tf = lsUnifyParm(eid, 2, cINT, &lhostaddr);
	if (tf != TRUE) return(FALSE);

	lhostport = (_int32) ntohs(sockdesc.sin_port);
	tf = lsUnifyParm(eid, 3, cINT, &lhostport);
	if (tf != TRUE) return(FALSE);

	return(TRUE);
}

/*
	getsockopt(SocketI, LevelI, OptionI, ValueNumV)
*/
TF EXPFUNC p_getsockopt(ENGid eid)
{
	SOCKET	sock;
	struct linger	ling;
	int		level, optname, valuelen, ecode;
	_int32	value;
	RC		rc;
	TF		tf;

	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 2, cINT, &level);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 3, cINT, &optname);
	if (rc != 0) return(FALSE);
	valuelen = 4;
	if (optname != SO_LINGER)
		ecode = getsockopt(sock, level, optname, (char *)&value, &valuelen);
	else
	{
		ecode = getsockopt(sock, level, optname, (char *)&ling, &valuelen);
		if (ling.l_onoff == 0) value = 0;
		else value = ling.l_linger;
	}
	if (ecode != 0) return(FALSE);
	tf = lsUnifyParm(eid, 4, cINT, &value);
	if (tf != TRUE) return(FALSE);
	return(TRUE);
}

/*
	inet_addr(DotHostNameS, HostAddrV)
*/
TF EXPFUNC p_inet_addr(ENGid eid)
{
	char	dotaddr[20];
	_int32	inetaddr;
	RC		rc;
	TF		tf;

	if (20 <= lsStrParmLen(eid, 1) )
		lsErrRaise(eid, "host address too long");
	rc = lsGetParm(eid, 1, cSTR, dotaddr);
	if (rc != 0) return(FALSE);

	inetaddr = inet_addr(dotaddr);
	tf = lsUnifyParm(eid, 2, cINT, &inetaddr);
	if (tf != TRUE) return(FALSE);
	if (inetaddr == INADDR_NONE) return(FALSE);
	return(TRUE);
}

/*
	inet_ntoa(HostAddrI, DotHostNameV)
*/
TF EXPFUNC p_inet_ntoa(ENGid eid)
{
	struct in_addr	iaddr;
	_int32	inetaddr;
	char	*dotaddr;
	RC		rc;
	TF		tf;

	rc = lsGetParm(eid, 1, cINT, &inetaddr);
	if (rc != 0) return(FALSE);
	//iaddr.S_un.S_addr = inetaddr;
	iaddr.S_ADDR = inetaddr;
	
	dotaddr = inet_ntoa(iaddr);
	tf = lsUnifyParm(eid, 2, cSTR, dotaddr);
	if (tf != TRUE) return(FALSE);
	return(TRUE);
}

#if defined(MSWIN)     // This seems to be a winsock only option
/*
	ioctlsocket(SocketI, CmdI, ArgI, ValueV)
*/
TF EXPFUNC p_ioctlsocket(ENGid eid)
{
	SOCKET	sock;
	BOOL	barg;
	long	larg;
	int		cmd, ecode;
	RC		rc;
	TF		tf;

	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 2, cINT, &cmd);
	if (rc != 0) return(FALSE);
	switch (cmd)
	{
	case SIOCATMARK:
		ecode = ioctlsocket(sock, cmd, &barg);
		larg = (BOOL) barg;
		break;
	case FIONBIO:
		rc = lsGetParm(eid, 2, cLONG, &larg);
		if (rc != 0) return(FALSE);
		ecode = ioctlsocket(sock, cmd, &larg);
		break;
	default:
		ecode = ioctlsocket(sock, cmd, &larg);
		break;
	}
	if (ecode != 0) return(FALSE);
	tf = lsUnifyParm(eid, 4, cLONG, &larg);
	if (tf != TRUE) return(FALSE);
	return(TRUE);
}
#elif defined(UNIX)
TF EXPFUNC p_ioctlsocket(ENGid eid) { return TRUE; }
#endif

/*
	listen(SocketI, BacklogI)
*/
TF EXPFUNC p_listen(ENGid eid)
{
	SOCKET	sock;
	int		backlog, ecode;
	RC		rc;

	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 2, cINT, &backlog);
	if (rc != 0) return(FALSE);
	ecode = listen(sock, backlog);
	if (ecode != 0) return(FALSE);
	return(TRUE);
}

/*
	recv(SocketI, DataStrV, DataLenI, FlagsI, ReceivedLenV)
*/
TF EXPFUNC p_recv(ENGid eid)
{
	SOCKET	sock;
	char	*buf;
	int		flags, buflen, recvlen;
	RC		rc;
	TF		tf;

	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 3, cINT, &buflen);
	if (rc != 0) return(FALSE);
	buf = (char *)malloc(buflen+1);
	if (buf == NULL)
		lsErrRaise(eid, "out of memory allocating recv buffer");
	rc = lsGetParm(eid, 4, cINT, &flags);
	if (rc != 0) goto p_recv_error;

	recvlen = recv(sock, buf, buflen, flags);
	if (recvlen == SOCKET_ERROR) goto p_recv_error;
	buf[recvlen] = '\0';
	tf = lsUnifyParm(eid, 2, cSTR, buf);
	if (tf != TRUE) goto p_recv_error;
	tf = lsUnifyParm(eid, 5, cINT, &recvlen);
	if (tf != TRUE) goto p_recv_error;
	free(buf);
	return(TRUE);

p_recv_error:
	free(buf);
	return(FALSE);
}

/*
	recvfrom(SocketI, DataStrV, DataLenI, FlagsI, FromAddrV, FromPortNumV, ReceivedLenV)
*/
TF EXPFUNC p_recvfrom(ENGid eid)
{
	SOCKET	sock;
	_int32	rhostaddr;
	int		rhostport, sockdesclen;
	struct sockaddr_in	sockdesc;
	struct sockaddr		*sdp;
	char	*buf;
	int		flags, buflen, recvlen;
	RC		rc;
	TF		tf;
	
	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 3, cINT, &buflen);
	if (rc != 0) return(FALSE);
	buf = (char *)malloc(buflen+1);
	if (buf == NULL)
		lsErrRaise(eid, "out of memory allocating recv buffer");
	rc = lsGetParm(eid, 4, cINT, &flags);
	if (rc != 0) goto p_recvfrom_error;

	sdp = (struct sockaddr*)&sockdesc;
	sockdesclen = sizeof(sockdesc);

	recvlen = recvfrom(sock, buf, buflen, flags, sdp, &sockdesclen);
	if (recvlen == SOCKET_ERROR) goto p_recvfrom_error;

	buf[recvlen] = '\0';
	tf = lsUnifyParm(eid, 2, cSTR, buf);
	if (tf != TRUE) goto p_recvfrom_error;

	//rhostaddr = sockdesc.sin_addr.S_un.S_addr;
	rhostaddr = sockdesc.sin_addr.S_ADDR;
	tf = lsUnifyParm(eid, 5, cINT, &rhostaddr);
	if (tf != TRUE) goto p_recvfrom_error;

	rhostport = (_int32) ntohs(sockdesc.sin_port);
	tf = lsUnifyParm(eid, 6, cINT, &rhostport);

	tf = lsUnifyParm(eid, 7, cINT, &recvlen);
	if (tf != TRUE) goto p_recvfrom_error;
	free(buf);
	return(TRUE);

p_recvfrom_error:
	free(buf);
	return(FALSE);
}

/*
	select(ReadSocketsL, WriteSocketsL, ErrorSocketsL, WaitSecondsI, WaitThousSecsI,
		ReadSocketsResultListV, WriteSocketsResultListV, ErrorSocketsResultListV)
*/
TF EXPFUNC p_select(ENGid eid)
{
	SOCKET	tsock;
#ifdef MSWIN
	struct fd_set	readset, writeset, errorset;
#else
	fd_set	readset, writeset, errorset;
#endif
	struct timeval	waitlen;
	TERM	list, newlist, newitem;
	int		numsock;
	RC		rc;
	TF		tf;

	rc = lsGetParm(eid, 1, cTERM, &list);
	if (rc != 0) return(FALSE);
	FD_ZERO(&readset);
	while (lsPopList(eid, &list, cINT, &tsock) == OK)
		FD_SET(tsock, &readset);
	
	rc = lsGetParm(eid, 2, cTERM, &list);
	if (rc != 0) return(FALSE);
	FD_ZERO(&writeset);
	while (lsPopList(eid, &list, cINT, &tsock) == OK)
		FD_SET(tsock, &writeset);
	
	rc = lsGetParm(eid, 3, cTERM, &list);
	if (rc != 0) return(FALSE);
	FD_ZERO(&errorset);
	while (lsPopList(eid, &list, cINT, &tsock) == OK)
		FD_SET(tsock, &errorset);
	
	rc = lsGetParm(eid, 4, cLONG, &waitlen.tv_sec);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 5, cLONG, &waitlen.tv_usec);
	if (rc != 0) return(FALSE);

	numsock = select(0, &readset, &writeset, &errorset, &waitlen);
	if (numsock == SOCKET_ERROR) return(FALSE);

	rc = lsMakeList(eid, &newlist);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 1, cTERM, &list);
	if (rc != 0) return(FALSE);
	while (lsPopList(eid, &list, cINT, &tsock) == OK)
		if (FD_ISSET(tsock, &readset))
		{
			rc = lsMakeInt(eid, &newitem, tsock);
			rc = lsPushList(eid, &newlist, newitem);
		}
	tf = lsUnifyParm(eid, 6, cTERM, &newlist);
	if (tf != TRUE) return(FALSE);

	rc = lsMakeList(eid, &newlist);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 2, cTERM, &list);
	if (rc != 0) return(FALSE);
	while (lsPopList(eid, &list, cINT, &tsock) == OK)
		if (FD_ISSET(tsock, &writeset))
		{
			rc = lsMakeInt(eid, &newitem, tsock);
			rc = lsPushList(eid, &newlist, newitem);
		}
	tf = lsUnifyParm(eid, 7, cTERM, &newlist);
	if (tf != TRUE) return(FALSE);

	rc = lsMakeList(eid, &newlist);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 3, cTERM, &list);
	if (rc != 0) return(FALSE);
	while (lsPopList(eid, &list, cINT, &tsock) == OK)
		if (FD_ISSET(tsock, &errorset))
		{
			rc = lsMakeInt(eid, &newitem, tsock);
			rc = lsPushList(eid, &newlist, newitem);
		}
	tf = lsUnifyParm(eid, 8, cTERM, &newlist);
	if (tf != TRUE) return(FALSE);

	return(TRUE);
}

/*
	send(SocketI, DataS, FlagsI, SentLenV)
*/
TF EXPFUNC p_send(ENGid eid)
{
	SOCKET	sock;
	char	*buf;
	int		flags, buflen, sentlen;
	RC		rc;
	TF		tf;

	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	buflen = lsStrParmLen(eid, 2);
	buf = (char *)malloc(buflen+1);
	if (buf == NULL)
		lsErrRaise(eid, "out of memory allocating send buffer");
	rc = lsGetParm(eid, 2, cSTR, buf);
	if (rc != 0) goto p_send_error;
	rc = lsGetParm(eid, 3, cINT, &flags);
	if (rc != 0) goto p_send_error;

	sentlen = send(sock, buf, buflen, flags);
	tf = lsUnifyParm(eid, 4, cINT, &sentlen);
	if (tf != TRUE) goto p_send_error;
	if (sentlen == SOCKET_ERROR) goto p_send_error;
	free(buf);
	return(TRUE);

p_send_error:
	free(buf);
	return(FALSE);
}

/*
	sendto(SocketI, DataS, FlagsI, ToAddrI, ToPortI, SentLenV)
*/
TF EXPFUNC p_sendto(ENGid eid)
{
	SOCKET	sock;
	_int32	rhostaddr;
	int		rhostport;
	struct sockaddr_in	sockdesc;
	struct sockaddr		*sdp;
	char	*buf;
	int		flags, buflen, sentlen;
	RC		rc;
	TF		tf;

	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	buflen = lsStrParmLen(eid, 2);
	buf = (char *)malloc(buflen+1);
	if (buf == NULL)
		lsErrRaise(eid, "out of memory allocating send buffer");
	rc = lsGetParm(eid, 2, cSTR, buf);
	if (rc != 0) goto p_sendto_error;
	rc = lsGetParm(eid, 3, cINT, &flags);
	if (rc != 0) goto p_sendto_error;
	rc = lsGetParm(eid, 4, cINT, &rhostaddr);
	if (rc != 0) goto p_sendto_error;
	rc = lsGetParm(eid, 5, cINT, &rhostport);
	if (rc != 0) goto p_sendto_error;

	sockdesc.sin_family = AF_INET;
	sockdesc.sin_port = htons((unsigned short) rhostport);
	//sockdesc.sin_addr.S_un.S_addr = (unsigned long) rhostaddr;
	sockdesc.sin_addr.S_ADDR = (unsigned long) rhostaddr;
	sdp = (struct sockaddr*)&sockdesc;

	sentlen = sendto(sock, buf, buflen, flags, sdp, sizeof(sockdesc));
	tf = lsUnifyParm(eid, 6, cINT, &sentlen);
	if (tf != TRUE) goto p_sendto_error;
	if (sentlen == SOCKET_ERROR) goto p_sendto_error;
	free(buf);
	return(TRUE);

p_sendto_error:
	free(buf);
	return(FALSE);
}

/*
	setsockopt(SocketI, LevelI, OptionI, ValueI)
*/
TF EXPFUNC p_setsockopt(ENGid eid)
{
	SOCKET	sock;
	struct linger	ling;
	int		level, optname, valuelen, ecode;
	_int32	value;
	RC		rc;

	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 2, cINT, &level);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 3, cINT, &optname);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 4, cINT, &value);
	if (rc != 0) return(FALSE);
	valuelen = 4;
	if (optname != SO_LINGER)
		ecode = setsockopt(sock, level, optname, (char *)&value, valuelen);
	else
	{
		if (value == 0) ling.l_onoff = 0;
		else { ling.l_onoff = 1;  ling.l_linger = (short) value; }
		ecode = setsockopt(sock, level, optname, (char *)&ling, valuelen);
	}
	if (ecode != 0) return(FALSE);
	return(TRUE);
}

/*
	shutdown(SocketI, HowI)
*/
TF EXPFUNC p_shutdown(ENGid eid)
{
	SOCKET	sock;
	RC		rc;
	int		how, ecode;

	rc = lsGetParm(eid, 1, cINT, &sock);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 2, cINT, &how);
	if (rc != 0) return(FALSE);
	ecode = shutdown(sock, how);
	if (ecode != 0) return(FALSE);
	return(TRUE);
}

/*
	socket(SocketType, I, SocketProtocolI, SocketNumV) 
*/
TF EXPFUNC p_socket(ENGid eid)
{
	SOCKET	sock;
	int		socktype, sockprotocol;
	RC		rc;
	TF		tf;

	rc = lsGetParm(eid, 1, cINT, &socktype);
	if (rc != 0) return(FALSE);
	rc = lsGetParm(eid, 2, cINT, &sockprotocol);
	if (rc != 0) return(FALSE);

	sock = socket(PF_INET, socktype, sockprotocol);

	tf = lsUnifyParm(eid, 3, cINT, &sock);
	if (tf != TRUE) return(FALSE);
	return(TRUE);
}

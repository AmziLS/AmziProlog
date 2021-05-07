/* 
   sockets.pro -- Defines for sockets.lsx Linux & Winsock API 1.1 implementation

   Copyright (c) 1992-2002 Amzi! inc.  All Rights Reserved.
*/

/* Socket Types */

define('SOCK_STREAM',     1).               /* stream socket */
define('SOCK_DGRAM',      2).               /* datagram socket */
define('SOCK_RAW',        3).               /* raw-protocol interface */
define('SOCK_RDM',        4).               /* reliably-delivered message */
define('SOCK_SEQPACKET',  5).               /* sequenced packet stream */

/* Internet Address Types */

define('INADDR_ANY',      0).
define('INADDR_LOOPBACK', 0x7f000001).
define('INADDR_BROADCAST',0xffffffff).
define('INADDR_NONE',     0xffffffff).

/* Option flags per-socket */

define('SO_DEBUG',        0x0001).          /* turn on debugging info recording */
define('SO_ACCEPTCONN',   0x0002).          /* socket has had listen() */
define('SO_REUSEADDR',    0x0004).          /* allow local address reuse */
define('SO_KEEPALIVE',    0x0008).          /* keep connections alive */
define('SO_DONTROUTE',    0x0010).          /* just use interface addresses */
define('SO_BROADCAST',    0x0020).          /* permit sending of broadcast msgs */
define('SO_USELOOPBACK',  0x0040).          /* bypass hardware when possible */
define('SO_LINGER',       0x0080).          /* linger on close if data present */
define('SO_DONTLINGER',   0xFF7F).
define('SO_OOBINLINE',    0x0100).          /* leave received OOB data in line */
define('SO_SNDBUF',       0x1001).          /* send buffer size */
define('SO_RCVBUF',       0x1002).          /* receive buffer size */
define('SO_SNDLOWAT',     0x1003).          /* send low-water mark */
define('SO_RCVLOWAT',     0x1004).          /* receive low-water mark */
define('SO_SNDTIMEO',     0x1005).          /* send timeout */
define('SO_RCVTIMEO',     0x1006).          /* receive timeout */
define('SO_ERROR',        0x1007).          /* get error status and clear */
define('SO_TYPE',         0x1008).          /* get socket type */

/* Socket Level Number */

define('SOL_SOCKET',      0xffff).          /* options for socket level */

/* Protocols */

define('IPPROTO_IP',      0).               /* dummy for IP */
define('IPPROTO_ICMP',    1).               /* control message protocol */
define('IPPROTO_GGP',     2).               /* gateway^2 (deprecated) */
define('IPPROTO_TCP',     6).               /* tcp */
define('IPPROTO_PUP',     12).              /* pup */
define('IPPROTO_UDP',     17).              /* user datagram protocol */
define('IPPROTO_IDP',     22).              /* xns idp */
define('IPPROTO_ND',      77).              /* UNOFFICIAL net disk proto */

define('IPPROTO_RAW',     255).             /* raw IP packet */
define('IPPROTO_MAX',     256).

/* Socket I/O Controls */

define('FIONREAD',        0x4004667F).      /* get # bytes to read */
define('FIONBIO',         0x8004667E).      /* set/clear non-blocking i/o */
define('SIOCATMARK',      0x40047307).      /* at oob mark? */

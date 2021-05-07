/*
   AmziCGI.C -- Standard CGI Script Start-Up Program

   Link with AmziSub.C

   Copyright (c)1996-2000 Amzi! inc.  All Rights Reserved.

   Under NT and 95/98, this is built as a console application.
   Under Unix this is built as a regular program.

   Relies on _WIN32 and __unix__ defines
*/

/* Uncomment WINCGI_NAMES if you prefer to use the Windows CGI
   variable names instead of Standard CGI names. */
/*#define WINCGI_NAMES*/

/* Uncomment DEBUG for a debugging log */
//#define DEBUG
/* Uncomment TEST for a test cgi that prints alot of information */
//#define TEST

#ifdef _WIN32
#include <windows.h>
#include <direct.h>
#else
#define	_MAX_PATH	512
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>

/* The header file required for linking with Prolog.  It contains
   definitions and common variables. */

#include "amzi.h"

extern RC InitPreds(ENGid, void*);
extern void doublechar1(char *s, char c);
extern void slashslash1(char *s);
extern void slashslash2(char* sout, const char* sin);

extern void cgiExtractProgramName(char *in, char *out);

ENGid CurEng;
FILE *debugFile;

/* We use words as names for environment variables.
   These are asserted as Prolog facts either named 
   'cgi', 'extraheaders' or 'system'.  
   
   For example, HTTP_REFERER is asserted as
   cgi('Referer', <value>). */

#ifdef	WINCGI_NAMES
char *envlist[][3] = 
{
	"AUTH_NAME", "'Authentication Realm", "cgi",
	"AUTH_USER", "'Authentication Username'", "cgi",
	"AUTH_TYPE", "'Authentication Method'", "cgi",
	"CGI_VERSION", "'CGI Version'", "cgi",
	"CONTENT_FILE", "'Content File'", "cgi",
	"CONTENT_LENGTH", "'Content Length'", "cgi",
	"CONTENT_TYPE", "'Content Type'", "cgi",
	"COOKIE", "'Cookie'", "extraheaders",
   	"GATEWAY_INTERFACE", "'Gateway Interface'", "cgi",
	"HTTP_COOKIE", "'Cookie'", "extraheaders",
	"HTTP_FROM", "'From'", "cgi",
	"HTTP_REFERER", "'Referer'", "cgi",
	"HTTP_USER_AGENT", "'User Agent'", "cgi",
	"LOGNAME", "'Remote User'", "cgi",
	"PATH_INFO", "'Logical Path'", "cgi",
	"PATH_TRANSLATED", "'Physical Path'", "cgi",
	"QUERY_STRING", "'Query String'", "cgi",
	"REFERER_URL", "'Referer'", "cgi",
 	"REMOTE_ADDR", "'Remote Address'",	"cgi",
	"REMOTE_HOST", "'Remote Host'",	"cgi",
	"REMOTE_USER", "'Remote User'", "cgi", 
	"REQUEST_METHOD", "'Request Method'", "cgi",
	"REQUEST_RANGE", "'Request Range'", "cgi",
	"SCRIPT_NAME", "'Executable Path'", "cgi",
	"SERVER_ADMIN", "'Server Admin'",	"cgi",
	"SERVER_NAME", "'Server Name'", "cgi",
	"SERVER_PORT", "'Server Port'", "cgi",
	"SERVER_PROTOCOL", "'Server Protocol'", "cgi",
	"SERVER_SOFTWARE", "'Server Software'", "cgi",
	"USER_AGENT", "'User Agent'",	"cgi",

	"DEBUG_MODE", "'Debug Mode'", "system",
	"GMT_OFFSET", "'GMT Offset'", "system",

	NULL, NULL
};
#else
char *envlist[][3] = 
{
	"AUTH_NAME", "auth_name", "cgi",
	"AUTH_USER", "auth_user", "cgi",
	"AUTH_TYPE", "auth_type", "cgi",
	"CGI_VERSION", "cgi_version", "cgi",
	"CONTENT_FILE", "content_file", "cgi",
	"CONTENT_LENGTH", "content_length", "cgi",
	"CONTENT_TYPE", "content_type", "cgi",
	"COOKIE", "cookie", "extraheaders",
   	"GATEWAY_INTERFACE", "gateway_interface", "cgi",
	"HTTP_COOKIE", "http_cookie", "extraheaders",
	"HTTP_FROM", "http_from", "cgi",
	"HTTP_REFERER", "http_referer", "cgi",
	"HTTP_USER_AGENT", "http_user_agent", "cgi",
	"LOGNAME", "logname", "cgi",
	"PATH_INFO", "path_info", "cgi",
	"PATH_TRANSLATED", "path_translated", "cgi",
	"QUERY_STRING", "query_string", "cgi",
	"REFERER_URL", "referer_url", "cgi",
 	"REMOTE_ADDR", "remote_addr", "cgi",
	"REMOTE_HOST", "remote_host",	"cgi",
	"REMOTE_USER", "remote_user", "cgi", 
	"REQUEST_METHOD", "request_method", "cgi",
	"REQUEST_RANGE", "request_range", "cgi",
	"SCRIPT_NAME", "script_name", "cgi",
	"SERVER_ADMIN", "server_admin",	"cgi",
	"SERVER_NAME", "server_name", "cgi",
	"SERVER_PORT", "server_port",	"cgi",
	"SERVER_PROTOCOL", "server_protocol", "cgi",
	"SERVER_SOFTWARE", "server_software", "cgi",
	"USER_AGENT", "user_agent", "cgi",

	"DEBUG_MODE", "debug_mode", "system",
	"GMT_OFFSET", "gmt_offset", "system",

	NULL, NULL
};
#endif

/* Converts nn to character */

char x2c(char *what) 
{
    register char digit;

    digit = (what[0] >= 'A' ? ((what[0] & 0xdf) - 'A')+10 : (what[0] - '0'));
    digit *= 16;
    digit += (what[1] >= 'A' ? ((what[1] & 0xdf) - 'A')+10 : (what[1] - '0'));
    return(digit);
}

/* Converts plus signs to spaces */

void plustospace(char *str) 
{
    register int x;

    for(x=0;str[x];x++) if(str[x] == '+') str[x] = ' ';
}

/* Converts &nn's and %nn's to characters in url encoded strings */

void unescape_url(char *url) 
{
    register int x,y;

    for(x=0,y=0;url[y];++x,++y) 
	{
        if((url[x] = url[y]) == '&') 
		{
            url[x] = x2c(&url[y+1]);
            y+=2;
        }
    }
    url[x] = '\0';
}

/* Converts %nn's to characters in url encoded strings */

void unpercent_url(char *url)
{
    register int x,y;

    for(x=0,y=0;url[y];++x,++y) 
	{
        if((url[x] = url[y]) == '%') 
		{
            url[x] = x2c(&url[y+1]);
            y+=2;
        }
    }
    url[x] = '\0';
}

void decode_url(char *url)
{
	plustospace(url);
	unescape_url(url);
    unpercent_url(url);
}

/* Extracts all the facts from the URL encoded input stream and asserts
   them to the Prolog dynamic database in the form 
   "fact(<attribute>, <value>). */

RC LSAPI  lsAssertaStrW(ENGid, wchar_t*);

void assertFacts()
{
	char	*p, *q, *tp, *val, *buf, factbuf[4096], valbuf[4096];
	wchar_t ufactbuf[8192];
	long	buflen;
	RC		rc;

	/* Get the length of the encoded URL */
	val = getenv("CONTENT_LENGTH");
	if (val == NULL)
	{
		val = getenv("QUERY_STRING");
		if (val == NULL) return;
		buf = (char *)malloc(strlen(val)+2);
		strcpy(buf, val);
		buf[buflen-1] = '\0';
#ifdef DEBUG
	fprintf(debugFile, "QUERY_STRING %s\n", val);
	fflush(debugFile);
#endif
	}
	else
	{
		/* Add two for two nuls and malloc space */
		buflen = atol(val)+2;
		buf = (char *)malloc(buflen);
		if (buf == NULL) return;
		fgets(buf, buflen-1, stdin);
		buf[buflen-1] = '\0';
#ifdef DEBUG
	fprintf(debugFile, "CONTENT_LENGTH %s\n", val);
	fflush(debugFile);
#endif
	}

#ifdef DEBUG
	fprintf(debugFile, "Encoded URL %s\n", buf);
	fflush(debugFile);
#endif

#ifdef TEST
	printf("<P>Encoded URL = %s\n", buf);
#endif

	p = buf;
	while (*p != '\0')
	{
		/* Create a structure of the form "fact(<attribute>, <value>)" */
		strncpy(factbuf, "fact('", 4096);

		/* Grab the attribute (up to the equal = sign) */
		q = strchr(p, '=');
      if (q == NULL) /* Not the right format */
      {
         free(buf);
         return;
      }
      *q = '\0';
		tp = q + 1;
		decode_url(p);

		/* Now append the attribute name */
		strncat(factbuf, p, 4096);
		strncat(factbuf, "', ", 4096);
 		
		/* Grab the value (up to the & or nul) */
		p = tp;
		q = strchr(p, '&');
		if (q == NULL)		/* End of string */
		{
			q = &buf[buflen-1];
			tp = q;
		}
		else				/* More values left to process */
		{
			*q = '\0';
			tp = q + 1;
		}
		decode_url(p);
		slashslash2(valbuf, p);
//		doublechar1(valbuf, '$');

		/* If the first char is not a delimiter, make it a string */
		if (valbuf[0] != '\'' && valbuf[0] != '$' && valbuf[0] != '[')
		{
			/* Now append the value */
			strncat(factbuf, "$", 4096);
			doublechar1(valbuf, '$');
			strncat(factbuf, valbuf, 4096);
			strncat(factbuf, "$)", 4096);
		}
		else
		{
			/* Now append the value */
			strncat(factbuf, valbuf, 4096);
			strncat(factbuf, ")", 4096);
		}

#ifdef	DEBUG
	fprintf(debugFile, "Encoded URL: asserting %s\n", factbuf);
	fflush(debugFile);
#endif
		/* Assert it in the Prolog logic-base */
		if (strlen(p) > 0)
		{
			rc = mbstowcs(ufactbuf, factbuf, 8192); 
			rc = lsAssertaStrW(CurEng, ufactbuf);
		}

		p = tp;
	}

	/* Free our url buffer */
	free(buf);
}

int main(int argc, char *argv[])
{
	char	*p, *val;
	char	exename[_MAX_PATH];
	char	xplname[_MAX_PATH];
	char	cgidir[_MAX_PATH];
	char	strbuf[4096], errmsg[1024];
	char	*sExeName, *sColon, *sSlash, *sDot;
	int		rc, exitrc, i;
	TF		tf;
	TERM	term;
#ifdef	_WIN32
	MSG		msg;
#endif

#ifdef	DEBUG
	debugFile = fopen("/tmp/acgi.log", "w");
	if (debugFile == NULL) exit(-100);
	fprintf(debugFile, "Starting...\n");
	fflush(debugFile);
#else
	debugFile = stdout;
#endif

#ifdef	TEST
	/*	printf("HTTP/1.0 200 OK\n");*/
	printf("Content-type: text/html\n\n");
	getcwd(strbuf, _MAX_PATH);
	printf("<P>Current Directory = %s\n", strbuf);
#endif

	/* Initialization */
	/* -------------- */
#ifdef __unix__
	umask(0);
#endif

	exitrc = 0;

	/* Load up the locale so wcstombs works */
	setlocale(LC_ALL, "");

	/* Get the name of the program to run */
	/* ---------------------------------- */

	/* First check the name of this .EXE file; run the .XPL file of the
	   same name. */
	strcpy(exename, argv[0]);

	/* Massage the EXE name into the corresponding XPL */
	sColon = strrchr(exename,':');           /* remove path from file name */
	sSlash = strrchr(exename,'\\');
	if (sSlash == NULL) sSlash = strrchr(exename,'/');       /* Unix path? */
   
	if (sColon == NULL && sSlash == NULL) sExeName = exename;
	else if (sSlash == NULL) sExeName = sColon + 1;
	else sExeName = sSlash + 1;

	sDot = strrchr(sExeName,'.');
	if (sDot) *sDot = '\0';

	strcpy(xplname, sExeName);

#ifdef DEBUG
	fprintf(debugFile, "xplname %s\n", xplname);
	fflush(debugFile);
#endif

	strcpy(cgidir, argv[0]);
	sSlash = strrchr(cgidir, '\\');
#ifdef _WIN32
	if (sSlash)
	{
		*sSlash = '\0';
		rc = chdir(cgidir);
		if (rc != 0)
		{
			printf("Content-type: text/plain\n\nFatal Error #%d trying to change directory to:\n%s", rc, cgidir);
			goto main_error;
		}
	}
#endif

#ifdef TEST
	if (sSlash) printf("<P>.exe Directory = %s\n", cgidir);
	else printf("<P>No directory for .exe\n");
	printf("<P>.exe Filename = %s\n", exename);
	printf("<P>.xpl Filename = %s\n", xplname);
#endif


	/* Initialize the Prolog environment */
	/* --------------------------------- */

#ifdef TEST
	printf("<P>Loading amzi.dll or libamzi.so");
#endif
	rc = lsInit(&CurEng, xplname);
	if (rc != 0)
	{
		lsGetExceptMsg(CurEng, errmsg, 1024);

		printf("Content-type: text/plain\n\nFatal Error #%d initializing Amzi! Logic Server:\n%s", rc, errmsg);
		goto main_error;
	}

#ifdef TEST
	printf("<P>Loading .lsx files (usually aosutils.lsx and under Windows aodbc.lsx)");
#endif
	rc = lsInitLSX(CurEng, NULL);
	if (rc != 0)
	{
		printf("Content-type: text/plain\n\nFatal Error #%d initializing LSXs", rc);
		goto main_error;
	}

	/* Initialize the extended predicates in amzisub.c */
	rc = InitPreds(CurEng, NULL);
	if (rc != 0)
	{
		printf("Content-type: text/plain\n\nFatal Error #%d loading Amzi! CGI extended predicates", rc);
		goto main_error;
	}


	/* Load the .xpl file */
	/* ------------------ */

#ifdef TEST
	printf("<P>Loading the .xpl file: %s", xplname);
#endif
	rc = lsLoad(CurEng, xplname);
	if (rc != 0)
	{
		lsGetExceptMsg(CurEng, errmsg, 1024);

		printf("Content-type: text/plain\n\nFatal Error #%d loading Amzi! Logic Server XPL file:\n%s", rc, errmsg);
		goto main_error;
	}


	/* Set Up the Dynamic Database */
	/* --------------------------- */

#ifdef TEST
	printf("<P>");
#endif
	/* Assert all the environment variables on our list */
	i = 0;
	p = envlist[i][0];
	while (p != NULL)
	{
		val = getenv(p);

		/* Skip the ones that have no value */
		if (val != NULL)
		{
			strcpy(strbuf, envlist[i][2]);
			strcat(strbuf, "(");
			strcat(strbuf, envlist[i][1]);

			slashslash1(val);

			/* Use atoms for names, strings for values */
			doublechar1(val, '$');
			strcat(strbuf, ", $");
			strcat(strbuf, val);
			strcat(strbuf, "$)");

#ifdef DEBUG
			fprintf(debugFile, "Environment: asserting %s \n", strbuf);
			fflush(debugFile);
#endif

#ifdef TEST
			printf("<BR>Asserting %s\n", strbuf);
#endif

			rc = lsAssertaStr(CurEng, strbuf);
			if (rc != 0)
			{
				printf("Content-type: text/plain\n\nFatal Error #%d asserting CGI environment variables", rc);
				goto main_error;
			}
		}

		i++;
		p = envlist[i][0];
	}

#ifdef TEST
	printf("<P>");
#endif

	/* Assert all the facts decoded from stdin */
	assertFacts();

#ifdef DEBUG
	fprintf(debugFile, "Facts Asserted");
	fflush(debugFile);
#endif

	/* Assert the name of the XPL file (used to load the logic-base) */
	strcpy(strbuf, "system('XPL File', '");
	strcat(strbuf, xplname);
	strcat(strbuf, "')");

	rc = lsAssertaStr(CurEng, strbuf);
	if (rc != 0)
	{
		printf("Content-type: text/plain\n\nFatal Error #%d asserting XPL file name", rc);
		goto main_error;
	}


	/* Finally call main/0 of the loaded program */
	/* ----------------------------------------- */

#ifdef DEBUG
	fprintf(debugFile, "Calling cgi_main\n");
	fflush(debugFile);
#endif
#ifdef TEST
	printf("<P>Calling cgi_main\n</p>");
#endif

	tf = lsExecStr(CurEng, &term, "cgi_main");
	if (tf == FALSE) 
	{
		printf("Content-type: text/plain\n\nFatal Error: cgi_main failed\n");
      exitrc = 1;
	}


	/* Close down the Prolog environment, freeing memory */
	/* ------------------------------------------------- */

#ifdef TEST
	printf("<P>SUCCESS -- Closing down");
#endif
	lsClose(CurEng);

#ifdef	_WIN32
	/* Let Windows run so all ends well */
	SetTimer(NULL,1,100,NULL);
	while ( PeekMessage((MSG FAR*)&msg, NULL, WM_TIMER, WM_TIMER, PM_REMOVE) );
#endif

	return exitrc;

main_error:
#ifdef TEST
	printf("<P>FAILURE -- Closing down");
#endif
	lsClose(CurEng);

	exit(rc);
}

/*
   AmziSub.C -- Extended Predicates for CGI Processing

   Link with AmziCGI.C

   Copyright (c) 1996-1998 Amzi! inc.  All Rights Reserved.
   See license.txt for Amzi! License Agreement
*/

/*#define	DEBUG*/

/* Solaris is Unix for CGI */
#ifdef __sun
#ifndef __unix__
#define  __unix__
#endif
#endif

#ifdef BSD
#ifndef __unix__
#define __unix__
#endif
#endif

#include <ctype.h>

#ifdef _WIN32
#include <direct.h>
#include <fcntl.h>
#include <io.h>
#include <winsock2.h>
#include <windows.h>
#else
#define	_MAX_PATH	512
#endif

#include <stdio.h>
#include <string.h>
#include <time.h>

#ifdef __unix__
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <fcntl.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif

#include "amzi.h"

#ifdef DEBUG
extern FILE *debugFile;
#endif

void doublechar1(char *s, char c);
void slashslash1(char *s);
void slashslash2(char* sout, const char* sin);


/* built-in predicates, callable from Prolog */
/* ----------------------------------------- */

/* function prototypes */

/* File/directory utilities */

TF EXPFUNC p_delfile(ENGid);
TF EXPFUNC p_chdir(ENGid);
TF EXPFUNC p_rmdir(ENGid);
TF EXPFUNC p_rename(ENGid);
TF EXPFUNC p_curdir(ENGid);
TF EXPFUNC p_mkdir(ENGid);
#ifdef	_WIN32
TF EXPFUNC p_setdrive(ENGid);
TF EXPFUNC p_getdrive(ENGid);
TF EXPFUNC p_findfirst(ENGid);
TF EXPFUNC p_findnext(ENGid);
#endif
#ifdef __unix__
TF EXPFUNC p_fileinfo(ENGid);
#endif

/* SDK functions */

TF EXPFUNC p_gmtime_str(ENGid);
TF EXPFUNC p_gmtime_later_str(ENGid);
TF EXPFUNC p_gethostbyname(ENGid);

/* CGI support functions */

TF EXPFUNC p_cgiOpen(ENGid);
TF EXPFUNC p_cgiWrite(ENGid);
TF EXPFUNC p_cgiWriteUnicode(ENGid);
TF EXPFUNC p_cgiWriteBinaryFile(ENGid);
TF EXPFUNC p_cgiRewind(ENGid);
TF EXPFUNC p_cgiClose(ENGid);
TF EXPFUNC p_cgiExtractFileName(ENGid);
TF EXPFUNC p_cgiExtractPath(ENGid);
TF EXPFUNC p_cgiPathSeparator(ENGid);
TF EXPFUNC p_cgiDecodeURL(ENGid);
TF EXPFUNC p_cgiCleanFiles(ENGid);

/* extended predicate table definitions */

PRED_INIT awinPreds[] = 
{
	/* File/directory utility predicates */
	{"delfile", 2, p_delfile},
	{"chdir", 2, p_chdir},
	{"rmdir", 2, p_rmdir},
	{"rename", 3, p_rename},
	{"curdir", 1, p_curdir},
	{"mkdir", 2, p_mkdir},
#ifdef	_WIN32
	{"setdrive", 1, p_setdrive},
	{"getdrive", 1, p_getdrive},
	{"findfirst", 3, p_findfirst},
	{"findnext", 1, p_findnext},
#endif
#ifdef __unix__
	{"fileinfo", 2, p_fileinfo},
#endif

	/* SDK functions */
	{"gmtime_str", 1, p_gmtime_str},
	{"gmtime_later_str", 2, p_gmtime_later_str},
	{"gethostbyname", 1, p_gethostbyname},

	/* CGI support functions */
	{"cgiOpen", 1, p_cgiOpen},
	{"cgiWrite", 1, p_cgiWrite},
	{"cgiWriteUnicode", 1, p_cgiWriteUnicode},
	{"cgiWriteBinaryFile", 1, p_cgiWriteBinaryFile},
	{"cgiRewind", 0, p_cgiRewind},
	{"cgiClose", 0, p_cgiClose},
	{"cgiExtractFileName", 2, p_cgiExtractFileName},
	{"cgiExtractPath", 2, p_cgiExtractPath},
	{"cgiPathSeparator", 1, p_cgiPathSeparator},
	{"cgiDecodeURL", 2, p_cgiDecodeURL},
	{"cgiCleanFiles", 2, p_cgiCleanFiles},

	{NULL, 0, NULL}
};


/* Global Variables */
/* ---------------- */

#define MAX_PROFILE 4096


/* Initialization Functions */
/* ------------------------ */

/* These are used when this is built as an LSX for debugging under the
   listener.  If you modify AMZICGI.PRO, we strongly suggest you build
   this file as an LSX, otherwise it is very hard to debug changes to
   the WebLS engine. */

/*
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
*/
extern RC InitPreds(ENGid eid, void* p)
 {
	RC rc;
	char buf[80];

	rc = lsInitPreds(eid, awinPreds);
	if (rc)
	{
#ifdef	_WIN32
		wsprintf(buf, "Error #%d Loading CGI Predicates", rc);
		MessageBox(NULL, buf, "CGI LSX", MB_OK);
#else
		printf("Error #%d Loading CGI Predicates", rc);
#endif
	}

	return 0;
}


/* File and Directory Manipulation */
/* ------------------------------- */

/* chdir(Dir, Err) - Changes directory to Dir, returns error code in Err.
   An error code of 0 means success. */

TF EXPFUNC p_chdir(ENGid CurEng)
{
	int	  rc;
	char  Buffer[_MAX_PATH];

	if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
		lsErrRaise(CurEng, "input path too long");
	lsGetParm(CurEng, 1, cSTR, Buffer);
	rc = chdir(Buffer);
	return(lsUnifyParm(CurEng, 2, cINT, &rc));
}

/* curdir(Dir) - Returns the current working directory.
 */
TF EXPFUNC p_curdir(ENGid CurEng)
{
   char	buffer[_MAX_PATH], *p;

   p = getcwd(buffer, _MAX_PATH);
	if (p == NULL)
      return(FALSE);
   else
   {
      lsUnifyParm(CurEng, 1, cSTR, buffer);
      return(TRUE);
   }
}

/* mkdir(Dir, Err) - Makes the directory Dir, returns error code in Err.
   An error code of 0 means success. */

TF EXPFUNC p_mkdir(ENGid CurEng)
{
	int	rc;
	char  Buffer[_MAX_PATH];

	if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
		lsErrRaise(CurEng, "input path too long");
	lsGetParm(CurEng, 1, cSTR, Buffer);
#ifdef __unix__
	rc = mkdir(Buffer, 0);
#else
	rc = mkdir(Buffer);
#endif
	return(lsUnifyParm(CurEng, 2, cINT, &rc));
}

/* rmdir(Dir, Err) - Removes directory Dir, returns error code in Err.
   An error code of 0 means success. */

TF EXPFUNC p_rmdir(ENGid CurEng)
{
	int	 rc;
	char  Buffer[_MAX_PATH];

	if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
		lsErrRaise(CurEng, "input path too long");
	lsGetParm(CurEng, 1, cSTR, Buffer);
	rc = rmdir(Buffer);
	return(lsUnifyParm(CurEng, 2, cINT, &rc));
}

#ifdef	_WIN32
/* setdrive(Drive, Err) - Makes Drive current, returns error code in Err.
   An error code of 0 means success. */

TF EXPFUNC p_setdrive(ENGid CurEng)
{
	char	  c;
	unsigned  i;
	char	  Buffer[_MAX_PATH];

	if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
		lsErrRaise(CurEng, "input path too long");
	lsGetParm(CurEng, 1, cSTR, Buffer);
	c = Buffer[0];
	if (c >= 'a' && c <= 'z')
		i = 1 + c - 'a';
	else if (c >= 'A' && c <= 'Z')
		i = 1 + c - 'A';
	else
		lsErrRaise(CurEng, "Bad drive spec sent to setdrive/1");

	_chdrive(i);
	return(TRUE);
}

/* getdrive(Drive) - Returns the current drive as a letter followed
   by a colon. */

TF EXPFUNC p_getdrive(ENGid CurEng)
{
	unsigned  i;
	char  drive[3];

	i = _getdrive();
	drive[0] = 'a' + i - 1;
	drive[1] = ':';
	drive[2] = '\0';
	lsUnifyParm(CurEng, 1, cSTR, drive);
	return(TRUE);
}

#endif /* _WIN32 */

/* delfile(File, Err) - Removes file File, returns error code in Err.
   An error code of 0 means success. */

TF EXPFUNC p_delfile(ENGid eid)
{
	char	sFile[_MAX_PATH];
	RC		rc, ecode;

	if (_MAX_PATH <= lsStrParmLen(eid, 1) )
		lsErrRaise(eid, "input path too long");
	rc = lsGetParm(eid, 1, cSTR, sFile);

	if (remove(sFile))
		ecode = 0;
	else
		ecode = -1;

	rc = lsUnifyParm(eid, 2, cINT, &ecode);

	return TRUE;
}

/* rename(OldFile, NewFile, Err) - Renames file, returns error code in Err.
   An error code of 0 means success. */

TF EXPFUNC p_rename(ENGid CurEng)
{
	RC	rc;
	int	ecode;
	char	sOldName[_MAX_PATH], sNewName[_MAX_PATH];

	if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
		lsErrRaise(CurEng, "input path too long");
	if (_MAX_PATH <= lsStrParmLen(CurEng, 2) )
		lsErrRaise(CurEng, "output path too long");
	rc = lsGetParm(CurEng, 1, cSTR, sOldName);
	rc = lsGetParm(CurEng, 2, cSTR, sNewName);

	ecode = rename(sOldName, sNewName);

	rc= lsUnifyParm(CurEng, 3, cINT, &ecode);
	return(TRUE);
}

#ifdef __unix__
/* stat(FileName,
   fileinfo(Size))
*/
TF EXPFUNC p_fileinfo(ENGid CurEng)
{
	char		file_name[_MAX_PATH];
	TERM		fileinfoT;
	int		rc;
	struct stat	file_stat;

	if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
		lsErrRaise(CurEng, "input path too long");
	lsGetParm(CurEng, 1, cSTR, &file_name);

	rc = stat(file_name, &file_stat);

	lsMakeFA(CurEng, &fileinfoT, "fileinfo", 1);

	lsUnifyArg(CurEng, &fileinfoT, 1, cINT, &(file_stat.st_size));

	return(lsUnifyParm(CurEng, 2, cTERM, &fileinfoT));
}
#endif

#ifdef _WIN32

/* Global definitions for findfirst family */

long			FileHand;
struct			_finddata_t FT;
unsigned int	mask;

/* findfirst(Pattern, Mask, 
   fileinfo(Name,Attr,time(Hour,Min,Sec),date(Year,Mon,Day),Size))
   Finds the first file that matches the Pattern and Mask */

TF EXPFUNC p_findfirst(ENGid CurEng)
{
	unsigned short	attrib;
	long			time, date;
	unsigned short	hour, min, sec;
	unsigned short	year, month, day;
	TERM			fileinfoT, dateT, timeT;
	char			pattern[_MAX_PATH];

	if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
		lsErrRaise(CurEng, "input path too long");
	lsGetParm(CurEng, 1, cSTR, &pattern);
	lsGetParm(CurEng, 2, cINT, &mask);
	if (-1 == (FileHand = _findfirst(pattern, &FT)))
		return(FALSE);
	/* Make sure the one we have is an acceptable type */
	while (((FT.attrib & 31) & mask ) == 0 && (FT.attrib & 31) != mask)
	{
			if (-1 == _findnext(FileHand, &FT))
				return(FALSE);
	}

	lsMakeFA(CurEng, &fileinfoT, "fileinfo", 5);

	lsUnifyArg(CurEng, &fileinfoT, 1, cSTR, &(FT.name));

	attrib = FT.attrib;
	lsUnifyArg(CurEng, &fileinfoT, 2, cSHORT, &attrib);

	time = FT.time_access;
	lsMakeFA(CurEng, &timeT, "time", 3);
	hour = (time & 0xf800) >> 11;
	min = (time & 0x07e0) >> 5;
	sec = time & 0x001f;
	lsUnifyArg(CurEng, &timeT, 1, cSHORT, &hour);
	lsUnifyArg(CurEng, &timeT, 2, cSHORT, &min);
	lsUnifyArg(CurEng, &timeT, 3, cSHORT, &sec);
	lsUnifyArg(CurEng, &fileinfoT, 3, cTERM, &timeT);

	date = FT.time_access;
	lsMakeFA(CurEng, &dateT, "date", 3);
	year = (((date & 0xfe00) >> 9) + 80) % 100;
	month = (date & 0x1e0) >> 5;
	day = date & 0x001f;
	lsUnifyArg(CurEng, &dateT, 1, cSHORT, &year);
	lsUnifyArg(CurEng, &dateT, 2, cSHORT, &month);
	lsUnifyArg(CurEng, &dateT, 3, cSHORT, &day);
	lsUnifyArg(CurEng, &fileinfoT, 4, cTERM, &dateT);

	lsUnifyArg(CurEng, &fileinfoT, 5, cLONG, &(FT.size));

	return(lsUnifyParm(CurEng, 3, cTERM, &fileinfoT));
}

/* findnext(fileinfo(Name,Attr,time(Hour,Min,Sec),date(Year,Mon,Day),Size)) */
/*   Finds the next file that matches the previous Pattern and Mask */

TF EXPFUNC p_findnext(ENGid CurEng)
{
	TERM				fileinfoT, dateT, timeT;
	unsigned short	attrib;
	long				time, date;
	unsigned short	hour, min, sec;
	unsigned short	year, month, day;

	if (-1 == _findnext(FileHand, &FT))
		return(FALSE);

	/* Make sure the one we have is an acceptable type */
	while (((FT.attrib & 31) & mask) == 0 && (FT.attrib & 31) != mask)
	{
		if (-1 == _findnext(FileHand, &FT))
			return(FALSE);
	}

	lsMakeFA(CurEng, &fileinfoT, "fileinfo", 5);

	lsUnifyArg(CurEng, &fileinfoT, 1, cSTR, &(FT.name));

	attrib = FT.attrib;
	lsUnifyArg(CurEng, &fileinfoT, 2, cSHORT, &attrib);

	time = FT.time_access;
	lsMakeFA(CurEng, &timeT, "time", 3);
	hour = (time & 0xf800) >> 11;
	min = (time & 0x07e0) >> 5;
	sec = time & 0x001f;
	lsUnifyArg(CurEng, &timeT, 1, cSHORT, &hour);
	lsUnifyArg(CurEng, &timeT, 2, cSHORT, &min);
	lsUnifyArg(CurEng, &timeT, 3, cSHORT, &sec);
	lsUnifyArg(CurEng, &fileinfoT, 3, cTERM, &timeT);

	date = FT.time_access;
	lsMakeFA(CurEng, &dateT, "date", 3);
	year = (((date & 0xfe00) >> 9) + 80) % 100;
	month = (date & 0x1e0) >> 5;
	day = date & 0x001f;
	lsUnifyArg(CurEng, &dateT, 1, cSHORT, &year);
	lsUnifyArg(CurEng, &dateT, 2, cSHORT, &month);
	lsUnifyArg(CurEng, &dateT, 3, cSHORT, &day);
	lsUnifyArg(CurEng, &fileinfoT, 4, cTERM, &dateT);

	lsUnifyArg(CurEng, &fileinfoT, 5, cLONG, &(FT.size));

	return(lsUnifyParm(CurEng, 1, cTERM, &fileinfoT));
}

#endif /* _WIN32 */


/* SDK Functions */
/* ------------- */

/* gmtime_str(time) returns the current GMT time. */

TF EXPFUNC p_gmtime_str(ENGid eid)
{
	struct tm	*gmt;
	long		ltime;
	char		sBuf[40];
	TF			tf;

	time(&ltime);
	gmt = gmtime(&ltime);
	strftime(sBuf, 40, "%a, %d-%b-%Y %H:%M:%S GMT", gmt);
	tf = lsUnifyParm(eid, 1, cSTR, sBuf);

	return tf;
}

/* gmtime_later_str(increment, time) returns the current GMT time plus the increment specified. */

TF EXPFUNC p_gmtime_later_str(ENGid eid)
{
	struct tm	*gmt;
	long		ltime, inc;
	char		sBuf[40];
	TF			tf;
	RC			rc;

	rc = lsGetParm(eid, 1, cINT, &inc);
	if (rc != 0) return FALSE;
	time(&ltime);
	ltime = ltime + inc;
	gmt = gmtime(&ltime);
	strftime(sBuf, 40, "%a, %d-%b-%Y %H:%M:%S GMT", gmt);
	tf = lsUnifyParm(eid, 2, cSTR, sBuf);

	return tf;
}

/* gethostbyname succeeds if the host exists */

TF EXPFUNC p_gethostbyname(ENGid eid)
{
#ifndef __sun
	char hostname[1000];
	int rc;

	/* For Windows must call WSAStartup first! (Not implemented) */
	/* Does not work under Solaris */
	if (lsStrParmLen(eid, 1) > 1000)
		lsErrRaise(eid, "host name too long");
	rc = lsGetParm(eid, 1, cSTR, hostname);
	if (gethostbyname(hostname) == NULL)
		return(FALSE);
	else
		return(TRUE);
#else
	return(FALSE);
#endif
}

/* CGI Support Functions */
/* --------------------- */

/* Global definitions for CGI output functions */

FILE	*outFile = NULL;
char	outName[_MAX_PATH];

/* These routines are used to open, write, rewind and close the
   output file, which is stdout in standard CGI */

/* cgiOpen(File) opens the named file (or if blank stdout) for write */

TF EXPFUNC p_cgiOpen(ENGid eid)
{
	int		rc;

	if (_MAX_PATH <= lsStrParmLen(eid, 1) )
		lsErrRaise(eid, "file name too long");
	rc = lsGetParm(eid, 1, cSTR, outName);

	if (strlen(outName) > 0)
	{
		outFile = fopen(outName, "w");
	}
	else
	{
		outFile = stdout;
	}

	if (outFile == NULL)
	{
#ifdef DEBUG
		fprintf(debugFile, "cgiOpen false\n");
		fflush(debugFile);
#endif
		return(FALSE);
	}
	else
	{
#ifdef DEBUG
		fprintf(debugFile, "cgiOpen true\n");
		fflush(debugFile);
#endif
		return(TRUE);
	}
}

/* cgiWriteUnicode(Term) writes the atom, string, structure, list to the output file 
   by getting it as a Unicode string and converting it to UTF-8
   No,that would be nice, but wcstombs does not do utf8.  Need special library
   to make it work, see engine.*/

RC LSAPI  lsTermToStrW(ENGid, TERM, wchar_t*, int);

TF EXPFUNC p_cgiWriteUnicode(ENGid eid)
{
	wchar_t	sBuf[100000];
	char	tBuf[200000];
	TERM	term;
	int		rc, ecode;

#ifdef DEBUG
	fprintf(debugFile, "cgiWriteUnicode\n");
	fflush(debugFile);
#endif

	rc = lsGetParm(eid, 1, cTERM, &term);
	if (rc != 0) return(FALSE);

	rc = lsTermToStrW(eid, term, sBuf, 100000);
	if (rc != 0) return(FALSE);

	if (wcslen(sBuf) == 0)
		return(TRUE);

   rc = wcstombs(tBuf, sBuf, 200000);
      //copy it's NOT utf8, wcstombs doesn't work...utf8line to sout
#ifdef DEBUG
	fprintf(debugFile, "cgiWriteUnicode rc,string=%d,%s\n", rc, tBuf);
	fflush(debugFile);
#endif

	ecode = fputs(tBuf, outFile);
	if (ecode < 0)
		return(FALSE);
	else
		return(TRUE);
}

/* cgiWrite(Term) writes the atom, string, structure, list to the output file */

TF EXPFUNC p_cgiWrite(ENGid eid)
{
	char	sBuf[100000];
	TERM	term;
	int		rc, ecode;

#ifdef DEBUG
	fprintf(debugFile, "cgiWrite\n");
	fflush(debugFile);
#endif

	rc = lsGetParm(eid, 1, cTERM, &term);
	if (rc != 0) return(FALSE);

	rc = lsTermToStr(eid, term, sBuf, 100000);
	if (rc != 0) return(FALSE);

	if (strlen(sBuf) == 0)
		return(TRUE);

#ifdef DEBUG
	fprintf(debugFile, "cgiWrite string=%s\n", sBuf);
	fflush(debugFile);
#endif

	ecode = fputs(sBuf, outFile);
	if (ecode < 0)
		return(FALSE);
	else
		return(TRUE);
}

/* cgiWriteBinaryFile(FileName) sends the file out */

TF EXPFUNC p_cgiWriteBinaryFile(ENGid eid)
{
	unsigned char	buf[8192];
	char	filename[_MAX_PATH];
	FILE	*f;
	int		rc, mode;
	size_t	count;

	rc = lsGetParm(eid, 1, cSTR, filename);
	if (rc != 0) return(FALSE);
	f = fopen(filename, "rb");
	if (f == NULL)
		return(FALSE);

	/* Change the output file to binary mode to prevent crlf translation */
#if _WIN32
	mode = _setmode(fileno(outFile), _O_BINARY);
	if (mode == -1) return(FALSE);
#endif
	while (!feof(f))
	{
		count = fread(buf, sizeof(unsigned char), 8192, f);
		if (ferror(f)) goto cgiWriteBinaryFileError;
		if (count > 0)
		{
			fwrite(buf, sizeof(unsigned char), count, outFile);
			if (ferror(outFile)) goto cgiWriteBinaryFileError;
		}
	}
	fclose(f);
	return(TRUE);

cgiWriteBinaryFileError:
#if _WIN32
	_setmode(fileno(outFile), mode);
#endif
	return(FALSE);
}

/* cgiRewind rewinds the output file to the beginning */

TF EXPFUNC p_cgiRewind(ENGid eid)
{
#ifdef DEBUG
	fprintf(debugFile, "cgiRewind\n");
	fflush(debugFile);
#endif
	if (outFile == stdout)
		rewind(outFile);
	else
	{
		if (outFile != NULL) fclose(outFile);
		outFile = fopen(outName, "w");
	}

	if (outFile == NULL)
		return(FALSE);
	else
		return(TRUE);
}


/* cgiClose closes the output file */

TF EXPFUNC p_cgiClose(ENGid eid)
{
#ifdef DEBUG
	fprintf(debugFile, "cgiClose\n");
	fflush(debugFile);
#endif

	if (outFile != stdout && outFile != NULL)
		fclose(outFile);
	return(TRUE);
}

/* cgiExtractProgramName gets the file name off the end of the path */

TF EXPFUNC p_cgiExtractFileName(ENGid eid)
{
	RC		rc;
	TF		tf;
	char	sPath[_MAX_PATH], sProg[_MAX_PATH], *p, *q;
	int		i;

#ifdef DEBUG
		fprintf(debugFile, "cgiExtractProgramName\n");
		fflush(debugFile);
#endif

	/* Get the path to purge files from */
	if (_MAX_PATH <= lsStrParmLen(eid, 1) )
		lsErrRaise(eid, "String too long");
	rc = lsGetParm(eid, 1, cSTR, sPath);
	p = strrchr(sPath, '\\');
	if (p == NULL)
	{
		p = strrchr(sPath, '/');
		if (p == NULL)
			p = &sPath[0];
	}
	p++;
	q = strchr(p, '.');
	if (q == NULL)
		q = &sPath[strlen(sPath)];
	i = 0;
	while (p != q)
	{
		sProg[i] = *p;
		p++; i++;
	}
	sProg[i] = '\0';

	tf = lsUnifyParm(eid, 2, cATOM, sProg);

	return tf;
}

/* cgiExtractPath removes the program name from a path */

TF EXPFUNC p_cgiExtractPath(ENGid eid)
{
	RC		rc;
	TF		tf;
	char	sPath[_MAX_PATH], *p;

#ifdef DEBUG
		fprintf(debugFile, "cgiExtractPath");
		fflush(debugFile);
#endif

	/* Get the path to purge files from */
	if (_MAX_PATH <= lsStrParmLen(eid, 1) )
		lsErrRaise(eid, "String too long");
	rc = lsGetParm(eid, 1, cSTR, sPath);
	p = strrchr(sPath, '\\');
	if (p == NULL)
	{
		p = strrchr(sPath, '/');
		if (p == NULL)
			p = &sPath[0];
	}
	p++;
   *p = '\0';

	tf = lsUnifyParm(eid, 2, cATOM, sPath);

	return tf;
}

/* cgiPathSeparator returns the path separator character as a string */

#if _WIN32
TF EXPFUNC p_cgiPathSeparator(ENGid eid)
{
	TF		tf;

   tf = lsUnifyParm(eid, 1, cSTR, "\\");
   return tf;
}
#endif

#ifdef __unix__
TF EXPFUNC p_cgiPathSeparator(ENGid eid)
{
	TF		tf;

   tf = lsUnifyParm(eid, 1, cSTR, "/");
   return tf;
}
#endif

/* cgiCleanFiles  */

#if _WIN32
TF EXPFUNC p_cgiCleanFiles(ENGid eid)
{
	long	FileHand;
	struct	_finddata_t FT;
	char	pattern[_MAX_PATH], dir[_MAX_PATH];
	char	olddir[_MAX_PATH], *p;
	long	ftime, ltime;
	int		rc;

	if (_MAX_PATH <= lsStrParmLen(eid, 1) )
		lsErrRaise(eid, "Input path too long");
	lsGetParm(eid, 1, cSTR, &dir);
	lsGetParm(eid, 2, cSTR, &pattern);

	/* Change directory to the right place; make sure this works */
	p = getcwd(olddir, _MAX_PATH);
	if (p == NULL) return(FALSE);
	rc = chdir(dir);
	if (rc != 0) return(FALSE);

	/* Findfirst file */
	if (-1 == (FileHand = _findfirst(pattern, &FT)))
		return(TRUE);

	do 
	{
		/* Delete it if its over 24 hours old */
		ftime = FT.time_write;
		time(&ltime);

		if (ftime != -1L && ltime - ftime > 86400)
			remove(FT.name);
	}
	while (-1 != _findnext(FileHand, &FT));

	chdir(olddir);

	return(TRUE);
}
#endif

#ifdef __unix__
/* Note: pattern must be *.ext !!!! */
TF EXPFUNC p_cgiCleanFiles(ENGid eid)
{
	char	pattern[_MAX_PATH], dir[_MAX_PATH];
	char	olddir[_MAX_PATH], *p;
	long	ltime;
	DIR		*cgidir;
	int		rc;
	struct	dirent *ent;
	struct	stat file_stat;

	if (_MAX_PATH <= lsStrParmLen(eid, 1) )
		lsErrRaise(eid, "Input path too long");
	lsGetParm(eid, 1, cSTR, &dir);
	lsGetParm(eid, 2, cSTR, &pattern);
	
	strcpy(dir, "/var/www/cgi-bin");
	strcpy(pattern, "*.kws");

	/* Change directory to the right place; make sure this works */
	p = getcwd(olddir, _MAX_PATH);
	if (p == NULL) return(FALSE);
	rc = chdir(dir);
	if (rc != 0) return(FALSE);

	/* Open directory */
	if (!(cgidir = opendir(dir)))
	{
		chdir(olddir);
		return(FALSE);
	}

	errno = 0;
	while ((ent = readdir(cgidir)))
	{
		/* See if it ends in the extension we expect */
		if (strstr(ent->d_name, &pattern[1]) != NULL)
		{
			/* Delete it if its over 24 hours old */
			rc = stat(ent->d_name, &file_stat);
			time(&ltime);
			if (ltime - file_stat.st_mtime > 86400)
				unlink(ent->d_name);
		}
	}
	
	closedir(cgidir);
	chdir(olddir);

	return(TRUE);
}
#endif

/* Convert a URL by changing pluses to spaces and &nn to characters */

extern void decode_url(char *);

TF EXPFUNC p_cgiDecodeURL(ENGid eid)
{
   TF    tf;
   RC    rc;
   int   len;
   char  *buf;

   len = lsStrParmLen(eid, 1);
   buf = (char*)malloc(len);
   if (buf == NULL) return FALSE;
   rc = lsGetParm(eid, 1, cSTR, buf);
   if (rc != 0) return FALSE;
   decode_url(buf);
   tf = lsUnifyParm(eid, 2, cSTR, buf);
   return tf;
}


/* -------------------- Utility Routines -------------------- */

void doublechar1(char* s, char c)
{
	int	nslash=0;
	int	len=0;
	int	i;
	char* s1;
	char* s2;
	
	if (strlen(s) == 0) return;

	/* Count the quotes, then copy in place from right to
		left so we don't write on ourself. */
		
	s1 = s;
	while(*s1)
	{
		len++;
		if (*s1++ == c) nslash++;
	}
	
	s2 = s + len + nslash;
	s1 = s + len;
	
	for(i=0; i<=len; i++)
	{
		if (*s1 == c)
		{
			*s2-- = *s1;
			*s2-- = *s1--;
		}
		else
			*s2-- = *s1--;
	}
	
	return;
}

/* Any string of characters sent to Prolog is read using the
   Prolog reader, which interprets a backslash as an escape
   character.  So, this means if you really want a backslash,
   such as in a file path, then it has to be a double
   backslash.  This function makes that conversion. */

void slashslash2(char* sout, const char* sin)
{
	while(*sin)
	{
		if (*sin == '\\')
		{
			*sout++ = *sin;
			*sout++ = *sin++;
		}
		else
			*sout++ = *sin++;
	}
	*sout = *sin;
	return;
}

/* Same as slashslash2 except conversion is done in place.
   Therefore, the input buffer must be large enough to handle
   the expanded string. */

void slashslash1(char* s)
{
	int	nslash=0;
	int	len=0;
	int	i;
	char* s1;
	char* s2;
	
	if (strlen(s) == 0) return;

	/* Count the slashes, then copy in place from right to
		left so we don't write on ourself. */
		
	s1 = s;
	while(*s1)
	{
		len++;
		if (*s1++ == '\\') nslash++;
	}
	
	s2 = s + len + nslash;
	s1 = s + len;
	
	for(i=0; i<=len; i++)
	{
		if (*s1 == '\\')
		{
			*s2-- = *s1;
			*s2-- = *s1--;
		}
		else
			*s2-- = *s1--;
	}
	
	return;
}



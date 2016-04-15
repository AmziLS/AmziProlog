/*********************************************************************\
*  osutils.cpp - Extended OS Dependent Predicates
*
*  Copyright (c) 1992-2012 by Amzi! inc.  All Rights Reserved.
*
*
\**********************************************************************/

#ifdef _UNICODE
#ifndef UNICODE
#define UNICODE
#endif
#endif

/* Solaris is Unix for osutils */
//#ifdef __sun
//#define  __unix__
//#endif

#include <ctype.h>
#include <time.h>

#ifdef _WIN32
#include <tchar.h>
#include <direct.h>
#include <io.h>
#include <afxwin.h>  // MFC core and standard components
#include <afxext.h>  // MFC extensions (including VB)
#endif /* _WIN32 */

#ifdef __unix__
#include <errno.h>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <unistd.h>
#include <sys/stat.h>
#define _MAX_PATH 512
#define _TCHAR char
#define _T(x) x
#define UNIX
#endif /* __unix__ */

#ifdef __APPLE__
#include <errno.h>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <unistd.h>
#include <sys/stat.h>
#define _MAX_PATH 512
#define _TCHAR char
#define _T(x) x
#define UNIX
#endif /* __APPLE__ */

#include "amzi.h"

void slashslash1(_TCHAR *s);
void slashslash2(_TCHAR* sout, const _TCHAR* sin);

// Windows operating system version, use to customize certain
// functions, such as findfiles, for Unicode/non-Unicode use.
enum OSVer
{
   OtherOS,
   W95,
   WNT
};

OSVer g_osver;

void get_osver();

/* built-in predicates, callable from Prolog */
/* ----------------------------------------- */

/* function prototypes */

// File/directory utilities

TF EXPFUNC p_delfile(ENGid);
TF EXPFUNC p_chdir1(ENGid);
TF EXPFUNC p_chdir2(ENGid);
TF EXPFUNC p_curdir(ENGid);
TF EXPFUNC p_mkdir(ENGid);
TF EXPFUNC p_rmdir(ENGid);
TF EXPFUNC p_rename(ENGid);
#ifdef _WIN32
TF EXPFUNC p_get_env_var(ENGid);
TF EXPFUNC p_setdrive(ENGid);
TF EXPFUNC p_getdrive(ENGid);
TF EXPFUNC p_findfirst(ENGid);
TF EXPFUNC p_findnext(ENGid);
#endif
#ifdef __unix__
TF EXPFUNC p_stat(ENGid);
#endif

// Common Windows functions

#ifdef _WIN32
TF EXPFUNC p_msgbox(ENGid);
TF EXPFUNC p_tfmsgbox(ENGid);
TF EXPFUNC p_getfile(ENGid);
#endif

/* extended predicate table definitions */

PRED_INIT extPreds[] = 
{
   // File/directory utility predicates
   {_T("delfile"), 2, p_delfile},
   {_T("chdir"), 1, p_chdir1},
   {_T("chdir"), 2, p_chdir2},
   {_T("curdir"), 1, p_curdir},
   {_T("mkdir"), 2, p_mkdir},
   {_T("rmdir"), 2, p_rmdir},
   {_T("rename"), 3, p_rename},
#ifdef _WIN32
   {_T("get_env_var"), 2, p_get_env_var},
   {_T("setdrive"), 1, p_setdrive},
   {_T("getdrive"), 1, p_getdrive},
   {_T("findfirst"), 4, p_findfirst},
   {_T("findnext"), 2, p_findnext},
#endif
#ifdef __unix__
   {_T("stat"), 9, p_stat},
#endif

   // Common Windows functions
#ifdef _WIN32
   {_T("w_msgbox"), 1, p_msgbox},
   {_T("w_tfmsgbox"), 1, p_tfmsgbox},
   {_T("w_getfile"), 1, p_getfile},
#endif

   {NULL, 0, NULL}
};

/* global variables */
/* ---------------- */

#define MAX_PROFILE 4096

/* DLL Entry and Exit Points */
/* ------------------------- */

#ifdef _WIN32
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
#endif /* _WIN32 */

/* Required LSX Initialization Function */
/* ------------------------------------ */

#ifdef _WIN32
extern "C" __declspec(dllexport) RC EXPFUNC InitPreds(ENGid eid, void* p)
#else
extern "C" RC EXPFUNC InitPreds(ENGid eid, void* p)
#endif
{
   RC rc;
   _TCHAR buf[80];

   rc = lsInitPreds(eid, extPreds);
   if (rc)
   {
#ifdef _WIN32
      _stprintf(buf, _T("Error #%d Loading osutils Predicates"), rc);
      MessageBox(NULL, buf, _T("osutils lsx"), MB_OK);
#else
      printf(_T("Error #%d Loading osutils Predicates"), rc);
#endif
   }
   //else
   //   MessageBox(NULL, "A4WIN Predicates Loaded", "A4WIN LSX", MB_OK);

   // Get the operation system version for those functions that care.
   get_osver();

   return 0;
}

/* File and directory manipulation */
/* ------------------------------- */

TF EXPFUNC p_chdir1(ENGid CurEng)
{
/*   chdir(Dir) - Changes directory to Dir, returns
   FALSE if error occurred. */

   int     rc;
   char  Buffer[_MAX_PATH];

   if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
      lsErrRaise(CurEng, _T("input path too long"));
   lsGetParm(CurEng, 1, cASTR, Buffer);
   rc = chdir(Buffer);
   if (rc == 0)
     return TRUE;
   else
     return FALSE;
}

TF EXPFUNC p_chdir2(ENGid CurEng)
{
/* chdir(Dir, Err) - Changes directory to Dir, returns error code in Err.
                     An error code of 0 means success. */

   int     rc;
   char  Buffer[_MAX_PATH];

   if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
      lsErrRaise(CurEng, _T("input path too long"));
   lsGetParm(CurEng, 1, cASTR, Buffer);
   rc = chdir(Buffer);
   return(lsUnifyParm(CurEng, 2, cINT, &rc));
}

/* curdir(Dir) - Returns the current working directory.
 */
TF EXPFUNC p_curdir(ENGid CurEng)
{
   char  buffer[_MAX_PATH], *p;
   int     zero = 0;

   p = getcwd(buffer, _MAX_PATH);
   if (p == NULL)
      return(FALSE);
   else
   {
      lsUnifyParm(CurEng, 1, cASTR, buffer);
      return TRUE;
   }
}

TF EXPFUNC p_mkdir(ENGid CurEng)
{
/* mkdir(Dir, Err) - Makes the directory Dir, returns error code in Err.
                     An error code of 0 means success. */

   int   rc;
   char  Buffer[_MAX_PATH];

   if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
      lsErrRaise(CurEng, _T("input path too long"));
   lsGetParm(CurEng, 1, cASTR, Buffer);
#ifdef UNIX
	rc = mkdir(Buffer, 0);
#else
   rc = mkdir(Buffer);
#endif
   return(lsUnifyParm(CurEng, 2, cINT, &rc));
}

TF EXPFUNC p_rmdir(ENGid CurEng)
{
/* rmdir(Dir, Err) - Removes directory Dir, returns error code in Err.
                     An error code of 0 means success. */

   int    rc;
   char  Buffer[_MAX_PATH];

   if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
      lsErrRaise(CurEng, _T("input path too long"));
   lsGetParm(CurEng, 1, cASTR, Buffer);
   rc = rmdir(Buffer);
   return(lsUnifyParm(CurEng, 2, cINT, &rc));
}

#ifdef _WIN32
/* get_env_var(VAR, VALUE) - Returns the current value of
   an environment variable. */

TF EXPFUNC p_get_env_var(ENGid CurEng)
{
   char  var[512];

   if ( 511 <= lsStrParmLen(CurEng, 1) )
      lsErrRaise(CurEng, _T("input variable too long"));
   lsGetParm(CurEng, 1, cASTR, var);

   char *value = getenv(var);
   if (value == NULL) return(FALSE);

   lsUnifyParm(CurEng, 2, cASTR, value);
   return(TRUE);
}

/* setdrive(Drive, Err) - Makes Drive current, returns error code in Err.
   An error code of 0 means success. */

TF EXPFUNC p_setdrive(ENGid CurEng)
{
   char     c;
   unsigned  i;
   char     Buffer[_MAX_PATH];

   if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
      lsErrRaise(CurEng, _T("input path too long"));
   lsGetParm(CurEng, 1, cASTR, Buffer);
   c = Buffer[0];
   if (c >= 'a' && c <= 'z')
      i = 1 + c - 'a';
   else if (c >= 'A' && c <= 'Z')
      i = 1 + c - 'A';
   else
      lsErrRaise(CurEng, _T("Bad drive spec sent to setdrive/1"));

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
   lsUnifyParm(CurEng, 1, cASTR, drive);
   return(TRUE);
}
#endif /* _WIN32 */

/* delfile(File, Err) - Removes file File, returns error code in Err.
   An error code of 0 means success. */

TF EXPFUNC p_delfile(ENGid eid)
{
   char   sFile[_MAX_PATH];
   RC      rc, ecode;

   if (_MAX_PATH <= lsStrParmLen(eid, 1) )
      lsErrRaise(eid, _T("input path too long"));
   rc = lsGetParm(eid, 1, cASTR, sFile);

   ecode = remove(sFile);

#ifdef _WIN32
   rc = lsUnifyParm(eid, 2, cINT, &ecode);
#else
   if (ecode == 0)
     rc = lsUnifyParm(eid, 2, cINT, &ecode);
   else
     rc = lsUnifyParm(eid, 2, cINT, &errno);
#endif

   return TRUE;
}

/* rename(OldFile, NewFile, Err) - Renames file, returns error code in Err.
   An error code of 0 means success. */

TF EXPFUNC p_rename(ENGid CurEng)
{
   RC   rc;
   int   ecode;
   char   sOldName[_MAX_PATH], sNewName[_MAX_PATH];

   if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
      lsErrRaise(CurEng, _T("input path too long"));
   if (_MAX_PATH <= lsStrParmLen(CurEng, 2) )
      lsErrRaise(CurEng, _T("output path too long"));
   rc = lsGetParm(CurEng, 1, cASTR, sOldName);
   rc = lsGetParm(CurEng, 2, cASTR, sNewName);

   ecode = rename(sOldName, sNewName);

   rc= lsUnifyParm(CurEng, 3, cINT, &ecode);
   return(TRUE);
}

#ifdef _WIN32
// In order to have a single LSX (DLL) that runs using wide character
// functions under Windows NT, and ascii character versions of the
// system functions under Windows 95, you need to dynamically dispatch
// certain functions, such as findfiles.

TF w95_findfirst(ENGid);
TF wNT_findfirst(ENGid);
TF w95_findnext(ENGid);
TF wNT_findnext(ENGid);

TF EXPFUNC p_findfirst(ENGid CurEng)
{
   switch(g_osver)
   {
   case W95: return w95_findfirst(CurEng);
   case WNT: return wNT_findfirst(CurEng);
   default:
      return FALSE;
   }
}

TF EXPFUNC p_findnext(ENGid CurEng)
{
   switch(g_osver)
   {
   case W95: return w95_findnext(CurEng);
   case WNT: return wNT_findnext(CurEng);
   default:
      return FALSE;
   }
}

/* Global definitions for findfirst family */

long         FileHand;
struct         _tfinddata_t wFT;   // wide version
struct          _finddata_t aFT;    // ascii version
unsigned int   mask;

/* findfirst(Pattern, Mask, 
     fileinfo(Name,Attr,time(Hour,Min,Sec),date(Year,Mon,Day),Size))
   - Finds the first file that matches the Pattern and Mask */
TF wNT_findfirst(ENGid CurEng)
{
   unsigned short   attrib;
   tm*            time;
   int   hour, min, sec;
   int   year, month, day;
   TERM         fileinfoT, dateT, timeT;
   _TCHAR         pattern[_MAX_PATH];

   if (_MAX_PATH <= lsStrParmLen(CurEng, 2) )
      lsErrRaise(CurEng, _T("input path too long"));
   lsGetParm(CurEng, 2, cSTR, &pattern);
   lsGetParm(CurEng, 3, cINT, &mask);
   if (-1 == (FileHand = _tfindfirst(pattern, &wFT)))
      return(FALSE);
   lsUnifyParm(CurEng, 1, cADDR, &FileHand);
   /* Make sure the one we have is an acceptable type */
   while (((wFT.attrib & 31) & mask ) == 0 && (wFT.attrib & 31) != mask)
   {
         if (-1 == _tfindnext(FileHand, &wFT))
            return(FALSE);
   }

   lsMakeFA(CurEng, &fileinfoT, _T("fileinfo"), 5);

   lsUnifyArg(CurEng, &fileinfoT, 1, cSTR, &(wFT.name));

   attrib = wFT.attrib;
   lsUnifyArg(CurEng, &fileinfoT, 2, cSHORT, &attrib);

   time = localtime(&wFT.time_write);
   lsMakeFA(CurEng, &timeT, _T("time"), 3);
   hour = time->tm_hour;
   min = time->tm_min;
   sec = time->tm_sec;

   lsUnifyArg(CurEng, &timeT, 1, cINT, &hour);
   lsUnifyArg(CurEng, &timeT, 2, cINT, &min);
   lsUnifyArg(CurEng, &timeT, 3, cINT, &sec);
   lsUnifyArg(CurEng, &fileinfoT, 3, cTERM, &timeT);

   lsMakeFA(CurEng, &dateT, _T("date"), 3);
   year = time->tm_year + 1900;
   month = time->tm_mon + 1;
   day = time->tm_mday;

   lsUnifyArg(CurEng, &dateT, 1, cINT, &year);
   lsUnifyArg(CurEng, &dateT, 2, cINT, &month);
   lsUnifyArg(CurEng, &dateT, 3, cINT, &day);
   lsUnifyArg(CurEng, &fileinfoT, 4, cTERM, &dateT);

   lsUnifyArg(CurEng, &fileinfoT, 5, cLONG, &(wFT.size));

   return(lsUnifyParm(CurEng, 4, cTERM, &fileinfoT));
}

/* findnext(fileinfo(Name,Attr,time(Hour,Min,Sec),date(Year,Mon,Day),Size))
   - Finds the next file that matches the previous Pattern and Mask */
TF wNT_findnext(ENGid CurEng)
{
   TERM            fileinfoT, dateT, timeT;
   unsigned short   attrib;
   tm*            time;
   int   hour, min, sec;
   int   year, month, day;

   lsGetParm(CurEng, 1, cADDR, &FileHand);
   if (-1 == _tfindnext(FileHand, &wFT))
      return(FALSE);

   /* Make sure the one we have is an acceptable type */
   while (((wFT.attrib & 31) & mask) == 0 && (wFT.attrib & 31) != mask)
   {
      if (-1 == _tfindnext(FileHand, &wFT))
         return(FALSE);
   }

   lsMakeFA(CurEng, &fileinfoT, _T("fileinfo"), 5);

   lsUnifyArg(CurEng, &fileinfoT, 1, cSTR, &(wFT.name));

   attrib = wFT.attrib;
   lsUnifyArg(CurEng, &fileinfoT, 2, cSHORT, &attrib);

   time = localtime(&wFT.time_write);
   lsMakeFA(CurEng, &timeT, _T("time"), 3);
   hour = time->tm_hour;
   min = time->tm_min;
   sec = time->tm_sec;
   lsUnifyArg(CurEng, &timeT, 1, cINT, &hour);
   lsUnifyArg(CurEng, &timeT, 2, cINT, &min);
   lsUnifyArg(CurEng, &timeT, 3, cINT, &sec);
   lsUnifyArg(CurEng, &fileinfoT, 3, cTERM, &timeT);

   lsMakeFA(CurEng, &dateT, _T("date"), 3);
   year = time->tm_year + 1900;
   month = time->tm_mon + 1;
   day = time->tm_mday;
   lsUnifyArg(CurEng, &dateT, 1, cINT, &year);
   lsUnifyArg(CurEng, &dateT, 2, cINT, &month);
   lsUnifyArg(CurEng, &dateT, 3, cINT, &day);
   lsUnifyArg(CurEng, &fileinfoT, 4, cTERM, &dateT);

   lsUnifyArg(CurEng, &fileinfoT, 5, cLONG, &(wFT.size));

   return(lsUnifyParm(CurEng, 2, cTERM, &fileinfoT));
}


/* findfirst(Pattern, Mask, 
     fileinfo(Name,Attr,time(Hour,Min,Sec),date(Year,Mon,Day),Size))
   - Finds the first file that matches the Pattern and Mask */
TF w95_findfirst(ENGid CurEng)
{
   unsigned short   attrib;
   tm*   time;
   int   hour, min, sec;
   int   year, month, day;
   TERM         fileinfoT, dateT, timeT;
   char         pattern[_MAX_PATH];

   if (_MAX_PATH <= lsStrParmLen(CurEng, 2) )
      lsErrRaise(CurEng, _T("input path too long"));
   lsGetParm(CurEng, 2, cASTR, &pattern);
   lsGetParm(CurEng, 3, cINT, &mask);
   if (-1 == (FileHand = _findfirst(pattern, &aFT)))
      return(FALSE);
   lsUnifyParm(CurEng, 1, cADDR, &FileHand);
   /* Make sure the one we have is an acceptable type */
   while (((aFT.attrib & 31) & mask ) == 0 && (aFT.attrib & 31) != mask)
   {
         if (-1 == _findnext(FileHand, &aFT))
            return(FALSE);
   }

   lsMakeFA(CurEng, &fileinfoT, _T("fileinfo"), 5);

   lsUnifyArg(CurEng, &fileinfoT, 1, cASTR, &(aFT.name));

   attrib = aFT.attrib;
   lsUnifyArg(CurEng, &fileinfoT, 2, cSHORT, &attrib);

   time = localtime(&aFT.time_write);
   lsMakeFA(CurEng, &timeT, _T("time"), 3);
   hour = time->tm_hour;
   min = time->tm_min;
   sec = time->tm_sec;
   lsUnifyArg(CurEng, &timeT, 1, cINT, &hour);
   lsUnifyArg(CurEng, &timeT, 2, cINT, &min);
   lsUnifyArg(CurEng, &timeT, 3, cINT, &sec);
   lsUnifyArg(CurEng, &fileinfoT, 3, cTERM, &timeT);

   lsMakeFA(CurEng, &dateT, _T("date"), 3);
   year = time->tm_year + 1900;
   month = time->tm_mon + 1;
   day = time->tm_mday;
   lsUnifyArg(CurEng, &dateT, 1, cINT, &year);
   lsUnifyArg(CurEng, &dateT, 2, cINT, &month);
   lsUnifyArg(CurEng, &dateT, 3, cINT, &day);
   lsUnifyArg(CurEng, &fileinfoT, 4, cTERM, &dateT);

   lsUnifyArg(CurEng, &fileinfoT, 5, cLONG, &(aFT.size));

   return(lsUnifyParm(CurEng, 4, cTERM, &fileinfoT));
}

/* findnext(fileinfo(Name,Attr,time(Hour,Min,Sec),date(Year,Mon,Day),Size))
   - Finds the next file that matches the previous Pattern and Mask */
TF w95_findnext(ENGid CurEng)
{
   TERM  fileinfoT, dateT, timeT;
   unsigned short  attrib;
   tm*              time;
   int  hour, min, sec;
   int  year, month, day;

   lsGetParm(CurEng, 1, cADDR, &FileHand);
   if (-1 == _findnext(FileHand, &aFT))
      return(FALSE);

   /* Make sure the one we have is an acceptable type */
   while (((aFT.attrib & 31) & mask) == 0 && (aFT.attrib & 31) != mask)
   {
      if (-1 == _findnext(FileHand, &aFT))
         return(FALSE);
   }

   lsMakeFA(CurEng, &fileinfoT, _T("fileinfo"), 5);

   lsUnifyArg(CurEng, &fileinfoT, 1, cASTR, &(aFT.name));

   attrib = aFT.attrib;
   lsUnifyArg(CurEng, &fileinfoT, 2, cSHORT, &attrib);

   time = localtime(&aFT.time_write);
   lsMakeFA(CurEng, &timeT, _T("time"), 3);
   hour = time->tm_hour;
   min = time->tm_min;
   sec = time->tm_sec;
   lsUnifyArg(CurEng, &timeT, 1, cINT, &hour);
   lsUnifyArg(CurEng, &timeT, 2, cINT, &min);
   lsUnifyArg(CurEng, &timeT, 3, cINT, &sec);
   lsUnifyArg(CurEng, &fileinfoT, 3, cTERM, &timeT);

   lsMakeFA(CurEng, &dateT, _T("date"), 3);
   year = time->tm_year + 1900;
   month = time->tm_mon + 1;
   day = time->tm_mday;
   lsUnifyArg(CurEng, &dateT, 1, cINT, &year);
   lsUnifyArg(CurEng, &dateT, 2, cINT, &month);
   lsUnifyArg(CurEng, &dateT, 3, cINT, &day);
   lsUnifyArg(CurEng, &fileinfoT, 4, cTERM, &dateT);

   lsUnifyArg(CurEng, &fileinfoT, 5, cLONG, &(aFT.size));

   return(lsUnifyParm(CurEng, 2, cTERM, &fileinfoT));
}
#endif /* _WIN32 */

#ifdef UNIX
/* stat(FileName,Atime,Ctime,Mtime,Size,Mode,NLink,Uid,Gid)
   Given FileName returns access time, change time, modify time,
   size (all unsigned longs) and mode, number of links, user-id, group-id
*/
TF EXPFUNC p_stat(ENGid CurEng)
{
	char		file_name[_MAX_PATH];
	TERM		fileinfoT;
	int		rc;
	struct stat	file_stat;

	if (_MAX_PATH <= lsStrParmLen(CurEng, 1) )
		lsErrRaise(CurEng, "input path too long");
	lsGetParm(CurEng, 1, cSTR, &file_name);

	rc = stat(file_name, &file_stat);

	lsUnifyParm(CurEng, 2, cLONG, &(file_stat.st_atime));
	lsUnifyParm(CurEng, 3, cLONG, &(file_stat.st_ctime));
	lsUnifyParm(CurEng, 4, cLONG, &(file_stat.st_mtime));
	lsUnifyParm(CurEng, 5, cLONG, &(file_stat.st_size));
	lsUnifyParm(CurEng, 6, cLONG, &(file_stat.st_mode));
	lsUnifyParm(CurEng, 7, cLONG, &(file_stat.st_nlink));
	lsUnifyParm(CurEng, 8, cLONG, &(file_stat.st_uid));
	lsUnifyParm(CurEng, 9, cLONG, &(file_stat.st_gid));

	if (rc == 0) return(TRUE);
	else return (FALSE);
}
#endif /* __unix__ */

/* -------------------- Utility Routines -------------------- */

/*   Get the version so we can dynamically dispatch to the
   right functions in W95 and NT environments. */

#ifdef _WIN32
void get_osver()
{
   DWORD v = ::GetVersion();
   if (v < 0x80000000)
      g_osver = WNT;
   else
      g_osver = W95;
}
#else
void get_osver()
{
  g_osver = OtherOS;
}
#endif

void slashslash2(_TCHAR* sout, const _TCHAR* sin)

/*   Any string of characters sent to Prolog is read using the
   Prolog reader, which interprets a backslash as an escape
   character.  So, this means if you really want a backslash,
   such as in a file path, then it has to be a double
   backslash.  This function makes that conversion. */
{
   while(*sin)
   {
      if (*sin == _T('\\'))
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

void slashslash1(_TCHAR* s)

/*   Same as slashslash2 except conversion is done in place.
   Therefore, the input buffer must be large enough to handle
   the expanded string. */
{
   int   nslash=0;
   int   len=0;
   int   i;
   _TCHAR* s1;
   _TCHAR* s2;
   
   /* Count the slashes, then copy in place from right to
      left so we don't write on ourself. */
      
   s1 = s;
   while(*s1)
   {
      len++;
      if (*s1++ == _T('\\')) nslash++;
   }
   
   s2 = s + len + nslash;
   s1 = s + len;
   
   for(i=0; i<=len; i++)
   {
      if (*s1 == _T('\\'))
      {
         *s2-- = *s1;
         *s2-- = *s1--;
      }
      else
         *s2-- = *s1--;
   }
   
   return;
}

#ifdef _WIN32

/* Common Windows functions */
#define MAX_MESSAGE 120

TF EXPFUNC p_msgbox(ENGid eid)
{
   TERM t;
   _TCHAR msg[MAX_MESSAGE];

   lsGetParm(eid, 1, cTERM, &t);
   lsTermToStr(eid, t, msg, MAX_MESSAGE);
   MessageBox(NULL, msg, _T(""), MB_OK);
   return(TRUE);
}

TF EXPFUNC p_tfmsgbox(ENGid eid)
{
   TERM t;
   _TCHAR msg[MAX_MESSAGE];

   lsGetParm(eid, 1, cTERM, &t);
   lsTermToStr(eid, t, msg, MAX_MESSAGE);
   if (IDYES == MessageBox(NULL, msg, _T(""), MB_YESNO))
      return TRUE;
   else
      return FALSE;
}

TF EXPFUNC p_getfile(ENGid eid)
{
   CString sFile;
   _TCHAR path[_MAX_PATH];

   CFileDialog fileDlg(TRUE);

   if (fileDlg.DoModal() != IDOK) return FALSE;
   sFile = fileDlg.GetPathName();
   slashslash2(path, (const _TCHAR *)sFile);
   lsUnifyParm(eid, 1, cWSTR, path);
   return TRUE;
}
#endif /* _WIN32 */


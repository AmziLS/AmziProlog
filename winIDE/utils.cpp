//---------------------------------------------------------------------------------------
// Utility C functions used by multiple routines
//

#include "stdafx.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "utils.h"

OSVer g_osver;
BYTE g_osrel;


void force_ext(_TCHAR* fbuf, _TCHAR* ext)
{
   _TCHAR *dot, *slash;

   dot = _tcschr(fbuf, _T('.'));
   slash = _tcschr(fbuf, _T('\\'));
   if (dot > slash)
      _tcscpy(dot, ext);
   else
      _tcscat(fbuf, ext);
}

BOOL chk_ext(_TCHAR* fbuf, _TCHAR* ext)
{
   _TCHAR*  dot;

   dot = _tcsrchr(fbuf, _T('.'));
   if (dot == NULL) return FALSE;
   if (0 == _tcsicmp(dot, ext))
      return TRUE;
   else
      return FALSE;
}

void get_osver()
{
   DWORD v = GetVersion();
   if (v < 0x80000000)
      g_osver = WNT;
   else
      g_osver = W95;

   g_osrel = (BYTE) v;
}

BOOL os_file_exists(_TCHAR* fbuf)
{
#ifdef _UNICODE
   if (g_osver == W95)
   {
      WIN32_FIND_DATAA finddataa;
      int len = 1 + _tcslen(fbuf);
      char *fbufa = new char[len];
      wcstombs(fbufa, fbuf, len);
      HANDLE hFind = FindFirstFileA(fbufa, &finddataa);
      delete[] fbufa;
      if (hFind == INVALID_HANDLE_VALUE)
         return FALSE;
      FindClose(hFind);
      return TRUE;
   }
#endif
   WIN32_FIND_DATA finddata;
   HANDLE hFind = FindFirstFile(fbuf, &finddata);
   if (hFind == INVALID_HANDLE_VALUE)
      return FALSE;
   FindClose(hFind);
   return TRUE;
}

BOOL file_needs_update(_TCHAR* f1, _TCHAR* f2)
// f1 needs update if it's last write is older than f2's
{
#ifdef _UNICODE
   if (g_osver == W95)
   {
      WIN32_FIND_DATAA fd1a, fd2a;
      int len1 = _tcslen(f1) + 1;
      int len2 = _tcslen(f2) + 1;
      char *f1a = new char[len1];
      char *f2a = new char[len2];
      wcstombs(f1a, f1, len1);
      wcstombs(f2a, f2, len2);
      HANDLE hf1a = FindFirstFileA(f1a, &fd1a);
      HANDLE hf2a = FindFirstFileA(f2a, &fd2a);
      delete[] f1a;
      delete[] f2a;
      if (hf1a == INVALID_HANDLE_VALUE)
         return TRUE; // doesn't exist, needs an update
      else
         FindClose(hf1a);
      if (hf2a == INVALID_HANDLE_VALUE)
         return FALSE; // no file to update from
      else
         FindClose(hf2a);
      int ia = CompareFileTime(&(fd1a.ftLastWriteTime), &(fd2a.ftLastWriteTime));
      if (ia < 0) // f1 is older than f2
         return TRUE;   // f1 needs update
      else
         return FALSE;
   }
#endif
   WIN32_FIND_DATA fd1, fd2;
   HANDLE hf1 = FindFirstFile(f1, &fd1);
   if (hf1 == INVALID_HANDLE_VALUE)
      return TRUE; // doesn't exist, needs an update
   else
      FindClose(hf1);
   HANDLE hf2 = FindFirstFile(f2, &fd2);
   if (hf2 == INVALID_HANDLE_VALUE)
      return FALSE; // no file to update from
   else
      FindClose(hf2);
   int i = CompareFileTime(&(fd1.ftLastWriteTime), &(fd2.ftLastWriteTime));
   if (i < 0) // f1 is older than f2
      return TRUE;   // f1 needs update
   else
      return FALSE;
}

FILE* os_fopen(_TCHAR* fname, _TCHAR* mode)
// Global file open routine, that handles at runtime the problems
// of NT being Unicode and Win95 not.
{
   switch (g_osver)
   {
#ifdef _UNICODE
   case W95:
   {  // must use block syntax if initializing variables
      int lname = 1 + _tcslen(fname);
      int lmode = 1 + _tcslen(mode);
      char* afname = new char[lname];
      char* amode = new char[lmode];
      wcstombs(afname, fname, lname);
      wcstombs(amode, mode, lmode);
      FILE* fp = fopen(afname, amode);
      delete[] afname;
      delete[] amode;
      return fp;
   }
      break;
#else
   case W95:
#endif
   case WNT:
   default:
      return _tfopen(fname, mode);
   };
}

void os_remove(_TCHAR* fname)
// removes the file if it exists
{
#ifdef _UNICODE
   if (g_osver == W95)
   {
      int len = _tcslen(fname) + 1;
      char *fa = new char[len];
      wcstombs(fa, fname, len);
      remove(fa);
      delete[] fa;
   }
   else
      _wremove(fname);
#else
   remove(fname);
#endif
}

/*
BOOL ValidProject(CString sFile)
{
#ifdef _UNICODE
   // Open file so fgets interprets text as WCS.
   FILE* f = _tfopen(sFile, _T("rb"));
#else
   FILE* f = _tfopen(sFile, _T("r"));
#endif

   if (f == NULL)
      return FALSE;

   _TCHAR buf[16];
   _TCHAR line1[] = _T("amzirel=4");

   if (NULL == _fgetts(buf, 10, f))
   {
      fclose(f);
      return FALSE;
   }

   if ( 0 == _tcsncmp(buf, line1, 9))
   {
      fclose(f);
      return TRUE;
   }


   fclose(f);
   return FALSE;
}
*/

#ifdef _UNICODE
// When reading a binary file, which the stream
// I/O functions assume is a stream of Unicode
// characters, the gets functions don't convert
// final CR-LF to a NULL, so these versions do.
_TCHAR * aWfgets(_TCHAR * buf, int len, FILE* fp)
{
   _TCHAR  *pbuf;

   pbuf = fgetws(buf, len, fp);
   if (pbuf == NULL)
      return NULL;

   // Convert the \n to a null, like gets is supposed to do.
   while (*pbuf != 10 && *pbuf != 13 && *pbuf != EOS)
      pbuf++;
   *pbuf = EOS;

   return buf;
}
#endif //_UNICODE

/****************************************************************************
*
* alnk.cpp -- Command line front end to the .plm file linker
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
\****************************************************************************/


//#ifdef _UNICODE
//#define UNICODE
//#endif

#include <stdio.h>
#include <wchar.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include <iostream>

#include "env.h"
#ifdef WINDOWS
#include <windows.h>
#endif
#include "defs.h"
#include "lenv.h"

#if defined(SOLARIS) && ! defined(GNU)
#include <widec.h>
#endif

#ifndef _MAX_PATH
#define _MAX_PATH 256
#endif

#if defined(__unix__) || defined(UNIX)
extern "C" { 
int aLinkW(void(*pfM)(aCHAR*), int argctr, aCHAR **pargv);
void disp_msg(aCHAR *);
}
#else
extern "C" int __cdecl aLinkW(void (__cdecl *pfM)(aCHAR*), int argctr, aCHAR **pargv);
void __cdecl disp_msg(aCHAR *);
#endif

void chk_ext(aCHAR *, aCHAR *);
void trim_nl(aCHAR *s);

#if defined(WINDOWS)  &&  defined(_UNICODE)
extern "C" void wmain(int argc, aCHAR **argv)
//#elif defined(__vms)
//int main(int argc, aCHAR **argv)
#else
int main(int argc, char **argv)
#endif
{
   int    argctr;
   aCHAR   *pargv[1000], *tp, fbuf[_MAX_PATH];
   char  fname[_MAX_PATH];
   FILE   *linput = NULL;
   int   buflen;

//   Linker  theLinker(&disp_msg);

   if (argc == 2 && *(argv[1]+1) == '?')
      {
      Lprintf(aS("Usage: alnk <output-file>.xpl <input-file>.plm <input-file>.plm ...\n"));
      exit(0);
      }

#ifdef UNIX  // command line is always ascii in unix
   if (argc > 2 && 0 == strcmp(argv[1],"-f"))
#else
   if (argc > 2 && 0 == Lstrcmp(argv[1],aS("-f")))
#endif
   // allow input to be read from a file.
   {
#if defined(UNIX)
      strcpy(fname, argv[2]);
#else 
      wcstombs(fname, argv[2], _MAX_PATH);
#endif

      linput = fopen(fname, "r");

#ifdef UNIX
      mbstowcs(fbuf, argv[1], _MAX_PATH);
#else
      Lstrcpy(fbuf, argv[1]);
#endif

      Lfgets(fbuf, _MAX_PATH, linput);
      buflen = Lstrlen(fbuf);
      fbuf[buflen-1] = 0;   // kill trailing newline
   }

   /* read stdin for run file */
   else if (argc == 1)
      {
        Lprintf(aS("Linked Module [.xpl]: "));
        Lfgets(fbuf, _MAX_PATH, stdin);
        trim_nl(fbuf);
      }
   else
   {
#ifdef UNIX
      mbstowcs(fbuf, argv[1], _MAX_PATH);
#else
      Lstrcpy(fbuf, argv[1]);
#endif
   }

   if (Lstrlen(fbuf) < 1) 
     exit(0);

   chk_ext(fbuf, aS(".xpl"));

   pargv[0] = new aCHAR[1 + Lstrlen(fbuf)];
   Lstrcpy(pargv[0], fbuf);
   pargv[1] = new aCHAR[1 + Lstrlen(aS("alib.plm"))];
   Lstrcpy(pargv[1], aS("alib.plm"));
   argctr = 2;

   do
     {                          //  read stdin for list of files to be linked 
       if (argc <= 2)
         {
           Lprintf(aS("Compiled Code Module [.plm]: "));
           tp = Lfgets(fbuf, _MAX_PATH, stdin);
           trim_nl(fbuf);
           //printf("read %d chars\n", Lstrlen(tp));
         }
      else if (linput != NULL)
      {
         tp = Lfgets(fbuf, _MAX_PATH, linput);
         buflen = Lstrlen(fbuf);
         fbuf[buflen-1] = 0;   // kill trailing newline
      }
       else
         {
           if ( (argctr) >= argc ) 
             tp = NULL;
           else
             {
#ifdef UNIX
               mbstowcs(fbuf, argv[argctr], _MAX_PATH);
#else
               Lstrcpy(fbuf, argv[argctr]);
#endif
               tp = fbuf;
             }
         }
       if (tp && Lstrlen(fbuf) > 0)
         {
         if ( (NULL != Lstrstr(fbuf, aS("alib"))) ) {
            Lprintf(aS("Link Error: alib.PLM automatically included"));
            exit(-1); }

         chk_ext(fbuf, aS(".plm"));
         pargv[argctr] = new aCHAR[1 + Lstrlen(fbuf)];
         Lstrcpy(pargv[argctr], fbuf);
         argctr++;
         }
      } while (tp && Lstrlen(fbuf) > 0);

   if (linput != NULL)
      fclose(linput);

   aLinkW(&disp_msg, argctr, pargv);
#ifdef UNIX
   return 0;
#else
   exit(0);
#endif
}

void chk_ext(aCHAR * fbuf, aCHAR * ext)
{
   aCHAR   *dot;

   dot = Lstrrchr(fbuf, aS('.'));

   if (dot == NULL || Lstrpbrk(dot, aS("/\\]")))
      Lstrcat(fbuf, ext);
//   if (NULL == (dot = strrchr(fbuf, '.')))
//      strcat(fbuf, ext);
   else if (0 != Lstrcmp(dot,ext))
     {
       Lprintf(aS("\nLink Error: Invalid file extension for %s"), fbuf);
       exit(-1);
     }

   return;
}

#if defined(__unix__) || defined(UNIX)
void disp_msg(aCHAR * msg)
#else
void __cdecl disp_msg(aCHAR * msg)
#endif
{
   Lprintf(msg);
   Lprintf(aS("\n"));
}

// fgets keeps the nl put we want it off.
void trim_nl(aCHAR *s)
{
   int i = Lstrlen(s);
   if (s[i-1] == '\n')
      s[i-1] = EOS;
}




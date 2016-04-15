/****************************************************************************
*
* arun.cpp -- the command line Amzi! runtime
*
* Copyright (c) 1992-2012 by Amzi! inc.  All Rights Reserved.
*
****************************************************************************/


#if defined(_UNICODE) && ! defined(WIN32)
#define UNICODE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Unbelievably this is needed in HP/UX for cout to work in libamzi.sl
#if defined(HPUX)
#include <iostream>
#endif

//#if defined(SOLARIS) && ! defined(GNU)
//#include <widec.h>
//#endif

/*   The header file required for linking with Prolog.  It contains
   definitions and common variables. */

#include "r_defs.h"
#include "amzi.h"

#if defined WIN32 || defined WIN64
#include <conio.h>
#endif

/*
#if defined(UNIX)
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/termios.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#endif
*/
//#if defined(UNIX)

#define MAX_ARGS 20

/*
#ifdef ENVsol25
// major kludge to get solaris to work
void SetProgramArgs(int, char **);
#endif
*/

int a_strncmp(aCHAR* s1, aCHAR* s2, int len);

/*   This simple console shell for the Logic Server takes a prolog program
   name and launches it.  The name can either be on the command line
   or given in response to a prompt. */

#if defined WIN32 || defined WIN64
#ifdef _UNICODE
extern "C" void wmain(int argc, aCHAR **argv)
#else
void main(int argc, aCHAR **argv)
#endif
// Solaris requires an integer return from main.
#else
int main(int argc, aCHAR **argv)
#endif
{
   aCHAR  xplname[80];
   aCHAR  *sExeName, *sColon, *sSlash, *sDot;
   int     len;
   TF     tf;
   int     i, nargc;
   aCHAR  *nargv[MAX_ARGS];
   TF     exexpl = FALSE;

   /* get the name of the program to run */
   /* ---------------------------------- */

   /* First check the name of this .EXE file, if its not
      a5run then its a copy meant to run the .XPL file of the
      same name. */

   sColon = Lstrrchr(argv[0],aS(':'));           /* remove path from file name */
   sSlash = Lstrrchr(argv[0],aS('\\'));
   if (sSlash == NULL) sSlash = Lstrrchr(argv[0],aS('/'));       /* Unix path? */
   
int x = sizeof(wchar_t);

   if (sColon == NULL && sSlash == NULL) sExeName = argv[0];
   else if (sSlash == NULL) sExeName = sColon + 1;
   else sExeName = sSlash + 1;

   if (0 != a_strncmp(sExeName, aS("arun"), 4))
   {
      sDot = Lstrrchr(sExeName,aS('.'));
      if (sDot)
         *sDot = EOS;
      Lstrcpy(xplname, sExeName);
	  if (0 == a_strncmp(xplname, aS("alis64"), 4))
		  Lstrcpy(xplname, aS("alis"));
	  if (0 == a_strncmp(xplname, aS("acmp64"), 4))
		  Lstrcpy(xplname, aS("acmp"));
      exexpl = TRUE;
   }

   else if (argc > 1)
      Lstrncpy(xplname, argv[1], 80);

   else
   {
      Lprintf(aS("Prolog program to run: "));
      Lfgets(xplname, 79, stdin);
      len = Lstrlen(xplname);
      if (xplname[len-1] == aS('\n'))
      {
         xplname[len-1] = aS('\0');
         len--;
      }
      if (len == 0) exit(0);
   }

   /* Fire up the Logic Server */

   try
   {
      CLogicServer ls;

      ls.Init(xplname);
      ls.InitLSX(NULL);
      ls.Load(xplname);

   /*   If the .exe name is the same as the .xpl name, then the arguments
      are OK.  If we're running a .xpl file, then shift the command
      line arguments over one so it appears as if the Prolog program
      was the first argument. */

      if (exexpl)
         ls.SetCommandArgs(argc, argv);
      else
      {
         nargc = argc - 1;
         for (i=0; i<=nargc; i++)
         nargv[i] = argv[i+1];
         ls.SetCommandArgs(nargc, nargv);
      }

      tf = ls.Main();
//#ifdef _DEBUG
//      if (tf == TRUE)
//         Lprintf(aS("\nmain/0 succeeded\n"));
//      else
//         Lprintf(aS("\nmain/0 failed\n"));
//#endif
      ls.Close();
   }
// A bug in GNU C++ results in segmentation faults
// when objects thrown as references, so they are thrown
// as pointers from the C++ class in amzi.h when
// GNU is defined
#ifdef GNU
#define EMETH(X) pE->X
   catch( CLSException *pE )
#else
#define EMETH(X) E.X
   catch( CLSException &E )
#endif
   {
      aCHAR *emsg = new aCHAR[512];
      EMETH(GetMsg(emsg, 511));
      ExType x = EMETH(GetType());
      switch( x )
      {
      case INTERNAL:
         Lprintf(aS("Internal error, notify Amzi! tech support\n"));
         Lprintf(aS("Engine shut down\n"));
         Lprintf(aS("Exception: %d %s\n"), EMETH(GetRC()), emsg);
         break;
      case ABORT:
         Lprintf(aS("Abort error - Prolog Engine shut down\n"));
         Lprintf(aS("Exception: %d %s\n"), EMETH(GetRC()), emsg);
         break;
      case FATAL:
         Lprintf(aS("Prolog resource exhausted, stacks and heaps reset\n"));
         Lprintf(aS("Exception: %d %s\n"), EMETH(GetRC()), emsg);
         break;
      case SECURITY:
         Lprintf(aS("Amzi! license exception:\n"));
         Lprintf(aS("%d %s\n"), EMETH(GetRC()), emsg);
         break;
      case READ:
         {  // braces make compiler happy when definition included
            Lprintf(aS("Prolog syntax error\n"));
            Lprintf(aS("Exception: %d %s\n"), EMETH(GetRC()), emsg);
            int lineno = EMETH(GetReadLineno());
            aCHAR *ereadbuf = new aCHAR[1024];
            EMETH(GetReadBuffer(ereadbuf, 1023));
            Lprintf(aS("Line Number %d\nRead Buffer: %s\n"), lineno, ereadbuf);
            delete[] ereadbuf;
            break;
         }
      default:
         {
            Lprintf(aS("Exception: %d %s\n"), EMETH(GetRC()), emsg);
            aCHAR *callstack = new aCHAR[1024];
            EMETH(GetCallStack(callstack, 1023));
            Lprintf(aS("Call Stack: %s\n"), callstack);
            delete[] callstack;
         }
      }
      delete[] emsg;
      // Under Linux, leaving the catch block causes a segmentation
      // fault, so we put this exit here, which makes the problem
      // go away, but leaves us wondering.  Is this a gcc problem
      // related to catching errors?
      exit(-1);
   }
// Windows has main returning void, so no return, Solaris has main
// returning int.

// Cause a pause for input so when running under the windows debugger
// we can see the output.
//   printf("OK?");
//   char buf[6];
//   fgets(buf, 5, stdin);

#if defined WIN32 || defined WIN64
   exit(0);
#else
   return(0);
#endif
}

// Case insensitive compare since its not a standard function.
int a_strncmp(aCHAR* s1, aCHAR* s2, int len)
{
   int rc = 0;

   for (int i=0; i<len; i++)
   {
      if (tolower(*s1++) != tolower(*s2++))
      {
         rc = -1;
         break;
      }
   }
   return rc;
}


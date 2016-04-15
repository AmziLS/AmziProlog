/****************************************************************************
* 
* ddbmem.h -- database support routines
* 
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
* 
* Added par 2 chunksize to Init. Ray 1998/09/12
*
* $Log: ddbmem.h,v $
* Revision 1.3  2006/12/05 20:17:38  mary
* Don't shut_down again if there's an error in shut_down. Catch the error
* and make a log entry.
*
* Revision 1.2  2006/03/08 23:38:59  dennis
* fixed LNEW bug, MS uses bad_alloc now...
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.23  2003/09/11 02:07:44  dennis
* fixed memory leak problem with dynamic iterators
*
* Revision 1.22  2003/07/10 20:20:21  dennis
* removed internal CLogicServer class, just use amzi.h from now on
*
* Revision 1.21  2002/09/20 20:59:40  dennis
* changes to get Solaris to build with gcc 3.2
*
* Revision 1.20  2002/09/10 23:53:34  dennis
* correct bad_alloc use, hopefully
*
* Revision 1.19  2002/07/04 16:20:25  dennis
* support academic registration
*
* Revision 1.18  2002/06/23 20:01:29  dennis
* fixed some gc issues
*
* Revision 1.17  2002/06/20 02:28:47  dennis
* updated makefiles, fixed ddmem bugs for gcc
*
* Revision 1.16  2002/06/19 04:04:39  dennis
* alib missing exports added, fixed gc/0
*
* Revision 1.15  2002/06/02 04:15:41  dennis
* updated makefiles
*
* Revision 1.14  2002/05/15 16:59:07  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.13  2002/05/01 02:26:32  dennis
* minor fixes
*
* Revision 1.12  2002/03/04 04:22:53  dennis
* changed sorted iterator erase again, as gcc follows the stl standard,
* which is dumb but standard, and ms, hate to see them as the good guys,
* does the right thing, which is not standard
*
* Revision 1.11  2002/01/06 20:31:27  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.10  2001/10/13 02:58:12  dennis
* see/tell bugs, used to close function streams
*
* Revision 1.9  2001/10/05 19:15:17  dennis
* string streams, function streams working, reals set up to not require
* m_peng, just passed in parameters as necessary
*
* Revision 1.8  2001/08/08 00:21:16  dennis
* unworking commit - stream bugs need fixing
*
* Revision 1.7  2001/07/27 16:18:29  dennis
* fixed linker bug
*
* Revision 1.6  2001/07/21 00:39:46  dennis
* added garbage collector for strings and things
*
* Revision 1.5  2001/07/11 15:52:26  dennis
* finished leak work, rubik runs no leaks, fixed abolish bug,
* was deleting stuff that wasn't necessary
*
* Revision 1.4  2001/07/10 16:51:31  dennis
* added more memory leak checking tools, all clean so far
*
* Revision 1.3  2001/06/30 02:40:35  dennis
* added stream I/O version of loader from 5.0 work
*
* Revision 1.2  2001/06/27 15:15:09  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.4  2000/08/26 00:32:05  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.3  2000/03/28 01:05:15  dennis
* merged Ray's changes with bigdig.  bigdig is at point where
* new Cell class is used, but there are no modules in the system.
*
* Revision 1.2.2.2  2000/03/14 09:02:58  dennis
* getting further
*
* Revision 1.2.2.1  2000/03/08 04:12:00  dennis
* builtin.cpp compiles
*
* Revision 1.2  2000/01/17 09:51:50  dennis
* original a5x modified for new directory structure and
* names, sans the 5, like alnk and alis
*
* Revision 4.3  1998/01/20 13:08:59  ray
* <>
*
* Revision 4.1  1998/01/13 07:33:43  pete
* Initial changes for solaris
*
* 
****************************************************************************/

#ifndef DDBMEM_H
#define DDBMEM_H

#ifdef LANDFILL
#define BUG_LEAK
#endif

#ifdef BUG_LEAK
void *operator new(size_t);
void *operator new[](size_t);
void operator delete(void *p);
void operator delete[](void *p);

void leak_report(char*);
void leak_report(char*, char*);

struct Leak
{
   long filler;   // for new[]
   Leak *prev;
   Leak *next;
   size_t size;
   char *file;
   aCHAR *note;
   int line;
   int count;
   int id;  // so we can tell if our new allocated or not
};

extern char *g_file;
extern int g_line;
extern aCHAR *g_note;

#define LEAK(desc) leak_report(desc)

//#if defined(WINDOWS)   // & BUG_LEAK  NULL returned from new
//
//#define LNEW(a,x,c)  \
//      g_file = __FILE__; g_line = __LINE__; g_note = c; \
//      if (! (a = new x) ) \
//        pXCPT->Error(outofmemE, c); \
//      g_file = "unknown"; g_line = 0; g_note = NULL;
//
//#define LNEWX(a,x)  \
//      g_file = __FILE__; g_line = __LINE__; \
//      if (! (a = new x) ) \
//         throw LExcept(outofmemE, aS("hidden")); \
//      g_file = "unknown"; g_line = 0;
//
//#if !defined(LINKER)   // linker uses this one two
//
//#define LNEWZ(a,x)  \
//      g_file = __FILE__; g_line = __LINE__; \
//      if (! (a = new x) ) \
//         throw LExcept(outofmemE, aS("hidden")); \
//      g_file = "unknown"; g_line = 0;
//
//#else  // is the linker
//#define LNEWZ(a,x)  \
//      g_file = __FILE__; g_line = __LINE__; \
//      a = new x; \
//      g_file = "unknown"; g_line = 0;
//#endif  // !defined(LINKER)
//
//#else    // not WINDOWS bad_alloc returned from new

#define LNEW(a,x,c)  \
      g_file = __FILE__; g_line = __LINE__; g_note = c; \
      try { a = new x; } \
      catch(std::bad_alloc ba) \
      { pXCPT->Error(outofmemE, c); } \
      g_file = "unknown"; g_line = 0; g_note = NULL;

#define LNEWX(a,x)  \
      g_file = __FILE__; g_line = __LINE__; \
      try { a = new x; } \
      catch(std::bad_alloc ba) \
      { throw LExcept(outofmemE, aS("hidden")); } \
      g_file = "unknown"; g_line = 0;

#if !defined(LINKER)   // linker uses this one two
#define LNEWZ(a,x)  \
      g_file = __FILE__; g_line = __LINE__; \
      try { a = new x; } \
      catch(std::bad_alloc ba) \
      { throw LExcept(outofmemE, aS("hidden")); } \
      g_file = "unknown"; g_line = 0;
#else
#define LNEWZ(a,x)  \
      g_file = __FILE__; g_line = __LINE__; \
      try { a = new x; } \
      catch(std::bad_alloc ba) \
      { a = NULL; } \
      g_file = "unknown"; g_line = 0;
#endif  // !defined(LINKER)
//#endif  // WINDOWS


#else  // not BUG_LEAK

#define LEAK(DESC)

//#if defined(WINDOWS)   // NULL returned from new
//
//#define LNEW(a,x,c)  \
//      if (! (a = new x) ) \
//         pXCPT->Error(outofmemE, c);
//
//#define LNEWX(a,x) \
//      if (! (a = new x) ) \
//         throw LExcept(outofmemE, aS("hidden"));
//
//#if !defined(LINKER)   // linker uses this one two
//#define LNEWZ(a,x) \
//      if (! (a = new x) ) \
//         throw LExcept(outofmemE, aS("hidden"));
//#else
//#define LNEWZ(a,x) a = new x;
//#endif   // !defined(LINKER)
//
//#else   // not WINDOWS
#define LNEW(a,x,c)  \
      try { a = new x; } \
      catch(std::bad_alloc ba) \
      { pXCPT->Error(outofmemE, c); }

#define LNEWX(a,x) \
      try { a = new x; } \
      catch(std::bad_alloc ba) \
      { throw LExcept(outofmemE, aS("hidden")); }

#if !defined(LINKER)   // linker uses this one two
#define LNEWZ(a,x) \
      try { a = new x; } \
      catch(std::bad_alloc ba) \
      { throw LExcept(outofmemE, aS("hidden")); }
#else
#define LNEWZ(a,x) \
      try { a = new x; } \
      catch(std::bad_alloc ba) \
      { a = NULL; }
#endif   // !defined(LINKER)

//#endif  // WINDOWS

#endif   // BUG_LEAK

#endif //DDBMEM_H

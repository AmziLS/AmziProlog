/**************************************************************************
* 
* main.h -- definitions used in external interface
* 
* Copyright (c) 1992-2009 by Amzi!.  All Rights Reserved.
* 
* $Log: main.h,v $
* Revision 1.8  2006/10/05 17:03:15  mary
* Added security built-ins and entry points for arules runtime registration.
*
* Revision 1.7  2005/08/10 23:38:40  dennis
* moved critical sections
*
* Revision 1.6  2005/08/04 21:05:39  dennis
* sync logging
*
* Revision 1.5  2005/07/28 19:49:59  mary
* Put Windows semaphore stuff under ifdef BROKEN_WINDOWS
*
* Revision 1.4  2005/06/02 18:49:58  dennis
* Linux changes
*
* Revision 1.3  2005/04/25 23:38:11  mary
* Use per engine locking except on init and close. Get rid of old CLogicServer.
*
* Revision 1.2  2003/09/23 20:54:34  dennis
* more security stuff in linker and engine
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.7  2003/08/27 03:04:43  dennis
* added initialization support for remote debugging LSX
*
* Revision 1.6  2002/09/05 23:05:37  dennis
* Added CheckProfessional to main.cpp, so the version can be checked
* by looking in unlock.xpl in case it didn't come in from the loaded
* .xpl.
*
* Revision 1.5  2002/05/15 16:59:09  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.4  2001/10/05 17:07:01  ray
* Added real things. Fixed some bugs. Stopped adding '.' in prep
*
* Revision 1.3  2001/08/24 16:06:44  dennis
* fixed header file comments at end of files, removed ostream include
* which isn't apparently necessary
*
* Revision 1.2  2001/04/16 05:21:14  dennis
* hacked together some fixes for sio/lex to be better friends,
* merged other changes, added new samples
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.4  2000/05/14 03:52:34  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
* Revision 1.3  2000/01/31 14:13:12  dennis
* Cleaned up system dependent code, creating new module, lenv,
* that has it all, or at least most of it.
*
* Revision 1.2  2000/01/17 09:51:52  dennis
* original a5x modified for new directory structure and
* names, sans the 5, like alnk and alis
*
* Revision 4.1  1998/01/13 07:37:58  pete
* Initial changes for solaris
*
* 
****************************************************************************/

#ifndef MAIN_H
#define MAIN_H

//bool CheckProfessional();

// #include "engine.h"

// This controls the creation of a debugging log
// called debug.log.
#define noBUG_LOG

#ifdef BUG_LOG
#define BUGLOGWR(x) fprintf(g_dbg, "%s\n", x)
#else
#define BUGLOGWR(x)
#endif

#ifdef BUG_LOG
#define  G_BUGBUF_SIZE 512
extern   FILE *g_dbg;
extern char g_bugbuf[G_BUGBUF_SIZE];
#endif

extern Lofstream *g_sync_file;  // only not NULL when main.cpp BUG_SYNC is on

#ifdef ARULESXLRT
int register_runtime(DWORD method, char *proxyname, char *redistkey, char *runtimeid);
#endif

#ifdef WINDOWS
#define AInitializeCriticalSection(x) InitializeCriticalSection(x)
#define ADeleteCriticalSection(x) DeleteCriticalSection(x)
#else
#define AInitializeCriticalSection(x)
#define ADeleteCriticalSection(x)
#endif

// utility now used elsewhere as well
char *toAscii(aCHAR *s);

class LEngNode
{
friend class LEngList;
private:
   LEngine *m_peng;
   LEngNode *m_next;
//#ifdef WINDOWS
//   CRITICAL_SECTION m_critical_section;
//#else
//   int m_critical_section;
//#endif
private:
   LEngNode(LEngine *peng, LEngNode *next)
   {
      m_peng = peng;
      m_next = next;
   }
   ~LEngNode()
   {   delete m_peng; }
};

class LEngList
{
private:
   LEngNode *m_head;
public:
   int nengines;

   LEngList() { m_head = NULL; nengines = 0; }
   ~LEngList();
   void Add( LEngine * );
   LBOOL Check( LEngine * );
   void Del( LEngine * );
//#ifdef WINDOWS
//   CRITICAL_SECTION * GetSemaphore( LEngine * );
//#endif
   LEngine *GetFirst()   // used for debugging
   { return m_head->m_peng; }
};

// A mere shadow if its former self.  Just keeping the engine list now.
// We'll fix this if it stays permanent
class CLogicServer
{
public:
   static LEngList g_EngList;
};

#endif //MAIN_H

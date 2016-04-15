/****************************************************************************
* 
* lsthread -- thread support
* 
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: lsthread.h,v $
* Revision 1.4  2006/10/05 18:26:21  dennis
* minor
*
* Revision 1.3  2006/09/19 14:36:02  dennis
* debug thread changes
*
* Revision 1.2  2006/09/11 16:12:04  dennis
* Ifdef threading predicates for Windows
*
* Revision 1.1  2006/07/25 15:12:05  dennis
* I don't know, maybe some thread stuff
*
*
****************************************************************************/

// A class that is used by Prolog code that needs to wait for user input.
// The query is started with lsExecStrTh() which launches it in a separate
// thread, so that the UI of the calling program is still active.  The UI
// can be used to put actions into the ActionBuffer.  This was implemented
// for the sole purpose of making a debugger for ARules.

#ifndef LSTHREAD_H
#define LSTHREAD_H

#ifdef THREADED_EXEC_STR

void ThreadedExecStr(ENGid eid, LSTERMptr tp, STRptr s);

class ActionBuffer
{
public:
   ENGid m_peng;
   bool buffer_set;
   bool prolog_waiting;
   bool running;
   bool break_set;
   aCHAR buffer[512];
   HANDLE hBufferPutEvent;
   HANDLE hPrologEvent;

   ActionBuffer(ENGid eng);

   void Init(void);
   void Put(STRptr s);
   RC GetState(void);
   bool IsBreakSet(void) { if (break_set) {break_set = FALSE; return TRUE;} else return FALSE; }
   TF p_get_async_action(void);
   TF p_set_event(void);
};

// Threaded Exec Str arguments
struct TES
{
   ENGid eid;
   LSTERMptr tp;
   STRptr s;

   TES(ENGid e, LSTERMptr t, STRptr ss)
   { eid = e; tp = t; s = ss; }

   ~TES()
   { delete s; }
};


#endif // THREADED_EXEC_STR
#endif // LSTHREAD_H

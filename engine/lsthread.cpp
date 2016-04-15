/****************************************************************************
* 
* lsthread -- thread support
* 
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: lsthread.cpp,v $
* Revision 1.7  2006/10/05 18:26:21  dennis
* minor
*
* Revision 1.6  2006/10/05 11:32:58  dennis
* thread tweaks
*
* Revision 1.5  2006/09/28 23:33:42  dennis
* async tweaks
*
* Revision 1.4  2006/09/19 14:36:02  dennis
* debug thread changes
*
* Revision 1.3  2006/08/11 15:32:23  dennis
* fixed ProveRedo cleanup, see comments
*
* Revision 1.2  2006/08/10 13:31:49  dennis
* fix to async
*
* Revision 1.1  2006/07/25 15:12:05  dennis
* I don't know, maybe some thread stuff
*
*
****************************************************************************/

#include "inc.h"
#include "process.h"
#include "pch.h"

#ifdef THREADED_EXEC_STR

#ifdef LANDFILL
#define BUG_ASYNC
#endif
ActionBuffer *theBuffer = NULL;

ActionBuffer::ActionBuffer(ENGid eng)
{
   m_peng = eng;
   buffer_set = FALSE;
   prolog_waiting = FALSE;
   break_set = FALSE;
   running = FALSE;
   hBufferPutEvent = CreateEvent(NULL, FALSE, FALSE, aS("BufferPutEvent"));
//   hBufferGetEvent = CreateEvent(NULL, FALSE, FALSE, aS("BufferGetEvent"));
   ResetEvent(hBufferPutEvent);
}

void ActionBuffer::Init(void)
{
   buffer_set = FALSE;
   prolog_waiting = FALSE;
   break_set = FALSE;
   running = TRUE;
   Lstrcpy(buffer, aS(""));
   ResetEvent(hBufferPutEvent);
}

void ActionBuffer::Put(STRptr s)
{
   if (! running) return;
   // the loop in lexec checks to see if break_set using IsBreakSet()
   if (Lstrcmp(s, aS("break")) == 0) break_set = TRUE;

   // No, we'll use the latest, since the user has been notified
   // the last one timed out.
//   if ( buffer_set == TRUE && break_set == FALSE )
//   {
//MessageBox(NULL, aS("Put buffer buffer_set = TRUE has: "), aS("ARules DLL"), MB_OK);
//MessageBox(NULL, buffer, aS("ARules DLL"), MB_OK);
//      return;
//   }
   Lstrcpy(buffer, s);
//MessageBox(NULL, aS("Put Action Buffer"), aS("ARules DLL"), MB_OK);
//MessageBox(NULL, s, aS("ARules DLL"), MB_OK);
   buffer_set = TRUE;
   prolog_waiting = FALSE;
//   ResetEvent(hBufferGetEvent);
   SetEvent(hBufferPutEvent);
   return;
}

RC ActionBuffer::GetState(void)
{
   // overloading RC in this case to have two OK values
   // This call is used by host program to see if it's safe to issue diagnostic
   // report like queries before starting execution of Prolog again.
   //if (prolog_waiting || run_done) return 1;
   //else return 0;

   if (prolog_waiting) return 1;
   else if (! running) return 2;
   else return 0;
}

TF ActionBuffer::p_get_async_action(void)
{
   int rc;
   Cell c;
   TF tf;
//   aCHAR buff[103];
   Cell* xo;

   // This is used by the VBA debugger for ARulesXL.  While waiting, other
   // calls to the logic server are made to get various information.  The
   // call stack should be set back to where it was after
   // those calls.  But X[0] has the value given it by CallStr.  So this
   // seems a bit of a hack, but we save it and then restore it when we get
   // what we were waiting for.
   xo = X(0);
#ifdef BUG_ASYNC
   FILL("***ASYNC*** Beginning Control Stack ");
   DUMP << "X(0) = ";
   pTSVC->Dump(X(0));
   pCNTL->Dump();
#endif
//MessageBox(NULL, aS("in p_get_async_action"), aS("ARules DLL"), MB_OK);
//MessageBox(NULL, aS("Initial value, X(0) = "), aS("ARules DLL"), MB_OK);
//pWRIT->termWriteString(X(0), buff, 88, TRUE);
//MessageBox(NULL, buff, aS("ARules DLL"), MB_OK);

   ResetEvent(hBufferPutEvent);
   prolog_waiting = TRUE;

   while( buffer_set == FALSE )
   {
//MessageBox(NULL, aS("Get Action Buffer Waiting"), aS("ARules DLL"), MB_OK);
      rc = WaitForSingleObject(hBufferPutEvent, 1000000);
	  prolog_waiting = FALSE;
//char buf[100];
//sprintf(buf, "Exit wait with rc = %d", rc);
//MessageBoxA(NULL, buf, "ARules DLL", MB_OK);
      if (rc != WAIT_OBJECT_0)
      {
         Lstrcpy(buffer, aS("error"));
      }
   }
   buffer_set = FALSE;

   if (IsBreakSet()) pXCPT->Error(breakE);
//MessageBox(NULL, aS("Get Action Buffer got something"), aS("ARules DLL"), MB_OK);
//MessageBox(NULL, buffer, aS("ARules DLL"), MB_OK);
   c.setString( pGCTH->make_string(buffer) );

   pHXL->SetXRef(0, xo);

   tf = pTSVC->UnifyConst(X(0), &c);
#ifdef BUG_ASYNC
if (tf == TRUE) {
 FILL("***ASYNC*** Ending Control Stack after success");
 DUMP << "X(0) = ";
 pTSVC->Dump(X(0));
 pCNTL->Dump();
 //MessageBox(NULL, aS("Returning TRUE"), aS("ARules DLL"), MB_OK);
} else {
 FILL("***ASYNC*** Ending Control Stack after failure");
 DUMP << "X(0) = ";
 pTSVC->Dump(X(0));
 pCNTL->Dump(); }
#endif

   //MessageBox(NULL, aS("Returning FALSE, X(0) = "), aS("ARules DLL"), MB_OK);
   //pWRIT->termWriteString(X(0), buff, 88, TRUE);
   //MessageBox(NULL, buff, aS("ARules DLL"), MB_OK);
   prolog_waiting = FALSE;
   return tf;
}

TF ActionBuffer::p_set_event(void)
{
   hPrologEvent = CreateEvent(NULL, FALSE, FALSE, aS("PrologEvent"));
   SetEvent(hPrologEvent);
   return TRUE;
}


void ExecStrProc( LPVOID pParam )
{
   TES *tes = (TES*)pParam;

   // set to false, because we're running now
   tes->eid->m_action_buffer->Init();
//MessageBox(NULL, aS("Starting asynchronous ExecStr"), aS("ARules DLL"), MB_OK);
   (tes->eid)->ExecStr( tes->tp, tes->s );
//MessageBox(NULL, aS("Finishing asynchronous ExecStr"), aS("ARules DLL"), MB_OK);
   // set to true, because the query is done which is sort of the same
   // as waiting.  maybe a poor choice of variable name?
   tes->eid->m_action_buffer->running = false;

   delete tes;
   return;   // thread completed successfully
}

void ThreadedExecStr(ENGid eid, LSTERMptr tp, STRptr s)
{
   TES *tes = new TES(eid, tp, s);

   _beginthread(ExecStrProc, 0, tes);

   return;
}


#endif // THREADED_EXEC_STR
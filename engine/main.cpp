/****************************************************************************
*
* main.cpp -- Provide C interface for DLL/Library
*
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
****************************************************************************/

/* ----- Includes -------------------------------------------------------- */

#include "inc.h"
#include "pch.h"

#include "llink.h"

#ifdef ARULESXLRT
#include <wininet.h>
#endif

// #define BUG_SYNC in debug.h now

//#ifdef BUG_SYNC
Lofstream *g_sync_file = NULL;
//#endif

#define CHECK(RESULT)   if(! CLogicServer::g_EngList.Check(eid)) return RESULT

// Global objects

// This semaphore is used when not performing per-engine operations
#ifdef WINDOWS
CRITICAL_SECTION  g_CriticalSection;
extern HANDLE g_semaphore;
#else
bool  g_CriticalSection;                        // Unix placeholder
#endif

// CLogicServer implementation
LEngList CLogicServer::g_EngList;

//OSVer g_osver;                                 // The Windows OS version
//LEngList g_EngList;                            // list of active engines
//LExStack g_ExStack;     // global stack of exceptions
//LExHandler g_ExH;     // global exception handler

#ifdef BUG_LOG
FILE     *g_dbg;
char     g_bugbuf[G_BUGBUF_SIZE];
#endif


// Synchronization for those functions that need it
#ifdef WINDOWS
// For Windows, the CRITICAL_SECTION variable is initialized in DllMain()
#ifdef xBUG_SYNC
#define START_SYNCHRONIZE( EID ) \
   { \
   EnterCriticalSection( EID ); \
   *g_sync_file << "START SYNC " << EID << "\n"; \
   g_sync_file->flush(); \
   }
#define STOP_SYNCHRONIZE( EID ) \
   { \
   LeaveCriticalSection( EID ); \
   *g_sync_file << "STOP SYNC " << EID << "\n"; \
   g_sync_file->flush(); \
   }
#else
#define START_SYNCHRONIZE( EID ) \
   EnterCriticalSection( EID )
#define STOP_SYNCHRONIZE( EID ) \
   LeaveCriticalSection( EID )
#endif

#else
#define START_SYNCHRONIZE( EID )
#define STOP_SYNCHRONIZE( EID )
#endif

// Used only for debugging, making it possible to quickly
// get ascii values of wide strings in the debugger, nope
// used to get ascii strings to initialize debug64 LSX
// as well.
char *toAscii(aCHAR *s)
{
#ifdef _UNICODE
   intC len = (intC)Lstrlen(s);
   //char *buf = new char[len];
   char *buf;
   LNEWX(buf, char[2*len+1]);
   if ((int)wcstombs(buf, s, len*2) >= len*2)
      buf[len*2-1] = 0;
   return buf;
#else
   return s;
#endif
}


/* ----- Internal Definitions -------------------------------------------- */


//aCHAR  *load_file;
//static aCHAR  xplfile[FILE_NAME_LENGTH];

/* functions defined in main that are bridges to external functions.
   these are the functions mapped to XF_GETC, WN_GETC, etc. in lsInit */

//int mainPutC(int);
//int mainPutS(aCHAR*);
//int mainGetC();
//int mainUngetC(int);

//int mainWnClose(void*);
//int mainWnPutS(aCHAR*, void*);
//int mainWnPutC(int, void*);
//int mainWnGetC(void*);
//int mainWnUngetC(int, void*);

/* the actual external call back functions that are provided by caller.
   these can be modified by subsequent API calls */

//int (lsCALLTYPE *exPutC)(int)=NULL;
//int (lsCALLTYPE *exPutS)(aCHAR*)=NULL;
//int (lsCALLTYPE *exGetC)()=NULL;
//int (lsCALLTYPE *exUngetC)(int)=NULL;

//int (*wnClose)(void*)=NULL;
//int (*wnPutS)(aCHAR*, void*)=NULL;
//int (*wnPutC)(int, void*)=NULL;
//int (*wnGetC)(void*)=NULL;
//int (*wnUngetC)(int, void*)=NULL;



/* ----- Internal Function Prototypes ------------------------------------ */

void str_chk(STRptr, intC);



/* ----- External Function Definitions ----------------------------------- */

/* Functions required for Windows DLL */
/* ---------------------------------- */
#if defined(LIB_DLL)

#if defined(WIN3)

int EXPFUNC LibMain( HANDLE hInstance, WORD wDataSeg, WORD wHeapSize, 
                     LPSTR lpszCmdLine)
{
/*
   if (wHeapSize > 0)
      UnlockData(0);
*/
  
   g_hInstance = hInstance;      // used when creating personal version notice 
   return 1;
}

int EXPFUNC _WEP(int param)
{
   //if (Initialized==TRUE)
   //   shut_down(g_CurEng);

   return 1;
}

#elif defined(WIN4)


extern "C" BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD dwReason, 
                               LPVOID lpRes)
{
   switch(dwReason)
   {
      case DLL_PROCESS_ATTACH:
         AInitializeCriticalSection(&g_CriticalSection);
#ifdef BUG_SYNC
         g_sync_file = new Lofstream;
         g_sync_file->open("c:\\temp\\syncfile.txt", std::ios::app);
         *g_sync_file << "Process Attach " << hinstDLL << "\n";
#endif BUG_SYNC
         g_hInstance = hinstDLL;  // Used in creating personal version notice
         break;
      case DLL_THREAD_ATTACH:
         g_hInstance = hinstDLL;  // Used in creating personal version notice
#ifdef BUG_SYNC
         *g_sync_file << "Thread Attach " << hinstDLL << "\n";
#endif BUG_SYNC
         break;
      case DLL_THREAD_DETACH:
#ifdef BUG_SYNC
         *g_sync_file << "Thread Detach " << hinstDLL << "\n";
#endif BUG_SYNC
         break;
      case DLL_PROCESS_DETACH:
         if (g_semaphore)
            ReleaseMutex(g_semaphore);
         g_semaphore = NULL;
#ifdef BUG_SYNC
         *g_sync_file << "Process Detach " << hinstDLL << "\n";
         g_sync_file->close();
#endif
         break;
   }
   return TRUE;
}

#endif                                      // WIN3/WIN4 

#endif                                      // LIB_DLL 

// LEngList maintains a list of open engines.
LEngList::~LEngList()
{
   LEngNode *oldhead;

   while (m_head != NULL)
     {
       oldhead = m_head;
       m_head = m_head->m_next;
       //delete oldhead;
       delete oldhead;
     }
}

void LEngList::Add( LEngine *pe )
{
   //LEngNode *pn = new LEngNode(pe, m_head);
//   START_SYNCHRONIZE
   LEngNode *pn;
   LNEWX(pn, LEngNode(pe, m_head));
//   AInitializeCriticalSection(&(pn->m_critical_section));
   m_head = pn;
   nengines++;
//   STOP_SYNCHRONIZE
}

inline LBOOL LEngList::Check( LEngine *pe )
{
   LEngNode *pnode = m_head;
   while (pnode != NULL)
     {
       if (pnode->m_peng == pe)
         return LTRUE;
       pnode = pnode->m_next;
     }
   return LFALSE;
}

//#ifdef WINDOWS
//CRITICAL_SECTION * LEngList::GetSemaphore( LEngine *pe )
//{
//   LEngNode *pnode = m_head;
//   while (pnode != NULL)
//     {
//       if (pnode->m_peng == pe)
//         return &(pnode->m_critical_section);
//       pnode = pnode->m_next;
//     }
//   return &g_CriticalSection;
//}
//#endif

void LEngList::Del( LEngine *pe )
{
//   START_SYNCHRONIZE
   LEngNode *pprev;
   LEngNode *ptemp;
   nengines--;

   if (m_head->m_peng == pe)
     {
       ptemp = m_head;
       //ADeleteCriticalSection(&(m_head->m_critical_section));
       m_head = m_head->m_next;
       //delete ptemp;
       delete ptemp;
//     STOP_SYNCHRONIZE
       return;
     }
   pprev = m_head;
   while (pprev != NULL)
     {
       if (pprev->m_next->m_peng == pe)
         {
           //ADeleteCriticalSection(&(pprev->m_critical_section));
           ptemp = pprev->m_next;
           pprev->m_next = ptemp->m_next;
           //delete ptemp;
           delete ptemp;
//         STOP_SYNCHRONIZE
           return;
         }
       pprev = pprev->m_next;
     }
//  STOP_SYNCHRONIZE
}

//bool CheckProfessional();


// LSAPI Interface 

#define CALLRC(FUNCTION) \
   START_SYNCHRONIZE( &(eid->m_critical_section) ); \
   RC rc = eid->FUNCTION; \
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); \
   return rc;

#define CALLRCG(FUNCTION) \
   START_SYNCHRONIZE( &g_CriticalSection ); \
   RC rc = eid->FUNCTION; \
   STOP_SYNCHRONIZE( &g_CriticalSection ); \
   return rc;

#define CALLTF(FUNCTION) \
   START_SYNCHRONIZE( &(eid->m_critical_section) ); \
   TF tf = eid->FUNCTION; \
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); \
   return tf;

#ifdef _UNICODE
#define CALLRCSIN(FUNCTION, STRING)\
   START_SYNCHRONIZE( &(eid->m_critical_section) ); \
   int len = (int)strlen(STRING) + 1; \
   aCHAR *ss = new aCHAR[len]; \
   g_lenv.e_mbs_in(eid, ss, STRING, len); \
   RC rc = eid->FUNCTION; \
   delete[] ss; \
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); \
   return rc;
#else
#define CALLRCSIN(FUNCTION, STRING) \
   START_SYNCHRONIZE( &(eid->m_critical_section) ); \
   aCHAR *ss = STRING; \
   RC rc = eid->FUNCTION; \
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); \
   return rc;
#endif

#ifdef _UNICODE
#define CALLRCSING(FUNCTION, STRING)\
   START_SYNCHRONIZE( &g_CriticalSection ); \
   int len = (int)strlen(STRING) + 1; \
   aCHAR *ss = new aCHAR[len]; \
   g_lenv.e_mbs_in(eid, ss, STRING, len); \
   RC rc = eid->FUNCTION; \
   delete[] ss; \
   STOP_SYNCHRONIZE( &g_CriticalSection ); \
   return rc;
#else
#define CALLRCSING(FUNCTION, STRING) \
   START_SYNCHRONIZE( &g_CriticalSection ); \
   aCHAR *ss = STRING; \
   RC rc = eid->FUNCTION; \
   STOP_SYNCHRONIZE( &g_CriticalSection ); \
   return rc;
#endif

#ifdef _UNICODE
#define CALLSOUT(FUNCTION, STRING, LEN) \
   START_SYNCHRONIZE( &(eid->m_critical_section) ); \
   aCHAR *ss = new aCHAR[LEN+1]; \
   eid->FUNCTION; \
   if (g_lenv.e_mbs_out(eid, STRING, ss, LEN) >= LEN) \
      ss[LEN-1] = 0; \
   delete[] ss; \
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); 
#else
#define CALLSOUT(FUNCTION, STRING, LEN) \
   START_SYNCHRONIZE( &(eid->m_critical_section) ); \
   aCHAR *ss = STRING; \
   eid->FUNCTION; \
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); 
#endif

#ifdef _UNICODE
#define CALLTFSIN(FUNCTION, STRING) \
   START_SYNCHRONIZE( &(eid->m_critical_section) ); \
   int len = (int)strlen(STRING) + 1; \
   aCHAR *ss = new aCHAR[len]; \
   g_lenv.e_mbs_in(eid, ss, STRING, len); \
   TF tf = eid->FUNCTION; \
   delete[] ss; \
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); \
   return tf;
#else
#define CALLTFSIN(FUNCTION, STRING) \
   START_SYNCHRONIZE( &(eid->m_critical_section) ); \
   aCHAR *ss = STRING; \
   TF tf = eid->FUNCTION;
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); \
   return tf;
#endif

#ifdef _UNICODE
#define CALLRCSOUT(FUNCTION, STRING, LEN) \
   START_SYNCHRONIZE( &(eid->m_critical_section) ); \
   aCHAR *ss = new aCHAR[LEN+1]; \
   RC rc = eid->FUNCTION; \
   if ((int)g_lenv.e_mbs_out(eid, STRING, ss, LEN) >= LEN) \
      ss[LEN-1] = 0; \
   delete[] ss; \
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); \
   return rc;
#else
#define CALLRCSOUT(FUN, STRING, LEN) \
   START_SYNCHRONIZE( &(eid->m_critical_section) ); \
   aCHAR *ss = STRING; \
   RC rc = eid->FUN;
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); \
   return rc;
#endif

/* Set up Prolog environment */
/* ------------------------- */

extern "C" RC LSAPI lsInitA(ENGidptr eidp, char* ininame)
{
//printf("Main lsInitA\n");
   START_SYNCHRONIZE( &g_CriticalSection );
   RC rc;
   // Allocate a new engine.  
   //LEAK("lsInitA before allocating new LEngine");
//printf("Main new LEngineing\n");
   *eidp = new LEngine;
//printf("Main new LEngined\n");
   //LEAK("lsInitA after allocating new LEngine");
   if (*eidp == NULL)
   {
     STOP_SYNCHRONIZE( &g_CriticalSection );
     return NOTOK;                      // If it fails simply return a -1
   }
// That was the only non-standard error message.
//printf("Main adding the eidp\n");
   CLogicServer::g_EngList.Add(*eidp);         // Save the engine on a list for verification.

   //LEAK("lsInitA after adding engine to list and before init");
#ifdef _UNICODE
//printf("Main, are we really unicode??\n");
   intC len = 1 + (intC)strlen(ininame);
   aCHAR *ss = new aCHAR[len];
//printf("Main, guess so, calling mbstowcs\n");
   mbstowcs(ss, ininame, len);
//printf("Main, now calling the real Init\n");
   rc = (*eidp)->Init(ss);
//printf("Main, that went better than expected, is the problem delete?\n");
   delete[] ss;
#else
//printf("Main, calling the real Init\n");
   rc = (*eidp)->Init(ininame);
#endif
//printf("Main, real init seems to have worked\n");
   //LEAK("lsInitA after calling init");
   STOP_SYNCHRONIZE( &g_CriticalSection );
   return rc;
}

extern "C" RC LSAPI lsInitW(ENGidptr eidp, STRptr ininame)
{
//printf("Main lsInitW\n"); 
   START_SYNCHRONIZE( &g_CriticalSection );
   // Allocate a new engine.
   //LEAK("lsInitW before allocating new LEngine");
   *eidp = new LEngine;
   //LEAK("lsInitW after allocating new LEngine");
   if (*eidp == NULL)
   {
     STOP_SYNCHRONIZE( &g_CriticalSection );
     return NOTOK;                      // If it fails simply return a -1.  
   }
                               // This is the only non-standard error message.
   CLogicServer::g_EngList.Add(*eidp);        // Save the engine on a list for verification.
   //LEAK("lsInitW after adding engine to list and before init");
   RC rc = (*eidp)->Init(ininame);
   //LEAK("lsInitW after calling init");
   STOP_SYNCHRONIZE( &g_CriticalSection );
   return rc;
}

extern "C" RC LSAPI lsInit2A(ENGidptr eidp, char* inistring)
{
   START_SYNCHRONIZE( &g_CriticalSection );
   RC rc;
   //*eidp = new LEngine;
   LNEWX(*eidp, LEngine);
   if (eidp == NULL)
   {
      STOP_SYNCHRONIZE( &g_CriticalSection );
      return NOTOK;
   }

   CLogicServer::g_EngList.Add(*eidp);

#ifdef _UNICODE
   intC len = 1 + (intC)strlen(inistring);
   aCHAR *ss;
   LNEWX(ss, aCHAR[len]);
   mbstowcs(ss, inistring, len);
   rc = (*eidp)->Init2(ss);
   //delete[] ss;
   delete[] ss;
#else
   rc = (*eidp)->Init2(inistring);
#endif
   STOP_SYNCHRONIZE( &g_CriticalSection );
   return rc;
}

extern "C" RC LSAPI lsInit2W(ENGidptr eidp, STRptr inistring)
{
   START_SYNCHRONIZE( &g_CriticalSection );
   //*eidp = new LEngine;
   LNEWX(*eidp, LEngine);
   if (eidp == NULL) {
      STOP_SYNCHRONIZE( &g_CriticalSection );
      return NOTOK;
   }

   CLogicServer::g_EngList.Add(*eidp);
   RC rc = (*eidp)->Init2(inistring);
   STOP_SYNCHRONIZE( &g_CriticalSection );
   return rc;
}

extern "C" RC  LSAPI lsInitLSX(ENGid eid, VOIDptr p)
{ CHECK(NOTOK); CALLRCG(InitLSX(p));}

extern "C" RC  LSAPI lsAddLSXA(ENGid eid, char* s, VOIDptr p)
{ CHECK(NOTOK); CALLRCSING(AddLSX(ss, p), s);}

extern "C" RC  LSAPI lsAddLSXW(ENGid eid, STRptr s, VOIDptr p)
{ CHECK(NOTOK); CALLRCG(AddLSX(s, p));}

extern "C" RC  LSAPI lsAddPredA(ENGid eid, char* name, ARITY arity, 
                                ExtPred func, VOIDptr vp)
                                       // Add a single predicate to the table.
{ CHECK(NOTOK); CALLRCSING(AddPred(ss, arity, func, vp), name);}

extern "C" RC  LSAPI lsAddPredW(ENGid eid, STRptr name, ARITY arity, 
                                ExtPred func, VOIDptr vp)
                                       // Add a single predicate to the table.
{ CHECK(NOTOK);   CALLRCG(AddPred(name, arity, func, vp));}

extern "C" RC  LSAPI lsInitPredsA(ENGid eid, PRED_INITAptr pint)
{
   //char buf[100];
   //sprintf(buf, "lsInitPredsA %x", eid);
   //MessageBoxA(NULL, buf, NULL, MB_OK);

   CHECK(NOTOK); CALLRCG(InitPreds(pint));}

#ifdef _UNICODE
extern "C" RC  LSAPI lsInitPredsW(ENGid eid, PRED_INITWptr pint)
{
   //char buf[100];
   //sprintf(buf, "lsInitPredsW %x", eid);
   //MessageBoxA(NULL, buf, NULL, MB_OK);
   
   CHECK(NOTOK); CALLRCG(InitPreds(pint));}
#else
extern "C" RC  LSAPI lsInitPredsW(ENGid eid, PRED_INITAptr pint)
{ CHECK(NOTOK); CALLRCG(InitPreds(pint));}
#endif

extern "C" RC  LSAPI lsLoadA(ENGid eid, char* xplname)
{ CHECK(NOTOK); CALLRCSIN(Load(ss), xplname);}

extern "C" RC  LSAPI lsLoadW(ENGid eid, STRptr xplname)
{ CHECK(NOTOK); CALLRC(Load(xplname));}

extern "C" RC  LSAPI lsLoadFromMemoryA(ENGid eid, char* xplname, int length, unsigned char* p)
{ CHECK(NOTOK); CALLRCSIN(LoadFromMemory(ss, length, p), xplname);}

extern "C" RC  LSAPI lsLoadFromMemoryW(ENGid eid, STRptr xplname, int length, unsigned char* p)
{ CHECK(NOTOK); CALLRC(LoadFromMemory(xplname, length, p));}

extern "C" TF  LSAPI lsMain(ENGid eid)
{ CHECK(NOTOK); CALLTF(Main());}

extern "C" RC  LSAPI lsReset(ENGid eid)
{ CHECK(NOTOK); CALLRC(Reset());}

extern "C" RC  LSAPI lsClose(ENGid eid)
{ 
   CHECK(NOTOK);   
   START_SYNCHRONIZE( &g_CriticalSection );
   LEAK("lsClose start");
   RC rc=eid->Close();
   LEAK("lsClose after calling close");
   CLogicServer::g_EngList.Del(eid);
   LEAK("lsClose after deleting engine");
   STOP_SYNCHRONIZE( &g_CriticalSection );
   return rc;
}


/* Functions to get and set predicate parameters */
/* --------------------------------------------- */

extern "C" RC  LSAPI lsGetParm(ENGid eid, int iarg, cTYPE ctyp, VOIDptr val)
{ CHECK(NOTOK); CALLRC(GetParm(iarg, ctyp, val));}

extern "C" int  LSAPI lsStrParmLen(ENGid eid, int iarg)
{ CHECK(NOTOK); START_SYNCHRONIZE( &(eid->m_critical_section) ); int i = eid->StrParmLen(iarg); STOP_SYNCHRONIZE( &(eid->m_critical_section) ); return i;}

extern "C" pTYPE  LSAPI lsGetParmType(ENGid eid, int iparm)
{ CHECK(pERR); START_SYNCHRONIZE( &(eid->m_critical_section) );  pTYPE p = eid->GetParmType(iparm); STOP_SYNCHRONIZE( &(eid->m_critical_section) ); return p;}

extern "C" TF  LSAPI lsUnifyParm(ENGid eid, int iarg, cTYPE ct, VOIDptr pVal)
{ CHECK(NOTOK); CALLTF(UnifyParm(iarg, ct, pVal));}

/* Calling Prolog predicates from C */
/* -------------------------------- */

extern "C" TF LSAPI lsExecStrA(ENGid eid, LSTERMptr tp, char* s)
{ CHECK(NOTOK); CALLTFSIN(ExecStr(tp, ss), s);}

extern "C" TF LSAPI lsExecStrW(ENGid eid, LSTERMptr tp, STRptr s)
{ CHECK(NOTOK); CALLTF(ExecStr(tp, s));}

extern "C" TF  LSAPI lsExec(ENGid eid, LSTERMptr tp)
{ CHECK(NOTOK); CALLTF(Exec(tp));}

extern "C" TF LSAPI lsCallStrA(ENGid eid, LSTERMptr tp, char* s)
{ CHECK(NOTOK); CALLTFSIN(CallStr(tp, ss), s);}

extern "C" TF LSAPI lsCallStrW(ENGid eid, LSTERMptr tp, STRptr s)
{ CHECK(NOTOK); CALLTF(CallStr(tp, s));}

extern "C" TF  LSAPI lsCall(ENGid eid, LSTERMptr tp)
{ CHECK(NOTOK); CALLTF(Call(tp));}

extern "C" TF  LSAPI lsRedo(ENGid eid)
{ CHECK(NOTOK); CALLTF(Redo());}

extern "C" RC  LSAPI lsClearCall(ENGid eid)
{ CHECK(NOTOK); CALLRC(ClearCall());}

extern "C" RC   LSAPI lsAsserta(ENGid eid, LSTERM t)
{ CHECK(NOTOK); CALLRC(Asserta(t));}

extern "C" RC   LSAPI lsAssertz(ENGid eid, LSTERM t)
{ CHECK(NOTOK); CALLRC(Assertz(t));}

extern "C" RC   LSAPI lsRetract(ENGid eid, LSTERM t)
{ CHECK(NOTOK); CALLRC(Retract(t));}

extern "C" RC LSAPI lsAssertaStrA(ENGid eid, char* s)
{ CHECK(NOTOK); CALLRCSIN(AssertaStr(ss), s);}

extern "C" RC LSAPI lsAssertaStrW(ENGid eid, STRptr s)
{ CHECK(NOTOK); CALLRC(AssertaStr(s));}

extern "C" RC LSAPI lsAssertzStrA(ENGid eid, char* s)
{ CHECK(NOTOK); CALLRCSIN(AssertzStr(ss), s);}

extern "C" RC LSAPI lsAssertzStrW(ENGid eid, STRptr s)
{ CHECK(NOTOK); CALLRC(AssertzStr(s));}

extern "C" RC LSAPI lsRetractStrA(ENGid eid, char* s)
{ CHECK(NOTOK); CALLRCSIN(RetractStr(ss), s);}

extern "C" RC LSAPI lsRetractStrW(ENGid eid, STRptr s)
{ CHECK(NOTOK); CALLRC(RetractStr(s));}

/* String/Term conversions and calls */
/* --------------------------------- */


extern "C" RC  LSAPI lsTermToStrA(ENGid eid, LSTERM t, char* s, int imax)
{ CHECK(NOTOK); CALLRCSOUT(TermToStr(t, ss, imax), s, imax);}

extern "C" RC  LSAPI lsTermToStrW(ENGid eid, LSTERM t, STRptr s, int imax)
{ CHECK(NOTOK); CALLRC(TermToStr(t, s, imax));}

extern "C" RC  LSAPI lsTermToStrQA(ENGid eid, LSTERM t, char* s, int imax)
{ CHECK(NOTOK); CALLRCSOUT(TermToStrQ(t, ss, imax), s, imax);}

extern "C" RC  LSAPI lsTermToStrQW(ENGid eid, LSTERM t, STRptr s, int imax)
{ CHECK(NOTOK); CALLRC(TermToStrQ(t, s, imax));}

extern "C" RC  LSAPI lsStrToTermA(ENGid eid, LSTERMptr tp, char* s)
{ CHECK(NOTOK); CALLRCSIN(StrToTerm(tp, ss), s);}

extern "C" RC  LSAPI lsStrToTermW(ENGid eid, LSTERMptr tp, STRptr s)
{ CHECK(NOTOK); CALLRC(StrToTerm(tp, s));}

/* Making Prolog Types */
/* ------------------- */


extern "C" RC  LSAPI lsMakeAtomA(ENGid eid, LSTERMptr tp, char* s)
{ CHECK(NOTOK); CALLRCSIN(MakeAtom(tp, ss), s);}

extern "C" RC  LSAPI lsMakeAtomW(ENGid eid, LSTERMptr tp, STRptr s)
{ CHECK(NOTOK); CALLRC(MakeAtom(tp, s));}

extern "C" RC  LSAPI lsMakeInt(ENGid eid, LSTERMptr tp, intC iC)
{ CHECK(NOTOK); CALLRC(MakeInt(tp, iC));}

extern "C" RC  LSAPI lsMakeFloat(ENGid eid, LSTERMptr tp, double f)
{ CHECK(NOTOK); CALLRC(MakeFloat(tp, f));}

extern "C" RC  LSAPI lsMakeAddr(ENGid eid, LSTERMptr tp, VOIDptr p)
{ CHECK(NOTOK); CALLRC(MakeAddr(tp, p));}

extern "C" RC  LSAPI lsMakeStrA(ENGid eid, LSTERMptr tp, char* s)
{ CHECK(NOTOK); CALLRCSIN(MakeStr(tp, ss), s);}

extern "C" RC  LSAPI lsMakeStrW(ENGid eid, LSTERMptr tp, STRptr s)
{ CHECK(NOTOK); CALLRC(MakeStr(tp, s));}

/* Getting C values from Prolog terms */
/* ---------------------------------- */


extern "C" RC  LSAPI lsGetTerm(ENGid eid, LSTERM t, cTYPE ct, VOIDptr pVal)
{ CHECK(NOTOK); CALLRC(GetTerm(t, ct, pVal));}

extern "C" pTYPE  LSAPI lsGetTermType(ENGid eid, LSTERM t)
{ CHECK(pERR); START_SYNCHRONIZE( &(eid->m_critical_section) ); pTYPE p = eid->GetTermType(t); STOP_SYNCHRONIZE( &(eid->m_critical_section) ); return p;}

extern "C" int  LSAPI lsStrTermLen(ENGid eid, LSTERM t)
{ CHECK(NOTOK); START_SYNCHRONIZE( &(eid->m_critical_section) ); int i = eid->StrTermLen(t); STOP_SYNCHRONIZE( &(eid->m_critical_section) ); return i;}

/* Structure hacking functions */
/* --------------------------- */

extern "C" RC  LSAPI lsGetFAA(ENGid eid, LSTERM t, char* s, ARITYptr ap)
{ CHECK(NOTOK); CALLRCSOUT(GetFA(t, ss, ap), s, 512);}

extern "C" RC  LSAPI lsGetFAW(ENGid eid, LSTERM t, STRptr s, ARITYptr ap)
{ CHECK(NOTOK); CALLRC(GetFA(t, s, ap));}

extern "C" RC  LSAPI lsMakeFAA(ENGid eid, LSTERMptr tp, char* s, ARITY a)
{ CHECK(NOTOK); CALLRCSIN(MakeFA(tp, ss, a), s);}

extern "C" RC  LSAPI lsMakeFAW(ENGid eid, LSTERMptr tp, STRptr s, ARITY a)
{ CHECK(NOTOK); CALLRC(MakeFA(tp, s, a));}

extern "C" TF  LSAPI lsUnifyArg(ENGid eid, LSTERMptr tpStruc, int iarg, 
                                cTYPE ct, VOIDptr pVal)
{ CHECK(NOTOK); CALLTF(UnifyArg(tpStruc, iarg, ct, pVal));}

extern "C" int  LSAPI lsStrArgLen(ENGid eid, LSTERM ti, int iarg)
{ CHECK(NOTOK); START_SYNCHRONIZE( &(eid->m_critical_section) ); int i = eid->StrArgLen(ti, iarg); STOP_SYNCHRONIZE( &(eid->m_critical_section) ); return i;}

extern "C" pTYPE  LSAPI lsGetArgType(ENGid eid, LSTERM tStruc, int iarg)
{ CHECK(pERR); START_SYNCHRONIZE( &(eid->m_critical_section) ); pTYPE p = eid->GetArgType(tStruc, iarg); STOP_SYNCHRONIZE( &(eid->m_critical_section) ); return p;}

extern "C" RC  LSAPI lsGetArg(ENGid eid, LSTERM tStruc, int iarg, cTYPE ctype, 
                              VOIDptr vp)
{ CHECK(NOTOK); CALLRC(GetArg(tStruc, iarg, ctype, vp));}

extern "C" TF  LSAPI lsUnify(ENGid eid, LSTERM t1, LSTERM t2)
{ CHECK(NOTOK); CALLTF(Unify(t1, t2));}

/* List hacking functions */
/* ---------------------- */

extern "C" RC  LSAPI lsMakeList(ENGid eid, LSTERMptr tp)
{ CHECK(NOTOK); CALLRC(MakeList(tp));}

extern "C" RC  LSAPI lsPopList(ENGid eid, LSTERMptr tp, cTYPE ctyp, VOIDptr vp)
{ CHECK(NOTOK); CALLRC(PopList(tp, ctyp, vp));}

extern "C" RC  LSAPI lsPushList(ENGid eid, LSTERMptr tpList, LSTERM tElem)
{ CHECK(NOTOK); CALLRC(PushList(tpList, tElem));}

extern "C" RC  LSAPI lsGetHead(ENGid eid, LSTERM t, cTYPE ctyp, VOIDptr vp)
{ CHECK(NOTOK); CALLRC(GetHead(t, ctyp, vp));}

extern "C" LSTERM  LSAPI lsGetTail(ENGid eid, LSTERM t)
{ CHECK(NULL); START_SYNCHRONIZE( &(eid->m_critical_section) ); LSTERM l = eid->GetTail(t); STOP_SYNCHRONIZE( &(eid->m_critical_section) ); return l;}

/* Error Handling */
/* -------------- */

extern "C" RC  LSAPI lsErrRaiseA(ENGid eid, char* s)
{ CHECK(NOTOK); CALLRCSIN(ErrRaise(ss), s);}

extern "C" RC  LSAPI lsErrRaiseW(ENGid eid, STRptr s)
{ CHECK(NOTOK); CALLRC(ErrRaise(s));}


extern "C" ExType  LSAPI lsGetExceptType(ENGid eid)
{ CHECK(BADENG); START_SYNCHRONIZE( &(eid->m_critical_section) ); ExType e = eid->GetExceptType(); STOP_SYNCHRONIZE( &(eid->m_critical_section) ); return e;}

extern "C" RC  LSAPI lsGetExceptRC(ENGid eid)
{ CHECK(NOTOK); CALLRC(GetExceptRC());}

extern "C" void  LSAPI lsGetExceptMsgA(ENGid eid, char* s, int buflen)
{
   if (! CLogicServer::g_EngList.Check(eid))
      return;

   CALLSOUT(GetExceptMsg(ss, buflen), s, buflen);
}

extern "C" void  LSAPI lsGetExceptMsgW(ENGid eid, wchar_t* s, int buflen)
{
#ifdef _UNICODE
   if (! CLogicServer::g_EngList.Check(eid))
      return;

   START_SYNCHRONIZE( &(eid->m_critical_section) ); 
   eid->GetExceptMsg(s, buflen);
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); 
#else
   return;
#endif
}

extern "C" void  LSAPI lsGetExceptReadBufferA(ENGid eid, char* s, int buflen)
{
   if (! CLogicServer::g_EngList.Check(eid))
      return;

   CALLSOUT(GetExceptReadBuffer(ss, buflen), s, buflen);
}

extern "C" void  LSAPI lsGetExceptReadBufferW(ENGid eid, STRptr s, int buflen)
{
   if (! CLogicServer::g_EngList.Check(eid))
      return;

   START_SYNCHRONIZE( &(eid->m_critical_section) ); 
   eid->GetExceptReadBuffer(s, buflen);
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); 
}

extern "C" void  LSAPI lsGetExceptReadFileNameA(ENGid eid, char* s, int buflen)
{
   if (! CLogicServer::g_EngList.Check(eid))
      return;

   CALLSOUT(GetExceptReadFileName(ss, buflen), s, buflen);
}

extern "C" void  LSAPI lsGetExceptReadFileNameW(ENGid eid, STRptr s, 
                                                int buflen)
{
   if (! CLogicServer::g_EngList.Check(eid))
      return;

   START_SYNCHRONIZE( &(eid->m_critical_section) ); 
   eid->GetExceptReadFileName(s, buflen);
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); 
}

extern "C" int  LSAPI lsGetExceptLineno(ENGid eid)
{
   if (! CLogicServer::g_EngList.Check(eid))
      return NOTOK;

   START_SYNCHRONIZE( &(eid->m_critical_section) ); 
   int i = eid->GetExceptLineno();
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); 
   return i;
}

extern "C" void  LSAPI lsGetExceptCallStackA(ENGid eid, char* s, int buflen)
{
   if (! CLogicServer::g_EngList.Check(eid))
      return;

   CALLSOUT(GetExceptCallStack(ss, buflen), s, buflen);
}

extern "C" void  LSAPI lsGetExceptCallStackW(ENGid eid, STRptr s, int buflen)
{
   if (! CLogicServer::g_EngList.Check(eid))
      return;

   START_SYNCHRONIZE( &(eid->m_critical_section) ); 
   eid->GetExceptCallStack(s, buflen);
   STOP_SYNCHRONIZE( &(eid->m_critical_section) ); 
}


/* Variable setting/getting functions */
/* ---------------------------------- */

extern "C" RC  LSAPI lsGetVersionA(ENGid eid, char* s)
{ CHECK(NOTOK); CALLRCSOUT(GetVersion(ss), s, 512);}

extern "C" RC  LSAPI lsGetVersionW(ENGid eid, STRptr s)
{ CHECK(NOTOK); CALLRC(GetVersion(s));}

extern "C" RC  LSAPI lsSetCommandArgsA(ENGid eid, int argc, char** argv)
{ CHECK(NOTOK);
#ifdef _UNICODE
   START_SYNCHRONIZE( &(eid->m_critical_section) );
   int len;
   LEngine *m_peng = eid;   // need both because of macro expansions
   eid->m_argwc = argc > MAX_COMMAND_ARGS ? MAX_COMMAND_ARGS : argc;
   for (int i = 0; i < argc; i++)
     {
       len = (int)strlen(argv[i]) + 1;
       //eid->m_argwv[i] = new wchar_t[len];
       LNEW(eid->m_argwv[i], wchar_t[len], aS("miscellaneous"));
       mbstowcs(eid->m_argwv[i], argv[i], len);
     }
   RC rc = eid->SetCommandArgs(m_peng->m_argwc, m_peng->m_argwv);
   STOP_SYNCHRONIZE( &(eid->m_critical_section) );
   return rc;
#else
   START_SYNCHRONIZE( &(eid->m_critical_section) );
   RC rc = eid->SetCommandArgs(argc, argv);
   STOP_SYNCHRONIZE( &(eid->m_critical_section) );
   return rc;
#endif
}

extern "C" RC  LSAPI lsSetCommandArgsW(ENGid eid, int argc, aCHAR** argv)
{ CHECK(NOTOK); CALLRC(SetCommandArgs(argc, argv));}

extern "C" RC  LSAPI lsSetStream(ENGid eid, STREAM iStream, int j)
{ CHECK(NOTOK); CALLRC(SetStream(iStream, j));}

extern "C" int  LSAPI lsGetStream(ENGid eid, STREAM iStream)
{ CHECK(NOTOK); START_SYNCHRONIZE( &(eid->m_critical_section) ); int i = eid->GetStream(iStream); STOP_SYNCHRONIZE( &(eid->m_critical_section) ); return i;}

extern "C" RC  LSAPI lsSetInput(ENGid eid, pX_GETC fpGetc, pX_UNGETC fpUngetc )
{ CHECK(NOTOK); CALLRC(SetInput(fpGetc, fpUngetc));}

extern "C" RC  LSAPI lsSetOutputA(ENGid eid, pX_PUTC fpPutc, pX_PUTSA fpPuts )
{ CHECK(NOTOK); CALLRC(SetOutput(fpPutc, fpPuts));}

#ifdef _UNICODE
extern "C" RC  LSAPI lsSetOutputW(ENGid eid, pX_PUTC fpPutc, pX_PUTSW fpPuts )
{ CHECK(NOTOK); CALLRC(SetOutput(fpPutc, fpPuts));}
#else
extern "C" RC  LSAPI lsSetOutputW(ENGid eid, pX_PUTC fpPutc, pX_PUTSA fpPuts )
                                                  // just shadow lsSetOutputA
{ CHECK(NOTOK); CALLRC(SetOutput(fpPutc, fpPuts));}
#endif

extern "C" RC  LSAPI lsSetIOArg(ENGid eid, VOIDptr vp)
{ CHECK(NOTOK); CALLRC(SetIOArg(vp));}

extern "C" int LSAPI lsOpenUserStreamA(ENGid eid, char* alias,
      paUSER_GET_LINE fpUserGetLine, paUSER_PUT_STRING fpUserPutString,
      VOIDptr vp)
{ CHECK(NOTOK); START_SYNCHRONIZE( &(eid->m_critical_section) ); int i = eid->OpenUserStream(alias, fpUserGetLine, fpUserPutString, vp); STOP_SYNCHRONIZE( &(eid->m_critical_section) ); return i;}

extern "C" int LSAPI lsOpenUserStreamW(ENGid eid, aCHAR* alias,
      pwUSER_GET_LINE fpUserGetLine, pwUSER_PUT_STRING fpUserPutString,
      VOIDptr vp)
{ CHECK(NOTOK); START_SYNCHRONIZE( &(eid->m_critical_section) ); int i = eid->OpenUserStream(alias, fpUserGetLine, fpUserPutString, vp); STOP_SYNCHRONIZE( &(eid->m_critical_section) ); return i;}


/* Linker functions */
/* ---------------- */

#ifdef WINDOWS
extern "C" int __cdecl aLinkW(void( __cdecl *pfM)(aCHAR*), int argctr, aCHAR **pargv)
{
   Linker L(pfM);

   int rc;
   rc = L.Link(argctr, pargv);
   return rc;
}

extern "C" int __cdecl aLinkA(void( __cdecl *pfM)(char*), int argctr, char **pargv)
{
   Linker L(pfM);
   int rc;
   rc = L.Link(argctr, pargv);
   return rc;
}
#else
extern "C" int aLinkW(void(*pfM)(aCHAR*), int argctr, aCHAR **pargv)
{
   Linker L(pfM);

   int rc;
   rc = L.Link(argctr, pargv);
   return rc;
}

extern "C" int aLinkA(void(*pfM)(char*), int argctr, char **pargv)
{
   Linker L(pfM);
   int rc;
   rc = L.Link(argctr, pargv);
   return rc;
}
#endif

// Support for threaded ExecStrs for Windows GUI applications,
// like debuggers
#ifdef THREADED_EXEC_STR

extern "C" TF LSAPI lsExecStrThA(ENGid eid, LSTERMptr tp, char* s)
{
   CHECK(NOTOK);
   START_SYNCHRONIZE( &(eid->m_critical_section) );
   int len = (int)strlen(s) + 1;
   aCHAR *ss = new aCHAR[len];
   mbstowcs(ss, s, len);
   ThreadedExecStr(eid, tp, ss);
   //delete[] ss;
   STOP_SYNCHRONIZE( &(eid->m_critical_section) );
   return TRUE;
}

extern "C" TF LSAPI lsExecStrThW(ENGid eid, LSTERMptr tp, STRptr s)
{
   CHECK(NOTOK); 
   START_SYNCHRONIZE( &(eid->m_critical_section) );
   ThreadedExecStr(eid, tp, s);
   STOP_SYNCHRONIZE( &(eid->m_critical_section) );
   return TRUE;
}

extern "C" RC LSAPI lsPutActionA(ENGid eid, char* s)
{
   START_SYNCHRONIZE( &(eid->m_critical_section) );
   int len = (int)strlen(s) + 1;
   aCHAR *ss = new aCHAR[len];
   mbstowcs(ss, s, len);
   eid->m_action_buffer -> Put(ss);
   delete[] ss;
   STOP_SYNCHRONIZE( &(eid->m_critical_section) );
   return OK;
}

extern "C" RC LSAPI lsPutActionW(ENGid eid, STRptr s)
{
   START_SYNCHRONIZE( &(eid->m_critical_section) );
   eid->m_action_buffer -> Put(s);
   STOP_SYNCHRONIZE( &(eid->m_critical_section) );
   return OK;
}

extern "C" RC LSAPI lsGetActionState(ENGid eid)
{
   // overload RC to use return code from GetState which indicates
   // if Prolog is waiting or not.
   RC rc;
   START_SYNCHRONIZE( &(eid->m_critical_section) );
   rc = eid->m_action_buffer -> GetState();
   STOP_SYNCHRONIZE( &(eid->m_critical_section) );
   return rc;
}

#endif // THREADED_EXEC_STR


//
// ARulesXL Runtime Registration
//

/* Helper to determine if a character is a whitespace 
   character.  Returns 1 for true, 0 for false. */ 
int bWhitespaceChar(char cChar) { 
    if((cChar == 0x0A) || /* Newline. */ 
       (cChar == 0x0D) || /* Carriage Return. */ 
       (cChar == 0x20) || /* Space. */ 
       (cChar == 0x0B) || 
       (cChar == 0x0C)) 
       return 1; 
    else 
       return 0; 
} 

/* Helper to determine if a character is a non-word 
   character.  Returns 1 for true, 0 for false. */ 
int bWordChar(char cChar) {
    if((cChar >= 'a' && cChar <= 'z') || 
       (cChar >= 'A' && cChar <= 'Z') || 
       (cChar >= '0' && cChar <= '9') || 
       (cChar == '+')) 
       return 1; 
    else 
       return 0; 
} 

/* %-encode a string according to RFC 1630.  This 
  function assumes that there are not already %-encoded 
  characters present in the string to be encoded. */ 
void RFC1630Encode(char* strString) {
    size_t uiStrLen,
           uiStrLenEncoded;
     unsigned int uiIndex, 
                  uiCurrentPosition = 0; 
    char cCurrentChar = 0; 
    char *strStringTemp; 
    char strEncodedChar[4]; 

    /* Find the initial length of the string. */ 
    uiStrLen = strlen(strString); 
    uiStrLenEncoded = uiStrLen; 

    /* Find out how big the %-encoded string will be. */ 
    for(uiIndex=0;uiIndex<uiStrLen;uiIndex++) 
    { 
        cCurrentChar = strString[uiIndex]; 

        /* If this character must be encoded, it will 
           take up an additional two characters in the 
           encoded string. */ 
        if(!bWordChar(cCurrentChar)) 
            uiStrLenEncoded += 2; 
    } 

    /* Allocate space for the extra characters that 
       will be necessary when this is encoded. */ 
    strStringTemp = (char *)malloc(uiStrLenEncoded+1); 

    /* Copy the un-encoded string to the temp string. */ 
    strcpy(strStringTemp,strString); 

    /* Empty the original string. */ 
    strString[0] = 0; 

    /* Encode the string into the new buffer. */ 
    for(uiIndex=0;uiIndex<uiStrLen;uiIndex++) 
    { 
        cCurrentChar = strStringTemp[uiIndex]; 

        /* If the character doesn't have to be encoded, */ 
        if(bWordChar(cCurrentChar)) 
        { 
            /* ...then simply copy it into the output. */ 
            strString[uiCurrentPosition++] = 
                strStringTemp[uiIndex]; 
            strString[uiCurrentPosition] = 0; 
        } 
        else 
            /* ...otherwise encode it into the output. */ 
        { 
            sprintf(strEncodedChar,"%%%X", 
                strStringTemp[uiIndex]); 
            strcat(strString,strEncodedChar); 
            uiCurrentPosition += 3; 
        } 
    } 
    free(strStringTemp);
}

/* Performs encoding on a string in accordance with 
   commonly accepted standards set forth in cgi-lib.pl 
   and CGI.pm. */ 
void CGIEncode(char* strString) {
    size_t uiStrLen; 
    unsigned int uiIndex = 0; 
    char cCurrentChar = 0; 

    /* Find the initial length of the string. */ 
    uiStrLen = strlen(strString); 

    /* Loop through the string, replacing 
       whitespace characters one-for-one. */ 
    for(uiIndex=0;uiIndex<uiStrLen;uiIndex++) 
    { 
        cCurrentChar = strString[uiIndex]; 

        /* If this is whitespace, replace it 
           with '+', otherwise pass it through. */ 
        if(bWhitespaceChar(cCurrentChar)) 
            strString[uiIndex] = '+'; 
    } 

    /* Encode non-word characters according to 
       RFC 1630. */ 
    RFC1630Encode(strString); 
} 

// ARulesXL Runtime Design Notes
//
// Either ARULESXL or ARULESXLRT is defined
// ARULESXL is defined for the developer/excel dll
// ARULESXLRT is always defined for all runtimes
// 
// Then additional defines exists for
// ARULESXLMODL is also defined for the MODL runtime
//
// In Armadillo, the runtime must never expire or custom versions like MODL will fail.
// All expiration must be handled in the engine or Prolog.

int register_runtime(DWORD method, char *proxyname, char *redistkey, char *runtimeid)
{
   return 0;
   /*
#ifdef ARULESXLMODL
   return 0;
#endif

#ifdef ARULESXLRT
   int result = -1;
   HINTERNET inet, ifile;
   char query[4096], arg[4096], buffer[1024], newkey[255], fingerprint[1024];
   char *proxylist;
   DWORD cnt;

   // Find Armadillo
   HINSTANCE libInst = LoadLibraryA("ArmAccess.DLL");  
   if (!libInst) return -1; // Couldn't load library
   InstallKeyFn InstallKey = (InstallKeyFn)GetProcAddress(libInst, "InstallKey");  
   if (InstallKey == 0) {
      FreeLibrary(libInst);  
      return -1; // Couldn't find function
   }

   // Make sure we have a valid license
   if (!g_license) {
      FreeLibrary(libInst);
      return -2;
   }

   // First see if we've already registered this runtime
   if (Lstrlen(g_license->m_license_key_str) == LICENSE_KEY_LENGTH) {
         FreeLibrary(libInst);
	      return 0;
   }

   // Get the fingeprint for this PC
   sprintf_s(fingerprint, 10, "%X", g_license->m_pc_fingerprint);

   // Try to connect to our website
   inet = InternetOpenA("AmziRegister/1.0", method, proxyname, NULL, 0);

   // Parameters: distributor's key, this pc's fingerprint
   strcpy(query, "http://www.arulesxl.com/download/licensing/register_runtime.php");
   strcat(query, "?key=");
   strcpy(arg, redistkey);
   CGIEncode(arg);
   strcat(query, arg);
   strcat(query, "&runtimeid=");
   strcpy(arg, runtimeid);
   CGIEncode(arg);
   strcat(query, arg);
   strcat(query, "&fingerprint=");
   strcpy(arg, fingerprint);
   CGIEncode(arg);
   strcat(query, arg);
   strcat(query, "&username=ARulesXL+Runtime");

   if (inet) {
      if (proxyname == NULL || strlen(proxyname) == 0) proxylist = NULL;
      else proxylist = proxyname;
      ifile = InternetOpenUrlA(inet, query, proxylist, 0, INTERNET_FLAG_PRAGMA_NOCACHE | 
         INTERNET_FLAG_NO_CACHE_WRITE | INTERNET_FLAG_RELOAD, 0);
      if (ifile) {
         while ( InternetReadFile(ifile, buffer, 1023, &cnt) ) {
            if (cnt == 0) break;
            buffer[cnt] = 0;
//            MessageBoxA(NULL, buffer, "buffer", MB_OK | MB_ICONERROR);
            if (strstr(buffer, "REGISTERED")) {
//               MessageBoxA(NULL, buffer, "REGISTERED", MB_OK | MB_ICONERROR);
               strncpy(newkey, &buffer[11], LICENSE_KEY_LENGTH);
               newkey[69] = '\0';
               result = 0;
            }
            if (strstr(buffer, "NOTFOUND")) {
//               MessageBoxA(NULL, buffer, "NOTFOUND", MB_OK | MB_ICONERROR);
               result = 1;
            }
            if (strstr(buffer, "OVERLIMIT")) {
//               MessageBoxA(NULL, buffer, "OVERLIMIT", MB_OK | MB_ICONERROR);
               result = 2;
            }
            if (strstr(buffer, "ERROR")) {
//               MessageBoxA(NULL, buffer, "ERROR", MB_OK | MB_ICONERROR);
               result = -5;
            }
         }
      }

      InternetCloseHandle(inet);
   }

   // -1 is used above for cannot find armadillo
   // -2 is cannot find the keys license structure
   // -3 is used in RegisterRuntime and is no license key found in axl file

   // Install the key
   if (result == 0) {
      // Also change this name in register_runtime.php on www.arulesxl.com
      if (!InstallKey("ARulesXL Runtime", newkey))
         result = -4;
   }

   // Check for an error
   if (inet == NULL || ifile == NULL) {
//      MessageBoxA(NULL, "ERROR: Unable to read response from Internet", "ERROR", MB_OK | MB_ICONERROR);
      result = -10;
   }

   FreeLibrary(libInst);
   return result;
#else
   return -100;
#endif
   */
}

// For ARulesXL Runtime users to provide their own UI for registration
// Method and ProxyNames are as documented for InternetOpen
//method = INTERNET_OPEN_TYPE_PRECONFIG;
//method = INTERNET_OPEN_TYPE_DIRECT;
//method = INTERNET_OPEN_TYPE_PROXY;

extern "C" RC  LSAPI RegisterRuntime(ENGid eid, DWORD Method, char *ProxyNames, char *RuntimeID) 
{
#ifdef ARULESXLRT
   char RedistKey[255];
   LSTERM t;

   // Get the author's license key (from the axl file).
   lsExecStrA(eid, &t, "arulesxl_user_license(?key)");
   RedistKey[0] = '\0';
   lsGetArg(eid, t, 1, cASTR, &RedistKey);
   if (strlen(RedistKey) != LICENSE_KEY_LENGTH)
      return -3;

   // Default RuntimeID to hardware fingerprint
   return register_runtime(Method, ProxyNames, RedistKey, RuntimeID);
#else
   return -100;
#endif
}

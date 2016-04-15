/****************************************************************************
*
* engine2.cpp -- top most engine class
*
* Copyright (c) 1992-2009 by Amzi!.  All Rights Reserved.
*
****************************************************************************/

// Includes --------------------------------------------------------

#include "inc.h"
#include "pch.h"
#include "lex.h"
#include <ctime>

#ifdef UNIX
#include <dlfcn.h>                            // dynamic load for LSXs
#include <sys/resource.h>   // for setting maxmemory on unix
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/sem.h>

extern int g_semaphore;
#endif

#ifdef LANDFILL
#define BUG_SETSTREAM
// BUG_INIT might cause GPF
#define BUG_INIT
#endif

bool g_logging = false;  // so we can keep multiple sessions on the same log
Lofstream   *g_log;      // the log stream, if logging enabled

// LEngine Private Functions -----------------------------------

RC LEngine::deal_with(LExcept &ex, LString apicall)
{
   LOG( SP2 << "error: " << (STRptr)apicall );
   switch (ex.GetType())
     {
     case INTERNAL:
       if (m_trAPI)
			LOG( SP2 << (aCHAR*)apicall << " returns internal error " <<
               ex.GetRC() << ", shutting down." );
       shut_down();
       break;
     case ABORT:
       if (m_trAPI)
			LOG( SP2 << (aCHAR*)apicall << " returns abort error " <<
               ex.GetRC() << ", shutting down." );
       shut_down();
       break;
     case FATAL:
       if (m_trAPI)
			LOG( SP2 << (aCHAR*)apicall << " returns fatal error " <<
               ex.GetRC() << ", resetting." );
       reset();
       break;
     case SECURITY:
       if (m_trAPI)
			LOG( SP2 << (aCHAR*)apicall << " returns fatal error " <<
               ex.GetRC() << ", resetting." );
       reset();
       break;
     default:
       if (m_trAPI)
			LOG( SP2 << (aCHAR*)apicall << " returns error " <<
               ex.GetRC() );
     }
   
   ex.SetAPICall(apicall);
   m_exLast = ex;             // Add the name of the api call to the exception.

   if (pLOGSERV )
     {                            //gcc on hp-ux wants these as two statements
      CLSException lsex(ex);
      throw lsex;
     }
   
   return ex.GetRC();
}

RC LEngine::initialize()
{
#ifdef BUG_SYNC
         *g_sync_file << "Ramping Up " << "\n";
#endif BUG_SYNC
//   Lprintf(aS("Engine Initializing"));
   FILL("Engine Initializing");
//   Lprintf(aS("wrote something to landfill...\n"));
   // Kill the beta release on a specific day, month is 0-11, year is year-1900
   struct tm *timep;
   time_t now;
   time(&now);
   timep = localtime(&now);

//if ( timep->tm_year != 107 || (timep->tm_year == 107 && timep->tm_mon > 3 ))
//   {
//      pXCPT->Error(internalE, aS("beta version expired"));
//   }

   // will be set to 1 for a single engine
  // if ( CLogicServer::g_EngList.nengines > 1 && ! CheckProfessional() )
  // {
  //    pXCPT->Error(locked_featureE, aS("multiple engine"));
  // }

   // this should set the locale to the user's machine, thus
   // making the wcs-mbs conversions work correctly.  Don't
   // mess with other locale types as they affect numbers
   // and we don't want to get into that...
   char locale_str[512];
   char *locs = setlocale(LC_CTYPE, "");
   // attempt to set utf-8 encoding, fails at this point
   //char *locs = setlocale(LC_CTYPE, "English_United State.65001");


   strcpy(locale_str, locs);
   
   //locale = setlocale (LC_CTYPE, NULL);

   //LEAK("pre initialize");
//FILL("Logging initialization");
   if (!g_logging && ! ((LString)(m_ini.logfile) == aS("")))
   {
      if (g_sync_file)
      {
         m_log = g_sync_file;
      }
      else
      {
         char *aname = g_lenv.new_mbs_dup(m_ini.logfile);
         LNEW(m_log, Lofstream, aS("logfile"));
         m_log->open(aname);
         delete aname;
      }
      *m_log << (aCHAR*)(
         LString(aS("Amzi! Log File  ")) + LDateString() + 
         aS("  ") + LTimeString() + aS("\n\n") );
      m_log->setf(std::ios::showbase);
      m_logging = true;
      m_trAPI = m_ini.apitrace;
      g_log = m_log;
   }
   if (g_logging)
   {
      m_log = g_log;
      m_logging = true;
      m_trAPI = m_ini.apitrace;
      *m_log << (aCHAR*)(
         LString(aS("Amzi! Log File  ")) + LDateString() + 
         aS("  ") + LTimeString() + aS("\n\n") );
   }
//Lprintf(aS("Logged the time and header\n"));
//FILL("Logging initialization complete");

   // If logfile is "", then nothing is opened and logging is still as it was
   //LNEW(m_plog, LLog(this),aS("initialization"));
   //if (! m_plog)
   //   pXCPT->Error(outofmemE, aS("Log"));
   //pLOG->Open(m_ini.logfile);
#if defined(UNIX) && ! defined(BSD)   // dcm - BSD doesn't like RLIMIT_AS
   struct rlimit rl;
   rlim_t newmaxmem;
   if (m_ini.maxmemory > 0)
   {
      newmaxmem = m_ini.maxmemory * 1000000;
#ifndef MACOSX
      getrlimit(RLIMIT_AS, &rl);
#else
      getrlimit(RLIMIT_RSS, &rl);  // MLW
#endif
      if (newmaxmem < rl.rlim_max)
      {
         rl.rlim_cur = newmaxmem;
#ifndef MACOSX
         setrlimit(RLIMIT_AS, &rl);
#else
         setrlimit(RLIMIT_RSS, &rl);  // MLW
#endif
      }
   }
#endif

//Lprintf(aS("Did the rlimit things\n"));
//FILL("LEngState -> dump");
#ifdef BUG_INIT
*g_dump << "LEngState " << "\n";
#endif
//FILL("LEngState -< dump");

   LNEW(m_pstate, LEngState(m_ini.readbuffer, m_ini.occurs_check, m_ini.double_quote_strings,
         m_ini.preprocessor, m_ini.upper_case_atoms, m_ini.vba, m_ini.utf8io, m_ini.undefined_predicate, m_ini.decimal_places,
         m_ini.decimals, m_ini.floats, m_ini.vars_sort_equal, m_ini.debug64_cut,
         m_ini.debug_port, m_ini.debug_host), aS("initialization"));

   pSTATE->m_locale = locale_str;
   LOG( "Using Locale: " << (aCHAR*)(pSTATE->m_locale) );
   //LOG( "Using Locale: " << locale_str );

   //if (! m_pstate)
   //   pXCPT->Error(outofmemE, aS("Engine State"));

   // Need to allocate the dynamic database early because other objects use it.
   //LNEW(m_pddbmem, DDBMemory(this), aS("initialization"));
   //if (! m_pddbmem)
   //   pXCPT->Error(outofmemE, aS("Dynamic Database"));

#ifdef BUG_INIT
*g_dump << "HXL " << "\n";
#endif
   LNEW(m_phxl, HXL(this), aS("initialization"));
   //if (! m_phxl)
   //   pXCPT->Error(outofmemE, aS("Heap and Local"));

#ifdef BUG_INIT
*g_dump << "ControlStack " << "\n";
#endif
   LNEW(m_pstack, ControlStack(this), aS("initialization"));
   //if (! m_pstack)
   //   pXCPT->Error(outofmemE, aS("Control Stack"));

#ifdef BUG_INIT
*g_dump << "PString " << "\n";
#endif
   LNEW(m_pstring, PString(this), aS("initialization"));
   //if (! m_pstring)
   //   pXCPT->Error(outofmemE, aS("PString"));

#ifdef BUG_INIT
*g_dump << "TermServices " << "\n";
#endif
   LNEW(m_pts, TermServices(this), aS("initialization"));
   //if (! m_pts)   // Must be allocated after HXL.
   //   pXCPT->Error(outofmemE, aS("Term Services"));
#ifdef BUG_INIT
*g_dump << "LMath " << "\n";
#endif
   LNEW(m_pmath, LMath(this), aS("initialization"));
#ifdef BUG_INIT
*g_dump << "LExec " << "\n";
#endif
   LNEW(m_pexec, LExec(this, m_ini.tracing), aS("initialization"));
#ifdef BUG_INIT
*g_dump << "LLoad " << "\n";
#endif
   LNEW(m_pload, LLoad(this), aS("initialization"));
#ifdef BUG_INIT
*g_dump << "AtomTable " << "\n";
#endif
   LNEW(m_patoms, AtomTable(this), aS("initialization"));
#ifdef BUG_INIT
*g_dump << "DDB " << "\n";
#endif
   LNEW(m_pddb, DDB(this), aS("initialization"));
#ifdef BUG_INIT
*g_dump << "TermReader " << "\n";
#endif
   LNEW(m_ptread, TermReader(this), aS("initialization"));
#ifdef BUG_INIT
*g_dump << "TermWriter " << "\n";
#endif
   LNEW(m_ptwrit, TermWriter(this), aS("initialization"));
#ifdef BUG_INIT
*g_dump << "LEX " << "\n";
#endif
   LNEW(m_plex, LEX(this), aS("initialization"));      
#ifdef BUG_INIT
*g_dump << "IO " << "\n";
#endif
   LNEW(m_pio, IO(this), aS("initialization"));
#ifdef BUG_INIT
*g_dump << "GCThings " << "\n";
#endif
   LNEW(m_pgcthings, GCThings(this), aS("initialization"));
#ifdef BUG_INIT
*g_dump << "BuiltIns " << "\n";
#endif
   LNEW(m_pbips, BuiltIns(this), aS("initialization"));

#ifdef PROBE
   LNEW(m_pprobe, Probe(this), aS("initialization"));
   //if (! m_pprobe)
   //   pXCPT->Error(outofmemE, aS("Performance Probe"));
   m_pprobe->Init();
#endif

   m_pstate->m_initialized = LTRUE;
   m_shutdown = false;

   //m_pddbmem->Init(m_ini.dbgc, m_ini.chunksize);
#ifdef BUG_INIT
*g_dump << "phxl " << "\n";
#endif
   m_phxl->Init(m_ini.heap, m_ini.local, m_ini.maxvars, 
              m_ini.heapbumper);  // heap, X, and local
#ifdef BUG_INIT
*g_dump << "pstack " << "\n";
#endif
   m_pstack->Init(m_ini.control, m_ini.trail);
#ifdef BUG_INIT
*g_dump << "pstring " << "\n";
#endif
   m_pstring->Init(m_ini.readbuffer);
#ifdef BUG_INIT
*g_dump << "pts " << "\n";
#endif
   m_pts->Init(m_ini.readbuffer);
#ifdef BUG_INIT
*g_dump << "pmath " << "\n";
#endif
   m_pmath->Init();
#ifdef BUG_INIT
*g_dump << "plex " << "\n";
#endif
   m_plex->Init(m_ini.readbuffer, m_ini.macroheapsz); // m_pio needs this
#ifdef BUG_INIT
*g_dump << "patoms " << "\n";
#endif
	m_patoms->Init();                    // ray: m_pio needs this
#ifdef BUG_INIT
*g_dump << "pio " << "\n";
#endif
   m_pio->Init();
#ifdef BUG_INIT
*g_dump << "pexec " << "\n";
#endif
   m_pexec->InitDebug(m_ini.breaks);
	//m_patoms->Init();                    // ray, moved earlier
#ifdef BUG_INIT
*g_dump << "pload " << "\n";
#endif
   m_pload->Init(m_ini.maxclauses, m_ini.maxfiles, m_ini.destbuf, 
          m_ini.srcbuf, m_ini.banner);
	//   m_patoms->Init();
#ifdef BUG_INIT
*g_dump << "pddb " << "\n";
#endif
   m_pddb->Init();
#ifdef BUG_INIT
*g_dump << "ptread " << "\n";
#endif
   m_ptread->Init(m_ini.readdepth, m_ini.string_esc, m_ini.maxvars, 
               m_ini.readbuffer);
#ifdef BUG_INIT
*g_dump << "ptwrit " << "\n";
#endif
   m_ptwrit->Init(m_ini.readbuffer);
#ifdef BUG_INIT
*g_dump << "pgcthings " << "\n";
#endif
   m_pgcthings->Init(m_ini.gcthingfreq);
#ifdef BUG_INIT
*g_dump << "pbips " << "\n";
#endif
   m_pbips->Init();

   //initReals(m_peng);
#ifdef BUG_INIT
*g_dump << "All the inits done" << "\n";
#endif

   //LEAK("post initialize");
   FILL("Engine Initialization Complete");
   return OK;
}

void LEngine::reset()
{                             // Reset the engine, usually after a fatal error.
   m_pexec->Reset();
   m_phxl->Reset();
   m_pstack->Reset();
   m_pio->Reset();             // note, listener must reset streams
}

RC LEngine::shut_down()
{
   aCHAR what[100];
   try
   {
#ifdef BUG_SYNC
         *g_sync_file << "Shutting Down " << "\n";
#endif BUG_SYNC
         if (m_shutdown == true)
            return OK;
         m_shutdown = true;

         //LEAK("pre shutdown");

         FILL("Engine Shutting Down");
         LOG( "Shutting Down" );

         // shut down remote debugging if its on
         if (m_debug64)
            (*m_DebugEnd)();

      /*  Maybe we don't really need to free any LSXs???
      std::map<LString, LSXptr>::iterator lsx_it;
        LSXptr lsxp;
        for (lsx_it = lsx_map.begin(); lsx_it != lsx_map.end(); lsx_it++)
        {
           if (lsx_it->first == aS("atcltk"))
              continue;
           LOG( "Freeing LSX" );
           lsxp = lsx_it->second;
      #if defined(WINDOWS)
           FreeLibrary(lsxp);
      #elif defined(UNIX)
           dlclose(lsxp);
      #endif
        }
      */

      /*
      #if defined(WINDOWS)
         HINSTANCE hLSX = m_lsxs.Pop();
         while (hLSX)
           {
             LOG( "Freeing LSX" );
             FreeLibrary(hLSX);
             hLSX = m_lsxs.Pop();
           }
      #elif defined(UNIX)
         void * hLSX = m_lsxs.Pop();
         while (hLSX)
           {
             LOG( "Closing LSX" );
             dlclose(hLSX);
             hLSX = m_lsxs.Pop();
           }
      #endif
      */
 
      //pKEYS->engine_goes();   // decrement engine count, if appropriate

         // Release our semaphore under Unix
         // Windows does this automatically when a process ends
#if defined(UNIX)
      if (g_semaphore != -1) 
        semctl(g_semaphore, 0, IPC_RMID);
#endif

      LOG( "Closing engine components" );
      // Shut down everything but the exception list,
      // which might still be in use, environment-specific
      // io functions, and the log.
      Lstrcpy(what, aS("heap, x, local"));
      delete pHXL;      // Heap, X and Local
      Lstrcpy(what, aS("control, stack, trail"));
      delete pCNTL;     // Control Stack and Trail
      Lstrcpy(what, aS("built-ins"));
      delete pBIPS;     // Built-in Predicates
      //delete pDMEM;     // Memory Management
      Lstrcpy(what, aS("atom table"));
      delete pATAB;     // Atom Table
      Lstrcpy(what, aS("dynamic database"));
      delete pDDB;      // Dynamic Database
      Lstrcpy(what, aS("strings"));
      delete pPSTR;     // Prolog Strings
      Lstrcpy(what, aS("term services"));
      delete pTSVC;     // Term Services
      Lstrcpy(what, aS("math"));
      delete pMATH;     // Math Functions
      Lstrcpy(what, aS("reader"));
      delete pREAD;     // Term Reader
      Lstrcpy(what, aS("writer"));
      delete pWRIT;     // Term Writer
      Lstrcpy(what, aS("executive object"));
      delete pEXEC;     // Code Executive Object
      Lstrcpy(what, aS("loader"));
      delete pLOAD;     // Loader
      //delete pLOG;      // Log file
      Lstrcpy(what, aS("lex parser"));
      delete pLEX;
      Lstrcpy(what, aS("garbage collector"));
      delete pGCTH;
      Lstrcpy(what, aS("i/o"));
      delete pIO;

      // And set them all to NULL, in case someone
      // decides to shutdown more than once.

      pHXL   = NULL;    // Heap, X and Local
      pCNTL  = NULL;    // Control Stack and Trail
      pBIPS  = NULL;    // Built-in Predicates
      //pDMEM  = NULL;    // Memory Management
      pATAB  = NULL;    // Atom Table
      pDDB   = NULL;    // Dynamic Database
      pPSTR  = NULL;    // Prolog Strings
      pTSVC  = NULL;    // Term Services
      pMATH  = NULL;    // Math Functions
      pREAD  = NULL;    // Term Reader
      pWRIT  = NULL;    // Term Writer
      pIO    = NULL;    // Stream I/O Services
      pEXEC  = NULL;    // Code Executive Object
      pLOAD  = NULL;    // Loader
      //pLOG   = NULL;    // Log file
      pLEX   = NULL;    // Lex
      pGCTH  = NULL;
      pIO    = NULL;    // Stream I/O

      /*
      LOG( "Closing log, good bye" );
      if (m_logging)
      {
         m_log->close();
         m_logging = false;
         delete m_log;
         g_log = NULL;
         g_logging = false;
      }
      */
   }
   catch(...)
   {
      LOG( "Error shutting down while closing " << what );
   }

   LOG("Flushing log at shutdown");
   if (m_logging) m_log->flush();

   delete m_pstate;    // Engine state variables
   m_pstate = NULL;  // Engine state variables

#ifdef LANDFILL
#ifdef PROBE
   m_pprobe->report();
#endif
//   mem_end.Checkpoint();
//   if (mem_diff.Difference(mem_begin, mem_end))
//   {
//      DUMP << "*** Memory Leak ***" << NL;
//      mem_diff.DumpStatistics();
//   }

   // Let's keep it open til the DLL shuts down
   //m_dump->close();
   //delete m_dump;

#endif

   //LEAK("post shutdown");

   FILL("Engine Shutdown Complete");
   return OK;
}

TERM LEngine::addModSpec(TERMptr t1)
{
   Goal g(m_peng, *t1);
   TERM t2;
   if (! g.explicit_modix)
   {
      t2 = pHXL->heapGETN(4);
      t2->setStruct(t2+1);
      (t2+1)->setFA(pATAB->colonA, 2);
      (t2+2)->setInt(USER_MODULE);
      *(t2+3) = **t1;
      return t2;
   }
   else
      return *t1;
}


void LEngine::str_chk(STRptr s, intC n)
{
// Check input string to make sure its null terminated,
//   necessary for DLLs called from environments with non-null strings
//   such as Visual Basic.

   int i;
   STRptr p;
   aCHAR buf[10];

   p = s;

   for (i=0; i<n; i++)
      if (*p++ == EOS) return;

   for (i=0; i<9; i++)
      buf[i] = s[i];
   buf[9] = EOS;
   pXCPT->Error(nonullE, buf);
}

TF LEngine::p_loadlsx(void)
// Allow Prolog program to dynamically load an LSX
{
   STRptr path;
   TERM tpath = X(0);

   if (tpath->IsAtom())
      path = *(tpath->getAtom());
   else if (tpath->IsStr())
      path = tpath->getStr();
   else
      pXCPT->Error(typeE, aS("arg1 must be atom or string"));

   LPathString lpath(path);
   lpath.ForceExt(aS(".lsx"));

   load_lsx(lpath, NULL);
   return TRUE;
}

// Keep a global of already loaded lsxs, so we can have
// duplicate loads that do nothing.  This is different
// from the engine's specific list of lsxs that prevents
// duplicates definition of predicates for an engine.
std::map<LString, LSXptr> g_lsx_map;


#if defined(WINDOWS)

LSXptr LEngine::LoadLibraryAW(LPathString f)
  // Call the wide or narrow character version depending
  // on whether we're running NT or 95
{
   LSXptr hi;
#ifdef _UNICODE
  if (g_lenv.getOSVer() == W95 )    // ray WNT
    //  if (m_osver == W95 || m_osver == WNT)    // ray WNT
   {
      int len = f.Length()+1;
      char *fa;
      LNEW(fa, char[len*2], aS("temporary"));
      wcstombs(fa, f, len*2);
      hi = LoadLibraryA(fa);
      delete fa;
      return hi;
   }
#endif
   hi = LoadLibrary(f);
   return hi;
}

void LEngine::load_lsx(LPathString lsxfile, VOIDptr p)
{
   HINSTANCE hdll;
   int rc;
   long drc;

   LString lsxname = lsxfile.GetFileName();

   // If the XPL file has been loaded, check if this lsx is OK
//   if (pSTATE->m_loaded)
//      pKEYS->load_lsx(lsxname);

   if (lsx_map.find(lsxname) != lsx_map.end())
      return;

   LOG( SP2 << "loading LSX " << (STRptr)lsxfile );

   std::map<LString, LSXptr>::iterator lsx_it;
   if ( (lsx_it=g_lsx_map.find(lsxname)) != g_lsx_map.end())
   {
      hdll = lsx_it->second;
   }
   else
   {
      hdll = LoadLibraryAW(lsxfile);
      if (!hdll) {
         LPathString lsxfile2 = g_lenv.amzi_directory();
         lsxfile2 = lsxfile2 + "bin\\";
         lsxfile2 = lsxfile2 + lsxfile;
         //MessageBox(NULL, lsxfile2, aS("duh"), MB_OK);
         hdll = LoadLibraryAW(lsxfile2);
      }
      if (hdll)
         g_lsx_map.insert(std::pair<LString, LSXptr>(lsxname, hdll));
   }

   if (hdll)
   {
      // This is the god-awful mess Microsoft has wrought.  
      // It you use __stdcall, which is required by VB, then
      // you must include the name decoration here... Arghhh, but do
      // other vendors decorate names as well???? who knows? 
      m_lsxinit = (LSXINITptr)GetProcAddress(hdll, "_InitPreds@8");
      if (m_lsxinit == NULL)
         m_lsxinit = (LSXINITptr)GetProcAddress(hdll, "InitPreds");
      if (m_lsxinit == NULL)
         m_lsxinit = (LSXINITptr)GetProcAddress(hdll, "InitPreds@8");
      if (m_lsxinit)
        {
          rc = (*m_lsxinit)(this, p);
          //m_lsxs.Push(hdll);
        }
      else
        {
          drc = (long)GetLastError();
          FreeLibrary(hdll);
          switch (drc)
            {
            case ERROR_PROC_NOT_FOUND:
              pXCPT->Error(lsxnoprocE, (aCHAR*)lsxfile);
              break;
            default:
              pXCPT->Error(lsxinitE, drc, (aCHAR*)lsxfile);
            }
        }

      // If we're loading the debug LSX, then we need an
      // additional initialization as well.
      if ( lsxfile.Strstr(aS("adebug.lsx")) >= 0 )
      {
         m_debug64 = true;
         m_DebugStart = (DEBUGSTARTptr)(GetProcAddress(hdll, "_DebugStart@8"));
         if (!m_DebugStart)
            m_DebugStart = (DEBUGSTARTptr)(GetProcAddress(hdll, "DebugStart"));
         if (!m_DebugStart)
            pXCPT->Error(lsxsysdebugE, aS("unable to find DebugStart"));

         m_DebugEnd = (DEBUGENDptr)(GetProcAddress(hdll, "_DebugEnd@0"));
         if (!m_DebugEnd)
            m_DebugEnd = (DEBUGENDptr)(GetProcAddress(hdll, "DebugEnd"));
         if (!m_DebugEnd)
            pXCPT->Error(lsxsysdebugE, aS("unable to find DebugEnd"));

         char* debug_host = toAscii(pSTATE->m_debug_host);
         rc = (*m_DebugStart)(debug_host, pSTATE->m_debug_port);
         delete debug_host;
         if (rc)
            pXCPT->Error(lsxdebugE, (STRptr)pSTATE->m_debug_host, pSTATE->m_debug_port);
      }

   }
   else
     {
       drc = (long)GetLastError();
       switch (drc)
         {
         case ERROR_MOD_NOT_FOUND:
           pXCPT->Error(lsxnomodE, (aCHAR*)lsxfile);
           break;
         case ERROR_DLL_INIT_FAILED:
           pXCPT->Error(lsxwinitE, (aCHAR*)lsxfile);
           break;
         case ERROR_DLL_NOT_FOUND:
           pXCPT->Error(lsxnodllE, (aCHAR*)lsxfile);
           break;
         default:
           pXCPT->Error(lsxloadE, drc, (aCHAR*)lsxfile);
         }
     }

   lsx_map.insert(std::pair<LString, LSXptr>(lsxname, hdll));
   return;
}
#elif defined(UNIX)

void LEngine::load_lsx(LPathString lsxfile, VOIDptr p)
{
   int rc;
   aCHAR fname[_MAX_FNAME];
   aCHAR path[_MAX_PATH];
   aCHAR *amzi_dir;
   //void *hlsx;
   LSXptr hlsx;
   char *errmsg;
   int errlen;
  
   LOG( SP2 << "loading LSX:: " << (STRptr)lsxfile );

   LString lsxname = lsxfile.GetFileName();
   
   // If the XPL file has been loaded, check if this lsx is OK
//   if (pSTATE->m_loaded)
//      pKEYS->load_lsx(lsxname);

   if (lsx_map.find(lsxname) != lsx_map.end())
      return;

  char *afname = g_lenv.new_mbs_dup(lsxfile);
  char here[_MAX_PATH] = "./";
  strcat(here, afname);

   LOG( SP2 << "trying here LSX " << here );

  hlsx = dlopen(here, RTLD_NOW);
  delete afname;
   
  if (hlsx == NULL)
    {
      //amzi_dir = Lgetenv_dup(aS("AMZI_DIR"));
      
      
      amzi_dir = g_lenv.amzi_directory();
      LOG( SP2 << "amzi_dir: " << amzi_dir );     
      if (amzi_dir != NULL)
        {
          Lstrcpy(path, amzi_dir);
          Lstrcat(path, aS("lib/"));
          Lstrcat(path, lsxfile);
          char *apath = g_lenv.new_mbs_dup(path);

   LOG( SP2 << "trying path LSX " << apath );

          hlsx = dlopen(apath, RTLD_NOW);
          delete amzi_dir;
          delete apath;
        }
    }
  
  if (hlsx)
    {
   LOG( SP2 << "success opening LSX" );
      dlerror();   // clear any open errors
      m_lsxinit = (LSXINITptr)dlsym(hlsx, "InitPreds");
      //if (!lsxinit)   // try C++ mangled name instead
      //    lsxinit = (LSXINITptr)dlsym(hlsx, "__1cJInitPreds6Fpv0_i_");
      if (m_lsxinit)
        {
          rc = (*m_lsxinit)(this, p);
          //m_lsxs.Push(hlsx);
        }
      else
        {
          errmsg = dlerror();
          dlclose(hlsx);
#ifdef _UNICODE
          errlen = strlen(errmsg);
          aCHAR* werrmsg;
          LNEW(werrmsg, aCHAR[errlen+1], aS("temporary"));
          mbstowcs(werrmsg, errmsg, errlen);
          werrmsg[errlen] = 0;
          pXCPT->Error(lsxinitE, (aCHAR*)lsxfile, werrmsg);
          delete werrmsg;
#else
          pXCPT->Error(lsxinitE, fname, errmsg);
#endif
        }
      // If we're loading the debug LSX, then we need an
      // additional initialization as well.
      if ( lsxfile.Strstr(aS("adebug.lsx")) >= 0 )
      {
         m_debug64 = true;
         m_DebugStart = (DEBUGSTARTptr)dlsym(hlsx, "DebugStart");
         if (!m_DebugStart)
            pXCPT->Error(lsxsysdebugE, aS("unable to find DebugStart"));

         m_DebugEnd = (DEBUGENDptr)dlsym(hlsx, "DebugEnd");
         if (!m_DebugEnd)
            pXCPT->Error(lsxsysdebugE, aS("unable to find DebugEnd"));

         char* debug_host = toAscii(pSTATE->m_debug_host);
         rc = (*m_DebugStart)(debug_host, pSTATE->m_debug_port);
         delete debug_host;
         if (rc)
            pXCPT->Error(lsxdebugE, (STRptr)pSTATE->m_debug_host, pSTATE->m_debug_port);
      }
    }
  else
    {
   LOG( SP2 << "failure opening LSX" );
      errmsg = dlerror();
   LOG( SP2 << "error msg: " << errmsg );
#ifdef _UNICODE
      errlen = strlen(errmsg);
      aCHAR* werrmsg;
      LNEW(werrmsg, aCHAR[errlen+1], aS("temporary"));
      mbstowcs(werrmsg, errmsg, errlen);
      werrmsg[errlen] = 0;
      pXCPT->Error(lsxloadE, (aCHAR*)lsxfile, werrmsg);
      delete werrmsg;
#else
      pXCPT->Error(lsxloadE, fname, errmsg);
#endif
    }
   lsx_map.insert(std::pair<LString, LSXptr>(lsxname, hlsx));
}
#endif  // UNIX

void LEngine::check_lsxs()
{
  std::map<LString, LSXptr>::iterator lsx_it;
  LString lsxname;
  for (lsx_it = lsx_map.begin(); lsx_it != lsx_map.end(); lsx_it++)
  {
     lsxname = lsx_it->first;
//     pKEYS->load_lsx(lsxname);
  }
}

int LEngine::cov_strtermlen(TERM ti)
{
  TERM   t;

   t = (ti)->dref();

   if (t->IsAtom())
      return((int)Lstrlen(*(t->getAtom())));
   if (t->IsStr())
      return((int)Lstrlen(t->getStr()));

   // If its not a string or atom, get the length
   // without quoting
   int imax = pSTATE->m_buflen;
   aCHAR *s = new aCHAR[imax];
   pWRIT->termWriteString(t, s, imax-1, FALSE);
   int len = (int)Lstrlen(s);
   delete s;
   return len;

//   pXCPT->Error(badstrlenE);
//   return NOTOK;
}
int LEngine::cov_strtermqlen(TERM ti)
{
  TERM   t;

   t = (ti)->dref();

   // get the length of the term if it were to be
   // written with writeq
   int imax = pSTATE->m_buflen;
   aCHAR *s = new aCHAR[imax];
   pWRIT->termWriteString(t, s, imax-1, TRUE);
   int len = (int)Lstrlen(s);
   delete s;
   return len;
}

/* not used
int LEngine::cov_realtermlen(TERM ti)
{
  TERM   t;

   t = (ti)->dref();

   if (t->IsReal())
	  //return 1 + ((Real)(*t))->getLen();
	  return 1 + t->getReal()->getLen();

   pXCPT->Error(badstrlenE);
   return NOTOK;
}
*/

RC LEngine::cov_getterm(TERM ti, cTYPE ctyp, VOIDptr val)
{
  TERM    t;
  STRptr  ss = NULL;
#ifdef _UNICODE
  STRptr  s;
  size_t  len;
#endif

   t = (ti)->dref();

   switch (ctyp)
     {
     case cASTR:
#ifdef _UNICODE
      if (t->IsAtom())
        s = (t->getAtom()->get_display_name(m_peng));
      else if (t->IsStr())
        s = t->getStr();
      else if (t->IsList())
        {
          LNEW(ss, aCHAR[m_pstate->m_buflen], aS("temporary"));
          s = ss;
          pHXL->CharListToStr(t, s);
        }
      else
        {
          pWRIT->termWriteString(ti, m_TrBuf, TRBUF_SIZE-1, TRUE);
          pXCPT->Error(notcstrE, m_TrBuf);
        }
      len = Lstrlen(s)+1;
      //if (wcstombs((char*)val, s, len*2) >= len*2)
      if (g_lenv.e_mbs_out(m_peng, (char*)val, s, len*2) >= len*2)
         ((char*)val)[len*2-1] = 0;
      delete ss;
#else
      if (t->IsAtom())
         Lstrcpy((STRptr)val, (t->getAtom()->get_display_name(m_peng)));
      else if (t->IsStr())
        Lstrcpy((STRptr)val, t->getStr());
      else if (t->IsList())
        pHXL->CharListToStr(t, (STRptr)val);
      else
        {
          pWRIT->termWriteString(ti, m_TrBuf, TRBUF_SIZE-1, TRUE);
          pXCPT->Error(notcstrE, m_TrBuf);
        }
#endif
      break;
      
#ifdef _UNICODE
    case cWSTR:
      if (t->IsAtom())
        Lstrcpy((STRptr)val, (t->getAtom()->get_display_name(m_peng)));
      else if (t->IsStr())
        Lstrcpy((STRptr)val, t->getStr());
      else if (t->IsList())
        pHXL->CharListToStr(t, (STRptr)val);
      else
        {
          pWRIT->termWriteString(ti, m_TrBuf, TRBUF_SIZE-1, TRUE);
          pXCPT->Error(notcstrE, m_TrBuf); 
        }
      break;
#endif
      
    case cINT:
      if (t->IsInt())
        *(INTptr)val = (int)t->getInt();
      //else if (pTSVC->TisXint(t))
      // *(INTptr)val = (int)pTSVC->XValue(t);
      else
        {
          pWRIT->termWriteString(ti, m_TrBuf, TRBUF_SIZE-1, TRUE);
          pXCPT->Error(notcintE, m_TrBuf); 
        }
      break;
      
    case cLONG:
      if (t->IsInt())
        *(LONGptr)val = (long)t->getInt();
      //else if (pTSVC->TisXint(t))
      //*(LONGptr)val = (long)pTSVC->XValue(t);
      else
        {
          pWRIT->termWriteString(ti, m_TrBuf, TRBUF_SIZE-1, TRUE);
          pXCPT->Error(notcintE, m_TrBuf); 
        }
      break;
      
    case cSHORT:
      if (t->IsInt())
        *(SHORTptr)val = (short)t->getInt();
      //else if (pTSVC->TisXint(t))
      //*(SHORTptr)val = (short)pTSVC->XValue(t);
      else
        {
          pWRIT->termWriteString(ti, m_TrBuf, TRBUF_SIZE-1, TRUE);
          pXCPT->Error(notcintE, m_TrBuf); 
        }
      break;
      
    case cFLOAT:
      if (t->IsSingle())
         *(FLOATptr)val = (float)t->getSingle();
      else if (t->IsInt())
         *(FLOATptr)val = (float)t->getInt();
      else if (t->IsDouble())
         *(FLOATptr)val = (float)t->getDouble();
      else if (t->IsReal())
         *(FLOATptr)val = t->getReal()->toSingle();
      else if (t->IsFixed())
         *(FLOATptr)val = (float)t->getFixed().toDouble();
      else
        {
          pWRIT->termWriteString(ti, m_TrBuf, TRBUF_SIZE-1, TRUE);
          pXCPT->Error(notcfltE, m_TrBuf); 
        }
       break;
      
    case cDOUBLE:
      if (t->IsSingle())
         *(DOUBLEptr)val = (double)t->getSingle();
      else if (t->IsInt())
         *(DOUBLEptr)val = (double)t->getInt();
      else if (t->IsDouble())
         *(DOUBLEptr)val = (double)t->getDouble();
      else if (t->IsReal())
         *(DOUBLEptr)val = t->getReal()->toDouble();
      else if (t->IsFixed())
         *(DOUBLEptr)val = t->getFixed().toDouble();
      else
        {
          pWRIT->termWriteString(ti, m_TrBuf, TRBUF_SIZE-1, TRUE);
          pXCPT->Error(notcfltE, m_TrBuf); 
        }
       break;
      
    case cADDR:
      if (t->IsPtr())
        *(VOIDhnd)val = (VOIDptr)t->getPtr();
      else
        {
          pWRIT->termWriteString(ti, m_TrBuf, TRBUF_SIZE-1, TRUE);
          pXCPT->Error(notcadrE, m_TrBuf); 
        }
      break;
      
    case cTERM:
      *(TERMptr)val = t;
      break;
      
    default:
      pXCPT->Error(badctypeE, ctyp);
    }
  
  return OK;
}

pTYPE LEngine::cov_termtype(TERM tin)
{
   TERM  t;

   t = (tin)->dref();
   if      (t->IsAtom())
     return(pATOM);
   else if (t->IsInt())
     return(pINT);
   else if (t->IsScientific()) 
     return(pFLOAT);
   else if (t->IsReal()) 
     return(pREAL);
   else if (t->IsStr())   
     return(pSTR);
   else if (t->IsPtr()) 
     return(pADDR);
   else if (t->IsRef())   
     return(pVAR);
   else if (t->IsStruct()) 
     return(pSTRUCT);
   else if (t->IsList())  
     return(pLIST);
   else if (t->IsReal())
     return(pFLOAT);
   else if (t->IsFixed())
     return(pFLOAT);
   else
      pXCPT->Error(badtypeE);

   return pERR;
}

void LEngine::cov_maketerm(TERM t, cTYPE ct, VOIDptr pVal)
{
   intC      i;
   double    f;
   STRptr    s;
   VOIDptr   vp;
   TERM      t2;
   //STRptr    ps;
   PATOM     a;
	//	Real      r;
#ifdef _UNICODE
   int       len;
#endif

   switch(ct)
   {
   case cAATOM:
#ifdef _UNICODE
      len = (int)strlen((char*)pVal) + 1;
      LNEW(s, aCHAR[len], aS("temporary"));
      //mbstowcs(s, (char*)pVal, len);
      g_lenv.e_mbs_in(m_peng, s, (char*)pVal, len);
      a = pATAB->EnterAtom(s);
      delete s;
#else
      s = (char*)pVal;
      a = pATAB->EnterAtom(s);
#endif
      t->setAtom( a);
      break;

#ifdef _UNICODE
   case cWATOM:
      s = (wchar_t*)pVal;
      a = pATAB->EnterAtom(s);
      t->setAtom( a);
      break;
#endif

   case cASTR:
#ifdef _UNICODE
      len = (int)strlen((char*)pVal) + 1;
      LNEW(s, aCHAR[len], aS("temporary"));
      g_lenv.e_mbs_in(m_peng, s, (char*)pVal, len);
      //pPSTR->strEnterString(s, &ps);
      t->setStr(pGCTH->make_string(s));
      delete s;
#else
      s = (char*)pVal;
      //pPSTR->strEnterString(s, &ps);
      t->setStr(pGCTH->make_string(s));
#endif
      //t->setStr(pGCTH->make_string(ps));
      break;

#ifdef _UNICODE
   case cWSTR:
      s = (wchar_t*)pVal;
      //pPSTR->strEnterString(s, &ps);
      t->setStr(pGCTH->make_string(s));
      break;
#endif

   case cLONG:
      i = *(long *)pVal;
      t->setInt(i);
      break;

   case cINT:
      i = *(int *)pVal;
      t->setInt(i);
      break;

   case cSHORT:
      i = *(short *)pVal;
      t->setInt(i);
      break;

   case cFLOAT:
      f = *(float *)pVal;
      t->setDouble(pGCTH->make_float(f));
      break;

   case cDOUBLE:
      f = *(double *)pVal;
      t->setDouble(pGCTH->make_float(f));
      break;

   case cADDR:
      vp = *(VOIDptr *)pVal;
      //t->setDBRef((DynamicClauseIterator*)vp);
      t->setPtr(vp);
      break;

   case cTERM:
      t2 = *(TERMptr)pVal;
      *t = *t2;
      break;

   default:
      pXCPT->Error(badhosttypeE);
   }
}


/****************************************************************************
*
* engine.cpp -- top most engine class
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
****************************************************************************/

// Includes --------------------------------------------------------

#include "inc.h"
#include "pch.h"
#include "lex.h"

#ifdef UNIX
#include <dlfcn.h>                            // dynamic load for LSXs
#endif

#ifdef MACOSX
#include <iostream.h>  // MLW
#endif

#ifdef WINDOWS
HANDLE g_semaphore = NULL;
#endif
#ifdef UNIX
int g_semaphore = -1;
#endif

#ifdef LANDFILL
#define BUG_ENGINE
LEngine *g_peng;
Lofstream *g_dump = NULL;
#define BUG_SETSTREAM
#endif

//extern void initReals(LEngine *);
extern Real floatToReal(double);

/*
// running professional version or not
//bool g_professional = false;
bool g_professional = true;  // default to true temporarily in 6.3
*/

// used for debugging, see note in defs.h
LExHandler *g_xcpt;

// LEngState Initialization

LEngState::LEngState(intC buflen, LFLAG oc, LFLAG dqs, LFLAG prep, LFLAG uca, LFLAG vba, LFLAG utf8io, LUNDEFFLAG undefined, intC dp,
      DECIMALS_ rn, FLOATS_ fl, LFLAG vars_sort_equal, LFLAG debug64_cut,
      intC debug_port, LString debug_host)
{
   m_initialized  = LFALSE;
   m_loaded       = LFALSE;

   //m_fileerrors   = LON;
   m_trace        = LOFF;
   //m_readerrors   = LOFF;
   //m_lint         = LOFF;
   //m_fatal_err    = LOFF;
   //m_protect_db   = LOFF;
   //m_aritherrors  = LON;
   m_occurs_check = oc;
   m_properNames  = uca;
   m_vba  = vba;
   m_undefined_predicate = undefined;
   m_properQuotes = dqs;
   //m_controls     = LOFF;
   m_prep         = prep;
   m_vars_sort_equal = vars_sort_equal;
   m_debug64_cut = debug64_cut;
   m_macrotrace   = LOFF;
   //m_real         = LON;
   m_locale = aS("");
   m_utf8io = utf8io;
   m_version = AMZI_VERSION;
   //m_version = m_version + SP + ENVIR;

#define DATE_STAMP
#ifdef DATE_STAMP
   //  + NL + __DATE__ + SP + __TIME__;  // no Unicode equivalent?
   char asciidate[256];
   //strcpy(asciidate, "\n");
   strcpy(asciidate, __DATE__);
   //strcat(asciidate, " ");
   //strcat(asciidate, __TIME__);
   //strcat(asciidate, "\n");
#ifdef _UNICODE
   mbstowcs(datestamp, asciidate, 255);
#else
   strncpy(datestamp, asciidate, 255);
#endif
   //m_version = m_version + "  " + datestamp;

#endif  // DATE_STAMP

   m_argc = 0;
   m_argv = NULL;
   m_buflen = buflen;   // Set from ini.read_buffer, but used in many places 
                        // to allocate buffers for various reasons.
   m_rounded = TO_ZERO;
   m_decimals = rn;
   m_floats = fl;
	m_decimal_places = dp;     // default to 1 gigit
   m_modulo = 0;        // when nonzero, modulus to use in integer arithmetic
   m_epsilon = 0;      // exponent of precision order (eg: inf. series stop )
   m_delta = 2;   // real div growth bound: qlen = max(numLen, denomLen)+delta 

   m_debug_port = debug_port;
   m_debug_host = debug_host;
}

//----------------------------
// LEngine Implementation
//

LEngine::LEngine(CLogicServer *pls) : m_ini()
{
   //LEAK("start of engine constructor");

   m_peng = this;

#ifdef WINDOWS
   AInitializeCriticalSection(&m_critical_section);
#else
   m_critical_section = 0;
#endif

#ifdef LANDFILL
   g_peng = this;   // used for debugging
#endif
   m_pstate = NULL;    // Engine state variables
   pHXL   = NULL;    // Heap, X and Local
   pCNTL  = NULL;    // Control Stack and Trail
   pBIPS  = NULL;    // Built-in Predicates
   //pDMEM  = NULL;    // Memory Management
   pATAB  = NULL;    // Atom Table Gate to DDB
   pDDB   = NULL;    // Dynamic Database
   pPSTR  = NULL;    // Prolog Strings
   pTSVC  = NULL;    // Term Services
   pMATH  = NULL;    // Math Functions
   pREAD  = NULL;    // Term Reader
   pWRIT  = NULL;    // Term Writer
//   pSIO   = NULL;    // Stream I/O Services
   pEXEC  = NULL;    // Code Executive Object
   pLOAD  = NULL;    // Loader
   //pLOG   = NULL;    // Log file
   pLEX   = NULL;    // Lex
   pGCTH  = NULL;    // Garbage Collection Things
   pIO    = NULL;    // Stream I/O
   m_trAPI = LOFF;
#ifdef _UNICODE
   m_argwc = 0;
   for (int i=0; i<MAX_COMMAND_ARGS; i++)
      m_argwv[i] = NULL;
#endif

   m_lsxs_initialized = false;
   m_log = NULL;
   m_logging = false;
   m_plogserv = pls;

   m_debug64 = false;
   m_DebugStart = NULL;
   m_DebugEnd = NULL;

#ifdef THREADED_EXEC_STR
   LNEW(m_action_buffer, ActionBuffer(this), aS("ActionBuffer"));
#endif

   LNEW(pXCPT, LExHandler(this), aS("exception"));
   g_xcpt = pXCPT;  // just used for debugging
   m_ini.setEng(this);    // Set up the ini reader with exception handler
   //LEAK("end of engine constructor");

   // For internal diagnostics.  see debug.h for various
   // ifdefs for turning on and off diagnostics
#ifdef LANDFILL
   Lprintf(aS("1Creating Land Fill\n"));
   if (g_dump == NULL)
   {
      if (g_sync_file)
      {
         g_dump = g_sync_file;
      }
      else
      {
         LNEW(g_dump, Lofstream, aS("landfill"));
#ifdef UNIX
         g_dump->open("/tmp/landfill.txt");
#else
         g_dump->open("c:\\temp\\landfill.txt");
#endif  //unix
      }
      //Lprintf(aS("1Writing header to Land Fill\n"));
      *g_dump <<
         LString(aS("LandFill  ")) + LDateString() + 
         aS("  ") + LTimeString() + aS("\n\n");
      g_dump->setf(std::ios::showbase);
      *g_dump << FLUSH;
#ifdef WINDOWS
//      MessageBox(NULL, aS("Land Fill is Open"), aS("Debug Info"), MB_OK);
#else
      printf("Dump file is open\n");
      //Lprintf(aS("1Land Fill is Open\n"));
#endif  //windows
   }
   m_dump = g_dump;
//   mem_begin.Checkpoint();
#endif  // landfill
}

LEngine::LEngine() : m_ini()
{
   //LEAK("start of engine constructor");
   m_peng = this;

#ifdef WINDOWS
   AInitializeCriticalSection(&m_critical_section);
#else
   m_critical_section = 0;
#endif

#ifdef LANDFILL
   g_peng = this;   // used for debugging
#endif
   m_pstate = NULL;    // Engine state variables
   pHXL   = NULL;    // Heap, X and Local
   pCNTL  = NULL;    // Control Stack and Trail
   pBIPS  = NULL;    // Built-in Predicates
   //pDMEM  = NULL;    // Memory Management
   pATAB  = NULL;    // Atom Table Gate to DDB
   pDDB   = NULL;    // Dynamic Database
   pPSTR  = NULL;    // Prolog Strings
   pTSVC  = NULL;    // Term Services
   pMATH  = NULL;    // Math Functions
   pREAD  = NULL;    // Term Reader
   pWRIT  = NULL;    // Term Writer
//   pSIO   = NULL;    // Stream I/O Services
   pEXEC  = NULL;    // Code Executive Object
   pLOAD  = NULL;    // Loader
   //pLOG   = NULL;    // Log file
   pLEX   = NULL;    // Lex
   pGCTH  = NULL;    // Garbage Collection Things
   pIO    = NULL;    // Stream I/O

   m_trAPI = LOFF;

#ifdef _UNICODE
   m_argwc = 0;
   for (int i=0; i<MAX_COMMAND_ARGS; i++)
      m_argwv[i] = NULL;
#endif

   m_lsxs_initialized = false;

   m_log = NULL;
   m_logging = false;
   m_plogserv = NULL;

   m_debug64 = false;
   m_DebugStart = NULL;
   m_DebugEnd = NULL;

#ifdef THREADED_EXEC_STR
   LNEW(m_action_buffer, ActionBuffer(this), aS("ActionBuffer"));
#endif

   LNEW(pXCPT, LExHandler(this), aS("exception"));
   g_xcpt = pXCPT;  // just used for debugging
   m_ini.setEng(this);          // Set up the ini reader with exception handler
   //LEAK("end of engine constructor");

   // For internal diagnostics.  see debug.h for various
   // ifdefs for turning on and off diagnostics
#ifdef LANDFILL
   //Lprintf(aS("2Creating Land Fill\n"));
   if (g_dump == NULL)
   {
      if (g_sync_file)
      {
         g_dump = g_sync_file;
      }
      else
      {
         LNEW(g_dump, Lofstream, aS("landfill"));
#ifdef UNIX
         g_dump->open("/tmp/landfill.txt");
#else
         g_dump->open("c:\\temp\\landfill.txt");
#endif
      }
      *g_dump <<
         LString(aS("LandFill  ")) + LDateString() + 
         aS("  ") + LTimeString() + aS("\n\n");
      g_dump->setf(std::ios::showbase);
      *g_dump << FLUSH;
#ifdef WINDOWS
//      MessageBox(NULL, aS("Land Fill is Open"), aS("Debug Info"), MB_OK);
#else
      //fprintf(stderr, "Dump file is open\n");
      //Lprintf(aS("2Land Fill is Open\n"));
#endif
   }
   m_dump = g_dump;
//   mem_begin.Checkpoint();
#endif
}

LEngine::~LEngine()
{
   shut_down();
   delete pXCPT;

#ifdef _UNICODE
   for (int i=0; i<m_argwc; i++)
      //delete[] m_argwv[i];
   {
      delete[] m_argwv[i];
   }
#endif

#ifdef WINDOWS
   ADeleteCriticalSection(&m_critical_section);
#else
   m_critical_section = 0;
#endif

}

RC LEngine::Init(LPathString ininame)
{                           // Initialize the ini parameters from an .ini file.
//Lprintf(aS("in Init\n"));
   try
     {
//Lprintf(aS("initializing, fileset\n"));
       m_ini.FileSet(ininame);
//Lprintf(aS("calling initialize\n"));
       initialize();
//Lprintf(aS("left initialize\n"));
     }
   catch (LExcept &pE)
     {
       if (m_trAPI) // For init, try to write the header here for failed inits,
         {          //  because we didn't know aboutm _trAPI before.
			   LOG( "lsInit" << PRENS( this SEP (STRptr)ininame ) );
         }
       return deal_with(pE, aS("lsInit"));
     }
   
   if (m_trAPI)
     {
       LOG( "lsInit" << PRENS( this SEP (STRptr)ininame ) );
       LOG( SP2 << "returns OK" );
     }
   
   return OK;
}

RC LEngine::Init2(LString inistring)
{                      // Initialize the ini parameters from an input string.
  try
    {
#ifdef BUG_ENGINE
  DUMP << NL << "Init2 with " << inistring << NL << FLUSH;
#endif
      m_ini.StringSet(inistring);
      initialize();
    }
  catch (LExcept &pE)
    {
      // For init, we need to try to write the header here for
      // failed initializations, because we didn't know about
      // m_trAPI before.
      if (m_trAPI)
		  LOG( "lsInit" << PRENS( this SEP (STRptr)inistring ) );
      return deal_with(pE, aS("lsInit2"));
    }
  
  if (m_trAPI)
    {
      LOG( "lsInit" << PRENS( this SEP (STRptr)inistring ) );
      LOG( SP2 << "returns OK" );
    }
  
  return OK;
}

RC LEngine::InitLSX(VOIDptr p)       // Load the LSXs from the .cfg parameters.
{ // This must be called before a .XPL file is loaded and after initialization.

   if (m_lsxs_initialized)
      return OK;
   m_lsxs_initialized = true;

   if (m_trAPI)
	  LOG( "lsInitLSX" << PRENS( this SEP p ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
//       if (m_pstate->m_loaded)
//         pXCPT->Error(afterloadE);
       
       LTokenString lsxfiles(m_ini.lsxload);
       LPathString lsx = (LPathString)(lsxfiles.GetTokenChar(aS(";")));
       lsx.TrimWhiteSpace();
       while ( lsx != LString(aS("")) )
         {
           lsx.AddExt(aS(".lsx"));
           load_lsx(lsx, p);
           lsx = lsxfiles.GetTokenChar(aS(";"));
         }
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsInitLSX"));
     }
   
   if (m_trAPI)
     {
       LOG( SP2 << "returns OK" );
     }
   
   return OK;
}

RC LEngine::AddLSX(LPathString lsx, VOIDptr p)
{
   if (m_trAPI)
	  LOG( "lsAddLSX" << PRENS( this SEP (STRptr)lsx SEP p ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
//       if (m_pstate->m_loaded)
//         pXCPT->Error(afterloadE);
       
       lsx.AddExt(aS(".lsx"));
       lsx.TiltSlashes();
       load_lsx(lsx, p);
     }
   catch(LExcept &pE)
     {
       LOG( SP2 << "Catching LExcept for AddLSX" );
       return deal_with(pE, aS("lsAddLSX"));
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns OK");
   
   return OK;
}

RC LEngine::AddPred(LString name, ARITY arity, ExtPred func, VOIDptr vp)
{                                   // Adds a single predicate to the table 
  //FILE* fp = fopen("c:\\dump.txt", "a");
  //fwprintf(fp, aS("nameW:%s:\n"), name.Buffer());
  //fclose(fp);
  if (m_trAPI)
      LOG( "lsAddPred" << PRENS( this SEP (STRptr)name SEP (int)arity SEP func SEP vp ) );
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
//      if (m_pstate->m_loaded)
//        pXCPT->Error(afterloadE);
      
      m_pddb->AddPred(name, arity, func, vp);
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsAddPred"));
    }
  
  if (m_trAPI)
    {
      LOG( SP2 << "returns OK");
    }
  
  return OK;
}

RC LEngine::InitPreds(PRED_INITAptr pint)
{
   if (m_trAPI)
       LOG( "lsInitPreds" << PRENS( this SEP pint ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
//       if (m_pstate->m_loaded)
//         pXCPT->Error(afterloadE);
       
       m_pddb->InitPredTab(pint);
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsInitPreds"));
     }
   
   if (m_trAPI)
       LOG( SP2 << "returns OK");
   
   return OK;
}

#ifdef _UNICODE
RC LEngine::InitPreds(PRED_INITWptr pint)
{
   if (m_trAPI)
	  LOG( "lsInitPreds" << PRENS( this SEP pint ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
//       if (m_pstate->m_loaded)
//         pXCPT->Error(afterloadE);
       
       m_pddb->InitPredTab(pint);
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsInitPreds"));
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns OK");
   
   return OK;
}
#endif

RC LEngine::Load(LPathString xplname)
{
  if (m_trAPI)
	 LOG( "lsLoad" << PRENS( this SEP STRptr(xplname) ) );
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
//      if (m_pstate->m_loaded)
//        pXCPT->Error(afterloadE);
      
      // if host didn't initialize the .cfg lSXs with a parameter,
      // lets do it for them using null
      if (! m_lsxs_initialized)
         InitLSX(NULL);

      xplname.AddExt(aS(".xpl"));
#ifdef ARULESXL
//      MessageBox(NULL, xplname.GetFileName(), aS("FileName"), 0);
//      MessageBox(NULL, xplname.GetPath(), aS("Path"), 0);
//      MessageBox(NULL, xplname.GetExt(), aS("Ext"), 0);
//      if (xplname.GetFileName() != "\\arules.xpl" && xplname.GetFileName() != "arules") 
//         throw( LExcept(securityE, aS("Bad XPL file")));
      if (Lstrstr( xplname.GetFileName(), aS("arules") ) == NULL)
         throw( LExcept(securityE, aS("Bad XPL file")));
#endif
#ifdef ARULESXLRT
//      MessageBox(NULL, xplname.GetFileName(), aS("FileName"), 0);
//      MessageBox(NULL, xplname.GetPath(), aS("Path"), 0);
//      MessageBox(NULL, xplname.GetExt(), aS("Ext"), 0);
      if (xplname.GetFileName() != "\\arulesrt.xpl" && xplname.GetFileName() != "arulesrt")
         throw( LExcept(securityE, aS("Bad XPL file")));
#endif
      m_pstate->m_loadfile = xplname;

      // There might be errors during execution of latent expressions,
      // so this flag is used to detect those cases to prevent the
      // Prolog handling of those errors.  Not needed with new
      // error handling?

      //ipl_B = TRUE;
      m_pload->load_file(xplname);
      //ipl_B = FALSE;
      m_pstate->m_loaded = LTRUE;
   }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsLoad"));
     }

   if (m_trAPI)
       LOG( SP2 << "returns OK");

   return OK;
}

RC LEngine::LoadFromMemory(LPathString xplname, int length, unsigned char *p)
{
   RC rc;

   if (m_trAPI)
	   LOG( "lsLoadLoadFromMemory" << PRENS( this SEP STRptr(xplname) ) );

   try
   {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
//      if (m_pstate->m_loaded)
//        pXCPT->Error(afterloadE);
      
      // if host didn't initialize the .cfg lSXs with a parameter,
      // lets do it for them using null
      if (! m_lsxs_initialized)
         InitLSX(NULL);

      xplname.AddExt(aS(".xpl"));
      m_pstate->m_loadfile = xplname;

      // There might be errors during execution of latent expressions,
      // so this flag is used to detect those cases to prevent the
      // Prolog handling of those errors.  Not needed with new
      // error handling?

      //ipl_B = TRUE;
      rc = m_pload->load_from_memory(xplname, length, p);
      if (rc != OK) return rc;
      //ipl_B = FALSE;
      m_pstate->m_loaded = LTRUE;
   }
   catch(LExcept &pE)
   {
       return deal_with(pE, aS("lsLoadFromMemory"));
   }

   if (m_trAPI)
       LOG( SP2 << "returns OK");

   return OK;
}

TF LEngine::Main()
{
   TF  tf;
   clock_t time;

   if (m_trAPI)
   {
	  LOG( "lsMain" << PRENS( this ) );
     time = clock();
   }
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       tf = m_pexec->ProveMain();
     }
   catch(LExcept &pE)
     {
      return (TF)deal_with(pE, aS("lsMain"));
     }
   
   if (m_trAPI)
     {
       if (tf == TRUE)
       {
			LOG( SP2 << "returns TRUE" );
       }
       else if (tf == FALSE)
       {
           LOG( SP2 << "returns FALSE" );
       }
       else
       {
           LOG( SP2 << "returns uncaught error, contact Amzi!" );
       }
       time = (1000 * ( clock() - time )) / CLOCKS_PER_SEC;
       LOG( SP2 << "time in milliseconds = " << time );
     }
   
   return tf;
}

RC LEngine::Reset()
{
   if (m_trAPI)
	  LOG( "lsReset" << PRENS( this ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       reset();
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsReset"));
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns OK");
   
   return OK;
}

RC LEngine::Close()
{
   if (m_trAPI)
	  LOG( "lsClose" << PRENS( this ) );
   
   try
     {
      shut_down();
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsClose"));
     }

   return OK;
}

/* Functions to get and set predicate parameters */
/* --------------------------------------------- */

RC LEngine::GetParm(int iarg, cTYPE ctyp, VOIDptr val)
{
   TERM   t;

   if (m_trAPI)
	  LOG( "lsGetParm" << PRENS( this SEP iarg SEP ctyp SEP val ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       //t = (&X[iarg-1])->dref();
       //t = (pHXL->XVar(iarg-1))->dref();
       t = X(iarg-1);
       cov_getterm(t, ctyp, val);
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsGetParm"));
     }
   
   if (m_trAPI)
   {
	  LOG( SP2 << "returns OK" );
     if (ctyp == cASTR)
        LOG( SP2 << "String value: " << * (char*) val << NL );
   }
   
   return OK;
}

int LEngine::StrParmLen(int iarg)
{
   int  l;
   TERM t;

   if (m_trAPI)
	  LOG( "lsStrParmLen" << PRENS( this SEP iarg ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       //t = (pHXL->XVar(iarg-1))->dref();
       t = X(iarg-1);
       l = cov_strtermlen(t);
     }
   catch(LExcept &pE)
     {
       deal_with(pE, aS("lsStrParmLen"));
       return NOTOK;
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns " << l );
   
   return l;
}
   
pTYPE LEngine::GetParmType(int iparm)
{
   pTYPE pt;
   TERM t;

   if (m_trAPI)
	  LOG( "lsGetParmType" << PRENS( this SEP iparm ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       //t = (pHXL->XVar(iparm-1))->dref();
       t = X(iparm-1);
       pt = cov_termtype(t);
     }
   catch(LExcept &pE)
     {
       deal_with(pE, aS("lsGetParmType"));
       return pERR;
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns " << pt );
   
   return pt;
}

TF LEngine::UnifyParm(int iarg, cTYPE ct, VOIDptr pVal)
{
   TF  tf;
   TERM  t;
   Cell  c;
   TRAIL_INDEX tr_top;
   //CHOICE_POINTptr newcp;

   if (m_trAPI)
	  LOG( "lsUnifyParm" << PRENS( this SEP iarg SEP ct SEP pVal ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       t = (pHXL->XVar(iarg-1))->dref();
       cov_maketerm(&c, ct, pVal);
       
       // We need to save the trail and create a dummy
       // choice point to fake termUnify into saving
       // partial bindings on the trail, so we can
       // back them out if the unify fails.
       
       // Actually causes a bug in lsUnifyParm, as
       // its part of real execution thread, so
       // don't create choice point.
       
       tr_top = pCNTL->TRTop();
       //newcp = pCNTL->NewChoicePoint(0);
       //newcp->Bc = pCNTL->BTop();
       //newcp->HBc = pHXL->HTop();
       //pCNTL->SetBTop(newcp);
       
       tf = pTSVC->Unify(t, &c);
       if (tf == FALSE)
         pCNTL->Unwind(tr_top);
       
       //pCNTL->SetBTop(newcp->Bc);
       //pHXL->SetHTop(newcp->HBc);
       //pCNTL->SetTRTop(tr_top);
     }
   catch(LExcept &pE)
     {
       return (TF)deal_with(pE, aS("lsUnifyParm"));
     }

   if (m_trAPI)
     {
       if (tf == TRUE)
         {
           pWRIT->termWriteString(t, m_TrBuf, TRBUF_SIZE-1, TRUE);
           LOG( SP2 << "returns TRUE," << NL << SP2 << "term bound to: " << m_TrBuf );
         }
       else if (tf == FALSE)
       {
			LOG( SP2 << "returns FALSE" );
       }
       else
       {
			LOG( SP2 << "returns uncaught error, contact Amzi!" );
       }
     }
   
   return tf;
}

/* Calling Prolog predicates from C */
/* -------------------------------- */

TF LEngine::ExecStr(LSTERMptr lstp, STRptr s)
{
  TERMptr tp = (TERMptr)lstp;
  TF  tf;
  Cell oldX0;
  //Cell *saveH;
  clock_t time;
  
  if (m_trAPI)
    {
      Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
      LOG( "lsExecStr" << PRENS( this SEP tp SEP m_TrBuf ) );
      time = clock();
    }
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      if (pREAD->IsReading())
         pXCPT->Error(nestedexecE);
      
      //saveH = pHXL->HTop();                         // ray
      str_chk(s, m_pstate->m_buflen);
      tf = pEXEC->ExecProve(s, tp);
/*
      pPSTR->strStringTerm(s, tp);
      
      // codeExecProve creates a choice point with one X variable,
      // which is X[0].  The callers terms is set to point to that
      // choice point X variable, so it is immune from garbage
      // collection.
      
      // oldX0 = X[0];
      // X[0] = (CELL)*tp;
      oldX0 = pHXL->XVal(0);
      //pHXL->SetXVal(0, (CELL)*tp);
      pHXL->SetXRef(0, *tp);
      //TERM t = addModSpec(tp);
      //pHXL->SetXRef(0, t);
      
      tf = pEXEC->ExecProve(tp);

      //pHXL->SetHTop(saveH);                          // ray

      // X[0] = oldX0;
      pHXL->SetXVal(0, oldX0);
*/
   }
   catch(LExcept &pE)
     {
         return (TF)deal_with(pE, aS("lsExecStr"));
     }

   if (m_trAPI)
     {
       if (tf == TRUE)
         {
           pWRIT->termWriteString(*tp, m_TrBuf, TRBUF_SIZE-1, TRUE);
           LOG( SP2 << "returns TRUE," << NL << SP2 << "term bound to: " << m_TrBuf );
         }
       else if (tf == FALSE)
       {
			LOG( SP2 << "returns FALSE" );
       }
       else
       {
			LOG( SP2 << "returns uncaught error, contact Amzi!" );
       }
        time = (1000 * ( clock() - time )) / CLOCKS_PER_SEC;
        LOG( SP2 << "time in milliseconds = " << time );
   }

   return tf;
}

TF LEngine::Exec(LSTERMptr lstp)
{
  TERMptr tp = (TERMptr)lstp;
  TF  tf;
  Cell oldX0;
  clock_t time;
  
  if (m_trAPI)
  {
	 LOG( "lsExec" << PRENS( this SEP tp ) );
      time = clock();
  }
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      if (pREAD->IsReading())
         pXCPT->Error(nestedexecE);
      
      // codeExecProve creates a choice point with one X variable,
      // which is X[0].  The callers terms is set to point to that
      // choice point X variable, so it is immune from garbage
      // collection.
      
      oldX0 = pHXL->XVal(0);
      //pHXL->SetXVal(0, (CELL)*tp);
      pHXL->SetXRef(0, *tp);
      //TERM t = addModSpec(tp);
      //pHXL->SetXRef(0, t);
      
      tf = pEXEC->ExecProve(tp);

      pHXL->SetXVal(0, oldX0);
   }
   catch(LExcept &pE)
     {
       return (TF)deal_with(pE, aS("lsExec"));
     }
   
   if (m_trAPI)
     {
       if (tf == TRUE)
         {
           pWRIT->termWriteString(*tp, m_TrBuf, TRBUF_SIZE-1, TRUE);
           LOG( SP2 << "returns TRUE" << NL << SP2 << "term bound to: " << m_TrBuf );
        }
       else if (tf == FALSE)
       {
			LOG( SP2 << "returns FALSE" );
       }
       else
       {
			LOG( SP2 << "returns uncaught error, contact Amzi!" );
       }
        time = (1000 * ( clock() - time )) / CLOCKS_PER_SEC;
        LOG( SP2 << "time in milliseconds = " << time );
     }
   
   return tf;
}

TF LEngine::CallStr(LSTERMptr lstp, STRptr s)
{
   TERMptr tp = (TERMptr)lstp;
   TF  tf;
   //Cell *saveH;
   clock_t time;

   if (m_trAPI)
     {
       Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
       LOG( "lsCallStr" << PRENS ( this SEP tp SEP m_TrBuf ) );
       time = clock();
     }
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       if (pREAD->IsReading())
         pXCPT->Error(nestedexecE);

       // this strategy is dangerous, because there may have
       // been a heap GC during the exec, best let heap be
       // reset when the control stack is unwound.
      //saveH = pHXL->HTop();                         // ray
      str_chk(s, m_pstate->m_buflen);
      tf = pEXEC->CallProve(s, tp);
/*
      pPSTR->strStringTerm(s, tp);
      
      // codeCallProve creates a choice point with one X variable,
      // which is X[0].  The callers terms is set to point to that
      // choice point X variable, so it is immune from garbage
      // collection.

      //X[0] = (CELL)*tp;
      //pHXL->SetXVal(0, (CELL)*tp);
      pHXL->SetXRef(0, *tp);
      //TERM t = addModSpec(tp);
      //pHXL->SetXRef(0, t);

      tf = pEXEC->CallProve(tp);

      //pHXL->SetHTop(saveH);                          // ray
*/
   }
   catch(LExcept &pE)
     {
       return (TF)deal_with(pE, aS("lsCallStr"));
     }
   
   if (m_trAPI)
     {
       if (tf == TRUE)
         {
			  pWRIT->termWriteString(*tp, m_TrBuf, TRBUF_SIZE-1, TRUE);
			  LOG( SP2 << "returns TRUE" << NL << "  term bound to: " << m_TrBuf );
			}
       else if (tf == FALSE)
       {
			LOG( SP2 << "returns FALSE" );
       }
       else
       {
			LOG( SP2 << "returns uncaught error, contact Amzi!" );
       }
        time = (1000 * ( clock() - time )) / CLOCKS_PER_SEC;
        LOG( SP2 << "time in milliseconds = " << time );
     }

	return tf;
}

TF LEngine::Call(LSTERMptr lstp)
{
   TERMptr tp = (TERMptr)lstp;
   TF  tf;
   clock_t time;
  
   if (m_trAPI)
   {
      pWRIT->termWriteString(*tp, m_TrBuf, TRBUF_SIZE-1, TRUE);
      LOG( "lsCall" << PRENS( this SEP m_TrBuf ) );
      time = clock();
    }

   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       if (pREAD->IsReading())
         pXCPT->Error(nestedexecE);
       
       // The term to be called is moved to X[0], where call/1
       // expects it to be.  tp is changed to point to X[0],
       // so if there is any garbage collection, it will still
       // be a valid term.
       
       //X[0] = (CELL)*tp;
       //pHXL->SetXVal(0, (CELL)*tp);
       pHXL->SetXRef(0, *tp);
      //TERM t = addModSpec(tp);
      //pHXL->SetXRef(0, t);
       
       // codeCallProve creates a choice point with one X variable,
       // which is X[0].  The callers terms is set to point to that
       // choice point X variable, so it is immune from garbage
       // collection.
       
       tf = pEXEC->CallProve(tp);
       
     }
   catch(LExcept &pE)
     {
       return (TF)deal_with(pE, aS("lsCall"));
     }
   
   if (m_trAPI)
     {
       if (tf == TRUE)
         {
           pWRIT->termWriteString(*tp, m_TrBuf, TRBUF_SIZE-1, TRUE);
           LOG( SP2 << "returns TRUE," << NL << SP2 << "term bound to: " << m_TrBuf );
         }
       else if (tf == FALSE)
       {
           LOG( SP2 << "returns FALSE" );
       }
       else
       {
           LOG( SP2 << "returns uncaught error, contact Amzi!" );
       }
        time = (1000 * ( clock() - time )) / CLOCKS_PER_SEC;
        LOG( SP2 << "time in milliseconds = " << time );
     }
   
   return tf;
}


TF LEngine::Redo()
{
   TF  tf;
   clock_t time;

   if (m_trAPI)
   {
	  LOG( "lsRedo" << PRENS( this ) );
     time = clock();
   }
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       tf = pEXEC->RedoProve();   
     }
   catch(LExcept &pE)
     {
       return (TF)deal_with(pE, aS("lsRedo"));
     }
   
   if (m_trAPI)
     {
       if (tf == TRUE)
       {
			LOG( SP2 << "returns TRUE" );
           //time = (1000 * ( clock() - time )) / CLOCKS_PER_SEC;
          // LOG( SP2 << "time in milliseconds = " << time );
       }
       else if (tf == FALSE)
       {
			LOG( SP2 << "returns FALSE" );
           //time = (1000 * ( clock() - time )) / CLOCKS_PER_SEC;
           //LOG( SP2 << "time in milliseconds = " << time );
       }
       else
       {
			LOG( SP2 << "returns uncaught error, contact Amzi!" );
       }
        time = (1000 * ( clock() - time )) / CLOCKS_PER_SEC;
        LOG( SP2 << "time in milliseconds = " << time );
     }
   
   return tf;
}

RC LEngine::ClearCall()
{
  if (m_trAPI)
	 LOG( "lsClearCall" << PRENS( this ) );
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      pEXEC->ClearCall();   
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsClearCall"));
    }
  
  if (m_trAPI)
	 LOG( SP2 << "returns OK");
  
  return OK;
}


RC LEngine::Asserta(LSTERM lst)
{
   TERM t = (TERM)lst;
  if (m_trAPI)
    {
      pWRIT->termWriteString(t, m_TrBuf, TRBUF_SIZE-1, TRUE);
      LOG( "lsAsserta" << PRENS( this SEP m_TrBuf ) );
    }
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      //X[0] = (CELL)t;
      //pHXL->SetXVal(0, (CELL)t);
      //pHXL->SetXRef(0, t);
      //pBIPS->p_asserta();
      pDDB->Assert('a', t);
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsAsserta"));
    }
  
  if (m_trAPI)
	 LOG( SP2 << "returns OK");
  
  return OK;
}

RC LEngine::Assertz(LSTERM lst)
{
   TERM t = (TERM)lst;
  if (m_trAPI)
    {
      pWRIT->termWriteString(t, m_TrBuf, TRBUF_SIZE-1, TRUE);
      LOG( "lsAssertz" << PRENS( this SEP m_TrBuf ) );
    }
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      //X[0] = (CELL)t;
      //pHXL->SetXVal(0, (CELL)t);
      //pHXL->SetXRef(0, t);
      //pBIPS->p_assertz();
      pDDB->Assert('z', t);
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsAssertz"));
    }
  
  if (m_trAPI)
	 LOG( SP2 << "returns OK");
  
  return OK;
}

TF LEngine::Retract(LSTERM lst)
{
  TERM t = (TERM)lst;
  TF  tf;
  
  if (m_trAPI)
    {
      pWRIT->termWriteString(t, m_TrBuf, TRBUF_SIZE-1, TRUE);
      LOG( "lsRetract" << PRENS( this SEP m_TrBuf ) );
    }
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      tf = pDDB->Retract(t);
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsRetract"));
    }
  
  if (m_trAPI)
    {
      if (tf == TRUE)
      {
		  LOG( SP2 << "returns TRUE" );
      }
      else if (tf == FALSE)
      {
		  LOG( SP2 << "returns FALSE" );
      }
      else
      {
		  LOG( SP2 << "returns uncaught error, contact Amzi!" );
      }
    }
  
  return tf;
}

RC LEngine::AssertaStr(STRptr s)
{
  TERM t;
  TERM saveH;
  
  if (m_trAPI)
    {
      Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
      LOG( "lsAssertaStr" << PRENS( this SEP m_TrBuf ) );
    }
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      str_chk(s, m_pstate->m_buflen);
      // this is only a good idea for assert and retract,
      // because on the execstr and callstr a heapgc might
      // have occurred, but here in assertstr and friends
      // this saves a potential heapgc.  that is, its a
      // performance mod, but somewhat dangerous if anyone
      // decides to heapgc
      saveH = pHXL->HTop();
      pPSTR->strStringTerm(s, &t);
      pDDB->Assert('a', t);
      pHXL->SetHTop(saveH);
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsAssertaStr"));
    }
  
  if (m_trAPI)
	 LOG( SP2 << "returns OK" );
  
  return OK;
}

RC LEngine::AssertzStr(STRptr s)
{
  TERM t;
  TERM saveH;
  
  if (m_trAPI)
    {
      Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
      LOG( "lsAssertzStr" << PRENS( this SEP m_TrBuf ) );
    }
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      str_chk(s, m_pstate->m_buflen);
      // this is only a good idea for assert and retract,
      // because on the execstr and callstr a heapgc might
      // have occurred, but here in assertstr and friends
      // this saves a potential heapgc.  that is, its a
      // performance mod, but somewhat dangerous if anyone
      // decides to heapgc
      saveH = pHXL->HTop();
      pPSTR->strStringTerm(s, &t);
      pDDB->Assert('z', t);
      pHXL->SetHTop(saveH);
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsAssertzStr"));
    }
  
  if (m_trAPI)
	 LOG( SP2 << "returns OK");
  
  return OK;
}

TF LEngine::RetractStr(STRptr s)
{
  TF    tf;
  TERM  t;
  TERM  saveH;
  
  if (m_trAPI)
    {
      Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
      LOG( "lsRetractStr" << PRENS( this SEP m_TrBuf ) );
    }
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      str_chk(s, m_pstate->m_buflen);
      // this is only a good idea for assert and retract,
      // because on the execstr and callstr a heapgc might
      // have occurred, but here in assertstr and friends
      // this saves a potential heapgc.  that is, its a
      // performance mod, but somewhat dangerous if anyone
      // decides to heapgc
      saveH = pHXL->HTop();
      pPSTR->strStringTerm(s, &t);
      tf = pDDB->Retract(t);
      pHXL->SetHTop(saveH);
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsRetractStr"));
    }
  
  if (m_trAPI)
    {
      if (tf == TRUE)
      {
		  LOG( SP2 << "returns TRUE" );
      }
      else if (tf == FALSE)
      {
		  LOG( SP2 << "returns FALSE" );
      }
      else
      {
		  LOG( SP2 << "returns uncaught error, contact Amzi!" );
      }
    }
  
  return tf;
}

/* String/Term conversions and calls */
/* --------------------------------- */

RC LEngine::TermToStr(LSTERM lst, STRptr s, int imax)
{
   TERM t = (TERM)lst;
   clock_t time;
   if (m_trAPI)
     {
       pWRIT->termWriteString(t, m_TrBuf, TRBUF_SIZE-1, TRUE);
       LOG( "lsTermToStr" << PRENS( this SEP m_TrBuf SEP imax ) );
       time = clock();
     }

   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       pWRIT->termWriteString(t, s, imax, FALSE);
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsTermToStr"));
     }
   
   if (m_trAPI)
     {
       Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
       LOG( SP2 << "returns OK," << NL << SP2 << "string set to: " << m_TrBuf );
        time = (1000 * ( clock() - time )) / CLOCKS_PER_SEC;
        LOG( SP2 << "time in milliseconds = " << time );
     }
   
   return OK;
}

RC LEngine::TermToStrQ(LSTERM lst, STRptr s, int imax)
{
   TERM t = (TERM)lst;
  clock_t time;
  if (m_trAPI)
    {
      pWRIT->termWriteString(t, m_TrBuf, TRBUF_SIZE-1, TRUE);
      LOG( "lsTermToStrQ" << PRENS( this SEP m_TrBuf SEP imax ) );
      time = clock();
    }
  
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       pWRIT->termWriteString(t, s, imax, TRUE);
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsTermToStrQ"));
     }
   
   if (m_trAPI)
     {
       Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
       LOG( SP2 << "returns OK," << NL << SP2 << "string set to: " <<	m_TrBuf );
        time = (1000 * ( clock() - time )) / CLOCKS_PER_SEC;
        LOG( SP2 << "time in milliseconds = " << time );
     }
   
   return OK;
}

RC LEngine::StrToTerm(LSTERMptr lstp, STRptr s)
{
   TERMptr tp = (TERMptr)lstp;
   clock_t time;

  if (m_trAPI)
    {
      Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
      LOG( "lsStrToTerm" << PRENS( this SEP tp SEP m_TrBuf ) );
      time = clock();
    }
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      str_chk(s, m_pstate->m_buflen);
      pPSTR->strStringTerm(s, tp);
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsStrToTerm"));
    }
  
  if (m_trAPI)
  {
	 LOG( SP2 << "returns OK");
    time = (1000 * ( clock() - time )) / CLOCKS_PER_SEC;
    LOG( SP2 << "time in milliseconds = " << time );
  }
  
  return OK;
}

/* Making Prolog Types */
/* ------------------- */

RC LEngine::MakeAtom(LSTERMptr lstp, STRptr s)
{
   TERMptr tp = (TERMptr)lstp;
   PATOM   a;

   if (m_trAPI)
	  LOG( "lsMakeAtom" << PRENS( this SEP tp SEP s ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       str_chk(s, m_pstate->m_buflen);
       *tp = pHXL->heapGET();
       a = pATAB->EnterAtom(s);
       //*tp->setAtom( a);
       (*tp)->setAtom(a);
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsMakeAtom"));
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns OK" );
   
   return OK;
}

RC LEngine::MakeInt(LSTERMptr lstp, intC iC)
{
   TERMptr tp = (TERMptr)lstp;
  if (m_trAPI)
	 LOG( "lsMakeInt" << PRENS( this SEP tp SEP iC ) );
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      *tp = pHXL->heapGET();
      
      //if (pTSVC->FULLINT(iC))
//        pTSVC->putXINT(*tp, iC);
      //else
//        pTSVC->putINT(*tp, iC);
      (*tp)->setInt(iC);
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsMakeInt"));
    }
  
  if (m_trAPI)
      LOG( SP2 << "returns OK");
  
  return OK;
}

RC LEngine::MakeFloat(LSTERMptr lstp, double f)
{
   TERMptr tp = (TERMptr)lstp;
  if (m_trAPI)
	 LOG( "lsMakeFloat" << PRENS( this SEP tp SEP f ) );
      
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      *tp = pHXL->heapGET();
      
      //pTSVC->putFLOAT(*tp, f);
      if (pSTATE->m_floats = single_)
         (*tp)->setSingle((float)f);
      else
         (*tp)->setDouble(pGCTH->make_float(f));
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsMakeFloat"));
    }
  
  if (m_trAPI)
	 LOG( SP2 << "returns OK");
  
  return OK;
}

RC LEngine::MakeAddr(LSTERMptr lstp, VOIDptr p)
{
   TERMptr tp = (TERMptr)lstp;
  if (m_trAPI)
	 LOG( "lsMakeAddr" << PRENS( this SEP tp SEP p ) );
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      *tp = pHXL->heapGET();
      
      //pTSVC->putDBREF(*tp, (DynamicClause*)p);
      //(*tp)->setDBRef((DynamicClauseIterator*)p);
      (*tp)->setPtr(p);
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsMakeAddr"));
    }
  
  if (m_trAPI)
	 LOG( SP2 << "returns OK");
  
  return OK;
}

RC LEngine::MakeStr(LSTERMptr lstp, STRptr s)
{
   TERMptr tp = (TERMptr)lstp;
   //STRptr  ps;

   if (m_trAPI)
       LOG( "lsMakeStr" << PRENS( this SEP tp SEP s ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       str_chk(s, m_pstate->m_buflen);
       *tp = pHXL->heapGET();
       
       //pPSTR->strEnterString(s, &ps);
       // bigdig *tp->setStr( ps);
       (*tp)->setStr(pGCTH->make_string(s));
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsMakeStr"));
     }
   
   if (m_trAPI)
       LOG( SP2 << "returns OK");
   
   return OK;
}

/* Getting C values from Prolog terms */
/* ---------------------------------- */

RC LEngine::GetTerm(LSTERM lst, cTYPE ct, VOIDptr pVal)
{
   TERM t = (TERM)lst;
   if (m_trAPI)
     {
       pWRIT->termWriteString(t, m_TrBuf, TRBUF_SIZE-1, TRUE);
       LOG( "lsGetTerm" << PRENS( this SEP m_TrBuf SEP ct SEP pVal ) );
     }

   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       cov_getterm(t, ct, pVal);
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsGetTerm"));
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns OK");
   
   return OK;
}

pTYPE LEngine::GetTermType(LSTERM lst)
{
  TERM t = (TERM)lst;
  pTYPE pt;
  
  if (m_trAPI)
	 LOG( "lsGetTermType" << PRENS( this SEP t ) );
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      t = (t)->dref();
      pt = cov_termtype(t);
    }
  catch(LExcept &pE)
    {
      deal_with(pE, aS("lsGetTermType"));
      return pERR;
    }
  
  if (m_trAPI)
  {
      LOG( SP2 << "returns " << pt );
  }
  
  return pt;
}

int LEngine::StrTermLen(LSTERM lsti)
{
  TERM ti = (TERM)lsti;
  TERM t;
  int  l;
  
  if (m_trAPI)
	 LOG( "lsStrTermLen" << PRENS( this SEP ti ) );
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      t = (ti)->dref();
      
      l = cov_strtermqlen(t);
    }
  catch(LExcept &pE)
    {
      deal_with(pE, aS("lsStrTermLen"));
      return NOTOK;
    }
  
  if (m_trAPI)
	 LOG( SP2 << "returns " << l );
  
  return l;
}

/* Structure hacking functions */
/* --------------------------- */

RC LEngine::GetFA(LSTERM lst, STRptr s, ARITYptr ap)
{
  TERM t = (TERM)lst;
  if (m_trAPI)
    {
		pWRIT->termWriteString(t, m_TrBuf, TRBUF_SIZE-1, TRUE);
		LOG( "lsGetFA" << PRENS( this SEP m_TrBuf ) );
    }
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      t = (t)->dref();
      
      if (t->IsAtom())
        {
          Lstrcpy(s, *(t->getAtom()));
          *ap = 0;
        }
      else if (t->IsStruct())
        {
          t = t->getTerm();
          Lstrcpy(s, *(t->getAtom()));
          *ap = t->getArity();
        }
      else if (t->IsList())
        {
          Lstrcpy(s, aS("."));
          *ap = 2;
        }
      else
        {
          pWRIT->termWriteString(t, m_TrBuf, TRBUF_SIZE-1, TRUE);
          pXCPT->Error(badgetfaE, m_TrBuf);
        }
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsGetFA"));
    }
  
  if (m_trAPI)
    {
      Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
      LOG( SP2 << "returns OK," << NL << SP2 << "functor set to: " << PREDAR(m_TrBuf, (int)(*ap)) );
    }
  
  return OK;
}

RC LEngine::MakeFA(LSTERMptr lstp, STRptr s, ARITY a)
{
   TERMptr tp = (TERMptr)lstp;
   PATOM fun_A;

   if (m_trAPI)
	  LOG( "lsMakeFA" << PRENS( this SEP tp SEP a ) );

   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       str_chk(s, m_pstate->m_buflen);
       fun_A = pATAB->EnterAtom(s);
       *tp = pTSVC->MakeStruc(fun_A, a);
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsMakeFA"));
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns OK");
   
   return OK;
}

TF LEngine::UnifyArg(LSTERMptr lstpStruc, int iarg, cTYPE ct, VOIDptr pVal)
{
   TERMptr tpStruc = (TERMptr)lstpStruc;
  TF  tf;
  TERM  t;
  Cell  c;
  TRAIL_INDEX  tr_top;
  CHOICE_POINTptr newcp;
  
  if (m_trAPI)
    {
      pWRIT->termWriteString(*tpStruc, m_TrBuf, TRBUF_SIZE-1, TRUE);
      LOG( "lsUnifyArg" << PRENS( this SEP m_TrBuf SEP iarg SEP ct SEP pVal ) );
   }

   try
   {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      t = (*tpStruc)->dref();
      
      // get the iargth argument

      if (t->IsStruct())
        {
          //t = pTSVC->StrucFA(t);
                         t = t->getTerm();
          if ( iarg<=0 || iarg>(int)t->getArity() )
            {
              pWRIT->termWriteString(*tpStruc, m_TrBuf, TRBUF_SIZE-1, TRUE);
              pXCPT->Error(argrangeE, iarg, m_TrBuf);
            }
          while (iarg--) ++t;
        }
      else if (t->IsList())
        {
          if (iarg<0 || iarg>2)
            pXCPT->Error(arglrangeE, iarg);
          t = t->getListHead();
          if (iarg==2) ++t;
        }
      
      cov_maketerm(&c, ct, pVal);
      
      // We need to save the trail and create a dummy
      // choice point to fake termUnify into saving
      // partial bindings on the trail, so we can
      // back them out if the unify fails.

      tr_top = pCNTL->TRTop();
      newcp  = pCNTL->NewChoicePoint(0);
      newcp->Bc = pCNTL->BTop();
      newcp->HBc = pHXL->HTop();
      pCNTL->SetBTop(newcp);

      tf = pTSVC->Unify(t, &c);
      if (tf == FALSE)
         pCNTL->Unwind(tr_top);

      pCNTL->SetBTop(newcp->Bc);
      pHXL->SetHTop(newcp->HBc);
      // See comment in Unify()
      //pCNTL->SetTRTop(tr_top);
   }
   catch(LExcept &pE)
   {
      return (TF)deal_with(pE, aS("lsUnifyArg"));
   }

   if (m_trAPI)
     {
       if (tf == TRUE)
         {
           pWRIT->termWriteString(t, m_TrBuf, TRBUF_SIZE-1, TRUE);
           LOG( SP2 << "returns TRUE," << NL << SP2 << "term bound to: " << m_TrBuf );
         }
       else if (tf == FALSE)
       {
			LOG( SP2 << "returns FALSE" );
       }
       else
       {
			LOG( SP2 << "returns uncaught error, contact Amzi!" );
       }
     }
   
	return tf;
}

int LEngine::StrArgLen(LSTERM lsti, int iarg)
{
   TERM ti = (TERM)lsti;
   TERM  t;
   int   l;
   cTYPE ct = cTERM;

   if (m_trAPI)
	  LOG( "lsStrArgLen" << PRENS( this SEP ti SEP iarg ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       t = (ti)->dref();
       t = get_arg(t, iarg, &ct);
       l = cov_strtermlen(t);
     }
   catch(LExcept &pE)
     {
       deal_with(pE, aS("lsStrArgLen"));
       return NOTOK;
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns " << l );
   
   return l;
}

/* Not called
int LEngine::RealArgLen(LSTERM lsti, int iarg)
{
   TERM ti = (TERM)lsti;
   TERM  t;
   int   l;

   if (m_trAPI)
	  LOG( "lsStrArgLen" << PRENS( this SEP ti SEP iarg ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       t = (ti)->dref();
              
       l = cov_realtermlen(t);
     }
   catch(LExcept &pE)
     {
       deal_with(pE, aS("lsStrArgLen"));
       return NOTOK;
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns " << l );
   
   return l;
}
*/

pTYPE LEngine::GetArgType(LSTERM lstStruc, int iarg)
{
   TERM tStruc = (TERM)lstStruc;
  pTYPE pt;
  TERM t;
  cTYPE ct = cTERM;
  
  if (m_trAPI)
	 LOG( "lsGetArgType" << PRENS( this SEP tStruc SEP iarg ) );
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      t = (tStruc)->dref();
      t = get_arg(t, iarg, &ct);
      pt = cov_termtype(t);
    }
  catch(LExcept &pE)
    {
      deal_with(pE, aS("lsGetArgType"));
      return pERR;
    }
  
  if (m_trAPI)
	 LOG( SP2 << "returns " << pt );
  
  return pt;
}

RC LEngine::GetArg(LSTERM lstStruc, int iarg, cTYPE ctype, VOIDptr vp)
{
   TERM tStruc = (TERM)lstStruc;
	TERM t;
   int  jarg = iarg;

   if (m_trAPI)
     {
       pWRIT->termWriteString(tStruc, m_TrBuf, TRBUF_SIZE-1, TRUE);
       LOG( "lsGetArg" << PRENS(	this SEP m_TrBuf SEP iarg SEP ctype SEP vp ) );
     }

   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       t = (tStruc)->dref();
       t = get_arg(t, iarg, &ctype);
       cov_getterm(t, ctype, vp);
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsGetArg"));
     }
   
   if (m_trAPI)
     {
       pWRIT->termWriteString(t, m_TrBuf, TRBUF_SIZE-1, TRUE);
       LOG( SP2 << "returns OK," << NL << SP2 << "arg " << jarg << " = " << m_TrBuf );
     }
   
   return OK;
}

TERM LEngine::get_arg(TERM t, int iarg, cTYPE *ctype)
{
// modules have created a sticky problem, we might be
// getting the arg of an execstr pets:get_pet(X), in
// which case the user really wants get_pet to be the
// structure, not :/2.  so we do the right thing, but
// the pathological case of a user actually working
// with a :/2 structure that looks like get_pet(X),
// will get burned in a bizarre way.  Nope cMOD and
// cGOAL fix that problem.  Aren't we clever.
   TERM t2;

   if (iarg <= 0)
    pXCPT->Error(arglrangeE, iarg);

   if (t->IsStruct())
   {
     t = t->getTerm();
     if (*ctype == cMOD)
     {
        ++t;
        //*ctype = cTERM;
     }
     else if (*ctype == cGOAL)
     {
        ++t; ++t;
        //*ctype = cTERM;
     }
     else if (t->getAtom() == pATAB->colonA)
     {
        t2 = t + 2;
        t2 = t2->dref();
        // if its likely its a module qualifying :/2
        if (t2->IsStruct() && iarg <= (int)t2->getTerm()->getArity())
        {
           t2 = t2->getTerm();

           t = t2;
           while (iarg--)
              ++t;
        }
        else  // really have a :/2 operator we're looking at
        {
           if (iarg > t->getArity())
              pXCPT->Error(arglrangeE, iarg);
           while (iarg--)
              ++t;
        }
     }
     else
     {
        if ( iarg > (int)t->getArity() )
          pXCPT->Error(arglrangeE, iarg);
        while (iarg--) 
		    ++t;
     }
   }
   else if (t->IsList())
   {
     if (iarg < 0 || iarg > 2)
       pXCPT->Error(arglrangeE, iarg);
     t = t->getListHead();
     if (iarg == 2) t++;
   }
   if (*ctype == cMOD || *ctype == cGOAL)
      *ctype = cTERM;
   return t;
}

TF LEngine::Unify(LSTERM lsta, LSTERM lstb)
{                     // don't use lst1 as var name, as its a microsoft macro

   TERM t1 = (TERM)lsta;
   TERM t2 = (TERM)lstb;
   TF   tf;
   TRAIL_INDEX tr_top;
   CHOICE_POINTptr newcp;

   if (m_trAPI)
	  LOG( "lsUnify" << PRENS( this SEP t1 SEP t2 ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);

      // We need to save the trail and create a dummy
      // choice point to fake termUnify into saving
      // partial bindings on the trail, so we can
      // back them out if the unify fails.

      tr_top = pCNTL->TRTop();
      newcp  = pCNTL->NewChoicePoint(0);
      newcp->Bc = pCNTL->BTop();
      newcp->HBc = pHXL->HTop();
      pCNTL->SetBTop(newcp);

      tf = pTSVC->Unify(t1, t2);
      if (tf == FALSE)
         pCNTL->Unwind(tr_top);

      pCNTL->SetBTop(newcp->Bc);
      pHXL->SetHTop(newcp->HBc);
      // This line caused the DMPROS bug, by resetting
      // TRTop we don't let other failures rewind the
      // cells set this time around.
      //pCNTL->SetTRTop(tr_top);
   }
   catch(LExcept &pE)
   {
      return (TF)deal_with(pE, aS("lsUnify"));
   }

   if (m_trAPI)
     {
      if (tf == TRUE)
        {
         pWRIT->termWriteString(t1, m_TrBuf, TRBUF_SIZE-1, TRUE);
         LOG( SP2 << "returns TRUE," << NL << SP2 << "term bound to: " << m_TrBuf );
        }
      else if (tf == FALSE)
      {
		  LOG( SP2 << "returns FALSE" );
      }
      else
      {
		  LOG( SP2 << "returns uncaught error, contact Amzi!" );
      }
     }

	return tf;
}

/* List hacking functions */
/* ---------------------- */

RC LEngine::MakeList(LSTERMptr lstp)
{
   TERMptr tp = (TERMptr)lstp;
   if (m_trAPI)
      LOG( "lsMakeList" << PRENS( this SEP tp ) );

   try
     {
      if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);

      *tp = pHXL->heapGET();
      (*tp)->setAtom( pATAB->nilA );
     }
   catch(LExcept &pE)
     {
      return deal_with(pE, aS("lsMakeList"));
     }

   if (m_trAPI)
	  LOG( SP2 << "returns OK");

   return OK;
}

RC LEngine::PopList(LSTERMptr lstp, cTYPE ctyp, VOIDptr vp)
{
   TERMptr tp = (TERMptr)lstp;
   TERM t;
   RC   rc;

   if (m_trAPI)
	  LOG( "lsPopList" << PRENS( this SEP tp SEP ctyp SEP vp ) );

   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       t = (*tp)->dref();
       if (t->IsList())
         {
           t = t->getListHead();       // point to head of list
           cov_getterm(t, ctyp, vp);
           *tp = t+1;                    // point tp to tail of list
           rc = OK;
         }
       else
         rc = NOTOK;
     }
   catch(LExcept &pE)
     {
       return (RC)deal_with(pE, aS("lsPopList"));
     }

   if (m_trAPI)
     {
       if (rc == OK)
       {
			LOG( SP2 << "returns OK");
       }
       else
       {
         LOG( SP2 << "returns NOTOK");
       }
     }
   
   return rc;
}

RC LEngine::GetHead(LSTERM lstin, cTYPE ctyp, VOIDptr vp)
{
   TERM tin = (TERM)lstin;
   TERM t;
   RC   rc;

   if (m_trAPI)
	  LOG( "lsGetHead" << PRENS( this SEP tin SEP ctyp SEP vp ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       t = (tin)->dref();
       if (t->IsList())
         {
           t = t->getListHead();       // point to head of list
           cov_getterm(t, ctyp, vp);
           rc = OK;
         }
       else
         pXCPT->Error(badlistE);
     }
   catch(LExcept &pE)
     {
       return (RC)deal_with(pE, aS("lsGetHead"));
     }
   
   if (m_trAPI)
     {
       if (rc == OK)
       {
			LOG( SP2 << "returns OK");
       }
       else
       {
         LOG( SP2 << "returns NOTOK");
       }
     }
   
   return rc;
}

LSTERM LEngine::GetTail(LSTERM lstin)
{
   TERM tin = (TERM)lstin;
  TERM t;
  
  if (m_trAPI)
	 LOG( "lsGetTail" << PRENS( this SEP tin ) );
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
        pXCPT->Error(noloadE);
      
      t = (tin)->dref();
      if (t->IsList())
        {
          t = t->getListHead();       // point to head of list
          t = (t+1)->dref();     // point tp to tail of list
          if (! t->IsList())
            t = NULL;
        }
      else
        t = NULL;
    }
  catch(LExcept &pE)
    {
      deal_with(pE, aS("lsGetTail"));
      return NULL;
    }
  
  if (m_trAPI)
    {
      if (t == NULL)
      {
		  LOG( SP2 << "returns NULL" );
      }
      else
      {
        LOG( SP2 << "returns " << t );
      }
    }
  
  return (LSTERM)t;
}

RC LEngine::PushList(LSTERMptr lstpList, LSTERM lstElem)
{
   TERM tElem = (TERM)lstElem;
   TERMptr tpList = (TERMptr)lstpList;
   TERM t, list, head, tail;

   if (m_trAPI)
	  LOG( "lsPushList" << PRENS( this SEP tpList SEP tElem ) );

   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       t = (*tpList)->dref();
       if (! ( t->IsList() || 
               (t->IsAtom() && t->getAtom()==pATAB->nilA) ))
         pXCPT->Error(badlistE);
       
       list = pHXL->heapGET();
       head = pHXL->heapGET();
       tail = pHXL->heapGET();
       (list)->setList( head);
       *head = *tElem;
       *tail = *t;
       *tpList = list;
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsPushList"));
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns OK");
   
   return OK;
}

/* Error Handling */
/* -------------- */

RC LEngine::ErrRaise(STRptr s)
{
   if (m_trAPI)
     {
       Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
       LOG( "lsErrRaise" << PRENS( this SEP m_TrBuf ) );
     }
   
   pXCPT->Error(extE, s);
   
   if (m_trAPI)
	  LOG( SP2 << "returns OK");
   
   return OK;
}

ExType LEngine::GetExceptType()
{
   ExType et;

   if (m_trAPI)
	  LOG( "lsGetExceptType" << PRENS( this ) );
   
   et = m_exLast.GetType();
   
   if (m_trAPI)
   {
	  LOG( SP2 << "returns " << et );
   }
   
   return et;
}

RC LEngine::GetExceptRC()
{
   RC  rc;

   if (m_trAPI)
	  LOG( "lsGetExceptRC" << PRENS( this ) );
   
   rc = m_exLast.GetRC();

   if (m_trAPI)
	  LOG( SP2 << "returns exception's RC " << rc );

   return rc;
}

intC LEngine::GetExceptLineno()
{
   intC  ln;

   if (m_trAPI)
	  LOG( "lsGetExceptLineno" << PRENS( this ) );
   
   ln = m_exLast.GetReadLineno();
   
   if (m_trAPI)
	  LOG( SP2 << "returns line number " << ln );
   
   return ln;
}

void LEngine::GetExceptMsg(STRptr s, int buflen)
{
   if (m_trAPI)
	  LOG( "lsGetExceptMsg" << PRENS( this SEP s SEP buflen ) );
   
   Lstrncpy(s, m_exLast.GetMsg(), buflen);
   
   if (m_trAPI)
     {
       Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
       LOG( SP2 << "returns msg string " << m_TrBuf );
     }
   
   return;
}

void LEngine::GetExceptReadBuffer(STRptr s, int buflen)
{
  if (m_trAPI)
	 LOG( "lsGetExceptReadBuffer" << PRENS( this SEP s SEP buflen ) );
  
  Lstrncpy(s, m_exLast.GetReadText(), buflen);
  
  if (m_trAPI)
    {
      Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
      LOG( SP2 << "returns msg string: " << m_TrBuf );
    }
  
  return;
}

void LEngine::GetExceptReadFileName(STRptr s, int buflen)
{
   if (m_trAPI)
	  LOG( "lsGetExceptReadFileName" << PRENS( this SEP s SEP buflen ) );
   
   Lstrncpy(s, m_exLast.GetReadFileName(), buflen);
   
   if (m_trAPI)
     {
       Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
       LOG( SP2 << "returns msg string: " << m_TrBuf );
     }
   
   return;
}

void LEngine::GetExceptCallStack(STRptr s, int buflen)
{
  if (m_trAPI)
	 LOG( "lsGetExceptCallStack" << PRENS(	this SEP s SEP buflen ) );
  
  Lstrncpy(s, m_exLast.GetCallStack(), buflen);
  
  if (m_trAPI)
    {
      Lstrncpy(m_TrBuf, s, TRBUF_SIZE-1);
      LOG( SP2 << "returns msg string: "  << m_TrBuf );
    }
  
  return;
}

/* Variable setting/getting functions */
/* ---------------------------------- */

RC LEngine::GetVersion(STRptr s)
{
  if (m_trAPI)
	 LOG( "lsGetVersion" << PRENS( this ) );
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      
      Lstrcpy(s, m_pstate->m_version);
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsGetVersion"));
    }
  
  if (m_trAPI)
	 LOG( SP2 << "returns OK, string set to: " << s );
  
  return OK;
}

RC LEngine::SetCommandArgs(int argc, aCHAR** argv)
{
  if (m_trAPI)
	 LOG( "lsSetCommandArgs" << PRENS( this ) );
  
  try
    {
      if (! m_pstate->m_initialized)
        pXCPT->Error(noninitE);
      
      m_pstate->m_argc = argc;
      m_pstate->m_argv = argv;
    }
  catch(LExcept &pE)
    {
      return deal_with(pE, aS("lsSetCommandArgs"));
    }
  
  if (m_trAPI)
	 LOG( SP2 << "returns OK");
  
  return OK;
}

RC LEngine::SetStream(STREAM iStream, int j)
{
  if (m_trAPI)
	 LOG( "lsSetStream" << PRENS( this SEP iStream SEP j ) );
  
  try
    {
      if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);

      pIO->set_stream(iStream, j);
   }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsSetStream"));
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns OK");

   return OK;
}

int LEngine::GetStream(STREAM iStream)
{
   int  rv;

   if (m_trAPI)
	  LOG( "lsGetStream" << PRENS( this SEP iStream ) );

   try
     {
      if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
      if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);

      rv = pIO->get_stream(iStream);
   }
   catch(LExcept &pE)
     {
       deal_with(pE, aS("lsGetStream"));
       return NOTOK;
     }

   if (m_trAPI)
	  LOG( SP2 << "returns " << rv );

   return rv;
}

RC LEngine::SetInput(pX_GETC fpGetc, pX_UNGETC fpUngetc)
{
   if (m_trAPI)
	  LOG( "lsSetInput" << PRENS( this SEP fpGetc SEP fpUngetc ) );

   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       pIO->SetGetC(fpGetc);
       pIO->SetUngetC(fpUngetc);
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsSetInput"));
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns OK");
   
   return OK;
}

// SetOutput needs two versions here due to the two different
// flavors of pX_PUTS that might come in.  Unlike other Unicode
// dependent functions, this one can't be fixed by a wcstombs call
// before getting here.                                                       
RC LEngine::SetOutput(pX_PUTC fpPutc, pX_PUTSA fpPuts)
{
   if (m_trAPI)
	  LOG( "lsSetOutput" << PRENS( this SEP fpPutc SEP fpPuts ) );

   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);

      //exPutS = fpPuts;
      //exPutC = fpPutc;
#ifdef xBUG_SETSTREAM
 DUMP << "==>LEngine::SetOutput" << NL;
 DUMP << SP2 << "streams before:" << NL;
 pIO->DumpStreams();
#endif
      pIO->SetPutC(fpPutc);
      pIO->SetPutS(fpPuts);
#ifdef xBUG_SETSTREAM
 DUMP << SP2 << "streams after:" << NL;
 pIO->DumpStreams();
 DUMP << "<==LEngine::SetOutput" << NL;
#endif
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsSetOutput"));
     }

   if (m_trAPI)
	  LOG( SP2 << "returns OK");

   return OK;
}

#ifdef _UNICODE
RC LEngine::SetOutput(pX_PUTC fpPutc, pX_PUTSW fpPuts)
{
   if (m_trAPI)
	  LOG( "lsSetOutput" << PRENS( this SEP fpPutc SEP fpPuts ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       //exPutS = fpPuts;
       //exPutC = fpPutc;

#ifdef BUG_SETSTREAM
 DUMP << "==>LEngine::SetOutput" << NL;
 DUMP << SP2 << "streams before:" << NL;
 pIO->DumpStreams();
#endif

       pIO->SetPutC(fpPutc);
       pIO->SetPutS(fpPuts);

#ifdef BUG_SETSTREAM
 DUMP << SP2 << "streams after:" << NL;
 pIO->DumpStreams();
 DUMP << "<==LEngine::SetOutput" << NL;
#endif

     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsSetOutput"));
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns OK");
   
   return OK;
}
#endif

// Open a user-defined stream

// OpenUserStream needs two versions due to different
// types of strings.  Unlike other Unicode
// dependent functions, this one can't be fixed by a wcstombs call
// before getting here.                                                       
int LEngine::OpenUserStream(char* alias, paUSER_GET_LINE fpUserGetLine,
         paUSER_PUT_STRING fpUserPutString, VOIDptr vp)
{
   int stream;

   if (m_trAPI)
	  LOG( "lsOpenUserStream" <<
         PRENS( this SEP alias SEP fpUserGetLine SEP fpUserPutString SEP vp ) );

   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);

      stream = pIO->OpenUserStream(alias, fpUserGetLine, fpUserPutString, vp);
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsOpenUserStream"));
     }

   if (m_trAPI)
	  LOG( SP2 << "returns: " << stream);

   return stream;
}

#ifdef _UNICODE
int LEngine::OpenUserStream(aCHAR* alias, pwUSER_GET_LINE fpUserGetLine,
            pwUSER_PUT_STRING fpUserPutString, VOIDptr vp)
{
   int stream;
   if (m_trAPI)
	  LOG( "lsOpenUserStream" <<
         PRENS( this SEP alias SEP fpUserGetLine SEP fpUserPutString SEP vp ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       stream = pIO->OpenUserStream(alias, fpUserGetLine, fpUserPutString, vp);
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsOpenUserStream"));
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns: " << stream);
   
   return stream;
}
#endif



RC LEngine::SetIOArg(VOIDptr vp)
{
   if (m_trAPI)
	  LOG( "lsSetIOArg" << PRENS( this SEP vp ) );
   
   try
     {
       if (! m_pstate->m_initialized)
         pXCPT->Error(noninitE);
       if (! m_pstate->m_loaded)
         pXCPT->Error(noloadE);
       
       //exPutS = fpPuts;
       //exPutC = fpPutc;
       pIO->SetIOArg(vp);
     }
   catch(LExcept &pE)
     {
       return deal_with(pE, aS("lsSetIOArg"));
     }
   
   if (m_trAPI)
	  LOG( SP2 << "returns OK");
   
   return OK;
}


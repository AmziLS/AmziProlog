//-*-C++-*-
/****************************************************************************
* 
* engine.h -- Class Engine header
* 
* Copyright (c) 1992-2009 by Amzi!.  All Rights Reserved.
* 
****************************************************************************/

#ifndef ENGINE_H
#define ENGINE_H

class LEngine;

#ifdef LANDFILL
extern LEngine *g_peng;
extern Lofstream *g_dump;
#endif

/*
// running professional version or not
extern bool g_professional;
*/

class LEngState;
class HXL;
class ControlStack;
class BuiltIns;
class DDBMemory;
class AtomTable;
class DDB;
class PString;
class TermServices;
class LMath;
class TermReader;
class TermWriter;
class SIO;
class LExec;
class LLoad;
class LExHandler;
//class LLog;
class LEX;
class READER;
class GCThings;

#ifdef PROBE
class Probe;
#endif
#ifdef THREADED_EXEC_STR
class ActionBuffer;
#endif

//#define WRITELOG if (m_plog) m_plog->Write
#define LOGSTREAM *(pENG->m_log)

#define LOG(X) \
   if (pENG->m_logging) *(pENG->m_log) << X << NL << FLUSH;

// NOTE: the Visual Basic definition files depend on this order,
//   so either add to the end or make sure Visual Basic is updated
//   as well

enum STREAM
{
   CUR_IN,
   CUR_OUT,
   CUR_ERR,
   USER_IN,
   USER_OUT,
   USER_ERR
};

// DO NOT change the order without changing
// amzi.h and all of the language wrappers,
// such as vb, delphi and java.
enum pTYPE
{
   pERR = -1,
   pATOM,
   pINT,
   pSTR,
   pFLOAT,
   pSTRUCT,
   pLIST,
   pTERM,
   pADDR,
   pVAR,
   pWSTR,
   pWATOM,
	pREAL
};

// DO NOT change the order without changing
// amzi.h and all of the language wrappers,
// such as vb, delphi and java.
enum cTYPE
{
   cAATOM,
   cASTR,
   cINT,
   cLONG,
   cSHORT,
   cFLOAT,
   cDOUBLE,
   cADDR,
   cTERM,
   cWSTR,
   cWATOM,
   // used to break apart :/2 structures
   cMOD,
   cGOAL
};

/*
enum PrologFlag_
{
   MAX_ARITY, MAX_ATOMS, BOUNDED,
	INT_RND, MAX_STREAM, SINGLETON_WARN, DBUG, CHAR_CNVN, UNKNWN, 
	PROLOG_NAME, PROLOG_DATE, PROLOG_COPYRIGHT, PROLOG_VERSION, 
	DEC_PLACES, REALG, MODULO, EPSILON, DELTA   // DELTA = 17
};
*/

enum ROUNDED_ {NOT_ROUNDED, TO_ZERO, TO_FLOOR, TO_NEAREST};

// The initialization function in an LSX, called in lsx_load
typedef int (lsCALLTYPE *LSXINITptr)(ENGid, VOIDptr);
//The two additional functions for debugging, one called when
// loading adebug.lsx, the other called when shutting down
typedef int (lsCALLTYPE *DEBUGSTARTptr)(char*, intC);
typedef void (lsCALLTYPE *DEBUGENDptr)(void);

class LEngState
{
public:
   LBOOL       m_initialized;
   LBOOL       m_loaded;
   LPathString m_loadfile;
   LString     m_version;                      // current Amzi! version

   //LFLAG       m_fileerrors;
   //LBOOL       m_verbose     = FALSE;
   LFLAG       m_trace;
   // TF m_creep       = FALSE;
   //LFLAG       m_readerrors;
   //LFLAG       m_lint;
   //LFLAG       m_fatal_err;
   //LFLAG       m_protect_db;
   //LFLAG       m_aritherrors;
   LFLAG       m_stringesc;
   LFLAG       m_properNames;
   LFLAG       m_vba;
   LUNDEFFLAG  m_undefined_predicate;
   LFLAG       m_occurs_check;
   LFLAG       m_properQuotes;
   //LFLAG       m_controls;
   LFLAG       m_prep;
   LFLAG       m_vars_sort_equal;
   LFLAG       m_debug64_cut;
   LFLAG       m_macrotrace;
   //LFLAG       m_real;
   LString     m_locale;
   LFLAG       m_utf8io;

   // for command_line() predicate
   int        m_argc;
   aCHAR    **m_argv;

   // read buffer length, used in various places for allocating
   // strings.
  intC      m_buflen;
  ROUNDED_  m_rounded;                        // for int div
  DECIMALS_  m_decimals;                       // what type decimals reals/floats?
  FLOATS_   m_floats;
  long      m_decimal_places;                 // decimal places required
  long      m_modulo;                         // for Zm arithmetic
  long      m_epsilon;                        // real precision 
  long      m_delta;                          // real div expansion

  intC      m_debug_port;
  LString   m_debug_host;

  aCHAR datestamp[256];
  int   build_date;   // YYMMDD in an integer
public:
   LEngState(intC buflen, LFLAG oc, LFLAG dqs, LFLAG prep, LFLAG uca, LFLAG vba, LFLAG utf8io, LUNDEFFLAG undefined, intC dp,
         DECIMALS_ rn, FLOATS_ fl, LFLAG vars_sort_equal, LFLAG debug64_cut,
         intC debug_port, LString debug_host);
   ~LEngState() {};
};

const aCHAR copyright[] = 
         aS("Copyright (c) 1992-2010 by Amzi! inc. All Rights Reserved.");
           //0123456789012345678901234567890123456789012345678
           //1         2         3         4         5        
const int FILE_NAME_LENGTH = 256;
const int TRBUF_SIZE       = 256;
const int MAX_COMMAND_ARGS = 32;

/*
class HXL;
class ControlStack;
class BuiltIns;
class DDBMemory;
class PString;
//class RealSvc;
#ifdef PROBE
class Probe;
#endif
*/

struct PRED_INITA;
#ifdef _UNICODE
struct PRED_INITW;
#endif

class LEngine
{
public:
   // major components of an engine
   LEngine       *m_peng; // this, so the various m_peng macros work in engine
   LEngState     *m_pstate;   // Engine state variables
   HXL           *m_phxl;     // The heap as global for the time being.
   ControlStack  *m_pstack;   // The control and trail as global
   BuiltIns      *m_pbips;    // Built in predicates
   //DDBMemory     *m_pddbmem;  // Memory management
   AtomTable     *m_patoms;   // Atom Table & Friends
   DDB           *m_pddb;     // Dynamic database
   PString       *m_pstring;  // Prolog Strings
   TermServices  *m_pts;      // Term Services
   LMath         *m_pmath;    // Math Functions
   TermReader    *m_ptread;   // Term Reader
   TermWriter    *m_ptwrit;   // Term Writer
//   SIO           *m_psio;     // Stream I/O Services
   IO            *m_pio;      // Stream I/O Services
   LExec         *m_pexec;    // Code Execution
   LLoad         *m_pload;    // Loader
   //LLog          *m_plog;     // Log
   LExHandler    *m_pxcpt;    // Exception Handler
   CLogicServer  *m_plogserv; // C++ Logic Server Interface, if defined
   LEX           *m_plex;      // lex
   GCThings      *m_pgcthings; // db of gc things (strings, etc.)
   //RealSvc       *m_preals;    // real numbers
#ifdef PROBE
   Probe         *m_pprobe;   // Performance Probe
#endif
   // public so prolog_flags can look at it
   LIni       m_ini;                     // initialization parameters
#ifdef THREADED_EXEC_STR
   ActionBuffer *m_action_buffer;   // for UI threaded apps
#endif

private:
   LFLAG      m_trAPI;                   // API tracing flag
   aCHAR      m_TrBuf[TRBUF_SIZE];       // Buffer for API trace output
   //LsList<LSXptr> m_lsxs;                // list of loaded .lsx files
   LExcept    m_exLast;                  // Last exception
   bool       m_shutdown;                // have we been shutdown
   bool       m_lsxs_initialized;        // has InitLSX been called?
   std::map<LString, LSXptr> lsx_map;    // Map of LSXs used in this engine
public:
   bool       m_logging;
   Lofstream   *m_log;             // the log stream, if logging enabled

#ifdef WINDOWS
   CRITICAL_SECTION m_critical_section;
#else
   int m_critical_section;
#endif

#ifdef _UNICODE
public:
   wchar_t*   m_argwv[MAX_COMMAND_ARGS]; // Wide shadows for when called from a
   int        m_argwc;           // narrow application. see lsSetCommandArgs().
#endif

   bool       m_debug64;

#ifdef LANDFILL
public:
   Lofstream  *m_dump;
//   CMemoryState mem_begin, mem_end, mem_diff;
#endif

public:
   LEngine();
   LEngine(CLogicServer *);
   ~LEngine();

   //OSVer getOSVer()
   //{ return m_osver; }

   // API Functions

   // Main entry points to set up Prolog environment.
   RC Init(LPathString);
   RC Init2(LString);
   RC InitLSX(VOIDptr);
   RC AddLSX(LPathString, VOIDptr);
   RC AddPred(LString, ARITY, ExtPred, VOIDptr);
   //RC AddDispatchPred(LString, ARITY, ExtDispPred);
   RC InitPreds(PRED_INITA *);
#ifdef _UNICODE
   RC InitPreds(PRED_INITW *);
#endif
   RC Load(LPathString);
   RC LoadFromMemory(LPathString, int, unsigned char *);
   TF Main();
   RC Reset();
   RC Close();

   // function and predicate parameters.

   RC    GetParm(int, cTYPE, VOIDptr);
   pTYPE GetParmType(int);
   int   StrParmLen(int);
   TF    UnifyParm(int, cTYPE, VOIDptr);

   // Calling Prolog from C.

   TF Exec(LSTERMptr);
   TF ExecStr(LSTERMptr, STRptr);
   TF Call(LSTERMptr);
   TF CallStr(LSTERMptr, STRptr);
   TF Redo();
   RC ClearCall();

   // Asserting and retracting.

   RC Asserta(LSTERM);
   RC Assertz(LSTERM);
   TF Retract(LSTERM);
   RC AssertaStr(STRptr);
   RC AssertzStr(STRptr);
   TF RetractStr(STRptr);

   // String/Term conversion functions.

   RC TermToStr(LSTERM, STRptr, int);
   RC TermToStrQ(LSTERM, STRptr, int);
   RC StrToTerm(LSTERMptr, STRptr);

   // Making Prolog types.

   RC MakeAtom(LSTERMptr, STRptr);
   RC MakeStr(LSTERMptr, STRptr);
   RC MakeInt(LSTERMptr, intC);
   RC MakeFloat(LSTERMptr, double);
   RC MakeAddr(LSTERMptr, VOIDptr);

   // Getting C values from Prolog terms.

   pTYPE GetTermType(LSTERM);
   RC    GetTerm(LSTERM, cTYPE, VOIDptr);
   int   StrTermLen(LSTERM);

   // Structure hacking functions.

   RC  GetFA(LSTERM, STRptr, ARITYptr);
   RC  MakeFA(LSTERMptr, STRptr, ARITY);
   TF  UnifyArg(LSTERMptr, int, cTYPE, VOIDptr);
   RC  GetArg(LSTERM, int, cTYPE, VOIDptr);
   pTYPE GetArgType(LSTERM, int);
   int StrArgLen(LSTERM, int);
   // int RealArgLen(LSTERM, int);  // not called
   TF  Unify(LSTERM, LSTERM);

   // List hacking functions.
   
   RC MakeList(LSTERMptr);
   RC PushList(LSTERMptr, LSTERM);
   RC PopList(LSTERMptr, cTYPE, VOIDptr);
   RC GetHead(LSTERM, cTYPE, VOIDptr);
   LSTERM GetTail(LSTERM);

   // Stream I/O functions.

   RC  SetStream(STREAM, int);
   int GetStream(STREAM);

   RC  SetInput(pX_GETC, pX_UNGETC);
   RC  SetOutput(pX_PUTC, pX_PUTSA);
#ifdef _UNICODE
   RC  SetOutput(pX_PUTC, pX_PUTSW);
#endif
   RC  SetIOArg(VOIDptr);

   int OpenUserStream(char*, paUSER_GET_LINE, paUSER_PUT_STRING, VOIDptr);
#ifdef _UNICODE
   int OpenUserStream(aCHAR*, pwUSER_GET_LINE, pwUSER_PUT_STRING, VOIDptr);
#endif

   // Miscellaneous functions.

   RC  GetVersion(STRptr);
   RC  SetCommandArgs(int, aCHAR**);
   //RC  SetAllocFree(MEMptr (*)(size_t), void (*)(MEMptr));

   // Error handling.

   RC ErrRaise(STRptr);

   ExType GetExceptType();
   RC     GetExceptRC();
   intC   GetExceptLineno();
   void   GetExceptMsg(STRptr, int);
   void   GetExceptReadBuffer(STRptr, int);
   void GetExceptReadFileName(STRptr s, int buflen);
   void   GetExceptCallStack(STRptr, int);

   // Engine-specific predicates

   //TF p_mode(void);
   //TF p_version(void);
   //TF p_flag_value(void);
   TF p_set_flag_value(void);
   TF p_loadlsx(void);

   // Needed by load
   void  check_lsxs();

private:
#ifdef WINDOWS
   HINSTANCE LEngine::LoadLibraryAW(LPathString f);
#endif
   LSXINITptr m_lsxinit;
   DEBUGSTARTptr m_DebugStart;
   DEBUGENDptr   m_DebugEnd;

   RC    deal_with(LExcept&, LString);
   RC    initialize();
   void  reset();
   int   cov_strtermlen(TERM);
   int   cov_strtermqlen(TERM);
   //int   cov_realtermlen(TERM);  // not used
   RC    cov_getterm(TERM, cTYPE, VOIDptr);
   pTYPE cov_termtype(TERM);
   void  cov_maketerm(TERM, cTYPE, VOIDptr);
   TERM addModSpec(TERMptr t1);
   void  str_chk(STRptr, intC);
   RC    shut_down();
   void  load_lsx(LPathString, VOIDptr);
   TERM  get_arg(TERM t, int iarg, cTYPE *ctype);
};

#endif //ENGINE_H













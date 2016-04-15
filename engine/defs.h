//-*-C++-*-
/****************************************************************************
* 
* defs.h - global definitions
* 
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* 1999/12/22 Ray Added pLEX, 
*
****************************************************************************/

#ifndef DEFS_H
#define DEFS_H

// not used anymore, a prolog flag instead
//#define VBA  // used for case insensitive versions of atoms

#ifdef WINDOWS
#define THREADED_EXEC_STR
#endif

#ifdef UNIX
#define __declspec
#define DLLExport
#define dllexport
typedef unsigned int DWORD;
#define   HINSTANCE  void *
typedef int BOOL;
#endif

#ifndef DLLExport
#define DLLExport __declspec(dllexport)
#endif

class Cell;

typedef Cell *TERM;
typedef TERM *TERMptr;

//#define noCOPY_DDB_TERMS   // do we always copy, or try to unify in place sometimes

//-----------------------------------------------------
// The system is designed to be built using either
// Unicode or ANSI character, so IFDEFs are used to
// set basic types.  The string functions are also
// differently named.
//
// This block of definitions is for Unicode/ANSI
// differences.
//

#ifdef _UNICODE
#ifndef UNICODE
#define UNICODE  // some places use this instead
#endif

#define aS(x) L ## x
#define aC(x) L ## x
#define aCHAR wchar_t

#ifdef LINUX   // wostream not support on gcc???
#define Lostream   std::ostream
//#define Lostream   std::ofstream
#define Lofstream  std::ofstream
#else
#define Lostream   std::ostream
//#define Lostream   std::ofstream
#define Lofstream  std::ofstream
#endif

#define Lstrlen    wcslen
#define Lstrcmp    wcscmp
#define Lstrncmp   wcsncmp
#define Lstrcpy    wcscpy
#define Lstrncpy   wcsncpy
#define Lstrstr    g_lenv.e_wcsstr
#define Lstrcat    wcscat
#define Lstrncat   wcsncat
#define Lstrcspn   wcscspn
#define Lstrchr    wcschr
#define Lstrrchr   wcsrchr
#define Lstrtok    g_lenv.e_wcstok
#define Lstrpbrk   wcspbrk

#define Lisspace   iswspace
#define Lisdigit   iswdigit
#define Lisalpha   iswalpha
#define Lisupper   iswupper
#define Lislower   iswlower
#define Lisxdigit  iswxdigit
//#ifdef MSC
//#define Lisalnum   _iswalnum
//#else
#define Lisalnum   iswalnum
//#endif
#define Lispunct   iswpunct
#define Ltolower   tolower

#define Latol      g_lenv.e_wtol
#define Latof      g_lenv.e_wtof

#define Lstrftime  g_lenv.e_wcsftime

#define Lmemset    memset
#define Lmemcpy    memcpy

#define Lsystem     g_lenv.e_wsystem
#define Lfopen      g_lenv.e_wfopen
#define Lgetenv_dup g_lenv.e_wgetenv_dup
#define Lfputs      g_lenv.e_fputws
#define Lfgets      g_lenv.e_fgetws
#define Lfgetc      g_lenv.e_fgetwc
#define Lfputc      g_lenv.e_fputwc
#define Lungetc     g_lenv.e_ungetwc
#define CharInt     wint_t                  // getc character integers

#define L_splitpath _wsplitpath
#define LEOF        WEOF

//#define Lvfprintf   g_lenv.e_vfwprintf
#define Lvsprintf   g_lenv.e_vswprintf
// Dropping use of this safer, but non-
// standard varient
//#define Lvsnprintf lenv->e_vsnwprintf

// We try to put the environment-specific
// code in lenv, but because of the ... arguments
// these are better handled as macros here.
#if defined(LINUX) || defined(HPUX)
#define Lprintf    g_lenv.e_wprintf
#else
#define Lprintf    wprintf
#endif

// Lsprintf(buffer, length, format, ...)
//   which is same as standard swprintf, but not MS
//   doesn't implement the standard, leaving out the count
#if defined(MSC) || defined(GNU)
#define Lsprintf   g_lenv.e_swprintf
#else
#define Lsprintf   swprintf
#endif

//---------------------------------
// non-Unicode definitions
//
#else

#define aS(x) x
#define aCHAR char

//#define Lostream   std::ofstream
#define Lostream   std::ostream
#define Lofstream  std::ofstream

// String functions
#define Lstrlen    strlen
#define Lstrcmp    strcmp
#define Lstrncmp   strncmp
#define Lstrcpy    strcpy
#define Lstrncpy   strncpy
#define Lstrstr    strstr
#define Lstrcat    strcat
#define Lstrncat   strncat
#define Lstrcspn   strcspn
#define Lstrchr    strchr
#define Lstrrchr   strrchr
#define Lstrtok    strtok

#define Lisspace   isspace
#define Lisdigit   isdigit
#define Lisalpha   isalpha
#define Lisupper   isupper
#define Lislower   islower
#define Lisxdigit  isxdigit
#define Lisalnum   isalnum
#define Lispunct   ispunct
#define Ltolower   tolower

#define Latol      atol
#define Latof      atof

#define Lfgets     fgets

#define Lstrftime  strftime

#define Lmemset    memset
#define Lmemcpy    memcpy

// Other string-dependent functions
#define Lsystem     system
#define Lfopen      fopen
#define Lgetenv_dup g_lenv.e_getenv_dup
#define Lfputs      fputs
#define Lfgets      fgets
#define Lfgetc      fgetc
#define Lfputc      fputc
#define Lungetc     ungetc
#define CharInt     int     // getc character integers

#ifdef MSC
#define L_splitpath _splitpath
#endif

//#define Lvfprintf   vfprintf
#define Lvsprintf   vsprintf
// Dropping use of this safer, but non-standard variant
//#define Lvsnprintf g_lenv.e_vsnwprintf
#define Lprintf    printf
#define Lsprintf   g_lenv.sprintf

#define LEOF        EOF

#endif  // UNICODE or not

//------------------------------------------
// Other environment specific functions
//

#define Lasys_fopen  g_lenv.asys_fopen
#define Laget_path   g_lenv.aget_path

// binary file info used by linker, loader, etc.

const int CC_HEAD_LENGTH = 8;          // length of .plm header
const int CL_HEAD_LENGTH = 32;         // length of .xpl header

//------------------------------
// Other useful definitions
//

#define X(n) pHXL->XVar(n)->dref()

typedef   aCHAR  *   STRptr;
typedef   STRptr *   STRhnd;

enum LFLAG { LOFF=0, LON };
enum LBOOL { LFALSE=0, LTRUE=1 };

typedef   void *     VOIDptr;
typedef   VOIDptr *  VOIDhnd;

// Logic Server definitions of a term.  To the outside world
// a term is a void*.  Internally we'll cast it to a Cell*.
typedef void* LSTERM;
typedef LSTERM* LSTERMptr;

typedef  int    *    INTptr;
typedef  short  *    SHORTptr;
typedef  long   *    LONGptr;
typedef  double *    DOUBLEptr;
typedef  float  *    FLOATptr;

// Basic Types & Defines

typedef   unsigned char  aBYTE;
typedef   aBYTE *      aBYTEptr;
typedef   char  aSBYTE;
typedef   aSBYTE *  aSBYTEptr;

typedef  int   RC;                    // intended to be 0 for OK, else err no. 
// NOTOK is return by API functions if the call doesn't
// refer to a valid engine, or if initialization
// of the engine fails.
const RC OK = 0;
const RC NOTOK = -1;

typedef int TF;                         // intended for Prolog returns 

#define TRUE  1
#define FALSE 0

#define ON   1                          // values for plmtrace .ini variable 
#define OFF  0
#define LINIT 2

typedef VOIDptr WINDOWPTR;              // makes all old window stuff work 

#define EOS aS('\0')

// Common formatting patterns for output

#define NL  aS("\n")
#define SP  aS(" ")
#define SP2  aS("  ")
#define SP3  aS("   ")
#define FLUSH std::flush
#define HEXOUT(x) std::hex << x << std::dec
#define HEXOUTP(x) aS("(") << HEXOUT(x) << aS(")")
#define DECHEX(x) x << HEXOUTP(x)
#define PREDAR(p,a) p << aS("/") << a
#define MODPREDAR(m,p,a) m << ":" << p << "/" << a
//#define INDENT(s, n) for (int iindent=0; iindent<n; iindent++) s << SP2
#define INDENT(s, n) s->width(2*n); *s << "";
#define SEP << ", " <<
#define PRENS(X) "(" << X << ")"

/* Integer types - these must keep the relationship that intC is an
 * integer of the same size as CELL, intCH is an integer of half the
 *size of a CELL and intCQ is an integer of a quarter of the size of
 * a CELL 
 */

#if defined(P64) && defined(WINDOWS)
// windows 64 conventions are int 32, long 32, longlong 64, ptr 64

typedef __int64             intC;
typedef unsigned __int64   uintC;
typedef int              intCH;
typedef unsigned int    uintCH;
typedef short            intCQ;
typedef unsigned short  uintCQ;
typedef short            aINT16;
typedef unsigned short  aUINT16;
typedef int              aINT32;
typedef unsigned int    aUINT32;
#define MAXintCH         0x7fffffff
#define MAXintC          0x7fffffffffffffff
//#define MAXarity         255
#define MAXarity         0xfff

#elif defined(P64) && defined(UNIX)
// unix 64 conventions are int 32, long 64, longlong 64, ptr 64

typedef long             intC;
typedef unsigned long   uintC;
typedef int              intCH;
typedef unsigned int    uintCH;
typedef short            intCQ;
typedef unsigned short  uintCQ;
typedef short            aINT16;
typedef unsigned short  aUINT16;
typedef int              aINT32;
typedef unsigned int    aUINT32;
#define MAXintCH         0x7fffffff
#define MAXintC          0x7fffffffffffffff
//#define MAXarity         255
#define MAXarity         0xfff

#elif defined(P32)

typedef long             intC;
typedef unsigned long   uintC;
typedef short            intCH;
typedef unsigned short  uintCH;
typedef aSBYTE           intCQ;
typedef aBYTE           uintCQ;
typedef short            aINT16;
typedef unsigned short  aUINT16;
typedef long             aINT32;
typedef unsigned long   aUINT32;
#define MAXintCH         0x7fff
#define MAXintC          0x7fffffff
//#define MAXarity         255
#define MAXarity         0xfff

#elif defined(P16)

typedef long             intC;
typedef unsigned long   uintC;
typedef short            intCH;
typedef unsigned short  uintCH;
typedef aSBYTE           intCQ;
typedef aBYTE           uintCQ;
typedef short            aINT16;
typedef unsigned short  aUINT16;
typedef long             aINT32;
typedef unsigned long   aUINT32;
#define MAXintCH         0x7fff
#define MAXintC          0x7fffffff
//#define MAXarity         255
#define MAXarity         0xfff

#endif

typedef intC *       intCptr;
typedef intCH *      intCHptr;
typedef uintC *      uintCptr;

// For reals

//#ifdef REALS

//#ifdef WINDOWS
//typedef __int64   INT64;
//#else
//typedef longlong INT64;
//#endif

//#else   // this probably breaks everything but gets things to compile
//typedef int INT64;
//#endif

class LReal;

typedef LReal *Real;
typedef aINT32 Gigit;   // a gigit is 32 bits



// Various WAM types 

class LAtom;
typedef LAtom *PATOM;
typedef PATOM *PATOMptr;

//typedef intCH          PATOM;             // Prolog Atom
//typedef PATOM *        PATOMptr;

//typedef uintCH         ARITY;  // hope this doesn't break anything
//typedef uintCQ         ARITY;
typedef uintCH         ARITY;
typedef ARITY *        ARITYptr;

//typedef intCH          COUNT;

#if defined(P16)
typedef uintCH         CODE;

#elif defined(P32)

// All 32-bit code is aligned 32 to allow long jumps
// in compiled code.
//#if defined(ALIGN32)
typedef uintC          CODE;
//#else
//typedef uintCH         CODE;
//#endif

//typedef uintC          CODE;                // ray

#elif defined(P64)
typedef uintC          CODE;

#endif

typedef CODE *         CODEptr;
typedef CODEptr *      CODEhnd;

#ifdef ZTC
#define BREAKPOINT asm(0xcc) // set a debugger breakpoint, only for ztc so far 
#else
#define BREAKPOINT
#endif

class LEngine;

typedef LEngine * ENGid;
typedef void * VENGid;     // Needed for ExtPred definition, used externally
typedef ENGid * ENGidptr;

                                           // function modifiers 
class CLogicServer;

#if defined(WIN3)
#define lsCALLTYPE __pascal
#define EXPFUNC __export lsCALLTYPE
#define IMPFUNC lsCALLTYPE
#define LSAPI EXPFUNC
typedef void (EXPFUNC *EXPFUNCptr)();
typedef void (IMPFUNC *IMPFUNCptr)();
typedef TF (IMPFUNC *ExtPred)(VENGid);
typedef TF (IMPFUNC *ExtDispPred)(CLogicServer*);

#elif defined(WIN4)
#define lsCALLTYPE __stdcall
#define EXPFUNC __declspec(dllexport) lsCALLTYPE
#define IMPFUNC __declspec(dllimport) lsCALLTYPE
#define LSAPI   lsCALLTYPE
typedef int (lsCALLTYPE *EXPFUNCptr)();
typedef int (lsCALLTYPE *IMPFUNCptr)();
typedef TF (lsCALLTYPE *ExtPred)(VENGid);
typedef TF (lsCALLTYPE *ExtDispPred)(CLogicServer*);

#elif defined(DOS)
#define lsCALLTYPE __pascal
#define EXPFUNC lsCALLTYPE
#define IMPFUNC lsCALLTYPE
#define LSAPI EXPFUNC
typedef int (EXPFUNC *EXPFUNCptr)();
typedef int (IMPFUNC *IMPFUNCptr)();
typedef TF (IMPFUNC *ExtPred)(VENGid);
typedef TF (IMPFUNC *ExtDispPred)(CLogicServer*);

#else
#define lsCALLTYPE
#define EXPFUNC lsCALLTYPE
#define IMPFUNC lsCALLTYPE
#define LSAPI EXPFUNC
typedef int (EXPFUNC *EXPFUNCptr)();
typedef int (IMPFUNC *IMPFUNCptr)();
typedef TF (IMPFUNC *ExtPred)(VENGid);
typedef TF (IMPFUNC *ExtDispPred)(CLogicServer*);
#endif

#define LOCALFUNC

#if defined(OPENVMS)                    // The directory delimiter character 
#define SLASH ']'
#elif defined(UNIX)
#define SLASH '/'
#else
#define SLASH '\\'
#endif

// External I/O functions used in callbacks.
// Note that these functions expect Unicode characters
// in both the strings and ints.  For ANSI characters,
// the ints can simply be cast, but for the string in
// puts the routine mbstowcs() can be used to convert
// an ASNI string to a wide Unicode string.
typedef int  (lsCALLTYPE *pX_GETC)(VOIDptr);
typedef int  (lsCALLTYPE *pX_UNGETC)(VOIDptr, int);
typedef int  (lsCALLTYPE *pX_PUTC)(VOIDptr, int);
typedef int  (lsCALLTYPE *pX_PUTSA)(VOIDptr, char*);
typedef char* (lsCALLTYPE *paUSER_GET_LINE)(VOIDptr);
typedef void (lsCALLTYPE *paUSER_PUT_STRING)(VOIDptr, char *);
#ifdef _UNICODE
typedef int  (lsCALLTYPE *pX_PUTSW)(VOIDptr, wchar_t*);
typedef wchar_t* (lsCALLTYPE *pwUSER_GET_LINE)(VOIDptr);
typedef void (lsCALLTYPE *pwUSER_PUT_STRING)(VOIDptr, wchar_t *);
#endif

#ifdef _UNICODE
#define pX_PUTS pX_PUTSW
#define pUSER_GET_LINE pwUSER_GET_LINE
#define pUSER_PUT_STRING pwUSER_PUT_STRING
#else
#define pX_PUTS pX_PUTSA
#define pUSER_GET_LINE paUSER_GET_LINE
#define pUSER_PUT_STRING paUSER_PUT_STRING
#endif

class LEngine;
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
//class SIO;
class IO;
class LExec;
class LLoad;
class LExHandler;
//class LLog;
class CLogicServer;
class LandFill;
class LEX;

#define pENG     m_peng             // The Engine
#define pSTATE   m_peng->m_pstate   // Engine state variables
#define pHXL     m_peng->m_phxl     // The heap as global for the time being.
#define pCNTL    m_peng->m_pstack   // The control and trail as global
#define pBIPS    m_peng->m_pbips    // Built in predicates
//#define pDMEM    m_peng->m_pddbmem  // Memory management
#define pATAB    m_peng->m_patoms   // Atom Table & Friends
#define pDDB     m_peng->m_pddb     // Dynamic Database
#define pPSTR    m_peng->m_pstring  // Things & Strings
#define pTSVC    m_peng->m_pts      // Term Services
#define pMATH    m_peng->m_pmath    // Math Functions
#define pREAD    m_peng->m_ptread   // Term Reader
#define pWRIT    m_peng->m_ptwrit   // Term Writer
//#define pSIO     m_peng->m_psio     // Stream I/O Services
#define pIO      m_peng->m_pio      // Stream I/O Services
#define pEXEC    m_peng->m_pexec    // Code Execution
#define pLOAD    m_peng->m_pload    // Loader
//#define pLOG     m_peng->m_plog     // Log
#define pXCPT    m_peng->m_pxcpt    // Exception Handler
#define pLOGSERV m_peng->m_plogserv // C++ Logic Server Interface
#define pLEX     m_peng->m_plex     // lex
#define pGCTH    m_peng->m_pgcthings  // gc things (strings etc.)
#define pKEYS    m_peng->m_pkeys    // Security Keys
#ifdef THREADED_EXEC_STR
#define pACTION  m_peng->m_action_buffer  // asynchronous action buffer
#endif
#ifdef LANDFILL
#define DUMP     *(m_peng->m_dump)     // Land Fill for dumping
#endif
#ifdef PROBE
#define pPROBE   m_peng->m_pprobe   // Performance Probe class
#endif


//typedef TF (CLogicServer::*pExtMemberPred)();
typedef TF (*pExtMemberPred)(CLogicServer *);

#ifdef _UNICODE
struct PRED_INITW
{
     wchar_t*  Pname;
     ARITY      Parity;
     ExtPred   Pfunc;
};
typedef PRED_INITW * PRED_INITWptr;
#endif

struct PRED_INITA
{
     char*     Pname;
     ARITY      Parity;
     ExtPred   Pfunc;
};
typedef PRED_INITA * PRED_INITAptr;

// A global exception handler for use during debugging, it is the
// exception handler for the running engine.  Used only in macros
// defined in debug.h.  The actual definition for g_xcpt is in
// engine.cpp.
extern LExHandler *g_xcpt;


#endif //DEFS_H



















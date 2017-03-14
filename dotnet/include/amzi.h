/****************************************************************************

Amzi.h - C/C++ Logic Server API Interface

Copyright (c) 1992-2012 by Amzi! inc.  All Rights Reserved.

****************************************************************************/

#ifndef LOGIC_SERVER_DEFINED
#define LOGIC_SERVER_DEFINED
#include <stdlib.h>   // needed for NULL definition used below

//----------------------------------------
// Environment dependent defines
// The challenge here is to tease out the predefined macros
// for each platform that let us define those that we use.
// UNIX/WINDOWS, P32/P64, GNU, UNICODE
//-----------------------------------------

// Unix flavors (building with gcc for all at this point)

#if defined(__bsdi__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#if !defined(BSD)  // should be defined with version number
#define BSD
#endif
#endif

#if defined(__linux__)
#define LINUX
#endif

#if defined(__hpux)
#define HPUX
#endif

#if defined(__APPLE__) || defined(__MACH__)   // maybe should be &&, not ||
#define MAC
#endif

#if defined(__unix__) || defined(BSD) || defined(LINUX) || defined(MAC) || defined(HPUX)
#define UNIX
#endif

#if defined(UNIX)

#if defined(__LP64__)  // specifically, long int and pointers are 64-bit
                       // and ints 32-bit.  true for most 64-bit Unix aps, but not
                       // so for Windows where in 64-bit apps, long is 32-bits
#define P64
#else
#define P32
#endif   // __LP64__

#if defined(__GNUC__)
#define GNU
#endif

#include <ctype.h>
#if defined(_UNICODE)
#include <ctype.h>
#include <wchar.h>
#if !defined(HPUX)
#include <wctype.h>
#endif  // ndef HPUX
#endif  // _UNICODE

#endif   // UNIX

// Windows flavors

#if defined(_WIN32) || defined(_WIN64)  // _WIN32 actually defined for all Windows
#define MSWIN
#endif

#if defined(MSWIN)   // Windows

#if defined(_WIN64)  // defined by compiler for 64-bit applications
                     // note: WIN64 is defined by SDK and indicates target machine
#define P64
#else
#define P32
#endif

#if defined(_MSC_VER)   // Microsoft C++
#define MSC
#endif

#endif  // Windows

/*
// Rat's nest of historical environment defines showing
// range of platforms Amzi! has been ported to.
   
// Microsoft

#ifdef _MSC_VER

#define MSC

#ifdef _WIN32
#define P32
#else
#define P16
#endif

#if defined(_WINDOWS) || defined(_WIN32)
#define MSWIN
#else
#define DOS
#endif

#endif // _MSC_VER

// Borland

#ifdef __BORLANDC__

#define BC

#ifdef __WIN32__
#define P32
#else
#define P16
#endif

#ifdef _Windows
#define MSWIN
#else
#define DOS
#endif

#endif // __BORLANDC__

// Solaris
#ifdef __sun
#define P32
#define UNIX
#define SOLARIS
#endif // __sun

// DEC Alpha

#ifdef __osf__
#define P64
#define UNIX
#endif

#ifdef __vms
#define P32
#define VMS
#endif

// Linux
// note that __unix__ is also defined
// as is __GNUC__, which may be of use someday

#ifdef __GNUC__
#define GNU
#endif

#ifdef __linux__
#define P32
#define UNIX
#define LINUX
#endif

#if defined(__bsd__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__BSD__)
#define BSD
#endif

#if defined(BSD)
#define P32
#define UNIX
#define LINUX
#endif

#if defined(__APPLE__)
#define UNIX
#define P64
#endif

#if !defined(UNIX) && defined(__unix__)
#define P32
#define UNIX
#endif

#ifdef __x86_64__
#define P64
#endif

#ifdef   UNIX
#include <ctype.h>
#endif

#ifdef _UNICODE
#ifdef   UNIX
#include <ctype.h>
#include <wchar.h>
#ifndef HPUX
#include <wctype.h>
#endif  // ndef HPUX
#endif  // UNIX
#endif  // _UNICODE

//#if defined(SOLARIS) && ! defined(GNU)
//#include <widec.h>
//#endif
*/

#ifdef _UNICODE
#define aCHAR wchar_t
#else
#define aCHAR char
#endif

/* Basic Types & Defines */

typedef   unsigned char  aBYTE;
typedef   char aSBYTE;

typedef   aBYTE *      aBYTEptr;
typedef   aSBYTE *     aSBYTEptr;

typedef  int   RC;          /* intended to be 0 for OK, else err no. */
#define  OK     0
#define  NOTOK  -1

typedef int TF;             /* intended for Prolog returns */
#define TRUE  1
#define FALSE 0

#ifdef BC
#define NULL   0
#endif

typedef   aCHAR  *    STRptr;
typedef   STRptr *    STRhnd;

typedef   void *      VOIDptr;
typedef   VOIDptr *   VOIDhnd;

/*   Amzi! integer sizes are often based on the size of a Prolog cell,
   which is the size of a pointer, which is 32-bits for most systems
   and 64-bits for others.  So, intC is a cell-sized integer, intCH
   a half cell, etc. */

#if defined(P64)
#if defined(MSWIN)
// Windows 64 defines long as 32, longlong as 64

typedef __int64             intC;
typedef unsigned __int64   uintC;
typedef int              intCH;
typedef unsigned int    uintCH;
typedef short            intCQ;
typedef unsigned short  uintCQ;
typedef short            aINT16;
typedef unsigned short  aUINT16;
typedef int              aINT32;

#elif defined(UNIX)
// Unix 64 defines long as 64

typedef long             intC;
typedef unsigned long   uintC;
typedef int              intCH;
typedef unsigned int    uintCH;
typedef short            intCQ;
typedef unsigned short  uintCQ;
typedef short            aINT16;
typedef unsigned short  aUINT16;
typedef int              aINT32;
#endif   // MSWIN / UNIX

#else   // defined(P32)

typedef long              intC;
typedef unsigned long    uintC;
typedef short            intCH;
typedef unsigned short  uintCH;
typedef aSBYTE           intCQ;
typedef aBYTE           uintCQ;
typedef short            aINT16;
typedef unsigned short  UaINT16;
typedef long            aINT32;

#endif

typedef intC *        intCptr;
typedef intCH *       intCHptr;
typedef uintC *       uintCptr;

/* Various Prolog types */

typedef void* TERM;
typedef TERM *     TERMptr;


typedef intCH      PATOM;             /* Prolog Atom */
typedef PATOM *    PATOMptr;

typedef uintCH     ARITY;
typedef ARITY *    ARITYptr;

// don't change these without changing
// various language wrappers.
typedef enum
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
} pTYPE;

// don't change these without changing
// various language wrappers.
typedef enum 
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
} cTYPE;

typedef enum
{
   CUR_IN,
   CUR_OUT,
   CUR_ERR,
   USER_IN,
   USER_OUT,
   USER_ERR
} STREAM;

// The exception types.
typedef enum
{ 
   BADENG,
   ABORT,
   INTERNAL,
   FATAL,
   INIT,
   API,
   LOAD,
   EXEC,
   READ,
	ARITH,
   SECURITY,
   UNKNOWN
} ExType;

#define STR_BUF 512

typedef void * ENGid;
typedef ENGid * ENGidptr;

/*   External function calling convention declarations */

#if defined(MSWIN) && defined(P32)
#define LSAPI __stdcall
#define EXPFUNC __stdcall
typedef TF (EXPFUNC *ExtPred)(VOIDptr);

#else
#define LSAPI
#define EXPFUNC
typedef TF (*ExtPred)(VOIDptr);

#endif

/*   External I/O functions used in callbacks. */

#define lsCALLTYPE LSAPI

/*   Note that these functions expect Unicode characters
   in both the strings and ints.  For ANSI characters,
   the ints can simply be cast, but for the string in
   puts the routine mbstowcs() can be used to convert
   an ASNI string to a wide Unicode string. */
typedef int  (lsCALLTYPE *pX_GETC)(VOIDptr);
typedef int  (lsCALLTYPE *pX_UNGETC)(VOIDptr, int);
typedef int  (lsCALLTYPE *pX_PUTC)(VOIDptr, int);
typedef int  (lsCALLTYPE *pX_PUTSA)(VOIDptr, char*);
typedef char* (lsCALLTYPE *paUSER_GET_LINE)(VOIDptr);
typedef void  (lsCALLTYPE *paUSER_PUT_STRING)(VOIDptr, char*);
#ifdef _UNICODE
typedef int  (lsCALLTYPE *pX_PUTSW)(VOIDptr, wchar_t*);
typedef wchar_t* (lsCALLTYPE *pwUSER_GET_LINE)(VOIDptr);
typedef void  (lsCALLTYPE *pwUSER_PUT_STRING)(VOIDptr, wchar_t*);
#endif

/*   Structure used to hold mappings between
   extended Prolog predicate names and the
   functions that implement them. */

#ifdef _UNICODE
typedef struct {
     wchar_t*   Pname;
     ARITY      Parity;
     ExtPred    Pfunc;
} PRED_INITW;
typedef PRED_INITW * PRED_INITWptr;
#endif

typedef struct {
     char*      Pname;
     ARITY      Parity;
     ExtPred    Pfunc;
} PRED_INITA;
typedef PRED_INITA * PRED_INITAptr;


/* C Logic Server API (LSAPI) Interface */
/* ------------------------------------ */

#ifdef _UNICODE

#define cSTR cWSTR
#define cATOM cWATOM
#define lsInit lsInitW
#define lsInit2 lsInit2W
#define lsAddLSX lsAddLSXW
#define lsAddPred lsAddPredW
#define PRED_INIT PRED_INITW
#define PRED_INITptr PRED_INITWptr
#define lsInitPreds lsInitPredsW
#define lsLoad lsLoadW
#define lsLoadFromMemory lsLoadFromMemoryW
#define lsExecStr lsExecStrW
#define lsCallStr lsCallStrW
#define lsAssertaStr lsAssertaStrW
#define lsAssertzStr lsAssertzStrW
#define lsRetractStr lsRetractStrW
#define lsTermToStr lsTermToStrW
#define lsTermToStrQ lsTermToStrQW
#define lsStrToTerm lsStrToTermW
#define lsMakeAtom lsMakeAtomW
#define lsMakeStr lsMakeStrW
#define lsGetFA lsGetFAW
#define lsMakeFA lsMakeFAW
#define lsGetVersion lsGetVersionW
#define lsSetCommandArgs lsSetCommandArgsW
#define lsErrRaise lsErrRaiseW
#define lsGetExceptMsg lsGetExceptMsgW
#define lsGetExceptReadBuffer lsGetExceptReadBufferW
#define lsGetExceptReadFileName lsGetExceptReadFileNameW
#define lsGetExceptCallStack lsGetExceptCallStackW
#define pX_PUTS pX_PUTSW
#define pUSER_PUT_STRING pwUSER_PUT_STRING
#define pUSER_GET_LINE pwUSER_GET_LINE
#define lsSetOutput lsSetOutputW
#define lsOpenUserStream lsOpenUserStreamW
#else

#define cSTR cASTR
#define cATOM cAATOM
#define lsInit lsInitA
#define lsInit2 lsInit2A
#define lsAddLSX lsAddLSXA
#define lsAddPred lsAddPredA
#define PRED_INIT PRED_INITA
#define PRED_INITptr PRED_INITAptr
#define lsInitPreds lsInitPredsA
#define lsLoad lsLoadA
#define lsLoadFromMemory lsLoadFromMemoryA
#define lsExecStr lsExecStrA
#define lsCallStr lsCallStrA
#define lsAssertaStr lsAssertaStrA
#define lsAssertzStr lsAssertzStrA
#define lsRetractStr lsRetractStrA
#define lsTermToStr lsTermToStrA
#define lsTermToStrQ lsTermToStrQA
#define lsStrToTerm lsStrToTermA
#define lsMakeAtom lsMakeAtomA
#define lsMakeStr lsMakeStrA
#define lsGetFA lsGetFAA
#define lsMakeFA lsMakeFAA
#define lsGetVersion lsGetVersionA
#define lsSetCommandArgs lsSetCommandArgsA
#define lsErrRaise lsErrRaiseA
#define lsGetExceptMsg lsGetExceptMsgA
#define lsGetExceptReadBuffer lsGetExceptReadBufferA
#define lsGetExceptReadFileName lsGetExceptReadFileNameA
#define lsGetExceptCallStack lsGetExceptCallStackA
#define pX_PUTS pX_PUTSA
#define pUSER_PUT_STRING paUSER_PUT_STRING
#define pUSER_GET_LINE paUSER_GET_LINE
#define lsSetOutput lsSetOutputA
#define lsOpenUserStream lsOpenUserStreamA
#endif //_UNICODE

#ifdef __cplusplus
extern "C"
{
#endif

/* Main entry points to set up Prolog environment */

RC  LSAPI  lsInitA(ENGidptr, char*);
RC  LSAPI  lsInit2A(ENGidptr, char*);
RC  LSAPI  lsInitLSX(ENGid, VOIDptr);
RC  LSAPI  lsAddLSXA(ENGid, char*, VOIDptr);
RC  LSAPI  lsAddPredA(ENGid, char*, ARITY, ExtPred, VOIDptr);
RC  LSAPI  lsInitPredsA(ENGid, PRED_INITAptr);
RC  LSAPI  lsLoadA(ENGid, char*);
RC  LSAPI  lsLoadFromMemoryA(ENGid, char*, int, aBYTEptr);
TF  LSAPI  lsMain(ENGid);
RC  LSAPI  lsReset(ENGid);
RC  LSAPI  lsClose(ENGid);

/* function and predicate parameters */

RC    LSAPI  lsGetParm(ENGid, int, cTYPE, VOIDptr);
pTYPE LSAPI  lsGetParmType(ENGid, int);
int   LSAPI  lsStrParmLen(ENGid, int);
TF    LSAPI  lsUnifyParm(ENGid, int, cTYPE, VOIDptr);

/* Calling Prolog from C */

TF LSAPI  lsExec(ENGid, TERMptr);
TF LSAPI  lsExecStrA(ENGid, TERMptr, char*);
TF LSAPI  lsCall(ENGid, TERMptr);
TF LSAPI  lsCallStrA(ENGid, TERMptr, char*);
TF LSAPI  lsRedo(ENGid);
RC LSAPI  lsClearCall(ENGid);

/* Asserting and retracting */

RC LSAPI  lsAsserta(ENGid, TERM);
RC LSAPI  lsAssertz(ENGid, TERM);
TF LSAPI  lsRetract(ENGid, TERM);
RC LSAPI  lsAssertaStrA(ENGid, char*);
RC LSAPI  lsAssertzStrA(ENGid, char*);
TF LSAPI  lsRetractStrA(ENGid, char*);


/* String/Term conversion functions */

RC LSAPI  lsTermToStrA(ENGid, TERM, char*, int);
RC LSAPI  lsTermToStrQA(ENGid, TERM, char*, int);
RC LSAPI  lsStrToTermA(ENGid, TERMptr, char*);

/* Making Prolog types */

RC LSAPI  lsMakeAtomA(ENGid, TERMptr, char*);
RC LSAPI  lsMakeStrA(ENGid, TERMptr, char*);
RC LSAPI  lsMakeInt(ENGid, TERMptr, intC);
RC LSAPI  lsMakeFloat(ENGid, TERMptr, double);
RC LSAPI  lsMakeAddr(ENGid, TERMptr, VOIDptr);

/* Getting C values from Prolog terms */

pTYPE LSAPI  lsGetTermType(ENGid, TERM);
RC    LSAPI  lsGetTerm(ENGid, TERM, cTYPE, VOIDptr);
int   LSAPI  lsStrTermLen(ENGid, TERM);

/* Structure hacking functions */

RC  LSAPI  lsGetFAA(ENGid, TERM, char*, ARITYptr);
RC  LSAPI  lsMakeFAA(ENGid, TERMptr, char*, ARITY);
TF  LSAPI  lsUnifyArg(ENGid, TERMptr, int, cTYPE, VOIDptr);
RC  LSAPI  lsGetArg(ENGid, TERM, int, cTYPE, VOIDptr);
pTYPE LSAPI  lsGetArgType(ENGid, TERM, int);
int LSAPI  lsStrArgLen(ENGid, TERM, int);
TF  LSAPI  lsUnify(ENGid, TERM, TERM);

/* List hacking functions */

RC LSAPI  lsMakeList(ENGid, TERMptr);
RC LSAPI  lsPushList(ENGid, TERMptr, TERM);
RC LSAPI  lsPopList(ENGid, TERMptr, cTYPE, VOIDptr);
RC LSAPI  lsGetHead(ENGid, TERM, cTYPE, VOIDptr);
TERM LSAPI  lsGetTail(ENGid, TERM);

/* Stream I/O functions */

RC  LSAPI  lsSetStream(ENGid, STREAM, int);
int LSAPI  lsGetStream(ENGid, STREAM);

RC  LSAPI  lsSetInput(ENGid, pX_GETC, pX_UNGETC);
RC  LSAPI  lsSetOutputA(ENGid, pX_PUTC, pX_PUTSA);
RC  LSAPI  lsSetIOArg(ENGid, VOIDptr);

int LSAPI  lsOpenUserStreamA(ENGid, char*, paUSER_GET_LINE, paUSER_PUT_STRING, VOIDptr);

/* Miscellaneous functions */

RC  LSAPI  lsGetVersionA(ENGid, char*);
RC  LSAPI  lsSetCommandArgsA(ENGid, int, char**);
RC  LSAPI  lsSetAllocFree(ENGid, VOIDptr(*)(), void(*)());

/* Error handling */

RC LSAPI  lsErrRaiseA(ENGid, char*);

ExType LSAPI  lsGetExceptType(ENGid);
RC     LSAPI  lsGetExceptRC(ENGid);
intC   LSAPI  lsGetExceptLineno(ENGid);
void   LSAPI  lsGetExceptMsgA(ENGid, char*, int);
void   LSAPI  lsGetExceptReadFileNameA(ENGid, char*, int);
void   LSAPI  lsGetExceptReadBufferA(ENGid, char*, int);
void   LSAPI  lsGetExceptCallStackA(ENGid, char*, int);

#ifdef _UNICODE
RC  LSAPI  lsInitW(ENGidptr, wchar_t*);
RC  LSAPI  lsInit2W(ENGidptr, wchar_t*);
RC  LSAPI  lsAddLSXW(ENGid, wchar_t*, VOIDptr);
RC  LSAPI  lsAddPredW(ENGid, wchar_t*, ARITY, ExtPred, VOIDptr);
RC  LSAPI  lsInitPredsW(ENGid, PRED_INITWptr);
RC  LSAPI  lsLoadW(ENGid, wchar_t*);
RC  LSAPI  lsLoadFromMemoryW(ENGid, wchar_t*, int, aBYTEptr);
TF LSAPI  lsExecStrW(ENGid, TERMptr, wchar_t*);
TF LSAPI  lsCallStrW(ENGid, TERMptr, wchar_t*);
RC LSAPI  lsAssertaStrW(ENGid, wchar_t*);
RC LSAPI  lsAssertzStrW(ENGid, wchar_t*);
TF LSAPI  lsRetractStrW(ENGid, wchar_t*);
RC LSAPI  lsTermToStrW(ENGid, TERM, wchar_t*, int);
RC LSAPI  lsTermToStrQW(ENGid, TERM, wchar_t*, int);
RC LSAPI  lsStrToTermW(ENGid, TERMptr, wchar_t*);
RC LSAPI  lsMakeAtomW(ENGid, TERMptr, wchar_t*);
RC LSAPI  lsMakeStrW(ENGid, TERMptr, wchar_t*);
RC  LSAPI  lsGetFAW(ENGid, TERM, wchar_t*, ARITYptr);
RC  LSAPI  lsMakeFAW(ENGid, TERMptr, wchar_t*, ARITY);
RC  LSAPI  lsSetOutputW(ENGid, pX_PUTC, pX_PUTSW);
int LSAPI  lsOpenUserStreamW(ENGid, wchar_t*, pwUSER_GET_LINE, pwUSER_PUT_STRING, VOIDptr);
RC  LSAPI  lsGetVersionW(ENGid, wchar_t*);
RC  LSAPI  lsSetCommandArgsW(ENGid, int, wchar_t**);
RC LSAPI  lsErrRaiseW(ENGid, wchar_t*);
void   LSAPI  lsGetExceptMsgW(ENGid, wchar_t*, int);
void   LSAPI  lsGetExceptReadFileNameW(ENGid, wchar_t*, int);
void   LSAPI  lsGetExceptReadBufferW(ENGid, wchar_t*, int);
void   LSAPI  lsGetExceptCallStackW(ENGid, wchar_t*, int);
#endif

#ifdef __cplusplus   // ending the extern "C" from above
}
#endif


/* C++ Logic Server API (LSAPI) Class Interface */
/* -------------------------------------------- */

#ifdef __cplusplus

class CLogicServer
{
private:
   ENGid    m_eng;

public:

// Constructors and Destructor

   CLogicServer();
   CLogicServer(aCHAR * xplname);
   virtual ~CLogicServer();

// Main entry points to set up Prolog environment

   void   Init(aCHAR * ininame);
   void   Init2(aCHAR * inistring);
   void   InitLSX(void * vp);
   void   InitLSX();
   void   AddLSX(aCHAR* s, void * vp);
   void   AddLSX(aCHAR* s);
   void   AddPred(aCHAR* s, ARITY a, ExtPred f, VOIDptr v);
   //void   AddDispatchPred(aCHAR* s, ARITY a, ExtDispPred f);
   void   InitPreds(PRED_INITptr preds);
   void   Load(aCHAR *lsprog);
   void   LoadFromMemory(aCHAR *lsprog, int len, aBYTEptr code);
   TF     Main(void);
   void   Reset(void);
   void   Close(void);

// function and predicate parameters

   void   GetParm(int i, cTYPE ct, void * vp);
   pTYPE  GetParmType(int i);
   int    StrParmLen(int i);
   TF     UnifyParm(int i, cTYPE ct, VOIDptr vp);

// Calling Prolog from C

   TF     Exec(TERMptr tp);
   TF     ExecStr(TERMptr tp, aCHAR* s);
   TF     Call(TERMptr tp);
   TF     CallStr(TERMptr tp, aCHAR* s);
   TF     Redo(void);
   TF     ClearCall(void);

// Asserting and retracting

   void   Asserta(TERM t);
   void   Assertz(TERM t);
   TF     Retract(TERM t);
   void   AssertaStr(aCHAR* s);
   void   AssertzStr(aCHAR* s);
   TF     RetractStr(aCHAR* s);

// String/Term conversion functions

   void   TermToStr(TERM t, aCHAR* s, int i);
   void   TermToStrQ(TERM t, aCHAR* s, int i);
   void   StrToTerm(TERMptr tp, aCHAR* s);

// Making Prolog types

   void   MakeAtom(TERMptr tp, aCHAR* s);
   void   MakeStr(TERMptr tp, aCHAR* s);
   void   MakeInt(TERMptr tp, intC i);
   void   MakeFloat(TERMptr tp, double f);
   void   MakeAddr(TERMptr tp, VOIDptr vp);

// Getting C values from Prolog terms

   pTYPE  GetTermType(TERM t);
   void   GetTerm(TERM t, cTYPE ct, VOIDptr vp);
   int    StrTermLen(TERM t);

// Structure hacking functions

   void   GetFA(TERM t, aCHAR* s, ARITYptr ap);
   void   MakeFA(TERMptr tp, aCHAR* s, ARITY a);
   TF     UnifyArg(TERMptr tp, int i, cTYPE ct, VOIDptr vp);
   void   GetArg(TERM t, int i, cTYPE ct, VOIDptr vp);
   pTYPE  GetArgType(TERM t, int i);
   int    StrArgLen(TERM t, int i);
   TF     Unify(TERM t1, TERM t2);

// List hacking functions

   void   MakeList(TERMptr tp);
   void   PushList(TERMptr tp, TERM t);
   RC     PopList(TERMptr tp, cTYPE ct, VOIDptr vp);
   RC     GetHead(TERM t, cTYPE ct, VOIDptr vp);
   TERM   GetTail(TERM t);

// Stream I/O functions

   void   SetStream(STREAM st, int i);
   int    GetStream(STREAM st);
   void   SetInput(pX_GETC fgetc, pX_UNGETC funget);
   void   SetOutput(pX_PUTC fputc, pX_PUTS fputs);
   void   SetIOArg(VOIDptr vp);
   int    OpenUserStream(aCHAR* alias, pUSER_GET_LINE ugl,
                         pUSER_PUT_STRING ups, VOIDptr vp);

// Miscellaneous functions

   void   GetVersion(aCHAR* s);
   void   SetCommandArgs(int pargc, aCHAR** pargv);

   void   ErrRaise(aCHAR* s);

// For Unicode build, here are single byte equivalents
#ifdef _UNICODE
   CLogicServer(char * xplname);
   void   Init(char * ininame);
   void   Init2(char * inistring);
   void   InitPreds(PRED_INITAptr preds);
   void   AddLSX(char* s, void * vp);
   void   AddLSX(char* s);
   void   AddPred(char* s, ARITY a, ExtPred f, VOIDptr v);
   void   Load(char *lsprog);
   void   LoadFromMemory(char *lsprog, int len, aBYTEptr code);
   TF     ExecStr(TERMptr tp, char* s);
   TF     CallStr(TERMptr tp, char* s);
   void   AssertaStr(char* s);
   void   AssertzStr(char* s);
   TF     RetractStr(char* s);
   void   TermToStr(TERM t, char* s, int i);
   void   TermToStrQ(TERM t, char* s, int i);
   void   StrToTerm(TERMptr tp, char* s);
   void   MakeAtom(TERMptr tp, char* s);
   void   MakeStr(TERMptr tp, char* s);
   void   GetFA(TERM t, char* s, ARITYptr ap);
   void   MakeFA(TERMptr tp, char* s, ARITY a);
   void   SetOutput(pX_PUTC fputc, pX_PUTSA fputs);
   int    OpenUserStream(char* alias, paUSER_GET_LINE ugl,
                         paUSER_PUT_STRING ups, VOIDptr vp);
   void   GetVersion(char* s);
   void   SetCommandArgs(int pargc, char** pargv);
   void   ErrRaise(char* s);

#endif
};

class CLSException
{
private:
   ENGid m_eng;

public:
   CLSException(ENGid e) { m_eng = e; }
   CLSException(const CLSException &e)
   {   m_eng = e.m_eng; }
   virtual ~CLSException() {};
   int GetRC();
   void GetMsg(aCHAR*, int);
   ExType GetType();
   void GetReadFileName(aCHAR*, int);
   void GetReadBuffer(aCHAR*, int);
   void GetCallStack(aCHAR*, int);
   intC GetReadLineno();
#ifdef _UNICODE
   void GetMsg(char*, int);
   void GetReadFileName(char*, int);
   void GetReadBuffer(char*, int);
   void GetCallStack(char*, int);
#endif // _UNICODE
};

/*----------------------------------------*/
/*  External CLogicServer implementation  */
/*                                        */

// A bug in GNU C++ causes segmentation faults when
// throwing complex objects as references, so for
// GNU we need to throw pointers instead.
#ifdef GNU
#define THROW_LSEXCEPTION throw new CLSException(m_eng)
#else
#define THROW_LSEXCEPTION throw CLSException(m_eng)
#endif


inline CLogicServer::CLogicServer()
{
   m_eng = NULL;
}

inline CLogicServer::CLogicServer(aCHAR* xplfile)
{
   Init(xplfile);
   Load(xplfile);
}

inline CLogicServer::~CLogicServer()
{
   //Close();
}

inline void CLogicServer::Init(aCHAR* ininame)
{
   RC rc = lsInit(&m_eng, ininame);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::Init2(aCHAR* inistring)
{
   RC rc = lsInit2(&m_eng, inistring);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::InitLSX(void * vp)
{
   RC rc = lsInitLSX(m_eng, vp);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::InitLSX()
{
   RC rc = lsInitLSX(m_eng, NULL);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::AddLSX(aCHAR* s, void * vp)
{
   RC rc = lsAddLSX(m_eng, s, vp);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::AddLSX(aCHAR* s)
{
   RC rc = lsAddLSX(m_eng, s, NULL);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::AddPred(aCHAR* s, ARITY a, ExtPred f, VOIDptr v)
{
   RC rc = lsAddPred(m_eng, s, a, f, v);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::InitPreds(PRED_INITptr preds)
{
   RC rc = lsInitPreds(m_eng, preds);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::Load(aCHAR* xplname)
{
   RC rc = lsLoad(m_eng, xplname);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::LoadFromMemory(aCHAR* xplname, int len, aBYTEptr code)
{
   RC rc = lsLoadFromMemory(m_eng, xplname, len, code);
   if (rc) THROW_LSEXCEPTION;
}

inline TF CLogicServer::Main(void)
{
   TF tf = lsMain(m_eng);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline void CLogicServer::Reset(void)
{
   RC rc = lsReset(m_eng);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::Close(void)
{
   RC rc = lsClose(m_eng);
   if (rc) THROW_LSEXCEPTION;
}


// function and predicate parameters

inline void CLogicServer::GetParm(int i, cTYPE ct, void * vp)
{
   RC rc = lsGetParm(m_eng, i, ct, vp);
   if (rc) THROW_LSEXCEPTION;
}

inline pTYPE CLogicServer::GetParmType(int i)
{
    pTYPE pT = lsGetParmType(m_eng, i);
   if (pT == pERR)
      THROW_LSEXCEPTION;
   return pT;
}

inline int CLogicServer::StrParmLen(int i)
{
   int len = lsStrParmLen(m_eng, i);
   if (len == NOTOK)
      THROW_LSEXCEPTION;
   return len;
}

inline TF CLogicServer::UnifyParm(int i, cTYPE ct, VOIDptr vp)
{
   TF tf = lsUnifyParm(m_eng, i, ct, vp);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}


// Calling Prolog from C

inline TF CLogicServer::Exec(TERMptr tp)
{
   TF tf = lsExec(m_eng, tp);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline TF CLogicServer::ExecStr(TERMptr tp, aCHAR* s)
{
   TF tf = lsExecStr(m_eng, tp, s);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline TF CLogicServer::Call(TERMptr tp)
{
   TF tf = lsCall(m_eng, tp);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline TF CLogicServer::CallStr(TERMptr tp, aCHAR* s)
{
   TF tf = lsCallStr(m_eng, tp, s);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline TF CLogicServer::Redo(void)
{
   TF tf = lsRedo(m_eng);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline TF CLogicServer::ClearCall(void)
{
   TF tf = lsClearCall(m_eng);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

// Asserting and retracting

inline void CLogicServer::Asserta(TERM t)
{
   RC rc = lsAsserta(m_eng, t);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::Assertz(TERM t)
{
   RC rc = lsAssertz(m_eng, t);
   if (rc) THROW_LSEXCEPTION;
}

inline TF CLogicServer::Retract(TERM t)
{
   TF tf = lsRetract(m_eng, t);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline void CLogicServer::AssertaStr(aCHAR* s)
{
   RC rc = lsAssertaStr(m_eng, s);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::AssertzStr(aCHAR* s)
{
   RC rc = lsAssertzStr(m_eng, s);
   if (rc) THROW_LSEXCEPTION;
}

inline TF CLogicServer::RetractStr(aCHAR* s)
{
   TF tf = lsRetractStr(m_eng, s);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}


// String/Term conversion functions

inline void CLogicServer::TermToStr(TERM t, aCHAR* s, int i)
{
   RC rc = lsTermToStr(m_eng, t, s, i);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::TermToStrQ(TERM t, aCHAR* s, int i)
{
   RC rc = lsTermToStrQ(m_eng, t, s, i);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::StrToTerm(TERMptr tp, aCHAR* s)
{
   RC rc = lsStrToTerm(m_eng, tp, s);
   if (rc) THROW_LSEXCEPTION;
}


// Making Prolog types

inline void CLogicServer::MakeAtom(TERMptr tp, aCHAR* s)
{
   RC rc = lsMakeAtom(m_eng, tp, s);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::MakeStr(TERMptr tp, aCHAR* s)
{
   RC rc = lsMakeStr(m_eng, tp, s);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::MakeInt(TERMptr tp, intC i)
{
   RC rc = lsMakeInt(m_eng, tp, i);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::MakeFloat(TERMptr tp, double f)
{
   RC rc = lsMakeFloat(m_eng, tp, f);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::MakeAddr(TERMptr tp, VOIDptr vp)
{
   RC rc = lsMakeAddr(m_eng, tp, vp);
   if (rc) THROW_LSEXCEPTION;
}


// Getting C values from Prolog terms

inline pTYPE CLogicServer::GetTermType(TERM t)
{
    pTYPE pT = lsGetTermType(m_eng, t);
   if (pT == pERR)
      THROW_LSEXCEPTION;
   return pT;
}

inline void CLogicServer::GetTerm(TERM t, cTYPE ct, VOIDptr vp)
{
   RC rc = lsGetTerm(m_eng, t, ct, vp);
   if (rc) THROW_LSEXCEPTION;
}

inline int CLogicServer::StrTermLen(TERM t)
{
   int len = lsStrTermLen(m_eng, t);
   if (len == NOTOK)
      THROW_LSEXCEPTION;
   return len;
}


// Structure hacking functions

inline void CLogicServer::GetFA(TERM t, aCHAR* s, ARITYptr ap)
{
   RC rc = lsGetFA(m_eng, t, s, ap);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::MakeFA(TERMptr tp, aCHAR* s, ARITY a)
{
   RC rc = lsMakeFA(m_eng, tp, s, a);
   if (rc) THROW_LSEXCEPTION;
}

inline TF CLogicServer::UnifyArg(TERMptr tp, int i, cTYPE ct, VOIDptr vp)
{
   TF tf = lsUnifyArg(m_eng, tp, i, ct, vp);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline void CLogicServer::GetArg(TERM t, int i, cTYPE ct, VOIDptr vp)
{
   RC rc = lsGetArg(m_eng, t, i, ct, vp);
   if (rc) THROW_LSEXCEPTION;
}

inline pTYPE CLogicServer::GetArgType(TERM t, int i)
{
    pTYPE pT = lsGetArgType(m_eng, t, i);
   if (pT == pERR)
      THROW_LSEXCEPTION;
   return pT;
}

inline int CLogicServer::StrArgLen(TERM t, int i)
{
    int len = lsStrArgLen(m_eng, t, i);
   if (len == NOTOK)
      THROW_LSEXCEPTION;
   return len;
}

inline TF CLogicServer::Unify(TERM t1, TERM t2)
{
   TF tf = lsUnify(m_eng, t1, t2);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}


// List hacking functions

inline void CLogicServer::MakeList(TERMptr tp)
{
   RC rc = lsMakeList(m_eng, tp);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::PushList(TERMptr tp, TERM t)
{
   RC rc = lsPushList(m_eng, tp, t);
   if (rc) THROW_LSEXCEPTION;
}

inline RC CLogicServer::PopList(TERMptr tp, cTYPE ct, VOIDptr vp)
{
   RC rc = lsPopList(m_eng, tp, ct, vp);
   if (rc != OK && rc != NOTOK)
      THROW_LSEXCEPTION;
   return rc;
}

inline RC CLogicServer::GetHead(TERM t, cTYPE ct, VOIDptr vp)
{
   RC rc = lsGetHead(m_eng, t, ct, vp);
   if (rc != OK && rc != NOTOK)
      THROW_LSEXCEPTION;
   return rc;
}

inline TERM CLogicServer::GetTail(TERM t)
{
   TERM tTail = lsGetTail(m_eng, t);
   //if (tTail == NULL)
   //   THROW_LSEXCEPTION;
   return tTail;
}


// Stream I/O functions

inline void CLogicServer::SetStream(STREAM st, int i)
{
   RC rc = lsSetStream(m_eng, st, i);
   if (rc) THROW_LSEXCEPTION;
}

inline int CLogicServer::GetStream(STREAM st)
{
   int iStr = lsGetStream(m_eng, st);
   if (iStr == NOTOK)
      THROW_LSEXCEPTION;
   return iStr;
}

inline void CLogicServer::SetInput(pX_GETC vfgetc, pX_UNGETC vfunget)
{
   RC rc = lsSetInput(m_eng, vfgetc, vfunget);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::SetOutput(pX_PUTC vfputc, pX_PUTS vfputs)
{
   RC rc = lsSetOutput(m_eng, vfputc, vfputs);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::SetIOArg(VOIDptr vp)
{
   RC rc = lsSetIOArg(m_eng, vp);
   if (rc) THROW_LSEXCEPTION;
}

inline int CLogicServer::OpenUserStream(aCHAR* alias,
      pUSER_GET_LINE fpUserGetLine, pUSER_PUT_STRING fpUserPutString,
      VOIDptr vp)
{
   int h = lsOpenUserStream(m_eng, alias, fpUserGetLine, fpUserPutString, vp);
   if (h == NOTOK) THROW_LSEXCEPTION;
   return h;
}

// Miscellaneous functions

inline void CLogicServer::GetVersion(aCHAR* s)
{
   RC rc = lsGetVersion(m_eng, s);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::SetCommandArgs(int pargc, aCHAR** pargv)
{
   RC rc = lsSetCommandArgs(m_eng, pargc, pargv);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::ErrRaise(aCHAR* s)
{
   lsErrRaise(m_eng, s);
   return;
}

// For Unicode build, here are single byte equivalents

#ifdef _UNICODE

inline CLogicServer::CLogicServer(char * ininame)
{
   RC rc = lsInitA(&m_eng, ininame);
   if (rc) THROW_LSEXCEPTION;
}

inline void   CLogicServer::Init(char * ininame)
{
   RC rc = lsInitA(&m_eng, ininame);
   if (rc) THROW_LSEXCEPTION;
}

inline void   CLogicServer::Init2(char * inistring)
{
   RC rc = lsInit2A(&m_eng, inistring);
   if (rc) THROW_LSEXCEPTION;
}

inline void   CLogicServer::AddLSX(char* s, void * vp)
{
   RC rc = lsAddLSXA(m_eng, s, vp);
   if (rc) THROW_LSEXCEPTION;
}

inline void   CLogicServer::AddLSX(char* s)
{
   RC rc = lsAddLSXA(m_eng, s, NULL);
   if (rc) THROW_LSEXCEPTION;
}

inline void   CLogicServer::AddPred(char* s, ARITY a, ExtPred f, VOIDptr v)
{
   RC rc = lsAddPredA(m_eng, s, a, f, v);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::InitPreds(PRED_INITAptr preds)
{
   RC rc = lsInitPredsA(m_eng, preds);
   if (rc) THROW_LSEXCEPTION;
}

inline void   CLogicServer::Load(char *xplname)
{
   RC rc = lsLoadA(m_eng, xplname);
   if (rc) THROW_LSEXCEPTION;
}

inline void   CLogicServer::LoadFromMemory(char *xplname, int len, aBYTEptr code)
{
   RC rc = lsLoadFromMemoryA(m_eng, xplname, len, code);
   if (rc) THROW_LSEXCEPTION;
}

inline TF     CLogicServer::ExecStr(TERMptr tp, char* s)
{
   TF tf = lsExecStrA(m_eng, tp, s);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline TF     CLogicServer::CallStr(TERMptr tp, char* s)
{
   TF tf = lsCallStrA(m_eng, tp, s);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline void   CLogicServer::AssertaStr(char* s)
{
   RC rc = lsAssertaStrA(m_eng, s);
   if (rc) THROW_LSEXCEPTION;
}

inline void   CLogicServer::AssertzStr(char* s)
{
   RC rc = lsAssertzStrA(m_eng, s);
   if (rc) THROW_LSEXCEPTION;
}

inline TF     CLogicServer::RetractStr(char* s)
{
   TF tf = lsRetractStrA(m_eng, s);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline void   CLogicServer::TermToStr(TERM t, char* s, int i)
{
   RC rc = lsTermToStrA(m_eng, t, s, i);
   if (rc) THROW_LSEXCEPTION;
}

inline void   CLogicServer::TermToStrQ(TERM t, char* s, int i)
{
   RC rc = lsTermToStrQA(m_eng, t, s, i);
   if (rc) THROW_LSEXCEPTION;
}

inline void   CLogicServer::StrToTerm(TERMptr tp, char* s)
{
   RC rc = lsStrToTermA(m_eng, tp, s);
   if (rc) THROW_LSEXCEPTION;
}

inline void   CLogicServer::MakeAtom(TERMptr tp, char* s)
{
   RC rc = lsMakeAtomA(m_eng, tp, s);
   if (rc) THROW_LSEXCEPTION;
}

inline void   CLogicServer::MakeStr(TERMptr tp, char* s)
{
   RC rc = lsMakeStrA(m_eng, tp, s);
   if (rc) THROW_LSEXCEPTION;
}

#define MAX_FUNCTOR 1000
inline void   CLogicServer::GetFA(TERM t, char* s, ARITYptr ap)
{
   RC rc = lsGetFAA(m_eng, t, s, ap);
   if (rc) THROW_LSEXCEPTION;
}

inline void   CLogicServer::MakeFA(TERMptr tp, char* s, ARITY a)
{
   RC rc = lsMakeFAA(m_eng, tp, s, a);
   if (rc) THROW_LSEXCEPTION;
}

inline void CLogicServer::SetOutput(pX_PUTC vfputc, pX_PUTSA vfputs)
{
   RC rc = lsSetOutputA(m_eng, vfputc, vfputs);
   if (rc) THROW_LSEXCEPTION;
}

inline int CLogicServer::OpenUserStream(char* alias,
         paUSER_GET_LINE fpUserGetLine, paUSER_PUT_STRING fpUserPutString,
         VOIDptr vp)
{
   int h = lsOpenUserStreamA(m_eng, alias, fpUserGetLine, fpUserPutString, vp);
   if (h == NOTOK) THROW_LSEXCEPTION;
   return h;
}

inline void   CLogicServer::GetVersion(char* s)
{
   RC rc = lsGetVersionA(m_eng, s);
   if (rc) THROW_LSEXCEPTION;
}

inline void  CLogicServer::SetCommandArgs(int pargc, char** pargv)
{
   RC rc = lsSetCommandArgsA(m_eng, pargc, pargv);
   if (rc) THROW_LSEXCEPTION;
}

inline void  CLogicServer::ErrRaise(char* s)
{
   lsErrRaiseA(m_eng, s);
   return;
}

#endif  //_UNICODE


/*--------------------------------*/
/*  CLSException implementation   */
/*                                */

inline int CLSException::GetRC()
{
   return lsGetExceptRC(m_eng);
}

inline void CLSException::GetMsg(aCHAR* msg, int len)
{
   lsGetExceptMsg(m_eng, msg, len);
}

inline ExType CLSException::GetType()
{
   return lsGetExceptType(m_eng);
}

inline void CLSException::GetReadFileName(aCHAR* name, int len)
{
   lsGetExceptReadFileName(m_eng, name, len);
}

inline void CLSException::GetReadBuffer(aCHAR* buf, int len)
{
   lsGetExceptReadBuffer(m_eng, buf, len);
}

inline void CLSException::GetCallStack(aCHAR* cs, int len)
{
   lsGetExceptCallStack(m_eng, cs, len);
}

inline intC CLSException::GetReadLineno()
{
   return lsGetExceptLineno(m_eng);
}

#ifdef _UNICODE
inline void CLSException::GetMsg(char* msg, int len)
{
   lsGetExceptMsgA(m_eng, msg, len);
}

inline void CLSException::GetReadFileName(char* name, int len)
{
   lsGetExceptReadFileNameA(m_eng, name, len);
}

inline void CLSException::GetReadBuffer(char* buf, int len)
{
   lsGetExceptReadBufferA(m_eng, buf, len);
}

inline void CLSException::GetCallStack(char* cs, int len)
{
   lsGetExceptCallStackA(m_eng, cs, len);
}

#endif // _UNICODE


#endif // __cplusplus

#endif // LOGIC_SERVER_DEFINED

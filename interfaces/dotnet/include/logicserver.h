/****************************************************************************

LogicServer.h - C and C++ Logic Server API Interface

(replaces amzi.h)

Copyright (c) 1992-2012 by Amzi! inc.  All Rights Reserved.

****************************************************************************/

#ifndef LOGIC_SERVER_DEFINED
#define LOGIC_SERVER_DEFINED
#include <stdlib.h>   // needed for NULL definition used below
#include <string>

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
#endif

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
#define WINDOWS
#define MSWIN
#endif

#if defined(WINDOWS)   // Windows

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

typedef   char  *    STRptr;
typedef   STRptr *    STRhnd;

typedef   void *      VOIDptr;
typedef   VOIDptr *   VOIDhnd;

/*   Amzi! integer sizes are often based on the size of a Prolog cell,
   which is the size of a pointer, which is 32-bits for most systems
   and 64-bits for others.  So, intC is a cell-sized integer, intCH
   a half cell, etc. */

#if defined(P64)

typedef long              intC;
typedef unsigned long    uintC;
typedef int              intCH;
typedef unsigned int    uintCH;
typedef short            intCQ;
typedef unsigned short  uintCQ;
typedef short            aINT16;
typedef unsigned short  UaINT16;
typedef int             aINT32;

#elif defined(P32)

typedef long              intC;
typedef unsigned long    uintC;
typedef short            intCH;
typedef unsigned short  uintCH;
typedef aSBYTE           intCQ;
typedef aBYTE           uintCQ;
typedef short            aINT16;
typedef unsigned short  UaINT16;
typedef long            aINT32;

#elif defined(P16)

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

//typedef uintC      CELL;
//typedef CELL *     CELLptr;

//typedef uintCptr   TERM;
typedef void* TERM;
typedef TERM *     TERMptr;


typedef intCH      PATOM;             /* Prolog Atom */
typedef PATOM *    PATOMptr;

typedef uintCH     ARITY;
typedef ARITY *    ARITYptr;

#ifdef BC  // Borland optimizes enums, so turn that feature off
#pragma option -b
#endif

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




#ifdef BC  // enums done, turn them back on, (Thanks to Jorge Pelizzoni)
#pragma option -b.
#endif

//typedef uintC   ENGid;
typedef void * ENGid;
typedef ENGid * ENGidptr;


/*   External function calling convention declarations */

#if defined(MSWIN) && defined(P16)
#define LSAPI __pascal
#define EXPFUNC __export __pascal
typedef TF (EXPFUNC *ExtPred)(VOIDptr);

#elif defined(MSWIN) && defined(P32)
#define LSAPI __stdcall
#define EXPFUNC __stdcall
typedef TF (EXPFUNC *ExtPred)(VOIDptr);

#elif defined(DOS)
#define LSAPI __pascal
#define EXPFUNC __pascal
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

typedef int  (lsCALLTYPE *pX_PUTSW)(VOIDptr, wchar_t*);
typedef wchar_t* (lsCALLTYPE *pwUSER_GET_LINE)(VOIDptr);
typedef void  (lsCALLTYPE *pwUSER_PUT_STRING)(VOIDptr, wchar_t*);

/*   Structure used to hold mappings between
   extended Prolog predicate names and the
   functions that implement them. */

typedef struct {
     wchar_t*   Pname;
     ARITY      Parity;
     ExtPred    Pfunc;
} PRED_INITW;
typedef PRED_INITW * PRED_INITWptr;

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

#ifdef __cplusplus   // ending the extern "C" from above
}
#endif

/* C++ Logic Server API (LSAPI) Class Interface */
/* -------------------------------------------- */

#ifdef __cplusplus

class LogicServer
{
private:
   ENGid    m_eng;

public:

// Constructors and Destructor

   LogicServer();
   LogicServer(std::string xplname);
   virtual ~LogicServer();

// Main entry points to set up Prolog environment

   void   AddLSX(std::string s, void * vp);
   void   AddLSXW(std::wstring s, void * vp);
   void   AddLSX(std::string s);
   void   AddLSXW(std::wstring s);
   void   AddPred(std::string s, ARITY a, ExtPred f, VOIDptr v);
   void   AddPredW(std::wstring s, ARITY a, ExtPred f, VOIDptr v);
   void   Close(void);
   void   Init(std::string ininame);
   void   InitW(std::wstring ininame);
   void   Init2(std::string inistring);
   void   Init2W(std::wstring inistring);
   void   InitLSX(void * vp);
   void   InitLSX();
   void   InitPreds(PRED_INITAptr preds);
   void   InitPredsW(PRED_INITWptr preds);
   void   Load(std::string lsprog);
   void   LoadW(std::wstring lsprog);
   void   LoadFromMemory(std::string lsprog, int length, aBYTEptr p);
   void   LoadFromMemoryW(std::wstring lsprog, int length, aBYTEptr p);
   TF     Main(void);
   void   Reset(void);

// function and predicate parameters

   double GetFloatParm(int i);
   int    GetIntParm(int i);
   TERM   GetParm(int i);
   std::string GetStrParm(int i);
   std::wstring GetStrParmW(int i);
   pTYPE  GetParmType(int i);
   int    StrParmLen(int i);
   TF     UnifyAtomParm(int i, std::string s);
   TF     UnifyAtomParmW(int i, std::wstring s);
   TF     UnifyFloatParm(int i, double f);
   TF     UnifyIntParm(int i, int val);
   TF     UnifyStrParm(int i, std::string s);
   TF     UnifyStrParmW(int i, std::wstring s);

// Calling Prolog from C

   TERM   Call(TERM t);
   TERM   CallStr(std::string s);
   TERM   CallStrW(std::wstring s);
   TF     ClearCall(void);
   TERM   Exec(TERM t);
   TERM   ExecStr(std::string s);
   TERM   ExecStrW(std::wstring s);
   TF     Redo(void);

// Asserting and retracting

   void   Asserta(TERM t);
   void   Assertz(TERM t);
   TF     Retract(TERM t);
   void   AssertaStr(std::string s);
   void   AssertaStrW(std::wstring s);
   void   AssertzStr(std::string s);
   void   AssertzStrW(std::wstring s);

// String/Term conversion functions

   int    StrTermLen(TERM t);
   TERM   StrToTerm(std::string s);
   TERM   StrToTermW(std::wstring s);
   std::string TermToStr(TERM t);
   std::wstring TermToStrW(TERM t);
   std::string TermToStrQ(TERM t);
   std::wstring TermToStrQW(TERM t);

// Making Prolog types

   TERM   MakeAtom(std::string s);
   TERM   MakeAtomW(std::wstring s);
   TERM   MakeFloat(double f);
   TERM   MakeInt(intC i);
   TERM   MakeStr(std::string s);
   TERM   MakeStrW(std::wstring s);

// Getting C values from Prolog terms

   double GetFloatTerm(TERM t);
   int    GetIntTerm(TERM t);
   std::string GetStrTerm(TERM t);
   std::wstring GetStrTermW(TERM t);
   pTYPE  GetTermType(TERM t);

// Structure hacking functions

   int    GetArity(TERM t);
   TERM   GetArg(TERM t, int i);
   pTYPE  GetArgType(TERM t, int i);
   double GetFloatArg(TERM t, int i);
   std::string GetFunctor(TERM t);
   std::wstring GetFunctorW(TERM t);
   int    GetIntArg(TERM t, int i);
   std::string GetStrArg(TERM t, int i);
   std::wstring GetStrArgW(TERM t, int i);
   TERM   MakeFA(std::string s, ARITY a);
   TERM   MakeFAW(std::wstring s, ARITY a);
   int    StrArgLen(TERM t, int i);
   TERM   UnifyAtomArg(TERM t, int i, std::string s);
   TERM   UnifyAtomArgW(TERM t, int i, std::wstring s);
   TERM   UnifyFloatArg(TERM t, int i, double f);
   TERM   UnifyIntArg(TERM t, int i, int v);
   TERM   UnifyStrArg(TERM t, int i, std::string s);
   TERM   UnifyStrArgW(TERM t, int i, std::wstring s);

// List hacking functions

   double GetFloatHead(TERM t);
   int    GetIntHead(TERM t);
   TERM   GetHead(TERM t);
   std::string GetStrHead(TERM t);
   std::wstring GetStrHeadW(TERM t);
   TERM   GetTail(TERM t);
   TERM   MakeList();
   TERM   PushList(TERM l, TERM t);

// Stream I/O functions

   int    GetStream(STREAM st);
   void   SetStream(STREAM st, int i);
   void   SetIOArg(VOIDptr vp);
   void   SetInput(pX_GETC fgetc, pX_UNGETC funget);
   void   SetOutput(pX_PUTC fputc, pX_PUTS fputs);
   int    OpenUserStream(std::string alias, paUSER_GET_LINE ugl,
                         paUSER_PUT_STRING ups, VOIDptr vp);
   int    OpenUserStreamW(std::wstring alias, pwUSER_GET_LINE ugl,
                         pwUSER_PUT_STRING ups, VOIDptr vp);

// Miscellaneous functions

   std::string GetVersion();
   std::wstring GetVersionW();

   void   ErrRaise(std::string s);
   void   ErrRaiseW(std::wstring s);

};

class LSException
{
private:
   ENGid m_eng;

public:
   LSException(ENGid e) { m_eng = e; }
   LSException(const LSException &e)
   {   m_eng = e.m_eng; }
   virtual ~LSException() {};
   int GetRC();
   std::string GetMsg();
   std::wstring GetMsgW();
   ExType GetType();
   std::string GetReadFileName();
   std::wstring GetReadFileNameW();
   std::string GetReadBuffer();
   std::wstring GetReadBufferW();
   std::string GetCallStack();
   std::wstring GetCallStackW();
   intC GetReadLineno();
};

/*----------------------------------------*/
/*  External LogicServer implementation  */
/*                                        */

// A bug in GNU C++ causes segmentation faults when
// throwing complex objects as references, so for
// GNU we need to throw pointers instead.
#ifdef GNU
#define THROW_LSEXCEPTION throw new LSException(m_eng)
#else
#define THROW_LSEXCEPTION throw LSException(m_eng)
#endif


inline LogicServer::LogicServer()
{
   m_eng = NULL;
}

inline LogicServer::LogicServer(std::string xplfile)
{
   Init(xplfile);
   Load(xplfile);
}

inline LogicServer::~LogicServer()
{
   //Close();
}

inline void LogicServer::AddLSX(std::string s, void * vp)
{
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   RC rc = lsAddLSXA(m_eng, c_str, vp);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::AddLSXW(std::wstring s, void * vp)
{
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   RC rc = lsAddLSXW(m_eng, c_str, vp);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::AddLSX(std::string s)
{
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   RC rc = lsAddLSXA(m_eng, c_str, NULL);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::AddLSXW(std::wstring s)
{
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   RC rc = lsAddLSXW(m_eng, c_str, NULL);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::AddPred(std::string s, ARITY a, ExtPred f, VOIDptr v)
{
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   RC rc = lsAddPredA(m_eng, c_str, a, f, v);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::AddPredW(std::wstring s, ARITY a, ExtPred f, VOIDptr v)
{
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   RC rc = lsAddPredW(m_eng, c_str, a, f, v);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::Close(void)
{
   RC rc = lsClose(m_eng);
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::Init(std::string ininame)
{
   char *c_str = new char[ininame.length()+1];
   strcpy(c_str, ininame.c_str());
   RC rc = lsInitA(&m_eng, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::InitW(std::wstring ininame)
{
   wchar_t *c_str = new wchar_t[ininame.length()+1];
   wcscpy(c_str, ininame.c_str());
   RC rc = lsInitW(&m_eng, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::Init2(std::string inistring)
{
   char *c_str = new char[inistring.length()+1];
   strcpy(c_str, inistring.c_str());
   RC rc = lsInit2A(&m_eng, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::Init2W(std::wstring inistring)
{
   wchar_t *c_str = new wchar_t[inistring.length()+1];
   wcscpy(c_str, inistring.c_str());
   RC rc = lsInit2W(&m_eng, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::InitLSX(void * vp)
{
   RC rc = lsInitLSX(m_eng, vp);
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::InitLSX()
{
   RC rc = lsInitLSX(m_eng, NULL);
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::InitPreds(PRED_INITAptr preds)
{
   RC rc = lsInitPredsA(m_eng, preds);
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::InitPredsW(PRED_INITWptr preds)
{
   RC rc = lsInitPredsW(m_eng, preds);
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::Load(std::string xplname)
{
   char *c_str = new char[xplname.length()+1];
   strcpy(c_str, xplname.c_str());
   RC rc = lsLoad(m_eng, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::LoadW(std::wstring xplname)
{
   wchar_t *c_str = new wchar_t[xplname.length()+1];
   wcscpy(c_str, xplname.c_str());
   RC rc = lsLoadW(m_eng, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::LoadFromMemory(std::string xplname, int length, aBYTEptr p)
{
   char *c_str = new char[xplname.length()+1];
   strcpy(c_str, xplname.c_str());
   RC rc = lsLoadFromMemory(m_eng, c_str, length, p);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::LoadFromMemoryW(std::wstring xplname, int length, aBYTEptr p)
{
   wchar_t *c_str = new wchar_t[xplname.length()+1];
   wcscpy(c_str, xplname.c_str());
   RC rc = lsLoadFromMemoryW(m_eng, c_str, length, p);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline TF LogicServer::Main(void)
{
   TF tf = lsMain(m_eng);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline void LogicServer::Reset(void)
{
   RC rc = lsReset(m_eng);
   if (rc) THROW_LSEXCEPTION;
}


// function and predicate parameters

inline double LogicServer::GetFloatParm(int i)
{
   double f;
   RC rc = lsGetParm(m_eng, i, cDOUBLE, &f);
   return f;
}

inline int LogicServer::GetIntParm(int i)
{
   int v;
   RC rc = lsGetParm(m_eng, i, cINT, &v);
   return v;
}

inline TERM LogicServer::GetParm(int i)
{
   TERM term;
   RC rc = lsGetParm(m_eng, i, cTERM, &term);
   return term;
}

inline std::string LogicServer::GetStrParm(int i)
{
   int len = lsStrParmLen(m_eng, i);
   if (len < 0) return NULL;
   char *c_str = new char[len+1];
   RC rc = lsGetParm(m_eng, i, cASTR, c_str);
   if (rc) {
      delete c_str;
      THROW_LSEXCEPTION;
   }
   std::string str(c_str);
   delete c_str;
   return str;
}

inline std::wstring LogicServer::GetStrParmW(int i)
{
   int len = lsStrParmLen(m_eng, i);
   if (len < 0) return NULL;
   wchar_t *c_str = new wchar_t[len+1];
   RC rc = lsGetParm(m_eng, i, cWSTR, c_str);
   if (rc) {
      delete c_str;
      THROW_LSEXCEPTION;
   }
   std::wstring str(c_str);
   delete c_str;
   return str;
}

inline pTYPE LogicServer::GetParmType(int i)
{
    pTYPE pT = lsGetParmType(m_eng, i);
   if (pT == pERR)
      THROW_LSEXCEPTION;
   return pT;
}

inline int LogicServer::StrParmLen(int i)
{
   int len = lsStrParmLen(m_eng, i);
   if (len == NOTOK)
      THROW_LSEXCEPTION;
   return len;
}

inline TF LogicServer::UnifyAtomParm(int i, std::string s)
{
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   TF tf = lsUnifyParm(m_eng, i, cAATOM, c_str);
   delete c_str;
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline TF LogicServer::UnifyAtomParmW(int i, std::wstring s)
{
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   TF tf = lsUnifyParm(m_eng, i, cWATOM, c_str);
   delete c_str;
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline TF LogicServer::UnifyFloatParm(int i, double f)
{
   TF tf = lsUnifyParm(m_eng, i, cDOUBLE, &f);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline TF LogicServer::UnifyIntParm(int i, int v)
{
   TF tf = lsUnifyParm(m_eng, i, cINT, &v);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline TF LogicServer::UnifyStrParm(int i, std::string s)
{
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   TF tf = lsUnifyParm(m_eng, i, cASTR, c_str);
   delete c_str;
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline TF LogicServer::UnifyStrParmW(int i, std::wstring s)
{
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   TF tf = lsUnifyParm(m_eng, i, cWSTR, c_str);
   delete c_str;
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}


// Calling Prolog from C

inline TERM LogicServer::Exec(TERM t)
{
   TERM term = t;
   TF tf = lsExec(m_eng, &term);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   if (tf == FALSE) return 0;
   else return term;
}

inline TERM LogicServer::ExecStr(std::string s)
{
   TERM term;
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   TF tf = lsExecStr(m_eng, &term, c_str);
   delete c_str;
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   if (tf == FALSE) return 0;
   else return term;
}

inline TERM LogicServer::ExecStrW(std::wstring s)
{
   TERM term;
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   TF tf = lsExecStrW(m_eng, &term, c_str);
   delete c_str;
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   if (tf == FALSE) return 0;
   else return term;
}

inline TERM LogicServer::Call(TERM t)
{
   TERM term = t;
   TF tf = lsCall(m_eng, &term);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   if (tf == FALSE) return 0;
   else return term;
}

inline TERM LogicServer::CallStr(std::string s)
{
   TERM term;
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   TF tf = lsCallStr(m_eng, &term, c_str);
   delete c_str;
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   if (tf == FALSE) return 0;
   else return term;
}

inline TERM LogicServer::CallStrW(std::wstring s)
{
   TERM term;
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   TF tf = lsCallStrW(m_eng, &term, c_str);
   delete c_str;
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   if (tf == FALSE) return 0;
   else return term;
}

inline TF LogicServer::Redo(void)
{
   TF tf = lsRedo(m_eng);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline TF LogicServer::ClearCall(void)
{
   TF tf = lsClearCall(m_eng);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}


// Asserting and retracting

inline void LogicServer::Asserta(TERM t)
{
   RC rc = lsAsserta(m_eng, t);
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::Assertz(TERM t)
{
   RC rc = lsAssertz(m_eng, t);
   if (rc) THROW_LSEXCEPTION;
}

inline TF LogicServer::Retract(TERM t)
{
   TF tf = lsRetract(m_eng, t);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   return tf;
}

inline void LogicServer::AssertaStr(std::string s)
{
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   RC rc = lsAssertaStrA(m_eng, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::AssertaStrW(std::wstring s)
{
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   RC rc = lsAssertaStrW(m_eng, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::AssertzStr(std::string s)
{
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   RC rc = lsAssertzStr(m_eng, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::AssertzStrW(std::wstring s)
{
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   RC rc = lsAssertzStrW(m_eng, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}


// String/Term conversion functions

inline int LogicServer::StrTermLen(TERM t)
{
   int len = lsStrTermLen(m_eng, t);
   if (len == NOTOK)
      THROW_LSEXCEPTION;
   return len;
}

inline TERM LogicServer::StrToTerm(std::string s)
{
   TERM term;
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   RC rc = lsStrToTermA(m_eng, &term, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline TERM LogicServer::StrToTermW(std::wstring s)
{
   TERM term;
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   RC rc = lsStrToTermW(m_eng, &term, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline std::string LogicServer::TermToStr(TERM t)
{
   int len = lsStrTermLen(m_eng, t);
   if (len < 0) return NULL;
   char *c_str = new char[len+1];
   RC rc = lsTermToStrA(m_eng, t, c_str, len);
   if (rc) {
      delete c_str;
      THROW_LSEXCEPTION;
   }
   std::string str(c_str);
   delete c_str;
   return str;
}

inline std::wstring LogicServer::TermToStrW(TERM t)
{
   int len = lsStrTermLen(m_eng, t);
   if (len < 0) return NULL;
   wchar_t *c_str = new wchar_t[len+1];
   RC rc = lsTermToStrW(m_eng, t, c_str, len);
   if (rc) {
      delete c_str;
      THROW_LSEXCEPTION;
   }
   std::wstring str(c_str);
   delete c_str;
   return str;
}

inline std::string LogicServer::TermToStrQ(TERM t)
{
   int len = lsStrTermLen(m_eng, t);
   if (len < 0) return NULL;
   char *c_str = new char[len+1];
   RC rc = lsTermToStr(m_eng, t, c_str, len);
   if (rc) {
      delete c_str;
      THROW_LSEXCEPTION;
   }
   std::string str(c_str);
   delete c_str;
   return str;
}

inline std::wstring LogicServer::TermToStrQW(TERM t)
{
   int len = lsStrTermLen(m_eng, t);
   if (len < 0) return NULL;
   wchar_t *c_str = new wchar_t[len+1];
   RC rc = lsTermToStrW(m_eng, t, c_str, len);
   if (rc) {
      delete c_str;
      THROW_LSEXCEPTION;
   }
   std::wstring str(c_str);
   delete c_str;
   return str;
}


// Making Prolog types

inline TERM LogicServer::MakeAtom(std::string s)
{
   TERM term;
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   RC rc = lsMakeAtomA(m_eng, &term, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
   return term;
}

inline TERM LogicServer::MakeAtomW(std::wstring s)
{
   TERM term;
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   RC rc = lsMakeAtomW(m_eng, &term, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
   return term;
}

inline TERM LogicServer::MakeInt(intC i)
{
   TERM term;
   RC rc = lsMakeInt(m_eng, &term, i);
   if (rc) THROW_LSEXCEPTION;
   return term;
}

inline TERM LogicServer::MakeFloat(double f)
{
   TERM term;
   RC rc = lsMakeFloat(m_eng, &term, f);
   if (rc) THROW_LSEXCEPTION;
   return term;
}

inline TERM LogicServer::MakeStr(std::string s)
{
   TERM term;
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   RC rc = lsMakeStrA(m_eng, &term, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
   return term;
}

inline TERM LogicServer::MakeStrW(std::wstring s)
{
   TERM term;
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   RC rc = lsMakeStrW(m_eng, &term, c_str);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
   return term;
}


// Getting C values from Prolog terms

inline double LogicServer::GetFloatTerm(TERM t)
{
   double f;
   RC rc = lsGetTerm(m_eng, t, cDOUBLE, &f);
   if (rc) THROW_LSEXCEPTION;
   return f;
}

inline int LogicServer::GetIntTerm(TERM t)
{
   int i;
   RC rc = lsGetTerm(m_eng, t, cINT, &i);
   if (rc) THROW_LSEXCEPTION;
   return i;
}

inline std::string LogicServer::GetStrTerm(TERM t)
{
   int len = lsStrTermLen(m_eng, t);
   if (len < 0) return NULL;
   char *c_str = new char[len+1];
   RC rc = lsGetTerm(m_eng, t, cASTR, c_str);
   if (rc) {
      delete c_str;
      THROW_LSEXCEPTION;
   }
   std::string str(c_str);
   delete c_str;
   return str;
}

inline std::wstring LogicServer::GetStrTermW(TERM t)
{
   int len = lsStrTermLen(m_eng, t);
   if (len < 0) return NULL;
   wchar_t *c_str = new wchar_t[len+1];
   RC rc = lsGetTerm(m_eng, t, cWSTR, c_str);
   if (rc) {
      delete c_str;
      THROW_LSEXCEPTION;
   }
   std::wstring str(c_str);
   delete c_str;
   return str;
}

inline pTYPE LogicServer::GetTermType(TERM t)
{
    pTYPE pT = lsGetTermType(m_eng, t);
   if (pT == pERR)
      THROW_LSEXCEPTION;
   return pT;
}


// Structure hacking functions

inline int LogicServer::GetArity(TERM t)
{
   char s[2048];
   ARITY a;
   RC rc = lsGetFA(m_eng, t, s, &a);
   if (rc) THROW_LSEXCEPTION;
   return a;
}

inline TERM LogicServer::GetArg(TERM t, int i)
{
   TERM term;
   RC rc = lsGetArg(m_eng, t, i, cTERM, &term);
   if (rc) THROW_LSEXCEPTION;
   return term;
}

inline pTYPE LogicServer::GetArgType(TERM t, int i)
{
    pTYPE pT = lsGetArgType(m_eng, t, i);
   if (pT == pERR)
      THROW_LSEXCEPTION;
   return pT;
}

inline double LogicServer::GetFloatArg(TERM t, int i)
{
   double d;
   RC rc = lsGetArg(m_eng, t, i, cDOUBLE, &d);
   if (rc) THROW_LSEXCEPTION;
   return d;
}

inline std::string LogicServer::GetFunctor(TERM t)
{
   char s[1024];
   ARITY a;
   RC rc = lsGetFAA(m_eng, t, s, &a);
   if (rc) THROW_LSEXCEPTION;
   std::string str(s);
   return str;
}

inline std::wstring LogicServer::GetFunctorW(TERM t)
{
   wchar_t s[1024];
   ARITY a;
   RC rc = lsGetFAW(m_eng, t, s, &a);
   if (rc) THROW_LSEXCEPTION;
   std::wstring str(s);
   return str;
}

inline int LogicServer::GetIntArg(TERM t, int i)
{
   int val;
   RC rc = lsGetArg(m_eng, t, i, cINT, &val);
   if (rc) THROW_LSEXCEPTION;
   return val;
}

inline std::string LogicServer::GetStrArg(TERM t, int i)
{
   int len = lsStrArgLen(m_eng, t, i);
   if (len < 0) return NULL;
   char *c_str = new char[len+1];
   RC rc = lsGetArg(m_eng, t, i, cASTR, c_str);
   if (rc) {
      delete c_str;
      THROW_LSEXCEPTION;
   }
   std::string str(c_str);
   delete c_str;
   return str;
}

inline std::wstring LogicServer::GetStrArgW(TERM t, int i)
{
   int len = lsStrArgLen(m_eng, t, i);
   if (len < 0) return NULL;
   wchar_t *c_str = new wchar_t[len+1];
   RC rc = lsGetArg(m_eng, t, i, cWSTR, c_str);
   if (rc) {
      delete c_str;
      THROW_LSEXCEPTION;
   }
   std::wstring str(c_str);
   delete c_str;
   return str;
}

inline TERM LogicServer::MakeFA(std::string s, ARITY a)
{
   TERM term;
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   RC rc = lsMakeFAA(m_eng, &term, c_str, a);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline TERM LogicServer::MakeFAW(std::wstring s, ARITY a)
{
   TERM term;
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   RC rc = lsMakeFAW(m_eng, &term, c_str, a);
   delete c_str;
   if (rc) THROW_LSEXCEPTION;
}

inline int LogicServer::StrArgLen(TERM t, int i)
{
   int len = lsStrArgLen(m_eng, t, i);
   if (len == NOTOK)
      THROW_LSEXCEPTION;
   return len;
}

inline TERM LogicServer::UnifyAtomArg(TERM t, int i, std::string s)
{
   TERM term = t;
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   TF tf = lsUnifyArg(m_eng, &term, i, cAATOM, c_str);
   delete c_str;
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   if (tf == FALSE) term = 0;
   return term;
}

inline TERM LogicServer::UnifyAtomArgW(TERM t, int i, std::wstring s)
{
   TERM term = t;
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   TF tf = lsUnifyArg(m_eng, &term, i, cWATOM, c_str);
   delete c_str;
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   if (tf == FALSE) term = 0;
   return term;
}

inline TERM LogicServer::UnifyFloatArg(TERM t, int i, double f)
{
   TERM term = t;
   TF tf = lsUnifyArg(m_eng, &term, i, cDOUBLE, &f);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   if (tf == FALSE) term = 0;
   return term;
}

inline TERM LogicServer::UnifyIntArg(TERM t, int i, int v)
{
   TERM term = t;
   TF tf = lsUnifyArg(m_eng, &term, i, cINT, &v);
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   if (tf == FALSE) term = 0;
   return term;
}

inline TERM LogicServer::UnifyStrArg(TERM t, int i, std::string s)
{
   TERM term = t;
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   TF tf = lsUnifyArg(m_eng, &term, i, cASTR, c_str);
   delete c_str;
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   if (tf == FALSE) term = 0;
   return term;
}

inline TERM LogicServer::UnifyStrArgW(TERM t, int i, std::wstring s)
{
   TERM term = t;
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   TF tf = lsUnifyArg(m_eng, &term, i, cWSTR, c_str);
   delete c_str;
   if (tf != TRUE && tf != FALSE)
      THROW_LSEXCEPTION;
   if (tf == FALSE) term = 0;
   return term;
}

// List hacking functions

inline double LogicServer::GetFloatHead(TERM t)
{
   double f;
   RC rc = lsGetHead(m_eng, t, cDOUBLE, &f);
   if (rc != OK)
      THROW_LSEXCEPTION;
   return f;
}

inline int LogicServer::GetIntHead(TERM t)
{
   int i;
   RC rc = lsGetHead(m_eng, t, cINT, &i);
   if (rc != OK)
      THROW_LSEXCEPTION;
   return i;
}

inline TERM LogicServer::GetHead(TERM t)
{
   TERM term;
   RC rc = lsGetHead(m_eng, t, cTERM, &term);
   if (rc != OK)
      THROW_LSEXCEPTION;
   return term;
}

inline std::string LogicServer::GetStrHead(TERM t)
{
   TERM term;
   RC rc = lsGetHead(m_eng, t, cTERM, &term);
   if (rc != OK)
      THROW_LSEXCEPTION;
   int len = lsStrTermLen(m_eng, term);
   if (len < 0) return NULL;
   char *c_str = new char[len+1];
   rc = lsGetHead(m_eng, t, cASTR, c_str);
   if (rc != OK) {
      delete c_str;
      THROW_LSEXCEPTION;
   }
   std::string str(c_str);
   delete c_str;
   return str;
}

inline std::wstring LogicServer::GetStrHeadW(TERM t)
{
   TERM term;
   RC rc = lsGetHead(m_eng, t, cTERM, &term);
   if (rc != OK)
      THROW_LSEXCEPTION;
   int len = lsStrTermLen(m_eng, term);
   if (len < 0) return NULL;
   wchar_t *c_str = new wchar_t[len+1];
   rc = lsGetHead(m_eng, t, cWSTR, c_str);
   if (rc != OK) {
      delete c_str;
      THROW_LSEXCEPTION;
   }
   std::wstring str(c_str);
   delete c_str;
   return str;
}

inline TERM LogicServer::GetTail(TERM t)
{
   TERM tTail = lsGetTail(m_eng, t);
   //if (tTail == NULL)
   //   THROW_LSEXCEPTION;
   return tTail;
}

inline TERM LogicServer::MakeList()
{
   TERM term;
   RC rc = lsMakeList(m_eng, &term);
   if (rc) THROW_LSEXCEPTION;
   return term;
}

inline TERM LogicServer::PushList(TERM l, TERM t)
{
   TERM term = l;
   RC rc = lsPushList(m_eng, &term, t);
   if (rc) THROW_LSEXCEPTION;
   return term;
}


// Stream I/O functions

inline void LogicServer::SetStream(STREAM st, int i)
{
   RC rc = lsSetStream(m_eng, st, i);
   if (rc) THROW_LSEXCEPTION;
}

inline int LogicServer::GetStream(STREAM st)
{
   int iStr = lsGetStream(m_eng, st);
   if (iStr == NOTOK)
      THROW_LSEXCEPTION;
   return iStr;
}

inline void LogicServer::SetInput(pX_GETC vfgetc, pX_UNGETC vfunget)
{
   RC rc = lsSetInput(m_eng, vfgetc, vfunget);
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::SetOutput(pX_PUTC vfputc, pX_PUTS vfputs)
{
   RC rc = lsSetOutput(m_eng, vfputc, vfputs);
   if (rc) THROW_LSEXCEPTION;
}

inline void LogicServer::SetIOArg(VOIDptr vp)
{
   RC rc = lsSetIOArg(m_eng, vp);
   if (rc) THROW_LSEXCEPTION;
}

inline int LogicServer::OpenUserStream(std::string alias,
      paUSER_GET_LINE fpUserGetLine, paUSER_PUT_STRING fpUserPutString,
      VOIDptr vp)
{
   char *c_str = new char[alias.length()+1];
   strcpy(c_str, alias.c_str());
   int h = lsOpenUserStreamA(m_eng, c_str, fpUserGetLine, fpUserPutString, vp);
   delete c_str;
   if (h == NOTOK) THROW_LSEXCEPTION;
   return h;
}

inline int LogicServer::OpenUserStreamW(std::wstring alias,
      pwUSER_GET_LINE fpUserGetLine, pwUSER_PUT_STRING fpUserPutString,
      VOIDptr vp)
{
   wchar_t *c_str = new wchar_t[alias.length()+1];
   wcscpy(c_str, alias.c_str());
   int h = lsOpenUserStreamW(m_eng, c_str, fpUserGetLine, fpUserPutString, vp);
   delete c_str;
   if (h == NOTOK) THROW_LSEXCEPTION;
   return h;
}

// Miscellaneous functions

inline std::string LogicServer::GetVersion()
{
   char version[1024];
   RC rc = lsGetVersion(m_eng, version);
   if (rc) THROW_LSEXCEPTION;
   std::string str(version);
   return str;
}

inline std::wstring LogicServer::GetVersionW()
{
   wchar_t version[1024];
   RC rc = lsGetVersionW(m_eng, version);
   if (rc) THROW_LSEXCEPTION;
   std::wstring str(version);
   return str;
}

inline void LogicServer::ErrRaise(std::string s)
{
   char *c_str = new char[s.length()+1];
   strcpy(c_str, s.c_str());
   lsErrRaiseA(m_eng, c_str);
   delete c_str;
   return;
}

inline void LogicServer::ErrRaiseW(std::wstring s)
{
   wchar_t *c_str = new wchar_t[s.length()+1];
   wcscpy(c_str, s.c_str());
   lsErrRaiseW(m_eng, c_str);
   delete c_str;
   return;
}


/*--------------------------------*/
/*  LSException implementation    */
/*                                */

inline int LSException::GetRC()
{
   return lsGetExceptRC(m_eng);
}

inline std::string LSException::GetMsg()
{
   int len = 1024;
   char *msg = new char[len+1];
   lsGetExceptMsgA(m_eng, msg, len);
   std::string c_str(msg);
   delete msg;
   return c_str;
}

inline std::wstring LSException::GetMsgW()
{
   int len = 1024;
   wchar_t *msg = new wchar_t[len+1];
   lsGetExceptMsgW(m_eng, msg, len);
   std::wstring c_str(msg);
   delete msg;
   return c_str;
}

inline ExType LSException::GetType()
{
   return lsGetExceptType(m_eng);
}

inline std::string LSException::GetReadFileName()
{
   int len = 1024;
   char *name = new char[len+1];
   lsGetExceptReadFileNameA(m_eng, name, len);
   std::string c_str(name);
   delete name;
   return c_str;
}

inline std::wstring LSException::GetReadFileNameW()
{
   int len = 1024;
   wchar_t *name = new wchar_t[len+1];
   lsGetExceptReadFileNameW(m_eng, name, len);
   std::wstring c_str(name);
   delete name;
   return c_str;

}

inline std::string LSException::GetReadBuffer()
{
   int len = 65536;
   char *buf = new char[len+1];
   lsGetExceptReadBufferA(m_eng, buf, len);
   std::string c_str(buf);
   delete buf;
   return c_str;
}

inline std::wstring LSException::GetReadBufferW()
{
   int len = 65536;
   wchar_t *buf = new wchar_t[len+1];
   lsGetExceptReadBufferW(m_eng, buf, len);
   std::wstring c_str(buf);
   delete buf;
   return c_str;
}

inline std::string LSException::GetCallStack()
{
   int len = 65536;
   char *cs = new char[len+1];
   lsGetExceptCallStackA(m_eng, cs, len);
   std::string c_str(cs);
   delete cs;
   return c_str;
}

inline std::wstring LSException::GetCallStackW()
{
   int len = 65536;
   wchar_t *cs = new wchar_t[len+1];
   lsGetExceptCallStackW(m_eng, cs, len);
   std::wstring c_str(cs);
   delete cs;
   return c_str;
}

inline intC LSException::GetReadLineno()
{
   return lsGetExceptLineno(m_eng);
}

#endif /* __cplusplus */

#endif /* LOGIC_SERVER_DEFINED */

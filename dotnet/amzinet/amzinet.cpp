// This is the main DLL file.

#using <mscorlib.dll>
#using <System.dll>
#using <System.EnterpriseServices.dll>

using namespace System;
using namespace System::IO;
using namespace System::Text;

using System::Runtime::InteropServices::Marshal;

[assembly:CLSCompliant(true)]

#include "stdio.h"
#include "stdlib.h"

#include "stdafx.h"
#include "malloc.h"
#include "amzi.h"
#include "amzinet.h"
#include "DExtPred.h"
#include "LSException.h"

// An integer that can hold a term, which is a pointer
// all the 'long's, which are only 32-bits even in 64-bit mode,
// are changed to INTTERMs
// dcm  2016-08-16
#ifdef _M_AMD64
#define INTTERM __int64
#else
#define INTTERM int
#endif

//#define  DEBUG

// To use: MessageBox(0, "LSException", "catch", 0);
//typedef void* HWND;
//[DllImport("user32", CharSet=CharSet::Ansi)]
//extern "C" int MessageBox(HWND hWnd, String ^ pText, String ^ pCaption, unsigned int uType);

TF EXPFUNC p_dotnet(void* p)
// AddPred will add as its predicate:
//		AddPred("predname", arity, &::p_dotnet, ptr) to the DExtPred block;
{
   Boolean tf;
   DExtPred* ep = (DExtPred*)p;
   
   tf = ep->dptr->Invoke(ep->that);

   return tf;
}


public ref class LogicServer
{
private:
   void * m_eng;
   DExtPred *EpList;

public:
   LogicServer()
   {
      m_eng = NULL;
      EpList = NULL;
   }
   ~LogicServer()
   {
      DExtPred *e1 = EpList;
      DExtPred *e2;
      while(e1)
      {
         e2 = e1->pnext;
         delete e1;
         e1 = e2;
      }
      if (m_eng != NULL)
         lsClose(m_eng);
   }
 
/*   // Implement IDisposable*
   void Dispose()
   {
      DExtPred *e1 = EpList;
      DExtPred *e2;
      while(e1)
      {
         e2 = e1->pnext;
         delete e1;
         e1 = e2;
      }
      EpList = NULL;
      if (m_eng != NULL)
         lsClose(m_eng);
      m_eng = NULL;

      // Take object off the finalization queue 
      // and prevent finalization code for this object
      // from executing a second time.
      GC::SuppressFinalize(this);
   }
*/
   virtual String ^ToString() override
   {
      String ^str;
      long l;

      l = (long)m_eng;
      str = l.ToString(); 
      return str;
   }


	// Main entry points to set up Prolog environment

   /// <summary>Initializes the Prolog environment</summary>
   /// <param name="inifile">Name of .cfg file for stacks, etc.</param>
   /// <seealso cref="String">
   void Init(String ^ inifile) //Initialize the Prolog engine
	{
      void * eng;
      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(inifile).ToPointer());
      RC rc = lsInit(&eng, pStr);
      m_eng = eng;
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (rc) throw gcnew LSException(m_eng);
   }

   //Initialize the Prolog environment from a string instead of a .cfg file
   void Init2(String ^ inistring)
	{
      void * eng;
      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(inistring).ToPointer());
      RC rc = lsInit2(&eng, pStr);
      m_eng = eng;
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (rc) throw gcnew LSException(m_eng);
   }

   void InitLSX()
   {
      RC rc = lsInitLSX(m_eng, NULL);
      if (rc) throw gcnew LSException(m_eng);
   }

   void AddLSX(String ^s)
	{
      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(s).ToPointer());
      RC rc = lsAddLSX(m_eng, pStr, NULL);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (rc) throw gcnew LSException(m_eng);
   }

   void AddPred(String ^name, int arity, PrologPredicate ^f)
   {
      DExtPred *ep;

      ep = new DExtPred();
      ep->dptr = f;
      ep->that = this;
      ep->pnext = EpList;
      EpList = ep;

      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(name).ToPointer());
      RC rc = lsAddPred(m_eng, pStr, arity, &::p_dotnet, ep);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (rc) throw gcnew LSException(m_eng);
   }

   void Load(String ^xplfile)
	{
      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(xplfile).ToPointer());
      RC rc = lsLoad(m_eng, pStr);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (rc) throw gcnew LSException(m_eng);
   }

   TF Main()
   {
      TF tf = lsMain(m_eng);
      if (tf != TRUE && tf != FALSE)
         throw gcnew LSException(m_eng);
      return tf;
   }

   void Reset()
   {
      RC rc = lsReset(m_eng);
      if (rc) throw gcnew LSException(m_eng);
   }

   void Close()
   {
      RC rc = lsClose(m_eng);
      if (rc) throw gcnew LSException(m_eng);
      m_eng = NULL;
   }

 	// Calling Prolog from .NET

   INTTERM Exec(INTTERM term)
   {
      TERM tterm = (TERM)term;
      TF tf = lsExec(m_eng, &tterm);
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      if (tf) return (long)tterm;
      else return FALSE;
   }

   INTTERM ExecStr(String ^s)
   {
      //void  *term;
	  TERM term;
      
      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(s).ToPointer());
      TF tf = lsExecStr(m_eng, &term, pStr);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      if (tf) return (INTTERM)term;
      else return (INTTERM)FALSE;
   }

   INTTERM Call(INTTERM term)
   {
      TERM tterm = (TERM)term;
      TF tf = lsCall(m_eng, &tterm);
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      if (tf) return (INTTERM)tterm;
      else return FALSE;
   }

   INTTERM CallStr(String ^s)
   {
      void  *term;

      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(s).ToPointer());
      TF tf = lsCallStr(m_eng, &term, pStr);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      if (tf) return (INTTERM)term;
      else return FALSE;
   }

   TF Redo(void)
   {
      TF tf = lsRedo(m_eng);
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      return tf;
   }

   TF ClearCall(void)
   {
      TF tf = lsClearCall(m_eng);
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      return tf;
   }

   // Asserting and retracting

   void Asserta(INTTERM term)
   {
      RC rc = lsAsserta(m_eng, (TERM)term);
      if (rc) throw gcnew LSException(m_eng);
   }

	void Assertz(INTTERM term) 
   {
      RC rc = lsAssertz(m_eng, (TERM)term);
      if (rc) throw gcnew LSException(m_eng);
   }
   
   TF Retract(INTTERM term) 
   {
      TF tf = lsRetract(m_eng, (TERM)term);
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      return tf;
   }
   
   void AssertaStr(String ^s) 
   {
      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(s).ToPointer());
      RC rc = lsAssertaStr(m_eng, pStr);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (rc) throw gcnew LSException(m_eng);
   }

   void AssertzStr(String ^s) 
   {
      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(s).ToPointer());
      RC rc = lsAssertzStr(m_eng, pStr);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (rc) throw gcnew LSException(m_eng);
   }
   
   TF RetractStr(String ^s) 
   {
      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(s).ToPointer());
      TF tf = lsRetractStr(m_eng, pStr);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      return tf;
   }

   // String/Term conversion functions

   String ^TermToStr(INTTERM term, int len)
   {
      char  *s;
      String ^str;

      s = (char *)malloc(len+1);
      RC rc = lsTermToStr(m_eng, (TERM)term, s, len);
      if (rc) 
      {
         free((void *)s);
         throw gcnew LSException(m_eng);
      }
      str = gcnew String(s);
      free((void *)s);
      return str;
   }

   String ^TermToStrQ(INTTERM term, int len)
   {
      char  *s;
      String ^str;

      s = (char *)malloc(len+1);
      RC rc = lsTermToStrQ(m_eng, (TERM)term, s, len);
      if (rc) 
      {
         free((void *)s);
         throw gcnew LSException(m_eng);
      }
      str = gcnew String(s);
      free((void *)s);
      return str;
   }

   INTTERM StrToTerm(String ^s)
   {
      TERM term;

      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(s).ToPointer());
      RC rc = lsStrToTerm(m_eng, &term, pStr);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (rc) throw gcnew LSException(m_eng);
      return (INTTERM)term;
   }

   int StrTermLen(INTTERM term)
   {
      int len = lsStrTermLen(m_eng, (TERM)term);
      if (len == NOTOK) throw gcnew LSException(m_eng);
      return len;
   }

	// Making Prolog types

	INTTERM MakeAtom(String ^atomstr) 
   {
      TERM term;
      
      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(atomstr).ToPointer());
      RC rc = lsMakeAtom(m_eng, &term, pStr);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (rc) throw gcnew LSException(m_eng);
      return (INTTERM)term;
   }

   INTTERM MakeStr(String ^str) 
   {
      TERM term;

      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(str).ToPointer());
      RC rc = lsMakeStr(m_eng, &term, pStr);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (rc) throw gcnew LSException(m_eng);
      return (INTTERM)term;
   }

	INTTERM MakeInt(int num) 
   {
      TERM term;

      RC rc = lsMakeInt(m_eng, &term, num);
      if (rc) throw gcnew LSException(m_eng);
      return (INTTERM)term;
   }

	INTTERM MakeFloat(float num) 
   {
      TERM term;

      RC rc = lsMakeFloat(m_eng, &term, num);
      if (rc) throw gcnew LSException(m_eng);
      return (INTTERM)term;
   }

	// Getting C++ values from Prolog terms

	int GetTermType(INTTERM term)
   {
      int pT = lsGetTermType(m_eng, (TERM)term);
      if (pT == pERR) throw gcnew LSException(m_eng);
      return pT;
   }

	String ^GetStrTerm(INTTERM term)
   {
      int len;
      char  *s;
      String ^str;

      len = lsStrTermLen(m_eng, (TERM)term);
      s = (char *)malloc(len+1);
      RC rc = lsGetTerm(m_eng, (TERM)term, cSTR, s);
      if (rc)
      {
         free((void *)s);
         throw gcnew LSException(m_eng);
      }
      str = gcnew String(s);
      free((void *)s);
      return str;
   }

	int GetIntTerm(INTTERM term)
   {
      int i;

      RC rc = lsGetTerm(m_eng, (TERM)term, cINT, &i);
      if (rc) throw gcnew LSException(m_eng);
      return i;
   }

	float GetFloatTerm(INTTERM term)
   {
      float f;

      RC rc = lsGetTerm(m_eng, (TERM)term, cFLOAT, &f);
      if (rc) throw gcnew LSException(m_eng);
      return f;
   }

   // Get Parameters for Extended Predicates 

   INTTERM GetParm(int num)
   {
      TERM term;
      RC rc = lsGetParm(m_eng, num, cTERM, &term);
      if (rc) throw gcnew LSException(m_eng);
      return (INTTERM)term;
   }

   String ^GetStrParm(int num)
   {
      int len;
      char  *s;
      String ^str;

      len = lsStrParmLen(m_eng, num);
      s = (char *)malloc(len+1);
      RC rc = lsGetParm(m_eng, num, cSTR, s);
      if (rc) 
      {
         free((void *)s);
         throw gcnew LSException(m_eng);
      }
      str = gcnew String(s);
      free((void *)s);
      return str;
   }

   int GetIntParm(int num)
   {
      int i2;

      RC rc = lsGetParm(m_eng, num, cINT, &i2);
      if (rc) throw gcnew LSException(m_eng);
      return i2;
   }

   float GetFloatParm(int num)
   {
      float f;

      RC rc = lsGetParm(m_eng, num, cFLOAT, &f);
      if (rc) throw gcnew LSException(m_eng);
      return f;
   }

   int GetParmType(int num)
   {
      int pT = lsGetParmType(m_eng, num);
      if (pT == pERR) throw gcnew LSException(m_eng);
      return pT;
   }

   int StrParmLen(int num)
   {
      int len = lsStrParmLen(m_eng, num);
      if (len == NOTOK) throw gcnew LSException(m_eng);
      return len;
   }
   
   TF UnifyParm(int num, INTTERM term)
   {
      TERM tterm;

      tterm = (TERM)term;
      TF tf = lsUnifyParm(m_eng, num, cTERM, &tterm);
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      return tf;
   }

   TF UnifyStrParm(int num, String ^s)
   {
      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(s).ToPointer());
      TF tf = lsUnifyParm(m_eng, num, cSTR, pStr);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      return tf;
   }

   TF UnifyAtomParm(int num, String ^s)
   {
      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(s).ToPointer());
      TF tf = lsUnifyParm(m_eng, num, cATOM, pStr);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      return tf;
   }

   TF UnifyIntParm(int num, int i)
   {
      TF tf = lsUnifyParm(m_eng, num, cINT, &i);
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      return tf;
   }

   TF UnifyFloatParm(int num, float f)
   {
      TF tf = lsUnifyParm(m_eng, num, cFLOAT, &f);
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      return tf;
   }

   int GetArgType(INTTERM term, int num)
   {
      int pT = lsGetArgType(m_eng, (TERM)term, num);
      if (pT == pERR) throw gcnew LSException(m_eng);
      return pT;
   }

   int StrArgLen(INTTERM term, int num)
   {
      int len = lsStrArgLen(m_eng, (TERM)term, num);
      if (len == NOTOK) throw gcnew LSException(m_eng);
      return len;
   }

	// Structure hacking functions

	String ^GetFunctor(INTTERM term)
   {
//      int len;
      ARITY arity;
      char  *s;
      String ^str;

//         len = cls->StrTermLen((TERM)term);
//         s = (char ^)malloc(len+1);
         // Work around a bug in StrTermLen that can't handle a term
      s = (char *)malloc(32000);
      RC rc = lsGetFA(m_eng, (TERM)term, s, &arity);
      if (rc) 
      {
         free((void *)s);
         throw gcnew LSException(m_eng);
      }
      str = gcnew String(s);
      free((void *)s);
      return str;
   }

	int GetArity(INTTERM term)
   {
//      int len;
      ARITY arity;
      char  *s;

//         len = cls->StrTermLen((TERM)term);
//         s = (char ^)malloc(len+1);
         // Work around a bug in StrTermLen that can't handle a term
      s = (char *)malloc(32000);
      RC rc = lsGetFA(m_eng, (TERM)term, s, &arity);
      free((void *)s);
      if (rc) throw gcnew LSException(m_eng);
      return arity;
   }

	INTTERM MakeFA(String ^functor, int arity)
   {
      TERM term;

      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(functor).ToPointer());
      RC rc = lsMakeFA(m_eng, &term, pStr, arity);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      return (long)term;
   }

	INTTERM GetArg(INTTERM term, int num)
   {
      TERM argterm, tterm;

      tterm = (TERM)term;
      RC rc = lsGetArg(m_eng, tterm, num, cTERM, &argterm);
      if (rc) throw gcnew LSException(m_eng);
      return (INTTERM)argterm;
   }

	String ^GetStrArg(INTTERM term, int num)
   {
      char  *s;
      String ^str;

      int len = lsStrArgLen(m_eng, (TERM)term, num);
      if (len == NOTOK) throw gcnew LSException(m_eng);

      s = (char *)malloc(len+1);
      RC rc = lsGetArg(m_eng, (TERM)term, num, cSTR, s);
      if (rc) 
      {
         free((void *)s);
         throw gcnew LSException(m_eng);
      }
      str = gcnew String(s);
      free((void *)s);
      return str;
   }

	int GetIntArg(INTTERM term, int num)
   {
      int i;

      RC rc = lsGetArg(m_eng, (TERM)term, num, cINT, &i);
      if (rc) throw gcnew LSException(m_eng);
      return i;
   }

	float GetFloatArg(INTTERM term, int num)
   {
      float f;

      RC rc = lsGetArg(m_eng, (TERM)term, num, cFLOAT, &f);
      if (rc) throw gcnew LSException(m_eng);
      return f;
   }

	INTTERM UnifyAtomArg(INTTERM term, int num, String ^str)
   {
      TERM tterm;

      tterm = (TERM)term;
      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(str).ToPointer());
      TF tf = lsUnifyArg(m_eng, &tterm, num, cATOM, pStr);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      return (INTTERM)tterm;
   }

	INTTERM UnifyStrArg(INTTERM term, int num, String ^str)
   {
      TERM tterm;

      tterm = (TERM)term;
      char * pStr = static_cast<char*>(Marshal::StringToHGlobalAnsi(str).ToPointer());
      TF tf = lsUnifyArg(m_eng, &tterm, num, cSTR, pStr);
      Marshal::FreeHGlobal(static_cast<IntPtr>(const_cast<void*>(static_cast<const void*>(pStr))));
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      return (INTTERM)tterm;
   }

	INTTERM UnifyIntArg(INTTERM term, int num, int val)
   {
      TERM tterm;

      tterm = (TERM)term;
      TF tf = lsUnifyArg(m_eng, &tterm, num, cINT, &val);
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      return (INTTERM)tterm;
   }

	INTTERM UnifyFloatArg(INTTERM term, int num, float val)
   {
      TERM tterm;

      tterm = (TERM)term;
      TF tf = lsUnifyArg(m_eng, &tterm, num, cFLOAT, &val);
      if (tf != TRUE && tf != FALSE) throw gcnew LSException(m_eng);
      return (INTTERM)tterm;
   }

	// List hacking functions

	INTTERM MakeList()
   {
      TERM term;

      RC rc = lsMakeList(m_eng, &term);
      if (rc) throw gcnew LSException(m_eng);
      return (INTTERM)term;
   }

	INTTERM PushList(INTTERM listterm, INTTERM term)
   {
      TERM tlistterm;

      tlistterm = (TERM)listterm;
      RC rc = lsPushList(m_eng, &tlistterm, (TERM)term);
      if (rc) throw gcnew LSException(m_eng);
      return (INTTERM)tlistterm;
   }

	INTTERM GetHead(INTTERM listterm)
   {
      TERM term;

      RC rc = lsGetHead(m_eng, (TERM)listterm, cTERM, &term);
      if (rc != OK && rc != NOTOK) throw gcnew LSException(m_eng);
      return (INTTERM)term;
   }

	String ^GetStrHead(INTTERM listterm)
   {
      TERM term;
      int len;
      char *s;
      String ^str;

      RC rc = lsGetHead(m_eng, (TERM)listterm, cTERM, &term);
      if (rc) throw gcnew LSException(m_eng);
      len = lsStrTermLen(m_eng, term);
      s = (char *)malloc(len+1);
      rc = lsGetHead(m_eng, (TERM)listterm, cSTR, s);
      if (rc)
      {
         free((void *)s);
         throw gcnew LSException(m_eng);
      }
      str = gcnew String(s);
      free((void *)s);
      return str;
   }

	int GetIntHead(INTTERM listterm)
   {
      int i;

      RC rc = lsGetHead(m_eng, (TERM)listterm, cINT, &i);
      if (rc) throw gcnew LSException(m_eng);
      return i;
   }

	float GetFloatHead(INTTERM listterm)
   {
      float f;

      RC rc = lsGetHead(m_eng, (TERM)listterm, cFLOAT, &f);
      if (rc) throw gcnew LSException(m_eng);
      return f;
   }

	INTTERM GetTail(INTTERM listterm)
   {
      TERM term = lsGetTail(m_eng, (TERM)listterm);
      return (INTTERM)term;
   }

	// Miscellaneous functions

	String ^GetVersion()
   {
      char s[100];
      RC rc = lsGetVersion(m_eng, s);
      if (rc) throw gcnew LSException(m_eng);
      return gcnew String(s);
   }

#ifdef DEBUG
   void Log(String* input)
   {
      FileStream* fs = gcnew FileStream("c:\\InetPub\\wwwroot\\KW\\temp\\amzinet.log", FileMode::OpenOrCreate, FileAccess::ReadWrite);

      StreamWriter* w = gcnew StreamWriter(fs);
      w->BaseStream->Seek(0, SeekOrigin::End);

      w->Write("{0} {1} \r\n", DateTime::Now.ToLongTimeString(),
        DateTime::Now.ToLongDateString());

      w->Write(String::Concat(input , "\r\n\r\n"));

      w->Flush();
      w->Close();
   }
#endif

};



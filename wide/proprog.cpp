//------------------------------------------------------------
// proprog.cpp - Prolog program object
//

#include "stdafx.h"
#include "cpwide.h"
#include "mainfrm.h"
#include "proprog.h"
#include "amzi.h"
#include "conview.h"
#include "cpwin.h"
#include "resource.h"
#include "editdoc.h"
#include "peditvw.h"
#include "resource.h"
#include "amziexcept.h"

//extern PRED_INIT winPreds[];   // Extended windows predicates

//------------------------------------------------------------
// Redirect Prolog I/O to view window
//

CConView *g_pConView;      // The view associated with current program


/*  Need this code for non-unicode ide calling unicode engine
void EXPFUNC my_puts(VOIDptr vp, wchar_t* ws)
{
   int len = wcslen(ws) + 1;
   char* s = new char[len];
   wcstombs(s, ws, len);
   //g_pConView->PutS(s);
   ((CConView*)vp)->PutS(s);
   delete[] s;
}
*/

void EXPFUNC my_puts(VOIDptr vp, aCHAR* s)
{
   ((CConView*)vp)->PutS(s);
}

void EXPFUNC my_putc(VOIDptr vp, unsigned int c)
{
   //g_pConView->PutC(c);
   ((CConView*)vp)->PutC(c);
}

int EXPFUNC my_getc(VOIDptr vp)
{
   int   c;
   if (theApp.m_pMFW->Debugging())
   {
      CFrameWnd* pPF;
      pPF = ((CConView*)vp)->GetParentFrame();
      pPF->ActivateFrame();
      pPF->SetActiveView(((CConView*)vp));
   }
   
   c = (int) ((CConView*)vp)->GetC();

   return(c);
}

void EXPFUNC my_ungetc(VOIDptr vp, unsigned int c)
{
   ((CConView*)vp)->UnGetC();
}

TF EXPFUNC p_keyb(VOIDptr vp)
{
   return ((CProProg*)vp)->p_keyb();
}

//------------------------------------------------------------
// Implementation of Prolog program class
//

CProProg::CProProg()
{
}

CProProg::~CProProg()
{
}

BOOL CProProg::Initialize(const _TCHAR* pszPathName)
{
   _TCHAR name[80];
    
   _tcscpy(name, pszPathName);

   try {

   Init(name);
   InitLSX(&theApp);
   AddPred(_T("keyb"), 1, &::p_keyb, this);
   Load(name);

   } catch (CLSException &E)
   {
      Error(E);
      //Close();
      return FALSE;
   }

   return TRUE;
}

TF CProProg::p_keyb()
{
   int   a;
   a = (int)(m_pConView->GetCh());
   if (! UnifyParm(1, cINT, &a)) return FALSE;
   return TRUE;
}

void CProProg::AttachView(CConView* pCV)
{
   m_pConView = pCV;

   try {

   SetStream(CUR_OUT, 3);
   SetStream(USER_OUT, 3);
   SetStream(CUR_IN, 3);
   SetStream(USER_IN, 3);
   SetStream(CUR_ERR, 3);
   SetStream(USER_ERR, 3);
   SetOutput((pX_PUTC)my_putc, (pX_PUTS)my_puts);
   SetInput((pX_GETC)my_getc, (pX_UNGETC)my_ungetc);
   SetIOArg(m_pConView);

   } catch (CLSException &E)
   {
      Error(E);
   }
}

BOOL CProProg::Run()
{
   TF tf;

   try {

   g_pConView = m_pConView;
   tf = Main();
   m_pConView->FlushBuf();

   } catch (CLSException &E)
   {
      Error(E);
   }
   //if (tf == ABORT_ERR)
   //{
   //   ErrMsg(msg);
   //   sprintf(buf, "Prolog Error\n%d: %s", tf, msg);
   //   AfxMessageBox(buf);
   //   Close();
   //}
   return TRUE;
}

//BOOL CProProg::Close()
//{
//   CLogicServer::Close();      // avoid endless recursion
//   return TRUE;
//}

int CProProg::Error(CLSException &E)
{
   CAmziExcept *ae = new CAmziExcept;
   return ae->SetException(E);
}

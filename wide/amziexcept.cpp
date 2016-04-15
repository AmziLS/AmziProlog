// AmziExcept.cpp : implementation file
//

#include "stdafx.h"
#include "cpwide.h"
#include "resource.h"
#include "amzi.h"
#include "AmziExcept.h"
#include "editdoc.h"
#include "mainfrm.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CAmziExcept dialog


CAmziExcept::CAmziExcept(CWnd* pParent /*=NULL*/)
   : CDialog(CAmziExcept::IDD, pParent)
{
   //{{AFX_DATA_INIT(CAmziExcept)
      // NOTE: the ClassWizard will add member initialization here
   //}}AFX_DATA_INIT
   Create(IDD, pParent);
   CenterWindow();
   CListBox *pLB = (CListBox*)GetDlgItem(IDC_E_CALLSTACK);
   pLB->SetFont( &(theApp.m_pMFW->m_font) );
   // Need to do this to get horizontal scroll bars
   pLB->SetHorizontalExtent(1024);
   pLB->ShowWindow(SW_HIDE);
   CEdit *pE = (CEdit*)GetDlgItem(IDC_E_READBUF);
   pE->SetFont( &(theApp.m_pMFW->m_font) );
   pE->ShowWindow(SW_HIDE);
   CStatic *pS = (CStatic*)GetDlgItem(IDC_ES_CALLSTACK);
   pS->ShowWindow(SW_HIDE);
   pS = (CStatic*)GetDlgItem(IDC_ES_READBUF);
   pS->ShowWindow(SW_HIDE);

   ShowWindow(SW_SHOWNORMAL);
}


void CAmziExcept::DoDataExchange(CDataExchange* pDX)
{
   CDialog::DoDataExchange(pDX);
   //{{AFX_DATA_MAP(CAmziExcept)
      // NOTE: the ClassWizard will add DDX and DDV calls here
   //}}AFX_DATA_MAP
}

//-------------------------------------
// Member functions
//


int CAmziExcept::SetException(CLSException &E)
{
   _TCHAR* msg = new _TCHAR[MAX_MSG+1];
   _TCHAR* errmsg = new _TCHAR[MAX_ERR_MSG+1];
   _TCHAR* readfilename = new _TCHAR[MAX_ERR_FILENAME+1];
   _TCHAR* readbuffer = new _TCHAR[MAX_ERR_READBUF+1];
   _TCHAR* buf = new _TCHAR[MAX_MSG+1];
   _TCHAR* callstack = new _TCHAR[MAX_ERR_CALLSTACK+1];
   long   lineno;
   CEditDoc* pDoc = NULL;

   ExType x = E.GetType();
   RC rc = E.GetRC();
   E.GetMsg(errmsg, MAX_ERR_MSG);

   // Set up the main message area.
   switch(x)
   {
   case BADENG:
      Lsprintf(msg, _T("Bad Engine Error: %d\n%s"), rc, errmsg);
      break;
   case ABORT:
      Lsprintf(msg, _T("Abort Error: %d  Logic Server shut down.\n%s"), rc, errmsg);
      break;
   case INTERNAL:
      Lsprintf(msg, _T("Internal Error: %d  Logic Server shut down.\n%s")
         _T("\nNotify technical support."), rc, errmsg);
      break;
   case FATAL:
      Lsprintf(msg, _T("Fatal Error: %d  Logic Server reset.\n%s"), rc, errmsg);
      break;
   case INIT:
      Lsprintf(msg, _T("Initialization Error: %d\n%s"), rc, errmsg);
      break;
   case API:
      Lsprintf(msg, _T("LSAPI Error: %d\n%s"), rc, errmsg);
      break;
   case LOAD:
      Lsprintf(msg, _T("Load Error: %d\n%s"), rc, errmsg);
      break;
   case EXEC:
      Lsprintf(msg, _T("Execution Error: %d\n%s"), rc, errmsg);
      break;
   case SECURITY:
      Lsprintf(msg, _T("Activation Error: %d\n%s"), rc, errmsg);
      break;
   case ARITH:
      Lsprintf(msg, _T("Math Error: %d\n%s"), rc, errmsg);
      break;
   case READ:
      Lsprintf(msg, _T("Read Error: %d\n%s"), rc, errmsg);
      E.GetReadFileName(readfilename, MAX_ERR_FILENAME);
      if (_tcslen(readfilename) > 0)
      {
         lineno = E.GetReadLineno();
         Lsprintf(buf, _T("\nLine(%d) File: %s\n"),
            lineno, readfilename);
         _tcscat(msg, buf);
      }
      break;
   default:
      Lsprintf(msg, _T("Unknown Error: %d\n%s"), rc, errmsg);
   }

   SetMessage(msg);

   // If a read error, set up the read buffer edit control.
   if (x == READ)
   {
      E.GetReadBuffer(readbuffer, MAX_ERR_READBUF);
      SetReadBuffer(readbuffer);
   }
   else
   // Display the call stack, if there is one.
   {
      //HideReadBuffer();
      E.GetCallStack(callstack, MAX_ERR_CALLSTACK);
      if (_tcslen(callstack) > 0)
      {
         SetCallStack(callstack);
      }
   }

   // If a read error, open the edit window with the error line highlighed.
   if (x == READ && lineno > 0)
   {
      pDoc = theApp.WOpenEdit(readfilename);
      if (pDoc)
         pDoc->HighlightLine(lineno);
   }

   // Clean up various character buffers
   delete[] msg;
   delete[] errmsg;
   delete[] readfilename;
   delete[] readbuffer;
   delete[] buf;
   delete[] callstack;

   // Tell caller if we had a serious problem.
   if (x == INTERNAL || x == ABORT)
      return -1;
   else
      return 0;
}


void CAmziExcept::SetMessage(_TCHAR* sMsg)
{
   CStatic *pM = (CStatic*)GetDlgItem(IDC_E_MESSAGE);
   pM->SetWindowText(sMsg);
}

void CAmziExcept::SetReadBuffer(_TCHAR* sRB)
{
   CEdit *pE = (CEdit*)GetDlgItem(IDC_E_READBUF);
   pE->SetWindowText(sRB);
   pE->ShowWindow(SW_SHOW);
   CStatic *pS = (CStatic*)GetDlgItem(IDC_ES_READBUF);
   pS->ShowWindow(SW_SHOW);
}

void CAmziExcept::SetCallStack(_TCHAR* sCS)
{
   CListBox *pLB = (CListBox*)GetDlgItem(IDC_E_CALLSTACK);
   _TCHAR* sLine = _tcstok(sCS, _T(";"));
   while (sLine != NULL)
   {
      pLB->AddString(sLine);
      sLine = _tcstok(NULL, _T(";"));
   }
   pLB->ShowWindow(SW_SHOW);
   CStatic *pS = (CStatic*)GetDlgItem(IDC_ES_CALLSTACK);
   pS->ShowWindow(SW_SHOW);
}

void CAmziExcept::HideReadBuffer()
{
   CStatic *pT = (CStatic*)GetDlgItem(IDC_ES_READBUF);
   CEdit *pE = (CEdit*)GetDlgItem(IDC_E_READBUF);
   pT->ShowWindow(SW_HIDE);
   pE->ShowWindow(SW_HIDE);
}


BEGIN_MESSAGE_MAP(CAmziExcept, CDialog)
   //{{AFX_MSG_MAP(CAmziExcept)
      // NOTE: the ClassWizard will add message map macros here
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CAmziExcept message handlers


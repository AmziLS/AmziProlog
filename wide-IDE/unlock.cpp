// Unlock.cpp : implementation file
//

#include "stdafx.h"
#include "resource.h"
#include "cpwide.h"
#include "mainfrm.h"
#include "Unlock.h"
#include "amzi.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CUnlock dialog


CUnlock::CUnlock(CWnd* pParent /*=NULL*/)
   : CDialog(CUnlock::IDD, pParent)
{
   //{{AFX_DATA_INIT(CUnlock)
   m_username = _T("");
   m_organization = _T("");
   m_serialno = _T("");
   m_unlockcode = _T("");
   //}}AFX_DATA_INIT

   m_bWaiting = FALSE;
}

void CUnlock::DoDataExchange(CDataExchange* pDX)
{
   CDialog::DoDataExchange(pDX);
   //{{AFX_DATA_MAP(CUnlock)
   DDX_Text(pDX, IDC_USERNAME, m_username);
   DDV_MaxChars(pDX, m_username, 30);
   DDX_Text(pDX, IDC_ORGANIZATION, m_organization);
   DDV_MaxChars(pDX, m_organization, 30);
   DDX_Text(pDX, IDC_SERIALNO, m_serialno);
   DDV_MaxChars(pDX, m_serialno, 23);
   DDX_Text(pDX, IDC_UNLOCKCODE, m_unlockcode);
   DDV_MaxChars(pDX, m_unlockcode, 19);
   //}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CUnlock, CDialog)
   //{{AFX_MSG_MAP(CUnlock)
   ON_WM_SETCURSOR()
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CUnlock message handlers

BOOL CUnlock::OnInitDialog() 
{
   m_username = theApp.GetProfileString(_T("Registration"), _T("UserName"));
   m_organization = theApp.GetProfileString(_T("Registration"), _T("Organization"));
   m_serialno = theApp.GetProfileString(_T("Registration"), _T("SerialNo"));
   m_unlockcode = theApp.GetProfileString(_T("Registration"), _T("UnlockCode"));

   CDialog::OnInitDialog();
   
   return TRUE;  // return TRUE unless you set the focus to a control
                 // EXCEPTION: OCX Property Pages should return FALSE
}

// Button is labeled 'Unlock'
void CUnlock::OnOK()
{
   UpdateData();

   theApp.WriteProfileString(_T("Registration"), _T("UserName"), m_username);
   theApp.WriteProfileString(_T("Registration"), _T("Organization"), m_organization);
   theApp.WriteProfileString(_T("Registration"), _T("SerialNo"), m_serialno);
   theApp.WriteProfileString(_T("Registration"), _T("UnlockCode"), m_unlockcode);

   CLogicServer *ls = new CLogicServer;
   _TCHAR *buf = new _TCHAR[513];
   //_TCHAR ver[5];
   TERM t;
   TF tf;

   //theApp.m_pMFW->WaitingOn();
   BeginWaitCursor();
   m_bWaiting = TRUE;

   try{

   ls->Init(_T(""));
   ls->Load(_T("unlock.xpl"));
   _stprintf(buf, _T("ide_unlock($%s$, $%s$, $%s$, $%s$, X)"),
         m_username, m_organization, m_serialno, m_unlockcode);
   tf = ls->ExecStr(&t, buf);
   if (tf != TRUE)
   {
      AfxMessageBox(_T("unlock failed, check each item carefully"));
   }
   else
   {
      ls->GetArg(t, 5, cSTR, buf);
      if (0 == _tcscmp(buf, _T("ok")))
      {
         theApp.m_pMFW->m_bUnlocked = TRUE;
         AfxMessageBox(_T("Successfully Unlocked"));
      }
      else
      {
         AfxMessageBox(buf);
      }
   }

   } catch(CLSException &E)
   {
      theApp.m_pMFW->WaitingOff();
      int rc = E.GetRC();
      E.GetMsg(buf, 256);
      //CAmziExcept *ae = new CAmziExcept;
      //ae->SetException(E);
      AfxMessageBox(buf);
   }

done:
   //theApp.m_pMFW->WaitingOff();
   m_bWaiting = FALSE;
   EndWaitCursor();
   delete buf;
   delete ls;

   //if (tf == TRUE)
   //   CDialog::OnOK();
   DestroyWindow();
}


BOOL CUnlock::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message) 
{
   if (m_bWaiting)
   {
      RestoreWaitCursor();
      return TRUE;
   }
   else
      return CDialog::OnSetCursor(pWnd, nHitTest, message);
}

void CUnlock::OnCancel() 
{
   DestroyWindow();
}

void CUnlock::PostNcDestroy() 
{
   // TODO: Add your specialized code here and/or call the base class
   
   CDialog::PostNcDestroy();
   delete this;
}

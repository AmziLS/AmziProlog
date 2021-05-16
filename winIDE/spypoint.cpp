// spypoint.cpp : implementation file
//

#include "stdafx.h"
#include "resource.h"
#include "cpwide.h"
#include "spypoint.h"
#include "proprog.h"
#include "conview.h"
#include "listen.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSpypoints dialog


CSpypoints::CSpypoints(CWnd* pParent /*=NULL*/)
   : CDialog(CSpypoints::IDD, pParent)
{
   //{{AFX_DATA_INIT(CSpypoints)
   //}}AFX_DATA_INIT
}

BOOL CSpypoints::OnInitDialog()
{
   CenterWindow();
   CDialog::OnInitDialog();

   CenterWindow();
   TERM t, tlist, tfa;
   TF tf;
   _TCHAR buf[120];

// Populate the predicate list box
// Use get_preds/1 to get a list of predicates in the form
//   F/A, and then convert to strings for the list box
   m_plbPred = (CListBox*)GetDlgItem(IDC_PREDICATES);

   theApp.m_pListen->SilentCall(_T("set_debug"), _T("off"));
   try {

   tf = theApp.m_pListen->ExecStr(&t, _T("get_preds(_X)"));
   theApp.m_pListen->GetArg(t, 1, cTERM, &tlist);
   while (OK == theApp.m_pListen->PopList(&tlist, cTERM, &tfa))
   {
      theApp.m_pListen->TermToStr(tfa, buf, 80);
      m_plbPred->AddString(buf);
   }

// Populate the spypoints list box with the same technique
//   using the spypoint/1 predicate.
   m_plbSpy = (CListBox*)GetDlgItem(IDC_SPYPOINTS);
   tf = theApp.m_pListen->ExecStr(&t, _T("setof(_X, amzi_debugger:spypoint(_X), _L)"));
   theApp.m_pListen->GetArg(t, 3, cTERM, &tlist);
   while (OK == theApp.m_pListen->PopList(&tlist, cTERM, &tfa))
   {
      theApp.m_pListen->TermToStr(tfa, buf, 80);
      m_plbSpy->AddString(buf);
   }
   theApp.m_pListen->SilentCall(_T("set_debug_wide"), _T("on"));

   } catch (CLSException &E)
   {
      theApp.m_pListen->Error(E);
      //Close();
      return FALSE;
   }
   
   return TRUE;  // return TRUE  unless you set the focus to a control
}

void CSpypoints::DoDataExchange(CDataExchange* pDX)
{
   CDialog::DoDataExchange(pDX);
   //{{AFX_DATA_MAP(CSpypoints)
   //}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CSpypoints, CDialog)
   //{{AFX_MSG_MAP(CSpypoints)
   ON_BN_CLICKED(IDC_SPY_CLEAR, OnSpyClear)
   ON_BN_CLICKED(IDC_SPY_DELETE, OnSpyDelete)
   ON_BN_CLICKED(IDC_SPY_ADD, OnSpyAdd)
   ON_LBN_DBLCLK(IDC_PREDICATES, OnDblclkPredicates)
   ON_LBN_DBLCLK(IDC_SPYPOINTS, OnDblclkSpypoints)
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CSpypoints message handlers

void CSpypoints::OnSpyAdd()
{
   _TCHAR buf[80], pred[80];
   int i;
   TERM t;
   
   i = m_plbPred->GetCurSel();
   m_plbPred->GetText(i, pred);
   Lsprintf(buf, _T("amzi_debugger:spy(%s)"), pred);

   try {
      theApp.m_pListen->SilentCall(_T("set_debug"), _T("off"));
      theApp.m_pListen->ExecStr(&t, buf);
      theApp.m_pListen->SilentCall(_T("set_debug_wide"), _T("on"));
   } catch (CLSException &E)
   {   theApp.m_pListen->Error(E); }

   m_plbSpy->AddString(pred);
   return;
}

void CSpypoints::OnSpyClear()
{
   TERM t;
   
   try {
      theApp.m_pListen->SilentCall(_T("set_debug"), _T("off"));
      theApp.m_pListen->ExecStr(&t, _T("amzi_debugger:nospyall"));
      theApp.m_pListen->SilentCall(_T("set_debug_wide"), _T("on"));
   } catch (CLSException &E)
   {   theApp.m_pListen->Error(E); }

   m_plbSpy->ResetContent();
   return;
}

void CSpypoints::OnSpyDelete()
{
   int i;
   _TCHAR buf[80], spy[80];
   TERM t;
   
   i = m_plbSpy->GetCurSel();
   m_plbSpy->GetText(i, spy);
   Lsprintf(buf, _T("amzi_debugger:nospy(%s)"), spy);

   try {
      theApp.m_pListen->SilentCall(_T("set_debug"), _T("off"));
      theApp.m_pListen->ExecStr(&t, buf);
      theApp.m_pListen->SilentCall(_T("set_debug"), _T("on"));
   } catch (CLSException &E)
   {   theApp.m_pListen->Error(E); }

   m_plbSpy->DeleteString(i);
   return;
}

void CSpypoints::OnCancel()
{
   CDialog::OnCancel();
}

void CSpypoints::OnOK()
{
   CDialog::OnOK();
}

void CSpypoints::OnDblclkPredicates()
{
   OnSpyAdd();
}

void CSpypoints::OnDblclkSpypoints()
{
   OnSpyDelete();
}


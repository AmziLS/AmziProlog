// debug.cpp - implementation of debugger frame and view
//

#include "stdafx.h"
#include "resource.h"
#include "cpwide.h"
#include "mainfrm.h"
#include "debug.h"
#include "proprog.h"
#include "conview.h"
#include "listen.h"
#include "spypoint.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CDebugFrame

IMPLEMENT_DYNCREATE(CDebugFrame, CMDIChildWnd)

CDebugFrame::CDebugFrame()
{
}

CDebugFrame::~CDebugFrame()
{
}

BOOL CDebugFrame::PreCreateWindow(CREATESTRUCT &cs)
{
   WINDOWPLACEMENT wp;
   if (theApp.ReadWindowPlacement(&wp, _T("Settings"), _T("DebugWindow")))
   {
      cs.x = wp.rcNormalPosition.left;
      cs.y = wp.rcNormalPosition.top;
      cs.cx = wp.rcNormalPosition.right - cs.x;
      cs.cy = wp.rcNormalPosition.bottom - cs.y;
   }

   return(CMDIChildWnd::PreCreateWindow(cs));
}

BOOL CDebugFrame::DestroyWindow()
{
   // before it is destroyed, save the position of the window

   WINDOWPLACEMENT wp;
   wp.length = sizeof wp;
   if (GetWindowPlacement(&wp))
   {
      wp.flags = 0;
      if (IsZoomed())
         wp.flags |= WPF_RESTORETOMAXIMIZED;
      // and write it to the .INI file
      theApp.WriteWindowPlacement(&wp, _T("Settings"), _T("DebugWindow"));
   }
   return(CMDIChildWnd::DestroyWindow());
}

BEGIN_MESSAGE_MAP(CDebugFrame, CMDIChildWnd)
   //{{AFX_MSG_MAP(CDebugFrame)
   ON_WM_CLOSE()
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CDebugFrame message handlers

void CDebugFrame::OnClose() 
{
   if (g_pDebug->m_bResp == FALSE)
      AfxMessageBox(_T("Stop debugging before closing"));
   else
   {
      theApp.m_pMFW->DebuggerOff();
      theApp.m_pListenView->NoDebug();
      CMDIChildWnd::OnClose();
      g_pDebug = NULL;
   }
}

/////////////////////////////////////////////////////////////////////////////
// CDebugView

IMPLEMENT_DYNCREATE(CDebugView, CFormView)

CDebugView::CDebugView()
   : CFormView(CDebugView::IDD)
{
   m_iLB = 0;
   //{{AFX_DATA_INIT(CDebugView)
   m_bCall = TRUE;
   m_bExit = TRUE;
   m_bFail = TRUE;
   m_bRedo = TRUE;
   m_iFormat = 0;
   //}}AFX_DATA_INIT
   m_bFormat = FALSE;
}

CDebugView::~CDebugView()
{
}

void CDebugView::DoDataExchange(CDataExchange* pDX)
{
   CFormView::DoDataExchange(pDX);
   //{{AFX_DATA_MAP(CDebugView)
   DDX_Control(pDX, IDC_TRACE, m_LB);
   DDX_Control(pDX, IDC_CREEP, m_Creep);
   DDX_Check(pDX, IDC_LEASH_CALL, m_bCall);
   DDX_Check(pDX, IDC_LEASH_EXIT, m_bExit);
   DDX_Check(pDX, IDC_LEASH_FAIL, m_bFail);
   DDX_Check(pDX, IDC_LEASH_REDO, m_bRedo);
   DDX_Radio(pDX, IDC_FOFF, m_iFormat);
   //}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CDebugView, CFormView)
   //{{AFX_MSG_MAP(CDebugView)
   ON_BN_CLICKED(IDC_CREEP, OnCreep)
   ON_BN_CLICKED(IDC_FAIL, OnFail)
   ON_BN_CLICKED(IDC_LEAP, OnLeap)
   ON_BN_CLICKED(IDC_SKIP, OnSkip)
   ON_WM_SIZE()
   ON_WM_CLOSE()
   ON_BN_CLICKED(IDC_LEASH_CALL, OnLeashCall)
   ON_BN_CLICKED(IDC_LEASH_EXIT, OnLeashExit)
   ON_BN_CLICKED(IDC_LEASH_FAIL, OnLeashFail)
   ON_BN_CLICKED(IDC_LEASH_REDO, OnLeashRedo)
   ON_BN_CLICKED(IDC_STOP, OnStop)
   ON_BN_CLICKED(IDC_SPY, OnSpy)
   ON_LBN_SELCHANGE(IDC_TRACE, OnSelchangeTrace)
   ON_WM_CREATE()
   ON_BN_CLICKED(IDC_FON, OnFon)
   ON_BN_CLICKED(IDC_FOFF, OnFoff)
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()

//----------------------------------------------------
// Display options
//

void CDebugView::PutS(_TCHAR * s)
{
   CEdit* pLB;
   pLB = (CEdit*)GetDlgItem(IDC_TRACE);

   pLB->ReplaceSel(s);
   pLB->ReplaceSel(_T("\r\n"));
   m_iLB++;
   if (m_iLB > MAXLINES)
   {
      int    i;
      i = pLB->LineIndex(DELTALINES);
      pLB->SetSel(0, i, TRUE);
      pLB->Clear();
      i = pLB->GetLineCount();
      i = pLB->LineIndex(i-1);
      pLB->SetSel(i, i, FALSE);
      m_iLB -= DELTALINES;
   }
}

int CDebugView::GetResp()
{
   MSG  msg;
   m_bResp = FALSE;
   theApp.m_pMFW->PrologGetOn();
   while (!m_bResp && GetMessage (&msg, NULL, 0, 0))
   {
      TranslateMessage (&msg) ;
      DispatchMessage (&msg) ;
   }
   theApp.m_pMFW->PrologGetOff();
   return m_iResp;
}

/////////////////////////////////////////////////////////////////////////////
// CDebugView message handlers


void CDebugView::OnCreep()
{
   m_iResp = _T('c');
   m_bResp = TRUE;
}

void CDebugView::OnFail()
{
   m_iResp = _T('f');
   m_bResp = TRUE;   
}

void CDebugView::OnLeap()
{
   m_iResp = _T('l');
   m_bResp = TRUE;   
}

void CDebugView::OnSkip()
{
   m_iResp = _T('s');
   m_bResp = TRUE;
}

void CDebugView::OnStop()
{
   m_iResp = _T('q');
   m_bResp = TRUE;
   theApp.m_pMFW->SendMessage(WM_COMMAND, ID_DEBUG_OFF);
}

void CDebugView::OnSize(UINT nType, int cx, int cy)
{
   CRect rC;
   int bwidth;
   int border = 10;
   int dx;
   CWnd* pwC;
   
   pwC = GetDlgItem(IDC_CREEP);
   if (pwC == NULL) goto skip;
   pwC->GetWindowRect(rC);
   ScreenToClient(rC);
   bwidth = rC.Width();
   dx = cx - rC.right - border;
   
   shift_control(IDC_CREEP, dx);
   shift_control(IDC_SKIP, dx);
   shift_control(IDC_LEAP, dx);
   shift_control(IDC_FAIL, dx);
   shift_control(IDC_LEASH, dx);
   shift_control(IDC_LEASH_CALL, dx);
   shift_control(IDC_LEASH_EXIT, dx);
   shift_control(IDC_LEASH_REDO, dx);
   shift_control(IDC_LEASH_FAIL, dx);
   shift_control(IDC_SPY, dx);
   shift_control(IDC_STOP, dx);
   shift_control(IDC_FORMAT, dx);
   shift_control(IDC_FON, dx);
   shift_control(IDC_FOFF, dx);
   
   // resize listbox
   rC.top = border;
   rC.left = border;
   rC.right = cx - bwidth - 2 * border;
   rC.bottom = cy - border;
   pwC = GetDlgItem(IDC_TRACE);
   pwC->SetWindowPos(&wndTop, rC.left, rC.top,
      rC.Width(), rC.Height(), SWP_DRAWFRAME); 

   skip: CFormView::OnSize(nType, cx, cy);
}

void CDebugView::shift_control(int id, int dx)
{
   CRect r;
   CWnd* pwC;
   
   pwC = GetDlgItem(id);
   pwC->GetWindowRect(r);
   ScreenToClient(r);
   r += CPoint(dx,0);
   pwC->SetWindowPos(&wndTop, r.left, r.top,
      r.Width(), r.Height(), SWP_DRAWFRAME); 
}

void CDebugView::OnClose()
{
// It turns out this code never gets called, the close
// calls go to the frame, not the window, so this code
// was copied to CDebugViewFr::OnClose().

   if (m_bResp == FALSE)
      AfxMessageBox(_T("Stop debugging before closing"));
   else
   {
      theApp.m_pMFW->DebuggerOff();
//   theApp.m_pListenView->NoDebug();
   
      CFormView::OnClose();
   }
}

void CDebugView::OnLeashCall()
{
   TERM t;
   UpdateData();
   theApp.m_pListen->ExecStr(&t, _T("amzi_debugger:leash(off, 'CALL')"));
   if (m_bCall)
      theApp.m_pListen->ExecStr(&t, _T("amzi_debugger:leash(on, 'CALL')"));
}

void CDebugView::OnLeashExit()
{
   TERM t;
   UpdateData();
   theApp.m_pListen->ExecStr(&t, _T("amzi_debugger:leash(off, 'EXIT')"));
   if (m_bExit)
      theApp.m_pListen->ExecStr(&t, _T("amzi_debugger:leash(on, 'EXIT')"));
}

void CDebugView::OnLeashFail()
{
   TERM t;
   UpdateData();
   theApp.m_pListen->ExecStr(&t, _T("amzi_debugger:leash(off, 'FAIL')"));
   if (m_bFail)
      theApp.m_pListen->ExecStr(&t, _T("amzi_debugger:leash(on, 'FAIL')"));
}

void CDebugView::OnLeashRedo()
{
   TERM t;
   UpdateData();
   theApp.m_pListen->ExecStr(&t, _T("amzi_debugger:leash(off, 'REDO')"));
   if (m_bRedo)
      theApp.m_pListen->ExecStr(&t, _T("amzi_debugger:leash(on, 'REDO')"));
}

void CDebugView::OnSpy()
{
   CSpypoints spy;

   spy.DoModal();
}

void CDebugView::OnFon() 
{
   m_bFormat = TRUE;
}

void CDebugView::OnFoff() 
{
   m_bFormat = FALSE;
}

void CDebugView::OnInitialUpdate()
{
   CFormView::OnInitialUpdate();
   m_Creep.SetFocus();
   CEdit* pLB = (CEdit*)GetDlgItem(IDC_TRACE);
   pLB->SetFont(&(theApp.m_pMFW->m_font));
   return;
}


void CDebugView::OnSelchangeTrace()
{
// If the user accidently selects an item from the debug
// listbox, then the listbox fails to keep the current
// line in view.  Hopefully this deselection will fix
// the problem
   CListBox* pLB;
   pLB = (CListBox*)GetDlgItem(IDC_TRACE);
   pLB->SetCurSel(-1);
   return;
}

int CDebugView::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
   if (CFormView::OnCreate(lpCreateStruct) == -1)
      return -1;
   
   return 0;
}

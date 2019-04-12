//---------------------------------------------------------------
// The C++ front-end on the Amzi! listener/debugger
//

#include "stdafx.h"
#include "resource.h"
#include <ctype.h>
#include "cpwide.h"
#include "mainfrm.h"
#include "proprog.h"
#include "conview.h"
#include "listen.h"
#include "prodoc.h"
#include "utils.h"
#include "debug.h"
#include "amzi.h"
#include "cpwin.h"
#include "project.h"

//extern PRED_INIT winPreds[];   // Extended windows predicates

CDebugView* g_pDebug;

TF EXPFUNC p_dbgmsg(ENGid);
TF EXPFUNC p_dbgreport(ENGid);
TF EXPFUNC p_dbgresp(ENGid);
TF EXPFUNC p_dbgnocls(ENGid);

PRED_INIT debugPreds[] =
{
   {_T("bug_message_wide"), 1, p_dbgmsg},
   {_T("bug_goal_report_wide"), 4, p_dbgreport},
   {_T("bug_action_wide"), 1, p_dbgresp},
   {_T("bug_noclause_wide"), 3, p_dbgnocls},
   {NULL, 0, NULL}
};

TF EXPFUNC p_dbgmsg(ENGid eid)
{
   if (!theApp.m_pMFW->Debugging() || g_pDebug == NULL)
   {
      //TERM t;
      //theApp.m_pListen->ExecStr(&t, _T("set_debug(off)"));
      return TRUE;
      //theApp.m_pListen->ErrRaise(
      //      _T("Internal debugger error,\nnotify Amzi! tech support."));
   }

   _TCHAR msg[120];
   lsGetParm(eid, 1, cSTR, msg);
   g_pDebug->PutS(msg);
   return(TRUE);
}

const int LONG_DEBUG_LINE=60;

TF EXPFUNC p_dbgreport(ENGid eid)
{

   if (!theApp.m_pMFW->Debugging() || g_pDebug == NULL)
   {
      //TERM t;
      //theApp.m_pListen->ExecStr(&t, _T("set_debug(off)"));
      return TRUE;
      //theApp.m_pListen->ErrRaise(
      //      _T("Internal debugger error,\nnotify Amzi! tech support."));
   }

   _TCHAR msg[300], port[10], goal[256], mod[256];
   _TCHAR invtab[22], inv10s[20];
   TERM t, tc, ta, tm;
   int  invoke, clnum;
   int  i, i10, itab;
   ARITY arity;
   
   lsGetParm(eid, 1, cSTR, port);
   lsGetParm(eid, 2, cINT, &invoke);
   lsGetParm(eid, 3, cTERM, &tc);
   lsGetParm(eid, 4, cINT, &clnum);
   lsTermToStrQ(eid, tc, goal, 255);
   
   i10 = 10 * (invoke/10);
   if (i10 > 0)
      _stprintf(inv10s, _T("[%d]"), i10);
   else
      inv10s[0] = EOS;

   itab = invoke % 10;
   for (i=0; i < itab; i++)
      invtab[2*i] = _T(' '), invtab[2*i+1] = _T(' ');
   invtab[2*i] = EOS;
   
   if (clnum == 0)
      _sntprintf(msg, 255, _T("%s%s %s: %s"), inv10s, invtab, port, goal);
   else
      _sntprintf(msg, 255, _T("%s%s %s:(%d) %s"), inv10s, invtab, port, clnum, goal);

   //if (strlen(goal) < LONG_DEBUG_LINE)
   if (!g_pDebug->FormatLines())
      g_pDebug->PutS(msg);
   else
   {
      lsGetArg(eid, tc, 1, cMOD, &tm);
      lsGetTerm(eid, tm, cSTR, mod);
      lsGetArg(eid, tc, 2, cGOAL, &t);
      lsGetFA(eid, t, goal, &arity);
      if (clnum == 0)
         _sntprintf(msg, 255, _T("%s%s %s: %s:%s / %d"), inv10s, invtab, port, mod, goal, (int)arity);
      else
         _sntprintf(msg, 255, _T("%s%s %s:(%d) %s:%s / %d"), inv10s, invtab, port, clnum, mod, goal, (int)arity);
      g_pDebug->PutS(msg);
      for (i=1; i<=arity; i++)
      {
         lsGetArg(eid, t, i, cTERM, &ta);
         lsTermToStrQ(eid, ta, goal, 255);
         _sntprintf(msg, 255, _T("%s%s             %s"), inv10s, invtab, goal);
         g_pDebug->PutS(msg);
      }
   }

   return TRUE;
}

TF EXPFUNC p_dbgresp(ENGid eid)
{
   if (!theApp.m_pMFW->Debugging() || g_pDebug == NULL)
   {
      //TERM t;
      //theApp.m_pListen->ExecStr(&t, _T("set_debug(off)"));
      return TRUE;
      //theApp.m_pListen->ErrRaise(
      //      _T("Internal debugger error,\nnotify Amzi! tech support."));
   }

   int c;
   TF  tf;
   // sometimes get_action is called with nl, just ignore
   if (pVAR != lsGetParmType(eid, 1))
      return TRUE;

   CFrameWnd* pPF;
   pPF = g_pDebug->GetParentFrame();
   pPF->ActivateFrame();
   pPF->SetActiveView(g_pDebug);
   c = g_pDebug->GetResp();
   //else c = EOS; used to do this when not debugging.
   if (c == _T('@'))
      tf = lsUnifyParm(eid, 1, cSTR, &(g_pDebug->m_sQuery));
   else
      tf = lsUnifyParm(eid, 1, cINT, &c);
   return tf;
}

TF EXPFUNC p_dbgnocls(ENGid eid)
{
   if (!theApp.m_pMFW->Debugging() || g_pDebug == NULL)
   {
      //TERM t;
      //theApp.m_pListen->ExecStr(&t, _T("set_debug(off)"));
      return TRUE;
      //theApp.m_pListen->ErrRaise(
      //      _T("Internal debugger error,\nnotify Amzi! tech support."));
   }

   _TCHAR msg[300], functor[128], module[128];
   int  arity;
   
   lsGetParm(eid, 1, cSTR, module);
   lsGetParm(eid, 2, cSTR, functor);
   lsGetParm(eid, 3, cINT, &arity);
   Lsprintf(msg, _T("No clauses for %s:%s/%d"), module, functor, arity);
   g_pDebug->PutS(msg);
   return TRUE;
}

CListen::CListen()
{
   m_env_loaded = FALSE;
}

CListen::~CListen()
{
}

BOOL CListen::Initialize(const _TCHAR* pszPathName)
{
   _TCHAR name[80];
    
   _tcscpy(name, pszPathName);

   try {

   Init(name);
   InitPreds(debugPreds);
   AddPred(_T("keyb"), 1, &::p_keyb, this);
   InitLSX(&theApp);
   Load(name);

   } catch (CLSException &E)
   {
      Error(E);
      //Close();
      return FALSE;
   }

   return TRUE;
}

void CListen::Start()
{
   TERM t;
   _TCHAR ver[120];
   g_pConView = m_pConView;
   GetVersion(ver);
   _TCHAR str_listener[512];
   int rc = LoadString(theApp.m_hInstance, IDS_LISTENER, str_listener, 512);

   if (FALSE == ExecStr(&t, _T("amzi_listener:greetings")))
   {
      AfxMessageBox(_T("Listener failed to initialize correctly (open)"));
      return;
   }
   
   SilentCall(_T("amzi_system:sys$assert('{sys}wide')"));
   m_pConView->PutS(_T("\n  Listener 'listens to' input from keyboard,"));
   m_pConView->PutS(_T("\n  not screen.  Multiline input is OK, '.' followed"));
   m_pConView->PutS(_T("\n  by 'Enter' enters line.  Mouse and cursor keys"));
   m_pConView->PutS(_T("\n  have no effect, use backspace key to edit a line."));
   m_pConView->PutS(_T("\n  Cut and paste works, but only at end of input line."));
   m_pConView->PutS(_T("\n  Type 'quit' to exit."));

   m_pConView->PutS(_T("\n\n"));
   m_pConView->PutS(_T("?- "));
   m_pConView->FlushBuf();
}

void CListen::Stop()
{
	SilentCall(_T("amzi_system:sys$retract('{sys}wide')"));
}

int CListen::SilentCall(_TCHAR* sFunctor, _TCHAR* sArg)
// This is designed for internal calls that in theory
// should just work.  They are of the form sFunctor(sArg).
// they are all in practice to the debugger.
{
   TERM t, tm;
   int rc;

   //Lsprintf(buf, _T("listen$((%s))"), sQuery);
   MakeFA(&t, sFunctor, 1);
   UnifyArg(&t, 1, cATOM, sArg);
   // all calls are, in practice, to the debugger, so
   // we hardwire that in here as a module call
   MakeFA(&tm, _T(":"), 2);
   UnifyArg(&tm, 1, cATOM, _T("amzi_debugger"));
   UnifyArg(&tm, 2, cTERM, &t);

//   _TCHAR buf[MAX_QUERY+1];
//   TermToStrQ(tm, buf, MAX_QUERY);

   try {
      //ExecStr(&t, sQuery);
      Exec(&tm);
      rc = 0;

   } catch (CLSException &E)
   {
      _TCHAR buf[80];
      rc = E.GetRC();
      if (rc == 1051)  // usually adding spypoints during read
      {
         Lsprintf(buf,
            _T("You can't change debugger state during a read operation\n"));
         rc = 0;
      }
      else
      {
         Lsprintf(buf,
            _T("Error (%d) occurred during an internal call,\n")
            _T("Notify Amzi! Tech Support"),
            E.GetRC());
      }
      AfxMessageBox(buf);
   }
   return rc;
}

int CListen::SilentCall(_TCHAR* sQuery)
// This is designed for internal calls that in theory
// should just work.  This is more generic than the
// two arg version above.
{
   _TCHAR buf[MAX_QUERY];
   TERM t;
   int rc;

   Lsprintf(buf, _T("amzi_listener:listen$((%s))"), sQuery);

   try {
      ExecStr(&t, buf);
      rc = 0;

   } catch (CLSException &E)

   {
      _TCHAR buf[80];
      rc = E.GetRC();
      if (rc == 1051)  // usually adding spypoints during read
      {
         Lsprintf(buf,
            _T("You can't change debugger state during a read operation\n"));
         rc = 0;
      }
      else
      {
         Lsprintf(buf,
            _T("Error (%d) occurred during an internal call,\n")
            _T("Notify Amzi! Tech Support"),
            E.GetRC());
      }
      AfxMessageBox(buf);
   }
   return rc;
}

int CListen::LCall(_TCHAR* sQuery)
{
   _TCHAR buf[MAX_QUERY];
   TERM t;
   int rc;

   if (theApp.m_pMFW->PrologGet())
   {
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return 0;
   }

   if (_tcslen(sQuery) > (MAX_QUERY - 14)) {
      AfxMessageBox(_T("Query too long, ignoring"));
      return 0;
   }
   Lsprintf(buf, _T("amzi_listener:listen$((%s))"), sQuery);

   try {

      theApp.m_pMFW->PrologGetOn();
      ExecStr(&t, buf);
      rc = 0;

   } catch (CLSException &E)
   {
      rc = Error(E);
      if (rc != -1)  // nothing too bad
      {
         if (theApp.m_pMFW->Debugging())
            theApp.m_pMFW->SendMessage(WM_COMMAND, ID_DEBUG_OFF);
         // If we were (re)consulting, close the file so it can be edited.
         // We just clean up here, the engine closes the file after a
         // read error.
         Reset();
         AttachView(theApp.m_pListenView); // setup streams again
         ExecStr(&t, _T("retract(amzi_system:'{sys}consult$read$file'(_H))"));
         Start();
      }
      // else just return, the caller will close up.
   }

   theApp.m_pMFW->PrologGetOff();
   return rc;
}


/////////////////////////////////////////////////////////////////////////////
// CListenFrame

IMPLEMENT_DYNCREATE(CListenFrame, CMDIChildWnd)

CListenFrame::CListenFrame()
{
}

CListenFrame::~CListenFrame()
{
}

BOOL CListenFrame::PreCreateWindow(CREATESTRUCT &cs)
{      
   WINDOWPLACEMENT wp;
   if (theApp.ReadWindowPlacement(&wp, _T("Settings"), _T("ListenerWindow")))
   {
      cs.x = wp.rcNormalPosition.left;
      cs.y = wp.rcNormalPosition.top;
      cs.cx = wp.rcNormalPosition.right - cs.x;
      cs.cy = wp.rcNormalPosition.bottom - cs.y;
   }

   return(CMDIChildWnd::PreCreateWindow(cs));
}

BOOL CListenFrame::DestroyWindow()
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
      theApp.WriteWindowPlacement(&wp, _T("Settings"), _T("ListenerWindow"));
   }
   return(CMDIChildWnd::DestroyWindow());
}

BEGIN_MESSAGE_MAP(CListenFrame, CMDIChildWnd)
   //{{AFX_MSG_MAP(CListenFrame)
   ON_WM_CLOSE()
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CListenFrame message handlers


void CListenFrame::OnClose()
{
   AfxGetMainWnd()->SendMessage(WM_COMMAND, ID_LISTEN_END);
   
//   CMDIChildWnd::OnClose();
}

/////////////////////////////////////////////////////////////////////////////
// CListenView

IMPLEMENT_DYNCREATE(CListenView, CConView)

CListenView::CListenView()
{
   m_sConsultFile = theApp.GetProfileString(_T("Settings"), _T("ConsultFile"), _T(""));
}

CListenView::~CListenView()
{
   theApp.WriteProfileString(_T("Settings"), _T("ConsultFile"), m_sConsultFile);
}

BEGIN_MESSAGE_MAP(CListenView, CConView)
   //{{AFX_MSG_MAP(CListenView)
   ON_WM_CREATE()
   ON_EN_CHANGE(AFX_IDW_PANE_FIRST, OnEditChange)
   ON_WM_SETFOCUS()
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CListenView drawing

void CListenView::OnDraw(CDC* pDC)
{
   CDocument* pDoc = (CDocument*)GetDocument();
   // TODO: add draw code here
}

/////////////////////////////////////////////////////////////////////////////
// CListenView message handlers

void CListenView::OnLineRead()
// OnLineRead allows the listener to function under control
// of the Prolog program, when Prolog is running, or as an
// event-driven interface when Prolog is idle.
{
   int i;
   _TCHAR querybuf[MAX_QUERY];
   // bGettingChars indicates we're in a Prolog read, so do nothing
   if (bGettingChars == TRUE) return;
   // Make sure the last character was a period, otherwise there're
   // more lines coming.
   i = lastchar(keybuf);
   if (quoting || keybuf[i] != _T('.')) {
         kbWrite--;
         lineRead = FALSE;
         return; }
   else
      keybuf[i] = EOS;
   // copy keybuf to query, and reset keybuf settings both before
   // the query, in case it does a read, and after to reinitialize
   // in case any extraneous characters were left.   
   _tcscpy(querybuf, keybuf);
   kbRead = kbWrite = 0;
   keybuf[0] = EOS;
   lineRead = FALSE;
   if (0 == _tcscmp(querybuf, _T("quit")))
      AfxGetMainWnd()->SendMessage(WM_COMMAND, ID_LISTEN_END);
   else if ( 0 == _tcscmp(querybuf, _T("debug")) &&
            ! theApp.m_pMFW->m_bDebuggerOn)
      AfxGetMainWnd()->SendMessage(WM_COMMAND, ID_DEBUG_ON);
   else if ( 0 == _tcscmp(querybuf, _T("nodebug")) &&
            theApp.m_pMFW->m_bDebuggerOn)
      AfxGetMainWnd()->SendMessage(WM_COMMAND, ID_DEBUG_OFF);
   else if ( 0 == _tcscmp(querybuf, _T("consult")))
      AfxGetMainWnd()->SendMessage(WM_COMMAND, ID_LISTEN_CONSULT);
   else if ( 0 == _tcscmp(querybuf, _T("reconsult")))
      AfxGetMainWnd()->SendMessage(WM_COMMAND, ID_LISTEN_RECONSULT);
   else
      docall(querybuf);   
}

int CListenView::lastchar(_TCHAR* s)
{
   int   i;
   
   for (i=_tcslen(s)-1; isspace(s[i]) && i > 0; i--) ;
   return i;
}

void CListenView::docall(_TCHAR* query)
{
   CProDoc* pDoc = GetDocument();
   CListen* pListen = (CListen*)(pDoc->GetProg());

   if (-1 != pListen->LCall(query))
   {
      Prompt();
   }
   else
      AfxGetMainWnd()->SendMessage(WM_COMMAND, ID_LISTEN_END);
}

void CListenView::Prompt()
{
   CProDoc* pDoc = GetDocument();
   CListen* pListen = (CListen*)(pDoc->GetProg());

   kbRead = kbWrite = 0;
   keybuf[0] = EOS;
   lineRead = FALSE;
   quoting = 0;
   if (theApp.m_pMFW->Debugging())
   {
      pListen->SilentCall(_T("set_creep"), _T("on"));
      PutS(_T("\n?\?- "));
   }
   else
      PutS(_T("\n?- "));
   FlushBuf();
   GetParent()->BringWindowToTop();
}

void CListenView::Debug()
{
   CProDoc* pDoc = GetDocument();
   CListen* pListen = (CListen*)(pDoc->GetProg());
   g_pDebug = ((CWideApp*)AfxGetApp())->m_pDebugView;
   //PutS(_T("debug"));
   //docall(_T("debug"));
   pListen->SilentCall(_T("set_debug_wide"), _T("on"));
   Prompt();
}

void CListenView::NoDebug()
{
   // This is all a bit tricky.  There is probably a current
   // goal that was executed in the listener.  We now prevent
   // goals being called when another goal is already active,
   // but in this case its OK.  (It calls a new prove variant,
   // so the other isn't really active right now.)  So, find out
   // the old state of PrologGet, and toggle it on and off around
   // this call.
   CProDoc* pDoc = GetDocument();
   CListen* pListen = (CListen*)(pDoc->GetProg());
   pListen->SilentCall(_T("set_debug"), _T("off"));
   if (! theApp.m_pMFW->PrologGet())
      Prompt();
}

void CListenView::ListenConsult()
{
   CFileDialog *pfileDlg = NULL;
   if (g_osrel >= 4)
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT|OFN_EXPLORER,
         _T("Prolog Files (*.ppj;*.pro;*.plm)|*.ppj;*.pro;*.plm|All (*.*)|*.*||"));
   else
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT,
         _T("Prolog Files (*.ppj;*.pro;*.plm)|*.ppj;*.pro;*.plm|All (*.*)|*.*||"));

   if (pfileDlg->DoModal() != IDOK)
   {
      delete pfileDlg;
      return;
   }

   m_sConsultFile = pfileDlg->GetPathName();
   _TCHAR fbuf[_MAX_PATH+10];
   _TCHAR buf[_MAX_PATH+30];
   // See if its a Prolog project file
   _tcscpy(fbuf, (const _TCHAR*)m_sConsultFile);
   
   if (chk_ext(fbuf, _T(".ppj")))
   {
      AfxMessageBox(_T("Use Project Dialog Box for consulting Projects"));
      //ConsultProject(m_sConsultFile);
   }
   else
   {
      slashslash1(fbuf);
      Lsprintf(buf, _T("consult('%s')"), fbuf);
      PutS(buf);
      docall(buf);
   }
   delete pfileDlg;
}

void CListenView::ListenReconsult()
{
//   CFileDialog fileDlg(TRUE, NULL, NULL,
//      OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT,
//      "Prolog Files (*.pro;*.ppj)|*.pro;*.ppj|All (*.*)|*.*||");

//   if (fileDlg.DoModal() != IDOK) return;
//   CString sFile = fileDlg.GetPathName();

   if (m_sConsultFile.IsEmpty())
   {
      ListenConsult();
      return;
   }

   _TCHAR fbuf[_MAX_PATH+10];
   _TCHAR buf[_MAX_PATH+30];
   // See if its a Prolog project file
   _tcscpy(fbuf, (const _TCHAR*)m_sConsultFile);
   
   if (chk_ext(fbuf, _T(".ppj")))
   {
      AfxMessageBox(_T("Use Project Dialog Box for consulting Projects"));
   }
   else
   {
      slashslash1(fbuf);
      Lsprintf(buf, _T("reconsult('%s')"), fbuf);
      PutS(buf);
      docall(buf);
   }
}

void CListenView::ReconsultProject()
{
   _TCHAR fbuf[_MAX_PATH+10];
   _TCHAR buf[_MAX_PATH+30];
   int i, cnt;

   cnt = theApp.m_pProjectDoc->GetLibCount();

      for (i=0; i<cnt; i++)
   {
      _tcscpy(fbuf, theApp.m_pProjectDoc->GetLib(i));
         slashslash1(fbuf);                          // convert '\'s to '\\'s
         if (chk_ext(fbuf, _T(".pro")))
            Lsprintf(buf, _T("reconsult('%s')"), fbuf);
         else
            Lsprintf(buf, _T("load('%s')"), fbuf);
      PutS(buf);
      docall(buf);
   }

   cnt = theApp.m_pProjectDoc->GetFileCount();

   // Use the special index, -1, to consult the operator
   // definitions first, if they are defined.
   int iFirstFile;
   if (_T("") != theApp.m_pProjectDoc->GetOpDefsFile())
      iFirstFile = -1;
   else
      iFirstFile = 0;

      for (i=iFirstFile; i<cnt; i++)
   {
      _tcscpy(fbuf, theApp.m_pProjectDoc->GetFile(i));
         slashslash1(fbuf);                          // convert '\'s to '\\'s
         if (chk_ext(fbuf, _T(".pro")))
            Lsprintf(buf, _T("reconsult('%s')"), fbuf);
         else
            Lsprintf(buf, _T("load('%s')"), fbuf);
      PutS(buf);
      docall(buf);
   }
}

int CListenView::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
   if (CConView::OnCreate(lpCreateStruct) == -1)
      return -1;
   
   return 0;
}

void CListenView::OnEditChange()
{
   return;
}

void CListenView::OnSetFocus(CWnd* pOldWnd) 
{
   CConView::OnSetFocus(pOldWnd);
   
   // TODO: Add your message handler code here

   //AfxMessageBox("OnSetFocus, CListenView");
}

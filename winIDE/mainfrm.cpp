// mainfrm.cpp : implementation of the CMainFrame class
//

#include "stdafx.h"
#include <stdio.h>
#include <stdlib.h>
#include <direct.h>
#include <io.h>
#include "resource.h"
#include "mainfrm.h"
#include "cpwide.h"
#include "conview.h"
#include "proprog.h"
#include "listen.h"
#include "peditvw.h"
#include <afxadv.h>
#include "project.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMainFrame

/* Local Utility */

BOOL file_exists(_TCHAR* fname)
{
   _TCHAR    wdrive[_MAX_DRIVE];
   _TCHAR    wdir[_MAX_DIR];
   _TCHAR    wfname[_MAX_FNAME];
   _TCHAR    wext[_MAX_EXT];
   _TCHAR    path[_MAX_PATH];
   _TCHAR    old_path[_MAX_PATH];

   if ( 0 == (_taccess(fname, 0)) )
      return TRUE;
   
   GetModuleFileName(NULL, path, _MAX_PATH);
   _tsplitpath(path, wdrive, wdir, wfname, wext);
   _tcscpy(path, wdrive);
   _tcscat(path, wdir);
   //_tcscat(path, _T("\\"));
   _tcscpy(old_path, path);
   _tcscat(path, fname);
   if ( 0 == (_taccess(path, 0)) )
      return TRUE;
   _tcscpy(path, old_path);
   _tcscat(path, _T("..\\abin\\"));
   _tcscat(path, fname);
   if ( 0 == (_taccess(path, 0)) )
      return TRUE;

   return FALSE;
}


/////////////////////////////////////////////////////////////////////////////
// Static initialization/termination
// From MFC sample application SuperPad, class PadView
//

static TCHAR BASED_CODE szSettings[] = _T("Settings");
static TCHAR BASED_CODE szTabStops[] = _T("TabStops");
static TCHAR BASED_CODE szFont[] = _T("Font");
static TCHAR BASED_CODE szPrintFont[] = _T("PrintFont");
static TCHAR BASED_CODE szHeight[] = _T("Height");
static TCHAR BASED_CODE szWidth[] = _T("Width");
static TCHAR BASED_CODE szWeight[] = _T("Weight");
static TCHAR BASED_CODE szItalic[] = _T("Italic");
static TCHAR BASED_CODE szUnderline[] = _T("Underline");
static TCHAR BASED_CODE szPitchAndFamily[] = _T("PitchAndFamily");
static TCHAR BASED_CODE szFaceName[] = _T("FaceName");
static TCHAR BASED_CODE szSystem[] = _T("System");
static TCHAR BASED_CODE szWordWrap[] = _T("WordWrap");

static BOOL GetProfileFont(LPCTSTR szSec, LOGFONT* plf)
{
   plf->lfHeight = theApp.GetProfileInt(szSec, szHeight, 0);
   if (plf->lfHeight == 0)
      return FALSE;

   plf->lfHeight = theApp.GetProfileInt(szSec, szHeight, 0);
   plf->lfWidth = theApp.GetProfileInt(szSec, szWidth, 0);
   plf->lfWeight = theApp.GetProfileInt(szSec, szWeight, 0);
   plf->lfItalic = (BYTE)theApp.GetProfileInt(szSec, szItalic, 0);
   plf->lfUnderline = (BYTE)theApp.GetProfileInt(szSec, szUnderline, 0);
   plf->lfPitchAndFamily = (BYTE)theApp.GetProfileInt(szSec, szPitchAndFamily, 0);
   CString strFont = theApp.GetProfileString(szSec, szFaceName, szSystem);
   lstrcpyn((TCHAR*)plf->lfFaceName, strFont, sizeof(plf->lfFaceName));
   plf->lfFaceName[sizeof(plf->lfFaceName)-1] = 0;
   plf->lfEscapement = 0;
   plf->lfOrientation = 0;
   plf->lfStrikeOut = FALSE;
   plf->lfOutPrecision = 0;
   plf->lfClipPrecision = 0;
   plf->lfQuality = 0;

   return TRUE;
}

static void WriteProfileFont(LPCTSTR szSec, const LOGFONT* plf)
{
   if (plf->lfHeight != 0)
   {
      theApp.WriteProfileInt(szSec, szHeight, plf->lfHeight);
      theApp.WriteProfileInt(szSec, szWidth, plf->lfWidth);
      theApp.WriteProfileInt(szSec, szWeight, plf->lfWeight);
      theApp.WriteProfileInt(szSec, szItalic, plf->lfItalic);
      theApp.WriteProfileInt(szSec, szUnderline, plf->lfUnderline);
      theApp.WriteProfileInt(szSec, szPitchAndFamily, plf->lfPitchAndFamily);
      theApp.WriteProfileString(szSec, szFaceName, (LPCTSTR)plf->lfFaceName);
   }
}

IMPLEMENT_DYNAMIC(CMainFrame, CMDIFrameWnd)

BEGIN_MESSAGE_MAP(CMainFrame, CMDIFrameWnd)
   //{{AFX_MSG_MAP(CMainFrame)
   ON_WM_CREATE()
   ON_WM_CLOSE()
   ON_UPDATE_COMMAND_UI(ID_BUILD_LINK, OnUpdateBuildLink)
   ON_UPDATE_COMMAND_UI(ID_BUILD_COMPILE, OnUpdateBuildCompile)
   ON_UPDATE_COMMAND_UI(ID_LISTEN_END, OnUpdateListenEnd)          
   ON_UPDATE_COMMAND_UI(ID_DEBUG_ON, OnUpdateDebugOn)
   ON_UPDATE_COMMAND_UI(ID_DEBUG_OFF, OnUpdateDebugOff)
   ON_UPDATE_COMMAND_UI(ID_LISTEN_OPENLOG, OnUpdateListenOpenlog)
   ON_UPDATE_COMMAND_UI(ID_LISTEN_CLOSELOG, OnUpdateListenCloselog)
   ON_UPDATE_COMMAND_UI(ID_LISTEN_CONSULT, OnUpdateListenConsult)
   ON_UPDATE_COMMAND_UI(ID_LISTEN_RECONSULT, OnUpdateListenReconsult)
   ON_UPDATE_COMMAND_UI(ID_BUILD_RUN, OnUpdateBuildRun)
   ON_UPDATE_COMMAND_UI(ID_LISTEN_START, OnUpdateListenStart)
   ON_COMMAND(ID_VIEW_SETFONT, OnViewSetFont)
   ON_COMMAND(ID_FILE_SAVE, OnFileSave)
   ON_COMMAND(ID_FILE_CLOSE, OnFileClose)
   ON_UPDATE_COMMAND_UI(ID_BUILD_BUILD, OnUpdateBuildBuild)
   ON_UPDATE_COMMAND_UI(ID_MRU_PROJECT1, OnUpdateMruProject1)
   ON_UPDATE_COMMAND_UI(ID_MRU_EDIT1, OnUpdateMruEdit1)
   ON_WM_SETCURSOR()
   ON_UPDATE_COMMAND_UI(ID_FILE_UNLOCK, OnUpdateFileUnlock)
   ON_WM_ACTIVATEAPP()
   //}}AFX_MSG_MAP
   // Global help commands
   //ON_COMMAND(ID_HELP_USING, CMDIFrameWnd::OnHelpUsing)
   //ON_COMMAND(ID_HELP, CMDIFrameWnd::OnHelp)
   //ON_COMMAND(ID_CONTEXT_HELP, CMDIFrameWnd::OnContextHelp)
   //ON_COMMAND(ID_DEFAULT_HELP, CMDIFrameWnd::OnHelpIndex)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// arrays of IDs used to initialize control bars

// toolbar buttons - IDs are command buttons
/*
static UINT BASED_CODE buttons[] =
{
   // same order as in the bitmap 'toolbar.bmp'
   ID_FILE_NEW,
   ID_FILE_OPEN,
   ID_FILE_SAVE,
      ID_SEPARATOR,
   ID_EDIT_CUT,
   ID_EDIT_COPY,
   ID_EDIT_PASTE,
      ID_SEPARATOR,
   ID_FILE_PRINT,
   ID_HELP_INDEX,
   ID_CONTEXT_HELP,
      ID_SEPARATOR,
   ID_LISTEN_START,
   ID_BUILD_COMPILE,
   ID_BUILD_LINK,
   ID_LISTEN_RECONSULT,
};
*/

static UINT BASED_CODE indicators[] =
{
   ID_SEPARATOR,           // status line indicator
   ID_INDICATOR_CAPS,
   ID_INDICATOR_NUM,
   ID_INDICATOR_SCRL,
};

/////////////////////////////////////////////////////////////////////////////
// CMainFrame construction/destruction

CMainFrame::CMainFrame()
{
   _TCHAR linker[13] = _T("amzi.dll");
   _TCHAR compiler[13] = _T("acmp.xpl");
   
   m_bLink = TRUE;
   m_bCompile = TRUE;
   m_bListenerOn = FALSE;
   m_bDebuggerOn = FALSE;
   m_bRunning = FALSE;
   m_bPrologGet = FALSE;
   m_bWaiting = FALSE;
   m_bUnlocked = FALSE;

#ifdef ALE
   m_bLink = FALSE;
   m_bCompile = FALSE;
#else
   if (file_exists(linker))
      m_bLink = TRUE;
   else
      m_bLink = FALSE;

   if (file_exists(compiler))
      m_bCompile = TRUE;
   else
      m_bCompile = FALSE;
#endif // ALE
}

CMainFrame::~CMainFrame()
{
}

int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
   if (CMDIFrameWnd::OnCreate(lpCreateStruct) == -1)
      return -1;

   /*
   if (!m_wndToolBar.Create(this) ||
      !m_wndToolBar.LoadBitmap(IDR_MAINFRAME) ||
      !m_wndToolBar.SetButtons(buttons,
        sizeof(buttons)/sizeof(UINT)))
   {
      TRACE("Failed to create toolbar\n");
      return -1;      // fail to create
   }
   */

   if (!m_wndToolBar.Create(this) ||
      !m_wndToolBar.LoadToolBar(IDR_MAINFRAME))
   {
      TRACE0("Failed to create toolbar\n");
      return -1;      // fail to create
   }

   if (!m_wndStatusBar.Create(this) ||
      !m_wndStatusBar.SetIndicators(indicators,
        sizeof(indicators)/sizeof(UINT)))
   {
      TRACE(_T("Failed to create status bar\n"));
      return -1;      // fail to create
   }

   // read the save font information
   LOGFONT lf;

   if (GetProfileFont(szFont, &lf))
      m_font.CreateFontIndirect(&lf);
   else
      m_font.CreateStockObject(SYSTEM_FIXED_FONT);


   return 0;
}

/////////////////////////////////////////////////////////////////////////////

void CMainFrame::InitialShowWindow(UINT nCmdShow)
{
   WINDOWPLACEMENT wp;
   if (!theApp.ReadWindowPlacement(&wp, _T("Settings"), _T("MainWindow")))
   {
      ShowWindow(nCmdShow);
      return;
   }
   if (nCmdShow != SW_SHOWNORMAL)
      wp.showCmd = nCmdShow;
   wp.length = sizeof(WINDOWPLACEMENT);
   SetWindowPlacement(&wp);
   ShowWindow(wp.showCmd);
   int iDrive = theApp.GetProfileInt(_T("Settings"), _T("Drive"), 0);
   if (iDrive > 0)
      _chdrive(iDrive);
   CString strDir = theApp.GetProfileString(_T("Settings"), _T("Directory"));
   if (!strDir.IsEmpty())
      _tchdir(strDir);
}

void CMainFrame::OnClose()
{
   if (theApp.m_pMFW->PrologGet())
   {
      //AfxMessageBox("End Prolog I/O interaction before closing.");
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return;
   }
   // Close the listener if its open.
   if (Listening())
      AfxGetMainWnd()->SendMessage(WM_COMMAND, ID_LISTEN_END);
   // before it is destroyed, save the position of the window
   WINDOWPLACEMENT wp;
   //wp.length = sizeof wp;
   wp.length = sizeof(WINDOWPLACEMENT);
   if (GetWindowPlacement(&wp))
   {
      wp.flags = 0;
      if (IsZoomed())
         wp.flags |= WPF_RESTORETOMAXIMIZED;
      // and write it to the .INI file
      theApp.WriteWindowPlacement(&wp, _T("Settings"), _T("MainWindow"));
   }
    int iDrive = _getdrive();    
    theApp.WriteProfileInt(_T("Settings"), _T("Drive"), iDrive);
    _TCHAR sPath[_MAX_DIR];
    if (NULL != _tgetcwd(sPath, _MAX_DIR))
       theApp.WriteProfileString(_T("Settings"), _T("Path"), sPath);
   // Close the open project if there is one
   if (theApp.m_pProjectDoc != NULL)
      theApp.m_pProjectDoc->Close();
   CMDIFrameWnd::OnClose();
}
/////////////////////////////////////////////////////////////////////////////
// CMainFrame diagnostics

#ifdef _DEBUG
void CMainFrame::AssertValid() const
{
   CMDIFrameWnd::AssertValid();
}

void CMainFrame::Dump(CDumpContext& dc) const
{
   CMDIFrameWnd::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CMainFrame message handlers

void CMainFrame::OnUpdateBuildLink(CCmdUI* pCmdUI)
{
   pCmdUI->Enable(m_bLink);
}

void CMainFrame::OnUpdateBuildCompile(CCmdUI* pCmdUI)
{
   pCmdUI->Enable(m_bCompile && !m_bRunning);
}

void CMainFrame::OnUpdateBuildBuild(CCmdUI* pCmdUI) 
{
   pCmdUI->Enable(m_bCompile && !m_bRunning);
}

void CMainFrame::OnUpdateListenEnd(CCmdUI* pCmdUI)
{
   pCmdUI->Enable(m_bListenerOn);
}

void CMainFrame::OnUpdateDebugOn(CCmdUI* pCmdUI)
{
   pCmdUI->Enable(m_bListenerOn && !m_bDebuggerOn);   
}

void CMainFrame::OnUpdateDebugOff(CCmdUI* pCmdUI)
{
   pCmdUI->Enable(m_bDebuggerOn);
}

void CMainFrame::OnUpdateListenOpenlog(CCmdUI* pCmdUI)
{
   pCmdUI->Enable(m_bListenerOn);
}

void CMainFrame::OnUpdateListenCloselog(CCmdUI* pCmdUI)
{
   pCmdUI->Enable(m_bListenerOn);
}

void CMainFrame::OnUpdateListenConsult(CCmdUI* pCmdUI)
{
   pCmdUI->Enable(m_bListenerOn);
}

void CMainFrame::OnUpdateListenReconsult(CCmdUI* pCmdUI)
{
   pCmdUI->Enable(m_bListenerOn);
}

void CMainFrame::OnUpdateBuildRun(CCmdUI* pCmdUI)
{
#ifdef ALE
   pCmdUI->Enable(FALSE);
#else
   pCmdUI->Enable(!m_bRunning);
#endif
}

void CMainFrame::OnUpdateListenStart(CCmdUI* pCmdUI)
{
   pCmdUI->Enable(!m_bRunning);
}

void CMainFrame::OnUpdateFileUnlock(CCmdUI* pCmdUI) 
{
   pCmdUI->Enable(!m_bUnlocked);   
}

/////////////////////////////////////////////////////////////////////////////
// CPadView Font Handling

void CMainFrame::OnViewSetFont() 
{
   // get current font description
   LOGFONT lf;
   m_font.GetObject(sizeof(LOGFONT), &lf);

   CFontDialog dlg(&lf, CF_SCREENFONTS|CF_INITTOLOGFONTSTRUCT);
   if (dlg.DoModal() == IDOK)
   {
      // switch to new font.
      m_font.DeleteObject();
      if (m_font.CreateFontIndirect(&lf))
         WriteProfileFont(szFont, &lf);

      // change the listener
      if (m_bListenerOn)
         theApp.m_pListenView->SetFont(&m_font);

      // change all the edit windows
      POSITION posDoc = theApp.m_dplateEdit->GetFirstDocPosition();
      CDocument* pDoc;
      while (posDoc != NULL)
      {
         pDoc = theApp.m_dplateEdit->GetNextDoc(posDoc);
         POSITION posView = pDoc->GetFirstViewPosition();
         CPEditView* pView = (CPEditView*)pDoc->GetNextView(posView);
         pView->SetFont(&m_font);
      }

      // change any running Prolog programs
      posDoc = theApp.m_dplateProg->GetFirstDocPosition();
      while (posDoc != NULL)
      {
         pDoc = theApp.m_dplateProg->GetNextDoc(posDoc);
         POSITION posView = pDoc->GetFirstViewPosition();
         CConView* pView = (CConView*)pDoc->GetNextView(posView);
         pView->SetFont(&m_font);
      }
   }   
}

void CMainFrame::OnFileSave() 
{
   if (theApp.m_pMFW->PrologGet())
   {
      //AfxMessageBox("End Prolog I/O interaction before closing.");
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return;
   }
}

void CMainFrame::OnFileClose() 
{
   if (theApp.m_pMFW->PrologGet())
   {
      //AfxMessageBox("End Prolog I/O interaction before closing.");
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return;
   }
}

void CMainFrame::OnUpdateMruProject1(CCmdUI* pCmdUI) 
{
   ASSERT_VALID(this);
   if (theApp.m_pProjectMRUList == NULL) // no MRU files
      pCmdUI->Enable(FALSE);
   else
      theApp.m_pProjectMRUList->UpdateMenu(pCmdUI);
}

void CMainFrame::OnUpdateMruEdit1(CCmdUI* pCmdUI) 
{
   ASSERT_VALID(this);
   if (theApp.m_pEditMRUList == NULL) // no MRU files
      pCmdUI->Enable(FALSE);
   else
      theApp.m_pEditMRUList->UpdateMenu(pCmdUI);
}


BOOL CMainFrame::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message) 
{
   if (m_bWaiting)
      return TRUE;

   return CMDIFrameWnd::OnSetCursor(pWnd, nHitTest, message);
}


//void CMainFrame::OnActivateApp(BOOL bActive, HTASK hTask) 
void CMainFrame::OnActivateApp(BOOL bActive, DWORD hTask) 
{
   CMDIFrameWnd::OnActivateApp(bActive, hTask);
   
   if (bActive == TRUE)
   {
      theApp.CheckChangedFiles();
   }
   //else
   //   AfxMessageBox(_T("Deactivating App"));
   
}

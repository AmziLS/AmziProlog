// cpwide.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "afxadv.h"
#include "afxwin.h"   // needed to get &wndBottom
#include "resource.h"
#include "cpwide.h"
#include "mainfrm.h"
#include "editdoc.h"
#include "prodoc.h"
#include "conview.h"
#include "proprog.h"
#include "compile.h"
#include "listen.h"
#include "link.h"
#include "debug.h"
#include "peditvw.h"
//#include "edsplit.h"
#include "project.h"
#include "utils.h"
#include "process.h"
//#include "browbrow.h"
#include "unlock.h"
#include "AmziExcept.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CWideApp

BEGIN_MESSAGE_MAP(CWideApp, CWinApp)
   //{{AFX_MSG_MAP(CWideApp)
   ON_COMMAND(ID_APP_ABOUT, OnAppAbout)
   ON_COMMAND(ID_BUILD_RUN, OnBuildRun)
   ON_COMMAND(ID_BUILD_COMPILE, OnBuildCompile)
   ON_COMMAND(ID_BUILD_LINK, OnBuildLink)
   ON_COMMAND(ID_LISTEN_START, OnListenStart)
   ON_COMMAND(ID_LISTEN_END, OnListenEnd)
   ON_COMMAND(ID_DEBUG_ON, OnDebugOn)
   ON_COMMAND(ID_DEBUG_OFF, OnDebugOff)
   ON_COMMAND(ID_FILE_NEW, OnFileNew)
   ON_COMMAND(ID_FILE_OPEN, OnFileOpen)
   ON_COMMAND(ID_FILE_UNLOCK, OnFileUnlock)
   ON_COMMAND(ID_LISTEN_CONSULT, OnListenConsult)
   ON_COMMAND(ID_LISTEN_RECONSULT, OnListenReconsult)
   ON_COMMAND(ID_BUILD_BUILD, OnBuildBuild)
   ON_COMMAND(ID_PROJECT_NEW, OnProjectNew)
   ON_COMMAND(ID_CLOSE_ALL, OnCloseAll)
   ON_COMMAND(ID_SAVE_ALL, OnSaveAll)
   ON_COMMAND(ID_HELP_INDEX, OnHelpIndex)
   ON_COMMAND(ID_FILE_OPENPROJECT, OnFileOpenproject)
   ON_COMMAND(ID_HELP_PROLOG, OnHelpProlog)
   ON_COMMAND(ID_HELP_LOGICSERVER, OnHelpLogicserver)
   ON_COMMAND(ID_HELP_TUTORIAL, OnHelpTutorial)
   ON_COMMAND(ID_HELP_WIDE, OnHelpWide)
   //ON_COMMAND(ID_HELP_NEWFEATURES, OnHelpNewfeatures)
   //ON_COMMAND(ID_HELP_LICENSE, OnHelpLicense)
   //ON_COMMAND(ID_HELP_TIBICRULES, OnHelpTibicrules)
   //}}AFX_MSG_MAP
   ON_COMMAND_EX_RANGE(ID_MRU_EDIT1, ID_MRU_EDIT10, OnOpenRecentEdit)
   ON_COMMAND_EX_RANGE(ID_MRU_PROJECT1, ID_MRU_PROJECT10, OnOpenRecentProject)
   // Standard file based document commands
   //ON_COMMAND(ID_FILE_OPEN, CWinApp::OnFileOpen)
   // Standard print setup command
   ON_COMMAND(ID_FILE_PRINT_SETUP, CWinApp::OnFilePrintSetup)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CWideApp construction

CWideApp::CWideApp()
{
   // Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CWideApp object

CWideApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CWideApp initialization

BOOL CWideApp::InitInstance()
{
   m_dplateEdit = NULL;
   m_dplateProg = NULL;
   m_dplateListen = NULL;
   m_dplateLink = NULL;
   m_dplateDebug = NULL;
   m_dplateBuild = NULL;
   m_dplateCompile = NULL;

   m_pwp = NULL;
   m_pProjectDoc = NULL;
   m_pListen = NULL;
   m_pListenDoc = NULL;
   m_pListenView = NULL;

   // utility function that figures out if this is 95 or NT, and
   // saves in a global for runtime switches of Unicode non-Unicode
   // library functions.
   get_osver();

   // Initialize global variables
   g_pDebug = NULL;
   
   // Standard initialization
   // If you are not using these features and wish to reduce the size
   //  of your final executable, you should remove from the following
   //  the specific initialization routines you do not need.

   SetDialogBkColor();        // Set dialog background color to gray
   LoadStdProfileSettings(0);  // Load standard INI file options (including MRU)

   // Create a separate MRU lists for projects and files
   m_pProjectMRUList =
      new CRecentFileList(0, _T("Recent Projects"), _T("Project%d"), 5);
   m_pProjectMRUList->ReadList();

   m_pEditMRUList =
      new CRecentFileList(0, _T("Recent Edits"), _T("Edit%d"), 5);
   m_pEditMRUList->ReadList();

   m_bOpeningProject = FALSE;

   // Save the recent file list so we can switch from projects to files
   // in the real m_pRecentFileList which is automatically updated by
   // CDocument.
   //m_pFileListHolder = m_pRecentFileList;

   // Register the application's document templates.  Document templates
   //  serve as the connection between documents, frame windows and views.

   m_dplateBuild = new CMultiDocTemplate(
      IDR_BUILDTYPE,
      RUNTIME_CLASS(CEditDoc),
      RUNTIME_CLASS(CMDIChildWnd),        // standard MDI child frame
      RUNTIME_CLASS(CConView));
   AddDocTemplate(m_dplateBuild);

   m_dplateCompile = new CMultiDocTemplate(
      IDR_COMPILETYPE,
      RUNTIME_CLASS(CProDoc),
      //RUNTIME_CLASS(CMDIChildWnd),        // standard MDI child frame
      RUNTIME_CLASS(CConFrame),
      RUNTIME_CLASS(CConView));
   AddDocTemplate(m_dplateCompile);

   m_dplateEdit = new CMultiDocTemplate(
      IDR_EDITTYPE,
      RUNTIME_CLASS(CEditDoc),
      //RUNTIME_CLASS(CMDIChildWnd),        // standard MDI child frame
      RUNTIME_CLASS(CEditFrame),          // standard MDI child frame
      RUNTIME_CLASS(CPEditView));
   AddDocTemplate(m_dplateEdit);

   m_dplateProg = new CMultiDocTemplate(
      IDR_PROGTYPE,
      RUNTIME_CLASS(CProDoc),
      //RUNTIME_CLASS(CMDIChildWnd),        // standard MDI child frame
      RUNTIME_CLASS(CConFrame),
      RUNTIME_CLASS(CConView));
   AddDocTemplate(m_dplateProg);

   m_dplateListen = new CMultiDocTemplate(
      IDR_LISTENTYPE,
      RUNTIME_CLASS(CProDoc),
      RUNTIME_CLASS(CListenFrame),        // standard MDI child frame
      RUNTIME_CLASS(CListenView));
   AddDocTemplate(m_dplateListen);
   
   m_dplateLink = new CMultiDocTemplate(
      IDR_LINKTYPE,
      RUNTIME_CLASS(CLink),
      RUNTIME_CLASS(CMDIChildWnd),        // standard MDI child frame
      RUNTIME_CLASS(CConView));
   AddDocTemplate(m_dplateLink);
   
   m_dplateDebug = new CMultiDocTemplate(
      IDR_DEBUGTYPE,
      RUNTIME_CLASS(CProDoc),
   //   RUNTIME_CLASS(CMDIChildWnd),        // standard MDI child frame
      RUNTIME_CLASS(CDebugFrame),
      RUNTIME_CLASS(CDebugView));

   m_dplateProject = new CMultiDocTemplate(
      IDR_PROJTYPE,
      RUNTIME_CLASS(CProjectDoc),
      RUNTIME_CLASS(CProjectFrame),
      RUNTIME_CLASS(CProjectView));
   AddDocTemplate(m_dplateProject);

   // create main MDI Frame window
   CMainFrame* pMainFrame = new CMainFrame;
   if (!pMainFrame->LoadFrame(IDR_MAINFRAME))
      return FALSE;
   m_pMainWnd = pMainFrame;
   m_pMFW = pMainFrame;

   // enable file manager drag/drop and DDE Execute open
   EnableShellOpen();
   RegisterShellFileTypes();
   m_pMainWnd->DragAcceptFiles();

   // The main window has been initialized, so show and update it.
   ((CMainFrame*)m_pMainWnd)->InitialShowWindow(m_nCmdShow);
   pMainFrame->UpdateWindow();

   // Parse command line for standard shell commands, DDE, file open
   CCommandLineInfo cmdInfo;
   ParseCommandLine(cmdInfo);

   // Dispatch commands specified on the command line
   if (!ProcessShellCommand(cmdInfo))
      return FALSE;

   // See if this works - MFC allocates this first, and then
   // does a free when cleaning up, so can't just assign constant string to it.
   if (m_pszHelpFilePath != NULL)
      free((void*)m_pszHelpFilePath);
   m_pszHelpFilePath = _tcsdup(_T("amzi.hlp"));

   return TRUE;
}

int CWideApp::ExitInstance()
{
   // delete templates here that were not added with
   // AddDocTemplate
   delete m_dplateDebug;

   m_pProjectMRUList->WriteList();
   delete m_pProjectMRUList;

   m_pEditMRUList->WriteList();
   delete m_pEditMRUList;

   return CWinApp::ExitInstance();
}


void CWideApp::OnProjectNew() 
{
   if (m_pProjectDoc != NULL)
   {
      if ( IDNO == AfxMessageBox(_T("Close Existing Project?"), MB_YESNO) )
         return;
      m_pProjectDoc->Close();
   }
   
   m_pProjectDoc = (CProjectDoc*)(m_dplateProject->OpenDocumentFile(NULL));
}

void CWideApp::OnFileNew()
{
   CEditDoc *pDoc = (CEditDoc*)m_dplateEdit->OpenDocumentFile(NULL);
   pDoc->AugmentTitle();
}

BOOL CWideApp::OnOpenRecentEdit(UINT nID)
{
   // Copied from CWinApp::OnOpenRecentFile in MFC sources
   ASSERT_VALID(this);
   ASSERT(m_pEditMRUList != NULL);

   ASSERT(nID >= ID_MRU_EDIT1);
   ASSERT(nID < ID_MRU_EDIT1 + (UINT)m_pEditMRUList->GetSize());
   int nIndex = nID - ID_MRU_EDIT1;
   ASSERT((*m_pEditMRUList)[nIndex].GetLength() != 0);

   TRACE2("MRU: open file (%d) '%s'.\n", (nIndex) + 1,
         (LPCTSTR)(*m_pEditMRUList)[nIndex]);

   if ( ! WOpen((*m_pEditMRUList)[nIndex]) )
      m_pEditMRUList->Remove(nIndex);

   return TRUE;
}

BOOL CWideApp::OnOpenRecentProject(UINT nID)
{
   // Copied from CWinApp::OnOpenRecentFile in MFC sources
   ASSERT_VALID(this);
   ASSERT(m_pProjectMRUList != NULL);

   ASSERT(nID >= ID_MRU_PROJECT1);
   ASSERT(nID < ID_MRU_PROJECT1 + (UINT)m_pProjectMRUList->GetSize());
   int nIndex = nID - ID_MRU_PROJECT1;
   ASSERT((*m_pProjectMRUList)[nIndex].GetLength() != 0);

   TRACE2("MRU: open file (%d) '%s'.\n", (nIndex) + 1,
         (LPCTSTR)(*m_pProjectMRUList)[nIndex]);

   if ( ! WOpen((*m_pProjectMRUList)[nIndex]) )
      m_pProjectMRUList->Remove(nIndex);

   return TRUE;
}

void CWideApp::OnFileOpen()
{
   CFileDialog *pfileDlg = NULL;

   if (g_osrel >= 4)
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT|OFN_EXPLORER,
         _T("Prolog Files (*.pro)|*.pro|Ini File (*.ini)|*.ini|Text File (*.txt)|*.txt|All (*.*)|*.*||"));
   else
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT,
         _T("Prolog Files (*.pro)|*.pro|Ini File (*.ini)|*.ini|Text File (*.txt)|*.txt|All (*.*)|*.*||"));

   if (pfileDlg->DoModal() != IDOK)
   {
      delete pfileDlg;
      return;
   }

   CString sFile = pfileDlg->GetPathName();
   delete pfileDlg;

   WOpen(sFile);
}

BOOL CWideApp::WOpen(CString sFile)
// Open routine called from OnFileOpen and OnOpenRecentFile.
// Open could be for project or a source file.
{
	if ( sFile.Find(_T(".ppj")) > 0 || sFile.Find(_T(".PPJ")) > 0 )
	{
		CProjectDoc* pDoc = WOpenProject(sFile);
		if (NULL == pDoc)
		{
			return FALSE;
		}
		else
		{
			// Open any edit windows associated with this project
			for (int i = 0; i < pDoc->m_wndOpen.GetSize(); ++i)
			{
				CEditDoc* pEdit = WOpenEdit(pDoc->m_wndOpen[i]);
				if (NULL == pEdit) continue;
				WINDOWPLACEMENT wp = pDoc->m_wndPlacement[i];
				pEdit->GetEditFrame()->SetWindowPlacement(&wp);
			}
		}
		return TRUE;
	}
	else
	{
		if (NULL == WOpenEdit(sFile))
			return FALSE;
		else
			return TRUE;
	}
}

CEditDoc* CWideApp::WOpenEdit(CString sFile)
{
   CEditDoc *pDoc = NULL;
   // First see if the document is already open.
   POSITION posDoc = m_dplateEdit->GetFirstDocPosition();
   while(posDoc != NULL)
   {
      pDoc = (CEditDoc*) m_dplateEdit->GetNextDoc(posDoc);
      if (TRUE == SameFile((const _TCHAR*)sFile, pDoc->GetPathName()))
         break;
      pDoc = NULL;
   }

   // If its not, then open it.
   if (pDoc == NULL)
   {
      pDoc = (CEditDoc*)m_dplateEdit->OpenDocumentFile((const _TCHAR*)sFile);
      if (pDoc == NULL)
         return NULL;
      pDoc->AugmentTitle();
   }

   // We couldn't figure out how to fake out the MFC default MRU
   // processing, so we implement our own for both files and projects.
   if (! m_bOpeningProject)
      m_pEditMRUList->Add(pDoc->GetPathName());

   pDoc->GetEditFrame()->BringWindowToTop();
   if (m_pProjectDoc != NULL)
      m_pProjectDoc->SetProject();

   return pDoc;
}

CProjectDoc* CWideApp::WOpenProject(CString sFile)
{
   if (m_pProjectDoc != NULL)
   {
      if ( IDNO == AfxMessageBox(_T("Close Existing Project?"), MB_YESNO) )
         return NULL;
      m_pProjectDoc->Close();
   }

   //if ( ! ValidProject(sFile) )
   //{
   //   AfxMessageBox(_T("Not a valid Amzi! project for this environment,\n please recreate."));
   //   return NULL;
   //}

   // Set the recent file list to the recent project list, so when
   // CDocument->OpenDocumentFile automatically adjusts it, it gets
   // the project list instead.
   m_bOpeningProject = TRUE;
   m_pProjectDoc =
      (CProjectDoc*)m_dplateProject->OpenDocumentFile((const _TCHAR*)sFile);
   if (m_pProjectDoc == NULL)
      return NULL;
   m_pProjectMRUList->Add(m_pProjectDoc->GetPathName());
   m_bOpeningProject = FALSE;

   // Enforce the project settings, especially after a file open which
   // might have changed the current directory.  The new project is
   // in m_pProjectDoc.
   if (m_pProjectDoc != NULL)
      m_pProjectDoc->SetProject();
   m_pProjectDoc->GetProjectFrame()->
      SetWindowPos(&CWnd::wndTop,0,0,0,0,SWP_NOMOVE|SWP_NOSIZE);
   return m_pProjectDoc;
}

/*
CEditDoc* CWideApp::ReadErrorOpen(CString sFile, long lineno)
// Called when there is a Prolog read error, the lineno is the
// line the error occurred on.
{
   CEditDoc *pDoc = NULL;
   POSITION posDoc = theApp.m_dplateEdit->GetFirstDocPosition();

   while(posDoc != NULL)
   {
      pDoc = (CEditDoc*) theApp.m_dplateEdit->GetNextDoc(posDoc);
      if (TRUE == SameFile((const _TCHAR*)sFile, pDoc->GetPathName()))
         break;
      pDoc = NULL;
   }

   if (pDoc == NULL)
      pDoc = (CEditDoc*)m_dplateEdit->OpenDocumentFile((const _TCHAR*)sFile);

   if (pDoc == NULL)
      return NULL;
   pDoc->AugmentTitle();
   if (! m_bOpeningProject)
      m_pEditMRUList->Add(pDoc->GetPathName());
   pDoc->HighlightLine(lineno);
   return pDoc;
}
*/

BOOL CWideApp::SameFile(const _TCHAR* f1, const _TCHAR* f2)
{
   if (0 == _tcsicmp(f1, f2))
      return TRUE;

   _TCHAR drive1[_MAX_DRIVE], drive2[_MAX_DRIVE];
   _TCHAR dir1[_MAX_DIR], dir2[_MAX_DIR];
   _TCHAR fname1[_MAX_FNAME], fname2[_MAX_FNAME];
   _TCHAR ext1[_MAX_EXT], ext2[_MAX_EXT];

   _tsplitpath(f1, drive1, dir1, fname1, ext1);
   _tsplitpath(f2, drive2, dir2, fname2, ext2);

   int l1, l2, c12;
   l1 = _tcslen(fname1);
   l2 = _tcslen(fname2);
   c12 = _tcsicmp(fname1, fname2);

/* I don't remember why the _tcslen() calls were added,
   so take them out.  They cause failure when an unnamed file,
   such as opened 'new', is compared, because the name does
   have 0 length causing these checks to fail.

   Now I remember, you need the 0 length checks for the drive
   and directory and such, because they might be zero because
   one path is relative and the other absolute. But for fname,
   they both must have one, so no length required on fname, but
   require it for other fields. */

   if ( 0 != _tcsicmp(fname1, fname2) )
      return FALSE;

   if (_tcslen(drive1) != 0 && _tcslen(drive2) != 0
      && _tcsicmp(drive1, drive2) != 0)
      return FALSE;

   if (_tcslen(dir1) != 0 && _tcslen(dir2) != 0
      && _tcsicmp(dir1, dir2) != 0)
      return FALSE;

   if (_tcslen(ext1) != 0 && _tcslen(ext2) != 0
      && _tcsicmp(ext1, ext2) != 0)
      return FALSE;

   return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
   CAboutDlg();

   // Dialog Data
   //{{AFX_DATA(CAboutDlg)
   enum { IDD = IDD_ABOUTBOX };
   //}}AFX_DATA

// Implementation
protected:
   virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
   //{{AFX_MSG(CAboutDlg)
      // No message handlers
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
   //{{AFX_DATA_INIT(CAboutDlg)
   //}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
   CDialog::DoDataExchange(pDX);
   //{{AFX_DATA_MAP(CAboutDlg)
   //}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
   //{{AFX_MSG_MAP(CAboutDlg)
      // No message handlers
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()

// App command to run the dialog
void CWideApp::OnAppAbout()
{
   CAboutDlg aboutDlg;
   aboutDlg.DoModal();
}

/////////////////////////////////////////////////////////////////////////////
// CWideApp commands

void CWideApp::OnBuildRun()
{
#if defined(_DEBUG) && defined(_WIN32)
   // memory leak detection
   CMemoryState oldMemState, newMemState, diffMemState;
   MEMORYSTATUS ms;
   oldMemState.Checkpoint();
   GlobalMemoryStatus(&ms);
   TRACE(_T("Memload: %d, TotalPhys: %d, TotalAvail: %d\n"),
         ms.dwMemoryLoad, ms.dwTotalPhys, ms.dwAvailPhys);

#endif
   CString newName;
   CProDoc *pProDoc;
   CDocTemplate* pTemplate = m_dplateProg;
   
   // quitely return if already running (to prevent double
   // clicks quicker than menus updated)

   //if (m_pMFW->Running()) return;
   
   // prompt the user for .xpl file

   if (m_pProjectDoc == NULL)
   {
      if (!DoPromptFileName(newName, AFX_IDS_OPENFILE,
      OFN_HIDEREADONLY | OFN_FILEMUSTEXIST, TRUE, pTemplate))
         return; // open cancelled
   }
   else
   {
      newName = m_pProjectDoc->GetXPLFile();
   }
      
   ASSERT(pTemplate != NULL);
   ASSERT(pTemplate->IsKindOf(RUNTIME_CLASS(CDocTemplate)));

   // open the prolog program document

   pProDoc = (CProDoc*)(pTemplate->OpenDocumentFile(newName));
   if (pProDoc == NULL) return;

   // get the view associated with the document

   POSITION pos = pProDoc->GetFirstViewPosition();
   CConView* pCV = (CConView*)pProDoc->GetNextView(pos);
   pProDoc->SetModifiedFlag(FALSE);

   // create the Prolog program, associate it with the view and document,
   // initialize and run it

   m_pMFW->RunningOn();
   CProProg aProProg;

   if (FALSE == aProProg.Initialize(newName))
   {
      m_pMFW->RunningOff();
      return;
   }
   aProProg.AttachView(pCV);
   pProDoc->AttachProg(&aProProg);
   
   if (TRUE == aProProg.Run())
      pCV->PutS(_T("\nProgram Succeeded\n"));
   else
      pCV->PutS(_T("\nProgram Failed\n"));
   
   //aProProg.Close();   // closed when aProProg deleted
   pProDoc->SetModifiedFlag(FALSE);
   m_pMFW->RunningOff();
#if defined(_DEBUG) && defined(_WIN32)
   // memory leak detection
   GlobalMemoryStatus(&ms);
   TRACE(_T("Memload: %d, TotalPhys: %d, TotalAvail: %d\n"),
         ms.dwMemoryLoad, ms.dwTotalPhys, ms.dwAvailPhys);
   newMemState.Checkpoint();
   if (diffMemState.Difference(oldMemState, newMemState))
   {
      TRACE(_T("Memory leak!\n"));
      diffMemState.DumpStatistics();
   }
   else
   {
      TRACE(_T("No memory leaks here\n"));
   }
#endif
}

void CWideApp::OnBuildCompile()
{
   CProDoc *pProDoc;
//   CDocTemplate* pTemplate = m_dplateProg;
   
   SaveOpenFiles();
   
   //ASSERT(pTemplate != NULL);
   //ASSERT(pTemplate->IsKindOf(RUNTIME_CLASS(CDocTemplate)));

   //pProDoc = (CProDoc*)(pTemplate->OpenDocumentFile(NULL));
   //_TCHAR str_compiler[512];
   //int rc = LoadString(theApp.m_hInstance, IDS_COMPILER, str_compiler, 512);
   //pProDoc->SetTitle(str_compiler);

   pProDoc = (CProDoc*) GetDocument(IDS_COMPILER, theApp.m_dplateCompile, TRUE);
   if (pProDoc == NULL) return;
   pProDoc->SetModifiedFlag(FALSE);

   // get the view associated with the document

   POSITION pos = pProDoc->GetFirstViewPosition();
   CConView* pCV = (CConView*)pProDoc->GetNextView(pos);

   // initialize compiler, attach view, and run
   
   m_pMFW->RunningOn();
   CCompile theCompiler;

   if (FALSE == theCompiler.Initialize(_T("acmp.xpl")))
   {
      pProDoc->SetModifiedFlag(FALSE);
      pProDoc->OnCloseDocument();
      m_pMFW->RunningOff();
      return;
   }
   theCompiler.AttachView(pCV);
   pProDoc->AttachProg(&theCompiler);
   //int yn;
   //   if (TRUE == theCompiler.Run())
   //      yn = AfxMessageBox(_T("Compiler Succeeded\nKeep Window Open?"), MB_YESNO);
   //   else
   //      yn = AfxMessageBox(_T("Compiler Failed\nKeep Window Open?"), MB_YESNO);
   theCompiler.Run();
   pProDoc->SetModifiedFlag(FALSE);
   //if (yn == IDNO)
   //   pProDoc->OnCloseDocument();
   m_pMFW->RunningOff();
}

void CWideApp::OnBuildLink()
{
   CLink* pLink;
   //CDocTemplate* pTemplate = m_dplateLink;
   //
   //ASSERT(pTemplate != NULL);
   //ASSERT(pTemplate->IsKindOf(RUNTIME_CLASS(CDocTemplate)));

   //pLink = (CLink*)(pTemplate->OpenDocumentFile(NULL));
   //_TCHAR str_linker[512];
   //int rc = LoadString(theApp.m_hInstance, IDS_LINKER, str_linker, 512);
   //pLink->SetTitle(str_linker);
   //if (pLink == NULL) return;
   //pLink->SetModifiedFlag(FALSE);

   pLink = (CLink*) GetDocument(IDS_LINKER, theApp.m_dplateLink, TRUE);
   if (pLink == NULL) return;

   // get the view associated with the document

   POSITION pos = pLink->GetFirstViewPosition();
   CConView* pCV = (CConView*)pLink->GetNextView(pos);
   if (m_pProjectDoc == NULL)
      pLink->Run(pCV);
   else
      pLink->LinkProject(pCV);

   pLink->SetModifiedFlag(FALSE);

   //if (IDNO == AfxMessageBox(_T("Linker done, keep window open?"), MB_YESNO))
   //      pLink->OnCloseDocument();
}

void CWideApp::OnBuildBuild() 
{
//   CDocTemplate* pTemplate;
   CString sFile;

   CFileDialog *pfileDlg = NULL;
   // Get project file name
   if (m_pProjectDoc == NULL)
   {
      if (g_osrel >= 4)
         pfileDlg = new CFileDialog(TRUE, NULL, NULL,
            OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT|OFN_EXPLORER,
            _T("Prolog Project (*.ppj)|*.ppj|All (*.*)|*.*||"));
      else
         pfileDlg = new CFileDialog(TRUE, NULL, NULL,
            OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT,
            _T("Prolog Project (*.ppj)|*.ppj|All (*.*)|*.*||"));

      if (pfileDlg->DoModal() != IDOK)
      {
         delete pfileDlg;
         return;
      }
      sFile = pfileDlg->GetPathName();
      delete pfileDlg;
   }

   //-----------------------
   // Set up compiler
   //

//   pTemplate = m_dplateProg;
   
   SaveOpenFiles();
   
//   ASSERT(pTemplate != NULL);
//   ASSERT(pTemplate->IsKindOf(RUNTIME_CLASS(CDocTemplate)));

   // attempt to reuse existing compiler window.
   CProDoc *pProDoc = (CProDoc*) GetDocument(IDS_BUILDER, theApp.m_dplateBuild, TRUE);
   if (pProDoc == NULL) return;

   pProDoc->SetModifiedFlag(FALSE);

   // get the view associated with the document

   POSITION pos;
   CConView* pCV;

   pos = pProDoc->GetFirstViewPosition();
   pCV = (CConView*)pProDoc->GetNextView(pos);

   // initialize compiler, attach view, and run
   
   m_pMFW->RunningOn();

   CCompile *theCompiler = new CCompile();

   if (FALSE == theCompiler->Initialize(_T("acmp.xpl")))
   {
      pProDoc->SetModifiedFlag(FALSE);
      pProDoc->OnCloseDocument();
      m_pMFW->RunningOff();
      return;
   }
   theCompiler->AttachView(pCV);
   //pProDoc->AttachProg(&theCompiler);
   pProDoc->AttachProg(theCompiler);

   BOOL bRes;
   if (m_pProjectDoc == NULL)
   {
      bRes = theCompiler->Run(sFile);
   }
   else
   {
      bRes = theCompiler->CompileProject();
   }
      
   pProDoc->SetModifiedFlag(FALSE);
   //if (yn == IDNO)
   //   pProDoc->OnCloseDocument();
   m_pMFW->RunningOff();

   delete theCompiler;   // so linker can use logic server as well


   if (!bRes) return;

   //if (yn == FALSE)
   //   return;

   //------------------------
   // Set up Linker
   //

   CLink Linker;

   if (m_pProjectDoc == NULL)
   {
      Linker.Run(pCV, sFile);
   }
   else
      Linker.LinkProject(pCV);

   pCV->PutS(_T("\nBuild done\n"));
   pProDoc->SetModifiedFlag(FALSE);

   //if (IDNO == AfxMessageBox(_T("Build done, keep window open?"), MB_YESNO))
   //      pProDoc->OnCloseDocument();
}


void CWideApp::OnListenStart()
{
   CProDoc *pProDoc;
   CDocTemplate* pTemplate = m_dplateListen;
   
   // prevent second opening when double click is quicker
   // than menu can be disabled
   
   if (m_pMFW->Running()) return;
   
   ASSERT(pTemplate != NULL);
   ASSERT(pTemplate->IsKindOf(RUNTIME_CLASS(CDocTemplate)));

   pProDoc = (CProDoc*)(pTemplate->OpenDocumentFile(NULL));
   _TCHAR str_listener[512];
   int rc = LoadString(theApp.m_hInstance, IDS_LISTENER, str_listener, 512);
   pProDoc->SetTitle(str_listener);
   if (pProDoc == NULL) return;
   pProDoc->SetModifiedFlag(FALSE);
   m_pListenDoc = pProDoc;

   // get the view associated with the document
   POSITION pos = pProDoc->GetFirstViewPosition();
   CListenView* pLV = (CListenView*)pProDoc->GetNextView(pos);
   m_pListenView = pLV;
   
   // initialize listener, attach view, and wait   
   m_pListen = new CListen;

   if (FALSE == m_pListen->Initialize(_T("aidl.xpl")))
   {
      delete m_pListen;
      m_pListen = NULL;
      pProDoc->OnCloseDocument();
      return;
   }
   m_pListen->AttachView(pLV);
   pProDoc->AttachProg(m_pListen);
   
   m_pMFW->ListenerOn();
   m_pListen->Start();
      
   pProDoc->SetModifiedFlag(FALSE);
}

void CWideApp::OnListenEnd()
{
   if (m_pMFW->PrologGet())
   {
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return;
   }

   m_pListen->Stop();
   if (m_pMFW->Debugging())
   {
      // Do everything OnDebugOff() does, except make the
      // call to Prolog to turn off debugging, because
      // we're leaving Prolog anyway, and it might be
      // because of an abort.

      m_pMFW->DebuggerOff();
      m_pDebugFrame->SendMessage(WM_CLOSE);
   }

   //m_pListen->Close();  // closed by delete
   delete m_pListen;
   m_pListen = NULL;
   m_pMFW->ListenerOff();
   
   m_pListenDoc->SetModifiedFlag(FALSE);
   m_pListenDoc->OnCloseDocument();
}


void CWideApp::OnListenConsult()
{
   SaveOpenFiles();
   CFrameWnd* pParentFrame;
   pParentFrame = m_pListenView->GetParentFrame();
   pParentFrame->ActivateFrame();
   pParentFrame->SetActiveView(m_pListenView);
   m_pListenView->ListenConsult();
}


void CWideApp::OnListenReconsult()
{
   SaveOpenFiles();
   CFrameWnd* pParentFrame;
   pParentFrame = m_pListenView->GetParentFrame();
   pParentFrame->ActivateFrame();
   pParentFrame->SetActiveView(m_pListenView);
   if (m_pProjectDoc == NULL)
      m_pListenView->ListenReconsult();
   else
      m_pListenView->ReconsultProject();
}

void CWideApp::OnDebugOn()
{
   CFrameWnd* pFW;
   pFW = m_dplateDebug->CreateNewFrame(m_pListenDoc, NULL);
   if (pFW == NULL) { AfxMessageBox(_T("Debug frame failed")); return; }
   m_dplateDebug->InitialUpdateFrame(pFW, m_pListenDoc, TRUE);
   pFW->SetWindowText(_T("Debugger"));

   POSITION pos = m_pListenDoc->GetFirstViewPosition();
   CView* pV;
   pV = m_pListenDoc->GetNextView(pos);
   pV = m_pListenDoc->GetNextView(pos);
   m_pDebugView = (CDebugView*)pV;
   m_pDebugFrame = pFW;

   m_pMFW->DebuggerOn();
   m_pListenView->Debug();
   m_pListenView->SetFocus();
}

void CWideApp::OnDebugOff()
{
   m_pMFW->DebuggerOff();
//   m_pListenView->NoDebug();
   m_pDebugFrame->SendMessage(WM_CLOSE);
}

void CWideApp::OnHelpIndex() 
{
   BrowseDoc(_T("amzidoc.htm"));
}

void CWideApp::BrowseDoc(_TCHAR* cDoc)
{
   // Get the full path to the docs subdirectory.  The input
   // cDoc to this function is a relative path to that directory.
   _TCHAR* sPath = new _TCHAR[_MAX_PATH];
   _TCHAR* p;
   GetModuleFileName(NULL, sPath, _MAX_PATH);
   p = _tcsstr(sPath, _T("\\bin\\"));
   if (p == NULL)
      p = _tcsstr(sPath, _T("\\BIN\\"));
   if (p == NULL)
      p = _tcsstr(sPath, _T("\\Bin\\"));
   if (p == NULL)
   {
      AfxMessageBox(_T("Unable to find Amzi! bin directory"));
      delete[] sPath;
      return;
   }
   *p = EOS;
   _tcscat(sPath, _T("\\docs\\"));
   _tcscat(sPath, cDoc);

   if ((long) ShellExecute(NULL, _T("open"), sPath, _T(""), _T(""), SW_SHOWNORMAL) <= 32)
      AfxMessageBox(_T("There is no default browser for .HTM files"));

   delete[] sPath;
}

/*
void CWideApp::OnChangebrowser() 
{
   CBrowseBrowser bb;
   bb.DoModal();
   if (m_sBrowser.IsEmpty())
      return;
   else
      WriteBrowser();
   
}
void CWideApp::ReadBrowser()
{
   m_sBrowser = GetProfileString(_T("Settings"), _T("Browser"));
   if (m_sBrowser.IsEmpty())
   {
      CBrowseBrowser bb;
      bb.DoModal();
      if (m_sBrowser.IsEmpty())
         return;
      else
         WriteBrowser();
   }
}

void CWideApp::WriteBrowser()
{
   WriteProfileString(_T("Settings"), _T("Browser"), m_sBrowser);
}
*/

////////////////////////////////////////////////////////////
// save and restore windows .ini settings
// Helpers for saving/restoring window state
// Sample code from MFC Superpad sample application
//

_TCHAR szFormat[] = _T("%u,%u,%d,%d,%d,%d,%d,%d,%d,%d");

BOOL CWideApp::ReadWindowPlacement(LPWINDOWPLACEMENT pwp,
      _TCHAR * psSection, _TCHAR * psWindowPos)
{
   CString strBuffer = GetProfileString(psSection, psWindowPos);
   if (strBuffer.IsEmpty())
      return FALSE;

   WINDOWPLACEMENT wp;
//   int nRead = _stscanf(strBuffer, szFormat,
   int nRead = _stscanf(strBuffer, szFormat,
      &wp.flags, &wp.showCmd,
      &wp.ptMinPosition.x, &wp.ptMinPosition.y,
      &wp.ptMaxPosition.x, &wp.ptMaxPosition.y,
      &wp.rcNormalPosition.left, &wp.rcNormalPosition.top,
      &wp.rcNormalPosition.right, &wp.rcNormalPosition.bottom);

   if (nRead != 10)
      return FALSE;

   wp.length = sizeof wp;
   *pwp = wp;
   return TRUE;
}

void CWideApp::WriteWindowPlacement(LPWINDOWPLACEMENT pwp,
      _TCHAR * psSection, _TCHAR * psWindowPos)
// write a window placement to settings section of app's ini file
{
   _TCHAR szBuffer[sizeof(_T("-32767"))*8 + sizeof(_T("65535"))*2];

   wsprintf(szBuffer, szFormat,
      pwp->flags, pwp->showCmd,
      pwp->ptMinPosition.x, pwp->ptMinPosition.y,
      pwp->ptMaxPosition.x, pwp->ptMaxPosition.y,
      pwp->rcNormalPosition.left, pwp->rcNormalPosition.top,
      pwp->rcNormalPosition.right, pwp->rcNormalPosition.bottom);
   WriteProfileString(psSection, psWindowPos, szBuffer);
}

void CWideApp::SaveOpenFiles()
{
   POSITION posDoc = theApp.m_dplateEdit->GetFirstDocPosition();
   CDocument* pDoc;

   if (theApp.m_pMFW->PrologGet())
   {
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return;
   }

   while(posDoc != NULL)
   {
      pDoc = theApp.m_dplateEdit->GetNextDoc(posDoc);
      if (pDoc->IsModified())
         pDoc->DoSave(pDoc->GetPathName());
   }
}

void CWideApp::CheckChangedFiles()
// Files might have been changed outside of the editor, for example
// when another application has been used for awhile, or the listener
// or build functions modified a file.  Call from CMainFrame::OnActivateApp().
{
   POSITION posDoc = theApp.m_dplateEdit->GetFirstDocPosition();
   CEditDoc* pDoc;
   CFileStatus fstat;
   _TCHAR buf[256];
   CFile * fp;
   _TCHAR fname[312];

   while(posDoc != NULL)
   {
      pDoc = (CEditDoc*) theApp.m_dplateEdit->GetNextDoc(posDoc);
      // Check all document current update stamps with saved ones,
      // if any have been changed, reload them.  Note that for this
      // to work, CEditDoc::Serialize must save the time/date stamp for
      // each file when it is saved and when it is loaded.

      CFile::GetStatus(pDoc->GetPathName(), fstat);
      if (pDoc->m_timestamp > 0 && fstat.m_mtime > pDoc->m_timestamp)
      {
         _tcscpy(fname, pDoc->GetPathName());
         _stprintf(buf, _T("File %s has been modified,\nupdate editor?"),
            fname);
         if (IDYES == AfxMessageBox(buf, MB_YESNO))
         {
            fp = new CFile(pDoc->GetPathName(), CFile::modeRead);
            pDoc->Serialize( CArchive( fp, CArchive::load ));
            fp->Close();
            delete fp;
         }
      }
   }
}

void CWideApp::CloseOpenFiles()
{
   POSITION posDoc = theApp.m_dplateEdit->GetFirstDocPosition();
   CDocument* pDoc;

   if (theApp.m_pMFW->PrologGet())
   {
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return;
   }

   while(posDoc != NULL)
   {
      pDoc = theApp.m_dplateEdit->GetNextDoc(posDoc);
      pDoc->OnCloseDocument();
   }
}

void CWideApp::OnCloseAll() 
{
   CloseOpenFiles();
}

void CWideApp::OnSaveAll() 
{
   SaveOpenFiles();
}

void CWideApp::OnFileUnlock() 
{
   //_TCHAR buf[_MAX_PATH];
   //_TCHAR *p;

   //GetModuleFileName(NULL, buf, _MAX_PATH);
   //p = _tcsrchr(buf, '\\');
   //*++p = 0;
   //_tcscat(buf, _T("license.txt"));
   //WOpenEdit(buf);

	ShellExecute(NULL, NULL, _T("activate.exe"), NULL, NULL, SW_SHOWNORMAL);

	//CUnlock *ud = new CUnlock(NULL);
   //ud->Create(IDD_UNLOCK, NULL);
	return;
}

void CWideApp::OnFileOpenproject() 
{
   CFileDialog *pfileDlg = NULL;

   if (g_osrel >= 4)
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT|OFN_EXPLORER,
         _T("Prolog Projects (*.ppj)|*.ppj|All (*.*)|*.*||"));
   else
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT,
         _T("Prolog Projects (*.ppj)|*.ppj|All (*.*)|*.*||"));

   if (pfileDlg->DoModal() != IDOK)
   {
      delete pfileDlg;
      return;
   }

   CString sFile = pfileDlg->GetPathName();
   delete pfileDlg;

   WOpen(sFile);
}


void CWideApp::OnHelpProlog() 
{
   BrowseDoc(_T("pro\\prfrtop.htm"));
}

void CWideApp::OnHelpLogicserver() 
{
   BrowseDoc(_T("ls\\lsfrtop.htm"));
}

void CWideApp::OnHelpTutorial() 
{
   BrowseDoc(_T("aip\\advfrtop.htm"));
}

void CWideApp::OnHelpWide() 
{
   BrowseDoc(_T("pro\\pug_wide.htm"));
}

/*void CWideApp::OnHelpLicense() 
{
   BrowseDoc(_T("license.htm"));
}

void CWideApp::OnHelpNewfeatures() 
{
   BrowseDoc(_T("whatsnew.htm"));   
}*/



CDocument* CWideApp::GetDocument(UINT docID, CDocTemplate* pTemplate, BOOL bCreate)
{
	ASSERT(pTemplate != NULL);
	ASSERT(pTemplate->IsKindOf(RUNTIME_CLASS(CDocTemplate)));

	// catch an unlikely error condition (ok, so it just happened which is why this check is now here!)
	if (pTemplate == NULL) return NULL;

	_TCHAR str_builder[512];
	int rc = LoadString(theApp.m_hInstance, docID, str_builder, 512);

	POSITION posDoc = pTemplate->GetFirstDocPosition();
	CDocument* pDoc;

	while(posDoc != NULL)
	{
		pDoc = pTemplate->GetNextDoc(posDoc);
		if (pDoc->GetTitle().Compare(str_builder) == 0) return pDoc;
	}

	// not found, create and return a new window if required
	if (bCreate)
	{
		pDoc = pTemplate->OpenDocumentFile(NULL);
		if (pDoc)
		{
			pDoc->SetTitle(str_builder);
			pDoc->SetModifiedFlag(FALSE);
		}
	}
	return pDoc;
}
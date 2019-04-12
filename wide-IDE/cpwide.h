// cpwide.h : main header file for the CPWIDE application
//

#ifndef __AFXWIN_H__
   #error include 'stdafx.h' before including this file for PCH
#endif

/////////////////////////////////////////////////////////////////////////////
// CWideApp:
// See cpwide.cpp for the implementation of this class
//
class CWideApp;

extern CWideApp theApp;

class CMainFrame;
class CListen;
class CListenView;
class CProDoc;
class CDebugView;
class CEditDoc;
class CProjectDoc;

class CWideApp : public CWinApp
{
public:
   CDocTemplate* m_dplateEdit;
   CDocTemplate* m_dplateProg;
   CDocTemplate* m_dplateListen;
   CDocTemplate* m_dplateCompile;
   CDocTemplate* m_dplateLink;
   CDocTemplate* m_dplateBuild;
   CDocTemplate* m_dplateDebug;
   CDocTemplate* m_dplateProject;
   
   CMainFrame*   m_pMFW;
   CListen*      m_pListen;
   CProDoc*      m_pListenDoc;
   CListenView*  m_pListenView;
   CDebugView*   m_pDebugView;
   CFrameWnd*    m_pDebugFrame;
   CProjectDoc*  m_pProjectDoc;

   WINDOWPLACEMENT *m_pwp;  // set briefly while projects open edit windows

   CRecentFileList *m_pProjectMRUList;
   CRecentFileList *m_pEditMRUList;
   BOOL  m_bOpeningProject;

   CString  m_sBrowser; // Internet browser

public:
   CWideApp();
   BOOL ReadWindowPlacement(LPWINDOWPLACEMENT pwp,
      _TCHAR * psSection, _TCHAR * psWindowPos);
   void WriteWindowPlacement(LPWINDOWPLACEMENT pwp,
      _TCHAR * psSection, _TCHAR * psWindowPos);
   void BrowseDoc(_TCHAR* cDoc);
   void ReadBrowser();
   void WriteBrowser();
   BOOL WOpen(CString sFile);
   CProjectDoc* WOpenProject(CString sFile);
   CEditDoc* WOpenEdit(CString sFile);
   //CEditDoc* ReadErrorOpen(CString sFile, long lineno);
   BOOL SameFile(const _TCHAR* f1, const _TCHAR* f2);
   BOOL OnOpenRecentProject(UINT nID);
   BOOL OnOpenRecentEdit(UINT nID);
   void SaveOpenFiles();
   void CheckChangedFiles();
   void CloseOpenFiles();
   CDocument* GetDocument(UINT docID, CDocTemplate* pTemplate, BOOL bCreate = TRUE);
private:

// Overrides
   virtual BOOL InitInstance();
   virtual int ExitInstance();

// Implementation

   //{{AFX_MSG(CWideApp)
   afx_msg void OnAppAbout();
   afx_msg void OnBuildRun();
   afx_msg void OnBuildCompile();
   afx_msg void OnBuildLink();
   afx_msg void OnListenStart();
   afx_msg void OnListenEnd();
   afx_msg void OnDebugOn();
   afx_msg void OnDebugOff();
   afx_msg void OnFileNew();
   afx_msg void OnFileOpen();
	afx_msg void OnFileUnlock();
   afx_msg void OnListenConsult();
   afx_msg void OnListenReconsult();
   afx_msg void OnBuildBuild();
   afx_msg void OnProjectNew();
   afx_msg void OnCloseAll();
   afx_msg void OnSaveAll();
   afx_msg void OnHelpIndex();
   afx_msg void OnFileOpenproject();
   afx_msg void OnHelpProlog();
   afx_msg void OnHelpLogicserver();
   afx_msg void OnHelpTutorial();
   afx_msg void OnHelpWide();
   //afx_msg void OnHelpNewfeatures();
   //afx_msg void OnHelpLicense();
   //afx_msg void OnHelpTibicrules();
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

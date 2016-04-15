// mainfrm.h : interface of the CMainFrame class
//
/////////////////////////////////////////////////////////////////////////////

class CMainFrame : public CMDIFrameWnd
{
   DECLARE_DYNAMIC(CMainFrame)
public:
   BOOL m_bLink;            // on if linker dll present
   BOOL m_bCompile;         // on if acmp.xpl present
   BOOL m_bListenerOn;
   BOOL m_bDebuggerOn;
   BOOL m_bRunning;
   BOOL m_bPrologGet;
   BOOL m_bWaiting;
   BOOL m_bUnlocked;
   CFont m_font;
   LOGFONT m_lfDefFont;
   LOGFONT m_lfDefFontOld;
   
public:
   CMainFrame();
   void InitialShowWindow(UINT nCmdShow);

// Operations
public:
   void ListenerOn()
      { m_bListenerOn = TRUE; m_bRunning = TRUE; }
   void ListenerOff()
      { m_bListenerOn = FALSE; m_bRunning = FALSE; }
   void DebuggerOn()   { m_bDebuggerOn = TRUE; }
   void DebuggerOff()  { m_bDebuggerOn = FALSE; }
   void RunningOn()    { m_bRunning = TRUE; }
   void RunningOff()   { m_bRunning = FALSE; }
   BOOL Running()      { return(m_bRunning); }
   BOOL Debugging()    { return(m_bDebuggerOn); }
   BOOL Listening()    { return(m_bListenerOn); }
   void PrologGetOn()  { m_bPrologGet = TRUE; }
   void PrologGetOff() { m_bPrologGet = FALSE; }
   BOOL PrologGet()    { return(m_bPrologGet); }
   void WaitingOn()    { BeginWaitCursor(); m_bWaiting = TRUE;  }
   void WaitingOff()   { m_bWaiting = FALSE; EndWaitCursor(); }
   
// Implementation
public:
   virtual ~CMainFrame();
#ifdef _DEBUG
   virtual void AssertValid() const;
   virtual void Dump(CDumpContext& dc) const;
#endif

protected:  // control bar embedded members
   CStatusBar  m_wndStatusBar;
   CToolBar    m_wndToolBar;

// Generated message map functions
protected:
   //{{AFX_MSG(CMainFrame)
   afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
   afx_msg void OnClose();
   afx_msg void OnUpdateBuildLink(CCmdUI* pCmdUI);
   afx_msg void OnUpdateBuildCompile(CCmdUI* pCmdUI);
   afx_msg void OnUpdateListenEnd(CCmdUI* pCmdUI);
   afx_msg void OnUpdateDebugOn(CCmdUI* pCmdUI);
   afx_msg void OnUpdateDebugOff(CCmdUI* pCmdUI);
   afx_msg void OnUpdateListenOpenlog(CCmdUI* pCmdUI);
   afx_msg void OnUpdateListenCloselog(CCmdUI* pCmdUI);
   afx_msg void OnUpdateListenConsult(CCmdUI* pCmdUI);
   afx_msg void OnUpdateListenReconsult(CCmdUI* pCmdUI);
   afx_msg void OnUpdateBuildRun(CCmdUI* pCmdUI);
   afx_msg void OnUpdateListenStart(CCmdUI* pCmdUI);
   afx_msg void OnViewSetFont();
   afx_msg void OnFileSave();
   afx_msg void OnFileClose();
   afx_msg void OnUpdateBuildBuild(CCmdUI* pCmdUI);
   afx_msg void OnUpdateMruProject1(CCmdUI* pCmdUI);
   afx_msg void OnUpdateMruEdit1(CCmdUI* pCmdUI);
   afx_msg BOOL OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);
   afx_msg void OnUpdateFileUnlock(CCmdUI* pCmdUI);
   afx_msg void OnActivateApp(BOOL bActive, DWORD hTask);
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

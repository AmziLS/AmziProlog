//---------------------------------------------------------------------
// listen.h - header for Amzi! compiler
//

const int MAX_QUERY = 512;

class CDebugView;
extern CDebugView* g_pDebug;

class CListen : public CProProg
{
private:
   BOOL  m_env_loaded;
      
public:
   CListen();
   ~CListen();
   
   virtual BOOL Initialize(const _TCHAR* pszPathName);
   void Start();
   void Stop();
   int SilentCall(_TCHAR* sFunctor, _TCHAR* sArg);
   int SilentCall(_TCHAR* sQuery);
   int LCall(_TCHAR * sQuery);

   // Overrides
};

/////////////////////////////////////////////////////////////////////////////
// CListenFrame frame

class CListenFrame : public CMDIChildWnd
{
   DECLARE_DYNCREATE(CListenFrame)
protected:
   CListenFrame();         // protected constructor used by dynamic creation

// Attributes
public:

// Operations
public:

// Implementation
protected:
   virtual ~CListenFrame();
   virtual BOOL PreCreateWindow(CREATESTRUCT &cs);
   virtual BOOL DestroyWindow();

   // Generated message map functions
   //{{AFX_MSG(CListenFrame)
   afx_msg void OnClose();
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// CListenView view

class CListenView;

class CListenView : public CConView
{
   DECLARE_DYNCREATE(CListenView)
public:
   CListenView();         // protected constructor used by dynamic creation
   virtual ~CListenView();

// Attributes
public:
   CString m_sConsultFile;

// Operations
public:
   void Debug();
   void NoDebug();
   void ListenConsult();
   void ListenReconsult();
   void docall(_TCHAR* query);
   void ReconsultProject();
   void Prompt();

// Implementation
protected:
   virtual   void OnDraw(CDC* pDC);      // overridden to draw this view

// Overrides
public:
   void OnLineRead();
  
private:
   int lastchar(_TCHAR* s);
   
   // Generated message map functions
protected:
   //{{AFX_MSG(CListenView)
   afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
   afx_msg void OnEditChange();
   afx_msg void OnSetFocus(CWnd* pOldWnd);
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

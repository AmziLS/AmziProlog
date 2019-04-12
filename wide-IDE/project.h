// Project.h - Project document, view & frame classes
//

#ifndef __AFXEXT_H__
#include <afxext.h>
#endif

// Used by serialization routines, needed
// because Unicode writes don't put in 13.
const _TCHAR CRLF[3] = {13,10,0};

// error codes returned by serialize
#define PPJERR_UNICODE -1
#define PPJERR_NOT40   -2

/////////////////////////////////////////////////////////////////////////////
// CProjectFrame frame

class CProjectFrame : public CMDIChildWnd
{
   DECLARE_DYNCREATE(CProjectFrame)
protected:
   CProjectFrame();           // protected constructor used by dynamic creation

// Attributes
public:

// Operations
public:

// Overrides
   // ClassWizard generated virtual function overrides
   //{{AFX_VIRTUAL(CProjectFrame)
   protected:
   virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
   //}}AFX_VIRTUAL

// Implementation
protected:
   virtual ~CProjectFrame();

   // Generated message map functions
   //{{AFX_MSG(CProjectFrame)
   afx_msg void OnClose();
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// CProjectView form view


class CProjectView : public CFormView
{
protected:
   CProjectView();           // protected constructor used by dynamic creation
   DECLARE_DYNCREATE(CProjectView)

// Form Data
public:
   //{{AFX_DATA(CProjectView)
   enum { IDD = IDP_PROJECT };
   CString   m_xplfile;
   CString   m_directory;
   CString   m_opdefs;
   //}}AFX_DATA

// Attributes
public:

// Operations
public:

// Overrides
   // ClassWizard generated virtual function overrides
   //{{AFX_VIRTUAL(CProjectView)
   public:
   virtual void OnInitialUpdate();
   protected:
   virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
   virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
   //}}AFX_VIRTUAL

// Implementation
public:
      void update_scroll(CListBox* pLB, CString path);

protected:
   virtual ~CProjectView();
#ifdef _DEBUG
   virtual void AssertValid() const;
   virtual void Dump(CDumpContext& dc) const;
#endif

   // Generated message map functions
   //{{AFX_MSG(CProjectView)
   afx_msg void OnFileadd();
   afx_msg void OnFiledel();
   afx_msg void OnLibadd();
   afx_msg void OnLibdel();
   afx_msg void OnDblclkFilelist();
   afx_msg void OnProplm();
   afx_msg void OnKillfocusDirectory();
   afx_msg void OnKillfocusOpdefs();
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// CProjectDoc document

class CProjectDoc : public CDocument
{
protected:
   CProjectDoc();           // protected constructor used by dynamic creation
   DECLARE_DYNCREATE(CProjectDoc)

// Attributes
public:
   WINDOWPLACEMENT *m_pwp;  // temporarily set for creating edit windows
   CString m_amzirel;   // keep this here to test for valid projects

// Operations
public:

// Overrides
   // ClassWizard generated virtual function overrides
   //{{AFX_VIRTUAL(CProjectDoc)
   public:
   virtual void Serialize(CArchive& ar);   // overridden for document i/o
   virtual void OnCloseDocument();
   protected:
   virtual BOOL OnNewDocument();
   //}}AFX_VIRTUAL

// Implementation
public:
   virtual ~CProjectDoc();
   CProjectView* GetProjectView();
   CProjectFrame* GetProjectFrame();
   void Close();
   void Save();
   void SaveOpenFiles(CArchive&);
   void CloseAllFiles();
   void SetProject();
   void ReadArchive(CArchive &ar);
   char* ReadLineA(char * buf, CArchive &ar, int max);
   wchar_t* ReadLineW(wchar_t* buf, CArchive &ar, int max);
   void ProcessLine(CString s, CProjectView* pview);
   int GetFileCount();
   int GetLibCount();
   CString GetFile(int);
   CString GetLib(int);
   CString GetXPLFile();
   CString GetOpDefsFile();
#ifdef _DEBUG
   virtual void AssertValid() const;
   virtual void Dump(CDumpContext& dc) const;
#endif

   CStringArray m_wndOpen;
   CArray<WINDOWPLACEMENT, WINDOWPLACEMENT> m_wndPlacement;

   // Generated message map functions
protected:
   //{{AFX_MSG(CProjectDoc)
   afx_msg void OnFileSave();
   afx_msg void OnFileSaveAs();
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

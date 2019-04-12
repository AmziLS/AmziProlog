// editdoc.h : interface of the CEditDoc class
//
/////////////////////////////////////////////////////////////////////////////

class CPEditView;
class CEditFrame;

// File types, correspond to entries in the saveas file dialog box
// for the editor.

#define FILE_ERROR            0
#define FILE_ASCII            1
#define FILE_ULITTLEEND       2
#define FILE_UBIGEND          3
#define FILE_ULITTLEENDNOTAG  4
#define FILE_UBIGENDNOTAG     5

class CEditDoc : public CDocument
{
protected: // create from serialization only
   CEditDoc();
   DECLARE_DYNCREATE(CEditDoc)

// Attributes
public:
   BOOL  m_btitst;  // true if title has been starred
   int   m_nFileType;   // The file type chosen in a saveas dialog box
   CTime m_timestamp;   // When the file was last saved

// Operations
public:

// Implementation
public:
   virtual ~CEditDoc();
   virtual void Serialize(CArchive& ar);   // overridden for document i/o
   void DeleteContents();
   void AugmentTitle();
#ifdef _DEBUG
   virtual void AssertValid() const;
   virtual void Dump(CDumpContext& dc) const;
#endif

   int GetFileType()
   {   return m_nFileType; }
   void SetFileType(int i)
   {   m_nFileType = i; }
   virtual BOOL DoSave(LPCTSTR lpszPathName, BOOL bReplace = TRUE);
   BOOL CDocumentDoSave(LPCTSTR lpszPathName, BOOL bReplace);
   BOOL CDocManagerDoPromptFileName(CString& fileName, UINT nIDSTitle,
      DWORD lFlags, BOOL bOpenFileDialog, CDocTemplate* pTemplate);
   CPEditView* GetPEditView();
   CEditFrame* GetEditFrame();
   void HighlightLine(long lineno);
//virtual BOOL DoFileSave();

protected:
   virtual BOOL OnNewDocument();
   virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);

// Generated message map functions
protected:
   //{{AFX_MSG(CEditDoc)
   afx_msg void OnFileClose();
   afx_msg void OnFileSave();
   afx_msg void OnFileSaveAs();
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
// CEditFrame frame


class CEditFrame : public CMDIChildWnd
{
   DECLARE_DYNCREATE(CEditFrame)
protected:
   CEditFrame();           // protected constructor used by dynamic creation

// Attributes
public:

// Operations
public:

// Overrides
   // ClassWizard generated virtual function overrides
   //{{AFX_VIRTUAL(CEditFrame)
   protected:
   virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
   //}}AFX_VIRTUAL

// Implementation
protected:
   virtual ~CEditFrame();

   // Generated message map functions
   //{{AFX_MSG(CEditFrame)
   afx_msg void OnClose();
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

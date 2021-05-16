// peditvw.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CPEditView view


class CPEditView : public CEditView
{
protected:
   CPEditView();           // protected constructor used by dynamic creation
   DECLARE_DYNCREATE(CPEditView)

// Attributes
public:
   CFont   m_font;
   CString m_title;
   CString m_title_star;
   BOOL    m_btitst;

// Operations
public:

   void DeduceFileType(CFile*, DWORD);

// Overrides
   // ClassWizard generated virtual function overrides
   //{{AFX_VIRTUAL(CPEditView)
   public:
   virtual void Serialize(CArchive& ar);
   protected:
   virtual void OnDraw(CDC* pDC);      // overridden to draw this view
   virtual void OnEditChange();
   virtual void OnActivateView(BOOL bActivate, CView* pActivateView, CView* pDeactiveView);
   //}}AFX_VIRTUAL

// Hand coded overrides
   void WriteToArchive(CArchive& ar);
   void ReadFromArchive(CArchive& ar, UINT nBytes);

// Implementation
protected:
   virtual ~CPEditView();
#ifdef _DEBUG
   virtual void AssertValid() const;
   virtual void Dump(CDumpContext& dc) const;
#endif

   // Generated message map functions
protected:
   //{{AFX_MSG(CPEditView)
   afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

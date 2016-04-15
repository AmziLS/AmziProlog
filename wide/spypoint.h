// spypoint.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CSpypoints dialog

class CSpypoints : public CDialog
{
private:
   CListBox* m_plbPred;
   CListBox* m_plbSpy;

// Construction
public:
   CSpypoints(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
   //{{AFX_DATA(CSpypoints)
   enum { IDD = IDD_SPYPOINTS };
   //}}AFX_DATA

// Implementation
protected:
   virtual void DoDataExchange(CDataExchange* pDX);   // DDX/DDV support

   // Generated message map functions
   //{{AFX_MSG(CSpypoints)
   afx_msg void OnSpyClear();
   afx_msg void OnSpyDelete();
   virtual void OnCancel();
   virtual void OnOK();
   afx_msg void OnSpyAdd();
   afx_msg void OnDblclkPredicates();
   afx_msg void OnDblclkSpypoints();
   virtual BOOL OnInitDialog();
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

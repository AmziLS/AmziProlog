// browbrow.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CBrowseBrowser dialog

class CBrowseBrowser : public CDialog
{
// Construction
public:
   CBrowseBrowser(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
   //{{AFX_DATA(CBrowseBrowser)
   enum { IDD = IDD_BROWSEBROWSER };
      // NOTE: the ClassWizard will add data members here
   //}}AFX_DATA


// Overrides
   // ClassWizard generated virtual function overrides
   //{{AFX_VIRTUAL(CBrowseBrowser)
   protected:
   virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
   //}}AFX_VIRTUAL

// Implementation
protected:

   // Generated message map functions
   //{{AFX_MSG(CBrowseBrowser)
   afx_msg void OnBrowsebrowser();
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

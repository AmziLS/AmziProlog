// Unlock.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CUnlock dialog

class CUnlock : public CDialog
{
private:
   BOOL m_bWaiting;

// Construction
public:
   CUnlock(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
   //{{AFX_DATA(CUnlock)
   enum { IDD = IDD_UNLOCK };
   CString   m_username;
   CString   m_organization;
   CString   m_serialno;
   CString   m_unlockcode;
   //}}AFX_DATA


// Overrides
   // ClassWizard generated virtual function overrides
   //{{AFX_VIRTUAL(CUnlock)
   protected:
   virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
   virtual void PostNcDestroy();
   //}}AFX_VIRTUAL

// Implementation
protected:

   // Generated message map functions
   //{{AFX_MSG(CUnlock)
   virtual void OnOK();
   virtual BOOL OnInitDialog();
   afx_msg BOOL OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);
   virtual void OnCancel();
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

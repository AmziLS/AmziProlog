// AmziExcept.h : header file
//

#define MAX_MSG          1024
#define MAX_ERR_MSG       256
#define MAX_ERR_FILENAME  256
#define MAX_ERR_READBUF   512
#define MAX_ERR_CALLSTACK 512

/////////////////////////////////////////////////////////////////////////////
// CAmziExcept dialog

class CAmziExcept : public CDialog
{
// Construction
public:
   CAmziExcept(CWnd* pParent = NULL);   // standard constructor
   int SetException(CLSException &E);

private:
   void SetMessage(_TCHAR*);
   void SetReadBuffer(_TCHAR*);
   void SetCallStack(_TCHAR*);
   void HideReadBuffer();

public:

// Dialog Data
   //{{AFX_DATA(CAmziExcept)
   enum { IDD = IDD_EXCEPT };
      // NOTE: the ClassWizard will add data members here
   //}}AFX_DATA


// Overrides
   // ClassWizard generated virtual function overrides
   //{{AFX_VIRTUAL(CAmziExcept)
   public:
   protected:
   virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
   //}}AFX_VIRTUAL

// Implementation
protected:

   // Generated message map functions
   //{{AFX_MSG(CAmziExcept)
      // NOTE: the ClassWizard will add member functions here
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

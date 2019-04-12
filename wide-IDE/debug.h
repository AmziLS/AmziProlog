// debug.h - The Prolog debugger
//

#ifndef DEBUG_H
#define DEBUG_H
/////////////////////////////////////////////////////////////////////////////
// CDebugFrame frame

class CDebugFrame : public CMDIChildWnd
{
   DECLARE_DYNCREATE(CDebugFrame)
protected:
   CDebugFrame();           // protected constructor used by dynamic creation

// Attributes
public:

// Operations
public:
   virtual BOOL PreCreateWindow(CREATESTRUCT &cs);
   virtual BOOL DestroyWindow();

// Overrides
   // ClassWizard generated virtual function overrides
   //{{AFX_VIRTUAL(CDebugFrame)
   //}}AFX_VIRTUAL

// Implementation
protected:
   virtual ~CDebugFrame();

   // Generated message map functions
   //{{AFX_MSG(CDebugFrame)
   afx_msg void OnClose();
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// CDebugView form view

#ifndef __AFXEXT_H__
#include <afxext.h>
#endif

const int MAXLINES = 500;
const int DELTALINES = 50;

class CDebugView : public CFormView
{
   DECLARE_DYNCREATE(CDebugView)
public:
   BOOL m_bResp; // checked by debugfr.OnClose
private:
   int  m_iResp;
   int  m_iLB;
   
   void shift_control(int id, int dx);

protected:
   CDebugView();         // protected constructor used by dynamic creation

public:
   _TCHAR m_sQuery[256];
   //{{AFX_DATA(CDebugView)
   enum { IDD = IDD_DEBUG };
   CEdit	m_LB;
   CButton	m_Creep;
   BOOL   m_bCall;
   BOOL   m_bExit;
   BOOL   m_bFail;
   BOOL   m_bRedo;
   int      m_iFormat;
   //}}AFX_DATA
   BOOL   m_bFormat;

// Attributes
public:

// Operations
public:
   void PutS(_TCHAR * s);
   int  GetResp();
   void OnInitialUpdate();
   BOOL FormatLines() { return m_bFormat; }

// Implementation
protected:
   virtual ~CDebugView();
   virtual void DoDataExchange(CDataExchange* pDX);   // DDX/DDV support
   // Generated message map functions
   //{{AFX_MSG(CDebugView)
   afx_msg void OnCreep();
   afx_msg void OnFail();
   afx_msg void OnLeap();
   afx_msg void OnSkip();
   afx_msg void OnSize(UINT nType, int cx, int cy);
   afx_msg void OnClose();
   afx_msg void OnLeashCall();
   afx_msg void OnLeashExit();
   afx_msg void OnLeashFail();
   afx_msg void OnLeashRedo();
   afx_msg void OnStop();
   afx_msg void OnSpy();
   afx_msg void OnSelchangeTrace();
   afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
   afx_msg void OnFon();
   afx_msg void OnFoff();
   //}}AFX_MSG
//   afx_msg void OnClose();     // override to shutdown debugger
   
   DECLARE_MESSAGE_MAP()
};

#endif // DEBUG_H
/////////////////////////////////////////////////////////////////////////////

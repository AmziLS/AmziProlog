// dbgendlg.h : header file
//

#include "progene.h"

/////////////////////////////////////////////////////////////////////////////
// CDbgeneDlg dialog

class CDbgeneDlg : public CDialog
{
public:
// The Class that encapsulates the Prolog Logic Server
// and the specific genealogical Prolog application
	CProGene proGene;

// Construction
	CDbgeneDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CDbgeneDlg)
	enum { IDD = IDD_DBGENE_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDbgeneDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;

	// Generated message map functions
	//{{AFX_MSG(CDbgeneDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnQuery();
	afx_msg void OnHelpb();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

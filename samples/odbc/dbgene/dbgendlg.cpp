// DBGENE - Implementation file for the dialog
//          boxes.  OnInitDialog and OnQuery
//          methods interact with the Logic Server.
//

#include "stdafx.h"
#include "dbgene.h"
#include "dbgendlg.h"
//#include "winuser.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

// Implementation
protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//{{AFX_MSG(CAboutDlg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg message handlers

BOOL CAboutDlg::OnInitDialog()
// The query dialog is initialized by calling the Prolog program
// object to fill in the relations and individuals list boxes.
{
	CDialog::OnInitDialog();
	CenterWindow();

	return TRUE;  // return TRUE  unless you set the focus to a control
}

/////////////////////////////////////////////////////////////////////////////
// CDbgeneDlg dialog

CDbgeneDlg::CDbgeneDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CDbgeneDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CDbgeneDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CDbgeneDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CDbgeneDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CDbgeneDlg, CDialog)
	//{{AFX_MSG_MAP(CDbgeneDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(IDC_QUERY, OnQuery)
	ON_BN_CLICKED(IDC_HELPB, OnHelpb)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDbgeneDlg message handlers

BOOL CDbgeneDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
	CenterWindow();

	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);

	CMenu* pSysMenu = GetSystemMenu(FALSE);
	CString strAboutMenu;
	strAboutMenu.LoadString(IDS_ABOUTBOX);
	if (!strAboutMenu.IsEmpty())
	{
		pSysMenu->AppendMenu(MF_SEPARATOR);
		pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
	}
	
// The query dialog is initialized by calling the Prolog program
// object to fill in the relations and individuals list boxes.

	CListBox *lb;
	if (!proGene.Open("gene")) return FALSE;
	lb = (CListBox*)GetDlgItem(IDC_INDIVIDUAL);
	if (!proGene.Persons(lb)) return FALSE;
 	lb = (CListBox*)GetDlgItem(IDC_RELATIONSHIP);
	if (!proGene.Relationships(lb)) return FALSE;
	
	return TRUE;  // return TRUE  unless you set the focus to a control
}

void CDbgeneDlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == IDM_ABOUTBOX)
	{
		CAboutDlg dlgAbout;
		dlgAbout.DoModal();
	}
	else
	{
		CDialog::OnSysCommand(nID, lParam);
	}
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CDbgeneDlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

// The system calls this to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CDbgeneDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

void CDbgeneDlg::OnQuery() 
// The query is executed by getting the name and relationship from
// their respective list boxes and then calling the Prolog program
// to fill in the answers list box.
{
   CListBox *lbNames, *lbRelations, *lbAnswers;
   char name[NAMELEN];
   char relation[NAMELEN];
   int  iName, iRelation;

   lbAnswers = (CListBox *)GetDlgItem(IDC_RELATIVES);
   lbRelations = (CListBox *)GetDlgItem(IDC_RELATIONSHIP);
   lbNames = (CListBox *)GetDlgItem(IDC_INDIVIDUAL);

   lbAnswers->ResetContent();

   iName = lbNames->GetCurSel();
   if (iName == LB_ERR) {
      AfxMessageBox("No name selected");
      return;
      }
   lbNames->GetText(iName, name);

   iRelation = lbRelations->GetCurSel();
   if (iRelation == LB_ERR) {
      AfxMessageBox("No relation selected");
      return;
      }
   lbRelations->GetText(iRelation, relation);

   proGene.Answers(lbAnswers, relation, name);
}

void CDbgeneDlg::OnHelpb() 
{
   if ((long) ShellExecute(NULL, "open", "doc.html", "", "", SW_SHOWNORMAL) <= 32)
	   AfxMessageBox("There is no default browser for .HTM files");

}

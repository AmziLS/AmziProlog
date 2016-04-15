// browbrow.cpp : implementation file
//

#include "stdafx.h"
#include "cpwide.h"
#include "resource.h"
#include "browbrow.h"
#include "utils.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CBrowseBrowser dialog


CBrowseBrowser::CBrowseBrowser(CWnd* pParent /*=NULL*/)
   : CDialog(CBrowseBrowser::IDD, pParent)
{
   //{{AFX_DATA_INIT(CBrowseBrowser)
      // NOTE: the ClassWizard will add member initialization here
   //}}AFX_DATA_INIT
}


void CBrowseBrowser::DoDataExchange(CDataExchange* pDX)
{
   CDialog::DoDataExchange(pDX);
   //{{AFX_DATA_MAP(CBrowseBrowser)
      // NOTE: the ClassWizard will add DDX and DDV calls here
   //}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CBrowseBrowser, CDialog)
   //{{AFX_MSG_MAP(CBrowseBrowser)
   ON_BN_CLICKED(IDC_BROWSEBROWSER, OnBrowsebrowser)
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CBrowseBrowser message handlers

void CBrowseBrowser::OnBrowsebrowser() 
{
   CFileDialog *pfileDlg = NULL;
   // Get the browser .exe
   if (g_osrel >= 4)
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT|OFN_EXPLORER,
         _T("Executables .exe (*.exe)|*.exe|All (*.*)|*.*||"));
   else
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT,
         _T("Executables .exe (*.exe)|*.exe|All (*.*)|*.*||"));

   if (pfileDlg->DoModal() != IDOK)
   {
      delete pfileDlg;
      return;
   }
   theApp.m_sBrowser = pfileDlg->GetPathName();

   delete pfileDlg;
}

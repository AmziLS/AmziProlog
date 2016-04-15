// project.cpp - Project document, view and frame implementation
//

#include "stdafx.h"
#include "afxadv.h"
#include "cpwide.h"
#include "resource.h"
#include "project.h"
#include "cpwin.h"    // to get slashslash utility function
#include "mainfrm.h"
#include "editdoc.h"
#include "utils.h"
#include "conview.h"
#include "proprog.h"
#include "listen.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CProjectFrame

IMPLEMENT_DYNCREATE(CProjectFrame, CMDIChildWnd)

CProjectFrame::CProjectFrame()
{
}

CProjectFrame::~CProjectFrame()
{
}

BEGIN_MESSAGE_MAP(CProjectFrame, CMDIChildWnd)
   //{{AFX_MSG_MAP(CProjectFrame)
   ON_WM_CLOSE()
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CProjectFrame message handlers

BOOL CProjectFrame::PreCreateWindow(CREATESTRUCT& cs) 
{
   cs.x = 0;
   cs.y = 0;
   cs.cx = 270;
   cs.cy = 415;
   return CFrameWnd::PreCreateWindow(cs);
}

void CProjectFrame::OnClose() 
// Save the project before closing
{
//   theApp.m_pProjectDoc->Save();
   CMDIChildWnd::OnClose();
//   theApp.m_pProjectDoc = NULL;
}


/////////////////////////////////////////////////////////////////////////////
// CProjectView

IMPLEMENT_DYNCREATE(CProjectView, CFormView)

CProjectView::CProjectView()
   : CFormView(CProjectView::IDD)
{
   //{{AFX_DATA_INIT(CProjectView)
   m_xplfile = _T("");
   m_directory = _T("");
   m_opdefs = _T("");
   //}}AFX_DATA_INIT
}

CProjectView::~CProjectView()
{
}

void CProjectView::DoDataExchange(CDataExchange* pDX)
{
   CFormView::DoDataExchange(pDX);
   //{{AFX_DATA_MAP(CProjectView)
   DDX_Text(pDX, IDP_XPLFILE, m_xplfile);
   DDX_Text(pDX, IDP_DIRECTORY, m_directory);
   DDX_Text(pDX, IDP_OPDEFS, m_opdefs);
   //}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CProjectView, CFormView)
   //{{AFX_MSG_MAP(CProjectView)
   ON_BN_CLICKED(IDP_FILEADD, OnFileadd)
   ON_BN_CLICKED(IDP_FILEDEL, OnFiledel)
   ON_BN_CLICKED(IDP_LIBADD, OnLibadd)
   ON_BN_CLICKED(IDP_LIBDEL, OnLibdel)
   ON_LBN_DBLCLK(IDP_FILELIST, OnDblclkFilelist)
   ON_BN_CLICKED(IDP_PROPLM, OnProplm)
   ON_EN_KILLFOCUS(IDP_DIRECTORY, OnKillfocusDirectory)
   ON_EN_KILLFOCUS(IDP_OPDEFS, OnKillfocusOpdefs)
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CProjectView diagnostics

#ifdef _DEBUG
void CProjectView::AssertValid() const
{
   CFormView::AssertValid();
}

void CProjectView::Dump(CDumpContext& dc) const
{
   CFormView::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CProjectView message handlers

void CProjectView::OnInitialUpdate() 
{
   CFormView::OnInitialUpdate();
}

void CProjectView::OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint) 
{
   // Use DDX to move variables to dialog
   UpdateData(FALSE);
   ((CProjectDoc*)GetDocument())->SetProject();
}

void CProjectView::OnFileadd() 
{
   // Start in the project directory
   ((CProjectDoc*)GetDocument())->SetProject();
   _TCHAR* FileBuffer = new _TCHAR[4096];

   CFileDialog *pfileDlg = NULL;

   if (g_osrel >= 4)
   {
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT|OFN_ALLOWMULTISELECT|OFN_EXPLORER,
         _T("Prolog Files (*.pro;*.plm)|*.pro;*.plm|All (*.*)|*.*||"));
   }
   else
   {
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT|OFN_ALLOWMULTISELECT,
         _T("Prolog Files (*.pro;*.plm)|*.pro;*.plm|All (*.*)|*.*||"));
   }

   pfileDlg->m_ofn.lpstrFile = FileBuffer;
   FileBuffer[0] = EOS;  // Note, must do this or W95 flags as error
   pfileDlg->m_ofn.nMaxFile = 4096;

   if (pfileDlg->DoModal() != IDOK)
   {
      //_TCHAR errbuf[512];
      //wsprintf(errbuf, _T("FileDialog error: %d"), CommDlgExtendedError());
      //AfxMessageBox(errbuf);
      delete pfileDlg;
      return;
   }

   CListBox* pLB = (CListBox*)GetDlgItem(IDP_FILELIST);

   // Loop through all the files to be added. If they are in the project
   // directory or below, just put in the relative path.
   POSITION pos = pfileDlg->GetStartPosition();
   CString dir(m_directory), path, upath;
   dir.MakeUpper();
   int i, idx;
   while (pos != NULL)
   {
      path = pfileDlg->GetNextPathName(pos);
      upath = path;
      upath.MakeUpper();
      if (dir.Right(1) != _T("\\"))
         dir = dir + _T("\\");
      i = upath.Find(dir);
      if (i >= 0)
         path = path.Mid(i+dir.GetLength());
      idx = pLB->AddString(path);
      update_scroll(pLB, path); 
   }
   delete FileBuffer;
   delete pfileDlg;

   ((CProjectDoc*)GetDocument())->SetProject();
}

void CProjectView::update_scroll(CListBox* pLB, CString path)
{
   CDC* pDC = GetDC();
   CSize sz = pDC->GetOutputTextExtent(path);
   pLB->SetHorizontalExtent(sz.cx);
}

void CProjectView::OnFiledel() 
{
   int nItem;
   CListBox* pLB = (CListBox*)GetDlgItem(IDP_FILELIST);
   nItem = pLB->GetCurSel();
   pLB->DeleteString(nItem);
}

void CProjectView::OnProplm() 
{
   int nItem;
   CListBox* p_projlist;
   _TCHAR buf[_MAX_PATH];
   
   p_projlist = (CListBox*)GetDlgItem(IDP_FILELIST);
   nItem = p_projlist->GetCurSel();
   p_projlist->GetText(nItem, buf);
   p_projlist->DeleteString(nItem);
   if (chk_ext(buf, _T(".PLM")))
      force_ext(buf, _T(".PRO"));
   else
      force_ext(buf, _T(".PLM"));
   p_projlist->AddString(buf);
}

void CProjectView::OnLibadd() 
{
   _TCHAR* FileBuffer = new _TCHAR[4096];

   CFileDialog *pfileDlg = NULL;

   if (g_osrel >= 4)
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT|OFN_ALLOWMULTISELECT|OFN_EXPLORER,
         _T("Prolog Files (*.pro;*.plm)|*.pro;*.plm|All (*.*)|*.*||"));
   else
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT|OFN_ALLOWMULTISELECT,
         _T("Prolog Files (*.pro;*.plm)|*.pro;*.plm|All (*.*)|*.*||"));

   pfileDlg->m_ofn.lpstrFile = FileBuffer;
   FileBuffer[0] = EOS;  // Note, must do this or W95 flags as error
   pfileDlg->m_ofn.nMaxFile = 4096;

   if (pfileDlg->DoModal() != IDOK)
   {
      delete pfileDlg;
      return;
   }

   CListBox* pLB = (CListBox*)GetDlgItem(IDP_LIBLIST);

   POSITION pos = pfileDlg->GetStartPosition();
   CString text;
   while (pos != NULL)
   {
      text = pfileDlg->GetNextPathName(pos);
      pLB->AddString(text);
      update_scroll(pLB, text);
   }
   delete FileBuffer;
   delete pfileDlg;

   ((CProjectDoc*)GetDocument())->SetProject();
}

void CProjectView::OnLibdel() 
{
   int nItem;
   CListBox* pLB = (CListBox*)GetDlgItem(IDP_LIBLIST);
   nItem = pLB->GetCurSel();
   pLB->DeleteString(nItem);
}

void CProjectView::OnDblclkFilelist()
// Open the file in the editor when double clicked
{
   CListBox* pLB = (CListBox*)GetDlgItem(IDP_FILELIST);
   int i = pLB->GetCurSel();
   CString sFile;
   pLB->GetText(i, sFile);
   theApp.WOpen(sFile);
}

void CProjectView::OnKillfocusDirectory() 
{
   ((CProjectDoc*)GetDocument())->SetProject();
}

void CProjectView::OnKillfocusOpdefs() 
{
   ((CProjectDoc*)GetDocument())->SetProject();
}


/////////////////////////////////////////////////////////////////////////////
// CProjectDoc

IMPLEMENT_DYNCREATE(CProjectDoc, CDocument)

CProjectDoc::CProjectDoc()
{
   m_pwp = NULL;
   theApp.m_pProjectDoc = this;
}

BOOL CProjectDoc::OnNewDocument()
{
   if (!CDocument::OnNewDocument())
      return FALSE;
   return TRUE;
}

CProjectDoc::~CProjectDoc()
{
}

BEGIN_MESSAGE_MAP(CProjectDoc, CDocument)
   //{{AFX_MSG_MAP(CProjectDoc)
   ON_COMMAND(ID_FILE_SAVE, OnFileSave)
   ON_COMMAND(ID_FILE_SAVE_AS, OnFileSaveAs)
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CProjectDoc diagnostics

#ifdef _DEBUG
void CProjectDoc::AssertValid() const
{
   CDocument::AssertValid();
}

void CProjectDoc::Dump(CDumpContext& dc) const
{
   CDocument::Dump(dc);
}
#endif //_DEBUG

CProjectView* CProjectDoc::GetProjectView()
{
   POSITION pos = GetFirstViewPosition();
   if (pos == NULL)
      return NULL;
   else
      return (CProjectView*)GetNextView(pos);
}

CProjectFrame* CProjectDoc::GetProjectFrame()
{
   return (CProjectFrame*)(GetProjectView()->GetParent());
}

void CProjectDoc::SetProject()
// Change application settings based on current project.
{
   GetProjectView()->UpdateData();
   CString dir = GetProjectView()->m_directory;
   if (dir.IsEmpty())
      return;
   if (FALSE == SetCurrentDirectory(dir))
      AfxMessageBox(_T("Unable to change to directory,\nmaybe missing drive specification."));
}


int CProjectDoc::GetFileCount()
{
   CListBox* pLB = (CListBox*)GetProjectView()->GetDlgItem(IDP_FILELIST);
   return pLB->GetCount();
}

int CProjectDoc::GetLibCount()
{
   CListBox* pLB = (CListBox*)GetProjectView()->GetDlgItem(IDP_LIBLIST);
   return pLB->GetCount();
}

CString CProjectDoc::GetFile(int i)
{
   CString sFile;
   // Consider file -1 a special case for getting the opdefs file
   if (i == -1)
      sFile = GetProjectView()->m_opdefs;
   else
   {
      CListBox* pLB = (CListBox*)GetProjectView()->GetDlgItem(IDP_FILELIST);
      pLB->GetText(i, sFile);
   }
   return sFile;
}

CString CProjectDoc::GetLib(int i)
{
   CString sLib;
   CListBox* pLB = (CListBox*)GetProjectView()->GetDlgItem(IDP_LIBLIST);
   pLB->GetText(i, sLib);
   return sLib;
}

CString CProjectDoc::GetXPLFile()
{
   return GetProjectView()->m_xplfile;
}

CString CProjectDoc::GetOpDefsFile()
{
   return GetProjectView()->m_opdefs;
}

/////////////////////////////////////////////////////////////////////////////
// CProjectDoc serialization


void CProjectDoc::Serialize(CArchive& ar)
{
   CProjectView* pview = GetProjectView();
   int i, l;

   if (ar.IsStoring())
   {  // Output
#ifdef _UNICODE
      _TCHAR uniflag = 0xfeff;  // written as fffe, which is right for PC unicode
      ar.Write(&uniflag, 2);
#endif
      pview->UpdateData(TRUE);
      ar.WriteString(_T("amzirel=4"));  // so we can check for valid ppj files
      ar.WriteString(CRLF);
      ar.WriteString(_T("xplfile="));
      ar.WriteString(pview->m_xplfile);
      ar.WriteString(CRLF);
      ar.WriteString(_T("directory="));
      ar.WriteString(pview->m_directory);
      ar.WriteString(CRLF);
      ar.WriteString(_T("opdefs="));
      ar.WriteString(pview->m_opdefs);
      ar.WriteString(CRLF);

      CListBox* pLB;
      CString s;

      pLB = (CListBox*)(pview->GetDlgItem(IDP_FILELIST));
      l = pLB->GetCount();
      for (i=0; i<l; i++)
      {
         pLB->GetText(i, s);
         ar.WriteString(_T("file="));
         ar.WriteString(s);
         ar.WriteString(CRLF);
      }

      pLB = (CListBox*)(pview->GetDlgItem(IDP_LIBLIST));
      l = pLB->GetCount();
      for (i=0; i<l; i++)
      {
         pLB->GetText(i, s);
         ar.WriteString(_T("library="));
         ar.WriteString(s);
         ar.WriteString(CRLF);
      }

      SaveOpenFiles(ar);
   }
   else
   {  // Input
      ReadArchive(ar);
   }
}

void CProjectDoc::SaveOpenFiles(CArchive &ar)
// Unlike CWideApp::SaveOpenFiles, this version saves them
// remembers their positions and closes them.  It is called
// when the project is being closed.
{
   if (theApp.m_pMFW->PrologGet())
   {
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return;
   }

   _TCHAR szFormat[] = _T("%u,%u,%d,%d,%d,%d,%d,%d,%d,%d");
   _TCHAR szBuffer[sizeof(_T("-32767"))*8 + sizeof(_T("65535"))*2];

   POSITION posDoc = theApp.m_dplateEdit->GetFirstDocPosition();
   CEditDoc* pEditDoc;
   CEditFrame* pEditFr;
   WINDOWPLACEMENT wp;
   wp.length = sizeof wp;

   while(posDoc != NULL)
   {
      pEditDoc = (CEditDoc*)theApp.m_dplateEdit->GetNextDoc(posDoc);
      if (pEditDoc->IsModified())
         pEditDoc->DoSave(pEditDoc->GetPathName());

	  CString pathName = pEditDoc->GetPathName();
	  if (pathName.IsEmpty()) continue;

      pEditFr = pEditDoc->GetEditFrame();

      pEditFr->GetWindowPlacement(&wp);
      wsprintf(szBuffer, szFormat,
         wp.flags, wp.showCmd,
         wp.ptMinPosition.x, wp.ptMinPosition.y,
         wp.ptMaxPosition.x, wp.ptMaxPosition.y,
         wp.rcNormalPosition.left, wp.rcNormalPosition.top,
         wp.rcNormalPosition.right, wp.rcNormalPosition.bottom);
      ar.WriteString( pathName );
      ar.WriteString(_T("="));
      ar.WriteString(szBuffer);
      ar.WriteString(CRLF);

      //pEditFr->SendMessage(WM_CLOSE);
   }
}

void CProjectDoc::CloseAllFiles()
{
   if (theApp.m_pMFW->PrologGet())
   {
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return;
   }

   POSITION posDoc = theApp.m_dplateEdit->GetFirstDocPosition();
   CEditDoc* pEditDoc;
   CEditFrame* pEditFr;

   while(posDoc != NULL)
   {
      pEditDoc = (CEditDoc*)theApp.m_dplateEdit->GetNextDoc(posDoc);
      pEditFr = pEditDoc->GetEditFrame();

      pEditFr->SendMessage(WM_CLOSE);
   }
}

void CProjectDoc::ReadArchive(CArchive &ar)
{
   CProjectView* pview = GetProjectView();
   CString s;
   unsigned short first;
   BOOL is_unicode;
   int ibufsize = 512;
   pview->UpdateData(FALSE);

   // Read first two bytes to check for Unicode,
   // now we can't undo with archive, so this presents
   // a problem when reading the first line and there
   // wasn't a unicode flag.  The answer is in ProcessLine
   // which accepts 'zirel' as well as 'amzirel' for the
   // first line attribute.
   ar.Read(&first, 2);
   if (first == 0xfeff)
      is_unicode = TRUE;
   else
      is_unicode = FALSE;

#ifdef _UNICODE
   // if unicode, just read it straight
   if (is_unicode)
   {
      while (ar.ReadString(s))
         ProcessLine(s, pview);
   }
   // if not unicode, read into an ascii buffer
   // and call mbstowcs().
   else
   {
      char* buf = new char[ibufsize+1];
      _TCHAR* sbuf = new _TCHAR[ibufsize+1];
      while (ReadLineA(buf, ar, ibufsize))
      {
         mbstowcs(sbuf, buf, ibufsize);
         s = sbuf;
         ProcessLine(s, pview);
      }
      delete[] buf;
      delete[] sbuf;
   }
#else
   // if not unicode, read it straight
   if (!is_unicode)
   {
      while (ar.ReadString(s))
         ProcessLine(s, pview);
   }
   // if unicode, get ReadLineW to read and
   // translate.
   else
   {
      wchar_t* buf = new wchar_t[ibufsize+1];
      _TCHAR* sbuf = new _TCHAR[ibufsize+1];
      while (ReadLineW(buf, ar, ibufsize))
      {
         wcstombs(sbuf, buf, ibufsize);
         s = buf;
         ProcessLine(s, pview);
      }
      delete[] buf;
      delete[] sbuf;
   }
#endif

   if (! m_amzirel == CString(_T("4")))
      AfxMessageBox(_T("Not a valid Amzi! project for this environment,\n please recreate."));

   return;
}

char* CProjectDoc::ReadLineA(char * buf, CArchive &ar, int max)
{
   char* p = buf;
   int   i;
   int   cnt = 0;

   while (1 == (i = ar.Read(p, 1)) && *p++ != 0x0d && cnt++ < max)
      ;

   if (i == 0 || cnt >= max)
      *p = 0;
   else
      // read the LF after the CR and replace both with EOS
   {
      ar.Read(p, 1);
      *--p = 0;
   }

   if (p > buf)
      return buf;
   else if (i == 1)
      return buf;
   else
      return NULL;
}

wchar_t* CProjectDoc::ReadLineW(wchar_t* buf, CArchive &ar, int max)
{
   int   i;
   int   cnt = 0;
   wchar_t* p = buf;

   while (2 == (i = ar.Read(p, 2)) && *p++ != 0x000d && cnt++ < max)
      ;

   if (i < 2 || cnt >= max)
      *p = 0;
   else
   // read the LF after the CR and replace both with EOS
   {
      ar.Read(p, 2);
      *--p = 0;
   }

   if (p > buf)
      return buf;
   else if (i == 2)
      return buf;
   else
      return NULL;
}

void CProjectDoc::ProcessLine(CString s, CProjectView* pview)
{
   CString attr, val;
   CListBox* pLB;

   _TCHAR szFormat[] = _T("%u,%u,%d,%d,%d,%d,%d,%d,%d,%d");

   int l = s.GetLength();
   int i = s.Find(_T('='));

   attr = s.Left(i);
   val  = s.Right(l-i-1);


   if (attr == _T("amzirel"))  // just checking
      m_amzirel = val;
   // First two bytes were read for unicode test, but
   // may have been chars, so take zirel for amzirel as well.
   else if (attr == _T("zirel"))
      m_amzirel = val;
   else if (attr == _T("xplfile"))
      pview->m_xplfile = val;
   else if (attr == _T("directory"))
      pview->m_directory = val;
   else if (attr == _T("opdefs"))
      pview->m_opdefs = val;
   else if (attr == _T("file"))
   {
      pLB = (CListBox*)(pview->GetDlgItem(IDP_FILELIST));
      pLB->AddString(val);
      pview->update_scroll(pLB, val);
   }
   else if (attr == _T("library"))
   {
      pLB = (CListBox*)(pview->GetDlgItem(IDP_LIBLIST));
      pLB->AddString(val);
      pview->update_scroll(pLB, val);
   }
   else
   {
		// make a new copy of the windowplacement structure for this window
		WINDOWPLACEMENT wp;
		memset(&wp, 0, sizeof(wp));
		wp.length = sizeof(wp);
		_stscanf(val, szFormat,
			&wp.flags, &wp.showCmd,
			&wp.ptMinPosition.x, &wp.ptMinPosition.y,
			&wp.ptMaxPosition.x, &wp.ptMaxPosition.y,
			&wp.rcNormalPosition.left, &wp.rcNormalPosition.top,
			&wp.rcNormalPosition.right, &wp.rcNormalPosition.bottom);

		// add the filename to the list of edit files to open
		m_wndOpen.Add(attr);

		// add the associated window position information
		m_wndPlacement.Add(wp);

		// kludge commented because it caused the project window to lose the
		// project name and path information when any other file(s) needed to
		// be opened.

		// This kludge is the way we decided to have this routine, which
		// is opening edit windows, let the CEditFrame.PreCreateWindow
		// know that there is a windowplacement for this window.  It checks
		// theApp.m_pwp, and if it is one of these brief moments when the
		// value isn't null, it uses it.
		// theApp.m_pwp = &wp;
		// theApp.WOpen(attr);
		// theApp.m_pwp = NULL;
   }
}

/////////////////////////////////////////////////////////////////////////////
// CProjectDoc commands

void CProjectDoc::Close()
// Called when a new Project is being opened and
// the old one needs to be closed.
{
   //OnFileSave();
   m_wndOpen.RemoveAll();
   m_wndPlacement.RemoveAll();
   GetProjectFrame()->SendMessage(WM_CLOSE);
}

void CProjectDoc::Save()
// Called to force a save when frame window is close.
{
   OnFileSave();
}

void CProjectDoc::OnFileSave() 
{
   CDocument::OnFileSave();
   SetProject();
}

void CProjectDoc::OnFileSaveAs() 
{
   CDocument::OnFileSaveAs();
   SetProject();
   theApp.m_pProjectMRUList->Add(GetPathName());
}

void CProjectDoc::OnCloseDocument() 
// Save will save all of the open documents and also record
// their window placements.  CloseAllFiles will close do
// the obvious.
{
   Save();
   CloseAllFiles();
   CDocument::OnCloseDocument();
   theApp.m_pProjectDoc = NULL;
}

// editdoc.cpp : implementation of the CEditDoc class
//

#include "stdafx.h"
#include "editdoc.h"
#include "peditvw.h"
#include "cpwide.h"
#include "mainfrm.h"
#include "resource.h"
#include "project.h"
#include "utils.h"
#include <direct.h>

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif


/////////////////////////////////////////////////////////////////////////////
// CEditDoc

IMPLEMENT_DYNCREATE(CEditDoc, CDocument)

BEGIN_MESSAGE_MAP(CEditDoc, CDocument)
   //{{AFX_MSG_MAP(CEditDoc)
   ON_COMMAND(ID_FILE_CLOSE, OnFileClose)
   ON_COMMAND(ID_FILE_SAVE, OnFileSave)
   ON_COMMAND(ID_FILE_SAVE_AS, OnFileSaveAs)
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEditDoc construction/destruction

CEditDoc::CEditDoc()
{
   m_btitst = FALSE;
#ifdef _UNICODE
   m_nFileType = FILE_ULITTLEEND;
#else
   m_nFileType = FILE_ASCII;
#endif
}

CEditDoc::~CEditDoc()
{
}

BOOL CEditDoc::OnNewDocument()
{
   if (!CDocument::OnNewDocument())
      return FALSE;

   // TODO: add reinitialization code here
   // (SDI documents will reuse this document)

//   AugmentTitle();
   m_timestamp = CTime(0);
   return TRUE;
}

BOOL CEditDoc::OnOpenDocument(LPCTSTR lpszPathName)
{
   // When the file is on the command line, like when
   // called from a click on a .pro link from a browser,
   // we come here directly, skipping our file open stuff
   // and the current directory never gets set.  So we
   // set it.
   _TCHAR drive[5];
   int idrive;
   _TCHAR dir[MAX_PATH];
   _TCHAR fname[MAX_PATH];
   _TCHAR ext[10];
   int rc;
   _tsplitpath( lpszPathName, drive, dir, fname, ext );
   if (_tcslen(drive) > 0) {
		int cd = drive[0];
		idrive = toupper(cd) - 'A' + 1;
		rc = _chdrive( idrive );
   }
   if (_tcslen(dir) > 0) {
		rc = _tchdir(dir);
   }

   BOOL ret = CDocument::OnOpenDocument(lpszPathName);
   //AugmentTitle();
   //AfxMessageBox(GetTitle());
   return ret;
}

/////////////////////////////////////////////////////////////////////////////
// CEditDoc serialization

void CEditDoc::Serialize(CArchive& ar)
{
   POSITION pos = GetFirstViewPosition();
   if (pos==NULL) return;
   
   CPEditView *pPEditView =
      (CPEditView *)GetNextView(pos);

   /*   Out of the box, this called SerializeRaw, but
      we've overridden Serialize to handle all of the
      Unicode/non-Unicode file issues. */
   //_TCHAR buf[80];
   //Lsprintf(buf, _T("File type = %d"), m_nFileType);
   //AfxMessageBox(buf);

   pPEditView->Serialize(ar);

   // Save the filestatus timestamp after a serialize
   CFileStatus fstat;
   ar.GetFile()->GetStatus(fstat);
   m_timestamp = fstat.m_mtime;
}

void CEditDoc::DeleteContents()
{
   POSITION pos = GetFirstViewPosition();
   if (pos==NULL) return;
   
   CPEditView *pEditView =
      (CPEditView *)GetNextView(pos);
   pEditView->DeleteContents();
}

/////////////////////////////////////////////////////////////////////////////
// CEditDoc diagnostics

#ifdef _DEBUG
void CEditDoc::AssertValid() const
{
   CDocument::AssertValid();
}

void CEditDoc::Dump(CDumpContext& dc) const
{
   CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CEditDoc commands

void CEditDoc::OnFileSave() 
{
   if (theApp.m_pMFW->PrologGet())
   {
      //AfxMessageBox(_T("End Prolog I/O interaction before closing."));
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return;
   }

   CDocument::OnFileSave();
   AugmentTitle();
}

void CEditDoc::OnFileClose() 
{
   if (theApp.m_pMFW->PrologGet())
   {
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return;
   }

   CDocument::OnFileClose();   
}

BOOL CEditDoc::DoSave(LPCTSTR lpszPathName, BOOL bReplace)
/*   Called in response to OnFileSaveAs messages. */
{
   if (theApp.m_pMFW->PrologGet())
   {
      //AfxMessageBox("End Prolog I/O interaction before closing.");
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return FALSE;
   }
   m_btitst = FALSE;

   /*   The code from here on is derived from CDocument::DoSave
      from the MFC source, and CDocManager::DoPromptFileName. */
   CFileDialog *pfileDlg = NULL;

   CString newName = lpszPathName;
   if (newName.IsEmpty())
   {
      // The file types correspond to the enum variable FileType
      // defined in peditvw.h.  The file type is saved in the 
      // view so serialize can do the right thing.
      if (g_osrel >= 4)
         pfileDlg = new CFileDialog(FALSE, _T("pro"), NULL,
            OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT|OFN_EXPLORER,
            _T("ASCII (*.pro;*.txt)|*.pro;*.txt|")
#ifdef _UNICODE
            _T("Unicode (*.pro;*.txt)|*.pro;*.txt|")
            //_T("Unicode Big Endian (*.pro;*.txt)|*.pro;*.txt|")
#endif
         _T("|"));
      else
         pfileDlg = new CFileDialog(FALSE, _T("pro"), NULL,
            OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT,
            _T("ASCII (*.pro;*.txt)|*.pro;*.txt|")
#ifdef _UNICODE
            _T("Unicode (*.pro;*.txt)|*.pro;*.txt|")
            //_T("Unicode Big Endian (*.pro;*.txt)|*.pro;*.txt|")
#endif
         _T("|"));

      if (pfileDlg->DoModal() != IDOK)
      {
         delete pfileDlg;
         return FALSE;
      }
      newName = pfileDlg->GetPathName();
      m_nFileType = pfileDlg->m_ofn.nFilterIndex;
      delete pfileDlg;
   }
   CWaitCursor wait;

   if (!OnSaveDocument(newName))
   {
      if (lpszPathName == NULL)
      {
         // be sure to delete the file
         try
         {
            CFile::Remove(newName);
         }
         catch(...)
         {
            TRACE0("Warning: failed to delete file after failed SaveAs.\n");
         }
      }
      return FALSE;
   }

   // reset the title and change the document name
   if (bReplace)
      SetPathName(newName);
   return TRUE;
}

void CEditDoc::OnFileSaveAs() 
{
   if (theApp.m_pMFW->PrologGet())
   {
      //AfxMessageBox("End Prolog I/O interaction before closing.");
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return;
   }
   CDocument::OnFileSaveAs();   
   AugmentTitle();
}

CPEditView* CEditDoc::GetPEditView()
{
   POSITION pos = GetFirstViewPosition();
   if (pos == NULL)
      return NULL;
   else
      return (CPEditView*)GetNextView(pos);
}

CEditFrame* CEditDoc::GetEditFrame()
{
   return (CEditFrame*)(GetPEditView()->GetParent());
}

void CEditDoc::HighlightLine(long lineno)
{
   int char1, len;

   char1 = (GetPEditView()->GetEditCtrl()).LineIndex(lineno-1);
   len = (GetPEditView()->GetEditCtrl()).LineLength(char1);
   (GetPEditView()->GetEditCtrl()).SetSel(char1, char1+len, FALSE);
   GetPEditView()->BringWindowToTop();
}

void CEditDoc::AugmentTitle()
{
#ifdef _UNICODE
   switch(GetFileType())
   {
   case FILE_ASCII:
      SetTitle(GetTitle() + _T(" (ASCII)"));
      break;
   case FILE_ULITTLEEND:
   case FILE_ULITTLEENDNOTAG:
   case FILE_UBIGEND:
   case FILE_UBIGENDNOTAG:
      SetTitle(GetTitle() + _T(" (Unicode)"));
      break;
   default:
      break;
   }
#else
   return;
#endif
}


/*   This is the MFC function, customized as necessary */

/*
BOOL CEditDoc::CDocumentDoSave(LPCTSTR lpszPathName, BOOL bReplace)
   // Save the document data to a file
   // lpszPathName = path name where to save document file
   // if lpszPathName is NULL then the user will be prompted (SaveAs)
   // note: lpszPathName can be different than 'm_strPathName'
   // if 'bReplace' is TRUE will change file name if successful (SaveAs)
   // if 'bReplace' is FALSE will not change path name (SaveCopyAs)
{
   CString newName = lpszPathName;
   if (newName.IsEmpty())
   {
      CDocTemplate* pTemplate = GetDocTemplate();
      ASSERT(pTemplate != NULL);

      newName = m_strPathName;
      if (bReplace && newName.IsEmpty())
      {
         newName = m_strTitle;
#ifndef _MAC
         // check for dubious filename
         int iBad = newName.FindOneOf(_T(" #%;/\\"));
#else
         int iBad = newName.FindOneOf(_T(":"));
#endif
         if (iBad != -1)
            newName.ReleaseBuffer(iBad);

#ifndef _MAC
         // append the default suffix if there is one
         CString strExt;
         if (pTemplate->GetDocString(strExt, CDocTemplate::filterExt) &&
           !strExt.IsEmpty())
         {
            ASSERT(strExt[0] == '.');
            newName += strExt;
         }
#endif
      }

      if (!CDocManagerDoPromptFileName(newName,
        bReplace ? AFX_IDS_SAVEFILE : AFX_IDS_SAVEFILECOPY,
        OFN_HIDEREADONLY | OFN_PATHMUSTEXIST, FALSE, pTemplate))
         return FALSE;       // don't even attempt to save
   }

   CWaitCursor wait;

   if (!OnSaveDocument(newName))
   {
      if (lpszPathName == NULL)
      {
         // be sure to delete the file
         try
         {
            CFile::Remove(newName);
         }
         catch(...)
         {
            TRACE0("Warning: failed to delete file after failed SaveAs.\n");
         }
      }
      return FALSE;
   }

   // reset the title and change the document name
   if (bReplace)
      SetPathName(newName);

   return TRUE;        // success
}

BOOL CEditDoc::CDocManagerDoPromptFileName(CString& fileName, UINT nIDSTitle, DWORD lFlags, BOOL bOpenFileDialog, CDocTemplate* pTemplate)
{
   CFileDialog dlgFile(bOpenFileDialog);

   CString title;
   VERIFY(title.LoadString(nIDSTitle));

   dlgFile.m_ofn.Flags |= lFlags;

   CString strFilter;
   CString strDefault;
   if (pTemplate != NULL)
   {
      ASSERT_VALID(pTemplate);
      AppendFilterSuffix(strFilter, dlgFile.m_ofn, pTemplate, &strDefault);
   }
   else
   {
      // do for all doc template
      POSITION pos = m_templateList.GetHeadPosition();
      BOOL bFirst = TRUE;
      while (pos != NULL)
      {
         CDocTemplate* pTemplate = (CDocTemplate*)m_templateList.GetNext(pos);
         AppendFilterSuffix(strFilter, dlgFile.m_ofn, pTemplate,
            bFirst ? &strDefault : NULL);
         bFirst = FALSE;
      }
   }

   // append the "*.*" all files filter
   CString allFilter;
   VERIFY(allFilter.LoadString(AFX_IDS_ALLFILTER));
   strFilter += allFilter;
   strFilter += (TCHAR)'\0';   // next string please
#ifndef _MAC
   strFilter += _T("*.*");
#else
   strFilter += _T("****");
#endif
   strFilter += (TCHAR)'\0';   // last string
   dlgFile.m_ofn.nMaxCustFilter++;

   dlgFile.m_ofn.lpstrFilter = strFilter;
#ifndef _MAC
   dlgFile.m_ofn.lpstrTitle = title;
#else
   dlgFile.m_ofn.lpstrPrompt = title;
#endif
   dlgFile.m_ofn.lpstrFile = fileName.GetBuffer(_MAX_PATH);

   BOOL bResult = dlgFile.DoModal() == IDOK ? TRUE : FALSE;
   //   Save the file type so Serialize can do the right thing.
   m_nFileType = dlgFile.m_ofn.nFilterIndex;
   fileName.ReleaseBuffer();
   return bResult;
}
*/

/////////////////////////////////////////////////////////////////////////////
// CEditFrame

IMPLEMENT_DYNCREATE(CEditFrame, CMDIChildWnd)

CEditFrame::CEditFrame()
{
}

CEditFrame::~CEditFrame()
{
}


BEGIN_MESSAGE_MAP(CEditFrame, CMDIChildWnd)
   //{{AFX_MSG_MAP(CEditFrame)
   ON_WM_CLOSE()
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CEditFrame message handlers

void CEditFrame::OnClose() 
{
   if (theApp.m_pMFW->PrologGet())
   {
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return;
   }
   
   CMDIChildWnd::OnClose();
}


BOOL CEditFrame::PreCreateWindow(CREATESTRUCT& cs)
// When edit windows are automatically opened as a project
// is opened, the project open routine puts the window placement
// information, temporarily, in theApp.m_pwp.  The fact that it's
// there, means it can be used.  Normally m_pwp is NULL.
{
   WINDOWPLACEMENT *pwp;
   if ((pwp = theApp.m_pwp) != NULL)
   {
      cs.x = pwp->rcNormalPosition.left;
      cs.y = pwp->rcNormalPosition.top;
      cs.cx = pwp->rcNormalPosition.right - cs.x;
      cs.cy = pwp->rcNormalPosition.bottom - cs.y;
   }

   return CMDIChildWnd::PreCreateWindow(cs);
}

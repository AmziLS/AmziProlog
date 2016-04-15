//---------------------------------------------------------------
// The C++ front-end on the Amzi! compiler
//

#include "stdafx.h"
#include "proprog.h"
#include "compile.h"
#include "conview.h"
#include "utils.h"
#include "cpwin.h"
#include "cpwide.h"
#include "resource.h"
#include "project.h"

CCompile::CCompile()
{
}

CCompile::~CCompile()
{
}

BOOL CCompile::Initialize(const _TCHAR* pszPathName)
{
   BOOL b = CProProg::Initialize(pszPathName);
   if (b == FALSE)
      return b;

   return b;

/*
   TERM t, topen;
   _TCHAR status[3];
   if (FALSE == ExecStr(&t, _T("amzi_compiler:op$en(_Status)")))
   {
      AfxMessageBox(_T("Compiler failed to initialize correctly"));
      return FALSE;
   }
   GetArg(t, 2, cTERM, &topen);
   GetArg(topen, 1, cSTR, &status);

   if (*status == 'x' || *status == 'l')
   {
      AfxMessageBox(_T("Compiler locked"));
      return FALSE;
   }

   return b;
*/
}

BOOL CCompile::Run()
{
   CFileDialog *pfileDlg = NULL;
   if (g_osrel >= 4)
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT|OFN_EXPLORER,
         _T("Prolog Files (*.pro;*.ppj)|*.pro;*.ppj|All (*.*)|*.*||"));
   else
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT,
         _T("Prolog Files (*.pro;*.ppj)|*.pro;*.ppj|All (*.*)|*.*||"));


   if (pfileDlg->DoModal() != IDOK)
   {
      delete pfileDlg;
      return FALSE;
   }
   CString sFile = pfileDlg->GetPathName();
   delete pfileDlg;
   return Run(sFile);
}

BOOL CCompile::Run(CString sFile)
{
   TF tf = TRUE;
   TERM t;

   _TCHAR fbuf[_MAX_PATH];
   _TCHAR fpbuf[_MAX_PATH+10];
   _TCHAR buf[_MAX_PATH+30];

   g_pConView = m_pConView;

   _TCHAR str_compiler[512];
   int rc = LoadString(theApp.m_hInstance, IDS_COMPILER, str_compiler, 512);
   m_pConView->PutS(str_compiler);
   m_pConView->PutS(_T("  "));

        // See if its a Prolog project file
   _tcscpy(fbuf, (const _TCHAR*)sFile);
   
   try {

   if (chk_ext(fbuf, _T(".ppj")))
   {
      AfxMessageBox(_T("Compile projects from project dialog box"));
	  tf = FALSE;
   }
   else {
      slashslash2(fpbuf, fbuf);
      Lsprintf(buf, _T("amzi_compiler:compile_file($%s$)"), fpbuf);
         if (TRUE != (tf = ExecStr(&t, buf))) {
            //force_ext(fbuf, _T(".PLM"));
            //CFile::Remove(fbuf);
         throw(_T("Compiler Failed"));
            //process_error(tf);
         }
//      tf = ExecStr(&t, buf);
   }

   } catch (CLSException &E)
      // This actually won't happen because the
      // compiler catches the errors and then
      // simply fails.
   {
      Error(E);
   }
   catch (_TCHAR*)
   {
      // Reset the engine to close the iostreams
      Reset();
      force_ext(fbuf, _T(".PLM"));
      os_remove(fbuf);
      //CFileStatus fs;
      //if (CFile::GetStatus(fbuf, fs))
      //   CFile::Remove(fbuf);
   }

   g_pConView->FlushBuf();
//   Close();
   return (BOOL)tf;
}

BOOL CCompile::CompileProject()
{
   if (theApp.m_pProjectDoc == NULL)
   {
      AfxMessageBox(_T("No project open"));
      return FALSE;
   }

   TF tf = TRUE;
   TERM t;

   _TCHAR fprobuf[_MAX_PATH];
   _TCHAR fplmbuf[_MAX_PATH];
   _TCHAR fpbuf[_MAX_PATH+10];
   _TCHAR buf[_MAX_PATH+30];
   //_TCHAR fbuf[_MAX_PATH+30];

   g_pConView = m_pConView;

   //CFileStatus fstatPLM;
   //CFileStatus fstatPRO;
   _TCHAR str_compiler[512];
   int rc = LoadString(theApp.m_hInstance, IDS_COMPILER, str_compiler, 512);
   m_pConView->PutS(str_compiler);
   m_pConView->PutS(_T("\n"));

   int cnt, i;

   try {

   // Load the library operator definitions first, so they
   // are known to the compiler for the other files.
   cnt = theApp.m_pProjectDoc->GetLibCount();
   for (i=0; i<cnt; i++)
   {
      //Lsprintf(fplmbuf, _T("load$ops('%s')"),
      Lsprintf(fplmbuf, _T("load$ops('%s')"),
            theApp.m_pProjectDoc->GetLib(i));
      slashslash2(fpbuf, fplmbuf);       // convert '\'s to '\\'s
      ExecStr(&t, fpbuf);
   }

   /*
   cnt = theApp.m_pProjectDoc->GetFileCount();
   for (i=0; i<cnt; i++)
   {
      _tcscpy(fbuf, theApp.m_pProjectDoc->GetFile(i));
         slashslash1(fbuf);                          // convert '\'s to '\\'s
         if (chk_ext(fbuf, _T(".pro")))
            Lsprintf(buf, _T("consult_ops('%s')"), fbuf);
         else
            Lsprintf(buf, _T("load_ops('%s')"), fbuf);
      //PutS(buf);
      ExecStr(&t, buf);
   }
   */

   // Compile the opdefs first, so the compiler knows what they are.

   // Compile the files in the project, note that file -1
   // is a special case that picks up the OpDefs file.
   cnt = theApp.m_pProjectDoc->GetFileCount();

   int iFirstFile;
   if (_T("") != theApp.m_pProjectDoc->GetOpDefsFile())
      iFirstFile = -1;
   else
      iFirstFile = 0;

   for (i=iFirstFile; i<cnt; i++)
   {
      _tcscpy(fprobuf, theApp.m_pProjectDoc->GetFile(i));
      _tcscpy(fplmbuf, fprobuf);
      force_ext(fprobuf, _T(".PRO"));
      //if (0 == CFile::GetStatus(fbuf, fstatPRO))
      if (TRUE != os_file_exists(fprobuf))
      {
         Lsprintf(buf, _T("Can't find file %s"), fprobuf);
         AfxMessageBox(buf);
         continue;
      }
      force_ext(fplmbuf, _T(".PLM"));
      //if (0 == CFile::GetStatus(fbuf, fstatPLM) ||
      //   fstatPRO.m_mtime > fstatPLM.m_mtime)
      // always recompile to opdefs
      if (i == -1 || TRUE == file_needs_update(fplmbuf, fprobuf))
      {
         slashslash2(fpbuf, fprobuf);       // convert '\'s to '\\'s
         Lsprintf(buf, _T("amzi_compiler:compile_file($%s$)"), fpbuf);
         if (TRUE != (tf = ExecStr(&t, buf)))
         {
            os_remove(fplmbuf);
            //force_ext(fbuf, _T(".PLM"));
            //CFile::Remove(fbuf);
            throw(_T("Compiler Failed"));
            //process_error(tf);
            break;
         }
      }
         else
      {
         slashslash2(fpbuf, fprobuf);       // convert '\'s to '\\'s
         Lsprintf(buf, _T("write($\n%s is up to date\n$)"), fpbuf);
            tf = ExecStr(&t, buf);
         }
   }

   } catch (CLSException &E)
      // This actually won't happen because the
      // compiler catches the errors and then
      // simply fails.
   {
      Error(E);
   }
   catch (_TCHAR*)
   {
      // Reset the engine to close the iostreams
      Reset();
      os_remove(fplmbuf);
      //force_ext(fbuf, _T(".PLM"));
      //CFileStatus fs;
      //if (CFile::GetStatus(fbuf, fs))
      //   CFile::Remove(fbuf);
   }

   g_pConView->FlushBuf();
   return TRUE;
}
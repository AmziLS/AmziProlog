//------------------------------------------------------------------
// link.cpp - Windows front-end on Amzi! Prolog Linker
//
// Copyright (c) 1994 Amzi! inc.  All Rights Reserved.
//

#include "stdafx.h"
#include "amzi.h"
#include "link.h"
#include "utils.h"
#include "conview.h"
#include "cpwide.h"
#include "resource.h"
#include "project.h"

CConView* s_wLout;

// The linker will always be sending us wide character strings
void disp_msg(_TCHAR * msg)
{
   s_wLout->PutS(msg);
   s_wLout->PutS(_T("\n"));
}

IMPLEMENT_DYNCREATE(CLink, CDocument)

CLink::CLink()
{
   _TCHAR buf[80];

#ifndef _WIN32
   m_hLinkDLL = LoadLibrary(_T("a4lnk16.dll"));
   if (m_hLinkDLL > HINSTANCE_ERROR)
   {
      m_pfL = (pfLINK)GetProcAddress(m_hLinkDLL, "cpLinkW");
      if (m_pfL == NULL)
      {
         wsprintf(buf, _T("Unable to find Linker entry point: %d"), GetLastError());
         AfxMessageBox(buf);
      }
   }
   else
   {
      m_pfL = NULL;
      wsprintf(buf, _T("Unable to open Linker"));
      AfxMessageBox(buf)
   }
#else
   //m_hLinkDLL = LoadLibrary(_T("alnk.dll"));
   m_hLinkDLL = LoadLibrary(_T("amzi.dll"));
   if (m_hLinkDLL != NULL)
   {
#ifdef _UNICODE
      //m_pfL = (pfLINK)GetProcAddress(m_hLinkDLL, "cpLinkW");
	  m_pfL = (pfLINK)GetProcAddress(m_hLinkDLL, "aLinkW");
#else
      //m_pfL = (pfLINK)GetProcAddress(m_hLinkDLL, "cpLinkA");
	  m_pfL = (pfLINK)GetProcAddress(m_hLinkDLL, "aLinkA");
#endif
      //if (m_pfL == NULL)
      //   m_pfL = (pfLINK)GetProcAddress(m_hLinkDLL, "_cpLink@12");
      if (m_pfL == NULL)
      {
         wsprintf(buf, _T("Unable to find Linker entry point: %d"), GetLastError());
         AfxMessageBox(buf);
      }
   }
   else
   {
      m_pfL = NULL;
      wsprintf(buf, _T("Unable to open Linker: %d"), GetLastError());
      AfxMessageBox(buf);
   }
#endif
}

CLink::~CLink()
{
   FreeLibrary(m_hLinkDLL);
}

BOOL CLink::Run(CConView* pCV)
{
   CFileDialog *pfileDlg = NULL;
   if (g_osrel >= 4)
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT|OFN_EXPLORER,
         _T("Prolog Files (*.plm;*.ppj)|*.plm;*.ppj|All (*.*)|*.*||"));
   else
      pfileDlg = new CFileDialog(TRUE, NULL, NULL,
         OFN_HIDEREADONLY|OFN_OVERWRITEPROMPT,
         _T("Prolog Files (*.plm;*.ppj)|*.plm;*.ppj|All (*.*)|*.*||"));

   if (pfileDlg->DoModal() != IDOK)
   {
      delete pfileDlg;
      return FALSE;
   }
   CString sFile = pfileDlg->GetPathName();
   delete pfileDlg;
   return Run(pCV, sFile);
}

BOOL CLink::Run(CConView* pCV, CString sFile)
{
   int   i, argctr;
   _TCHAR *   pargv[128];
   _TCHAR   fbuf[_MAX_PATH];

   _TCHAR str_linker[512];
   int rc = LoadString(theApp.m_hInstance, IDS_LINKER, str_linker, 512);
   pCV->PutS(str_linker);
   pCV->PutS(_T("\n"));


        
        _tcscpy(fbuf, (const _TCHAR*)sFile);
   // first link parameter is the .xpl file
   force_ext(fbuf, _T(".xpl"));
   pargv[0] = new _TCHAR[1+_tcslen(fbuf)];
   _tcscpy(pargv[0], fbuf);
   // second link parameter is alib.plm
   pargv[1] = new _TCHAR[1+_tcslen(_T("alib.plm"))];
   _tcscpy(pargv[1], _T("alib.plm"));
   
   _tcscpy(fbuf, (const _TCHAR*)sFile);
   if (chk_ext(fbuf, _T(".ppj")))      // link a project file
   {
      AfxMessageBox(_T("Open projects before linking"));
      }
   else                           // link a single plm file
   {
      force_ext(fbuf, _T(".plm"));
      pargv[2] = new _TCHAR[1+_tcslen(fbuf)];
      _tcscpy(pargv[2], fbuf);
      argctr = 3;
   }
   // Open an output window
   s_wLout = pCV;
   // Call the linker with the list of files to link
   (*m_pfL)(disp_msg, argctr, (_TCHAR **)pargv);

   for (i=0; i<argctr; i++)
      delete[] pargv[i];
   return TRUE;
}

BOOL CLink::LinkProject(CConView* pCV)
{
   int   i, cnt, argctr;
   _TCHAR *   pargv[128];
   _TCHAR   fbuf[_MAX_PATH];

   _TCHAR str_linker[512];
   int rc = LoadString(theApp.m_hInstance, IDS_LINKER, str_linker, 512);
   pCV->PutS(str_linker);
   pCV->PutS(_T("\n"));

   // first link parameter is the .xpl file
   _tcscpy(fbuf, (const _TCHAR*)(theApp.m_pProjectDoc->GetXPLFile()));
   force_ext(fbuf, _T(".xpl"));
   pargv[0] = new _TCHAR[1+_tcslen(fbuf)];
   _tcscpy(pargv[0], fbuf);

   // second link parameter is alib.plm
   pargv[1] = new _TCHAR[1+_tcslen(_T("alib.plm"))];
   _tcscpy(pargv[1], _T("alib.plm"));
   
   // next, get the library files
   argctr = 2;
   cnt = theApp.m_pProjectDoc->GetLibCount();
   for (i=0; i<cnt; i++)
   {
      _tcscpy(fbuf, theApp.m_pProjectDoc->GetLib(i));
         force_ext(fbuf, _T(".plm"));
         pargv[argctr] = new _TCHAR[1+_tcslen(fbuf)];
         _tcscpy(pargv[argctr], fbuf);
         argctr++;
   }

   // finally, get the project files
   cnt = theApp.m_pProjectDoc->GetFileCount();
   // If there's an opdef file, get it first, it will be
   // retrieved by GetFile(-1).
   int iFirstFile;
   if (_T("") != theApp.m_pProjectDoc->GetOpDefsFile())
      iFirstFile = -1;
   else
      iFirstFile = 0;
   for (i=iFirstFile; i<cnt; i++)
   {
      _tcscpy(fbuf, theApp.m_pProjectDoc->GetFile(i));
         force_ext(fbuf, _T(".plm"));
         pargv[argctr] = new _TCHAR[1+_tcslen(fbuf)];
         _tcscpy(pargv[argctr], fbuf);
         argctr++;
   }

   // Open an output window
   s_wLout = pCV;
   // Call the linker with the list of files to link
   (*m_pfL)(disp_msg, argctr, (_TCHAR **)pargv);

   for (i=0; i<argctr; i++)
      delete pargv[i];

   return TRUE;
}
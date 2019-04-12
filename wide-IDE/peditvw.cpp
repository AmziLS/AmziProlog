// peditvw.cpp : implementation file
//

#include "stdafx.h"
#include "cpwide.h"
#include "mainfrm.h"
#include "peditvw.h"
#include "editdoc.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPEditView

IMPLEMENT_DYNCREATE(CPEditView, CEditView)

CPEditView::CPEditView()
{
   m_btitst = FALSE;
}

CPEditView::~CPEditView()
{
}

BEGIN_MESSAGE_MAP(CPEditView, CEditView)
   //{{AFX_MSG_MAP(CPEditView)
   ON_WM_CREATE()
   ON_EN_CHANGE(AFX_IDW_PANE_FIRST, OnEditChange)
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CPEditView drawing

void CPEditView::OnDraw(CDC* pDC)
{
   CDocument* pDoc = GetDocument();
   // TODO: add draw code here
}

/////////////////////////////////////////////////////////////////////////////
// CPEditView diagnostics

#ifdef _DEBUG
void CPEditView::AssertValid() const
{
   CEditView::AssertValid();
}

void CPEditView::Dump(CDumpContext& dc) const
{
   CEditView::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CPEditView message handlers

int CPEditView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
   if (CEditView::OnCreate(lpCreateStruct) == -1)
      return -1;
   
   SetFont(&(theApp.m_pMFW->m_font));
   
   return 0;
}

void CPEditView::OnEditChange()
{
   CEditDoc* pDoc = (CEditDoc*)GetDocument();

   if (! pDoc->m_btitst)
   {
      m_title = pDoc->GetTitle();
      m_title_star = m_title+_T("*");
      pDoc->SetTitle(m_title_star);
      pDoc->m_btitst = TRUE;
   }
   CEditView::OnEditChange();
}

void CPEditView::Serialize(CArchive& ar)
/*   This code started from the MFC source code for
   CEditView::SerializeRaw() in viewedit.cpp. */
{
   CEditDoc *pDoc = (CEditDoc*)GetDocument();

#ifdef _UNICODE
   ASSERT_VALID(this);
   if (ar.IsStoring())
   {
      WriteToArchive(ar);
   }
   else
   {
      CFile* pFile = ar.GetFile();
      ASSERT(pFile->GetPosition() == 0);
      DWORD nFileSize = pFile->GetLength();
      if (nFileSize/sizeof(TCHAR) > nMaxSize)
      {
         AfxMessageBox(AFX_IDP_FILE_TOO_LARGE);
         AfxThrowUserException();
      }
      DeduceFileType(pFile, nFileSize);
      if (pDoc->GetFileType() == FILE_UBIGEND ||
            pDoc->GetFileType() == FILE_UBIGENDNOTAG)
         if (IDNO == AfxMessageBox(
               _T("The file is big endian Unicode,\n")
               _T("convert to little endian?"), MB_YESNO) )
            AfxThrowUserException();
      ReadFromArchive(ar, (UINT)nFileSize);
      if (pDoc->GetFileType() == FILE_UBIGEND ||
            pDoc->GetFileType() == FILE_UBIGENDNOTAG)
         pDoc->SetFileType(FILE_ULITTLEEND);
   }
   ASSERT_VALID(this);
#else
   CEditView::SerializeRaw(ar);
#endif
}

void CPEditView::DeduceFileType(CFile * pFile, DWORD nBytes)
/*   The document keeps the file type which can be
   1- ASCII,  a b c
   2- Unicode Little Endian notag, a 0 b 0 c 0
   3- Unicode Little Endian, ff fe a 0 b 0 c 0
   4- Unicode Big Endian notag, or 0 a 0 b 0 c
   5- Unicode Big Endian, fe ff 0 a 0 b 0 c
   The number corresponds to the number returned from the
   saveas file dialog as file type. */
{
   CEditDoc *pDoc = (CEditDoc*)GetDocument();

   // Odd number of bytes is always ASCII
   if (nBytes % 2 == 1)
   {
      pDoc->SetFileType(FILE_ASCII);
      return;
   }

   int nLen = nBytes > 512 ? 512 : nBytes;
   BYTE *buf = new BYTE[nLen];
   if (buf == NULL)
   {
      AfxMessageBox(_T("Insufficient memory for read"));
      pDoc->SetFileType(FILE_ERROR);
      goto cleanup;
   }

   pFile->Read(buf, nLen);

   if (buf[0] == 0xff && buf[1] == 0xfe)
   {
      pDoc->SetFileType(FILE_ULITTLEEND);
      goto cleanup;
   }

   if (buf[0] == 0xfe && buf[1] == 0xff)
   {
      pDoc->SetFileType(FILE_UBIGEND);
      goto cleanup;
   }

   // This is program text, so there will not be
   // any 0's naturally occurring in ASCII files.
   // Further, a Unicode file will have spaces and
   // carriage return line feeds which come from
   // the lower 256, so there will be 0's in Unicode.

   int i;

   for (i=0; i<nLen; i+=2)
   {
      if (buf[i+1] == 0)
      {
         pDoc->SetFileType(FILE_ULITTLEENDNOTAG);
         goto cleanup;
      }

      if (buf[i] == 0)
      {
         pDoc->SetFileType(FILE_UBIGENDNOTAG);
         goto cleanup;
      }
   }

   pDoc->SetFileType(FILE_ASCII);

cleanup:
   delete[] buf;
   pFile->SeekToBegin();
   return;
}

void CPEditView::ReadFromArchive(CArchive& ar, UINT nLen)
/*   From MFC source in viewedit.cpp */
{
   ASSERT_VALID(this);
   CEditDoc *pDoc = (CEditDoc*)GetDocument();
   long i;
   LPVOID hText;
   BYTE* pChar;
   _TCHAR* pTChar;
   BYTE  head[2];

   // This is where the 65K limitation comes from - local alloc
   // can only allocate 65k of movable storage.
   if (pDoc->GetFileType() == FILE_ASCII)
      hText = LocalAlloc(LMEM_MOVEABLE, (nLen+1)*sizeof(TCHAR));
   else
      hText = LocalAlloc(LMEM_MOVEABLE, nLen + sizeof(TCHAR));

   if (hText == NULL)
      AfxThrowMemoryException();

   BYTE* lpszText = (BYTE*)LocalLock(hText);
   ASSERT(lpszText != NULL);

   switch(pDoc->GetFileType())
   {
   case FILE_ASCII:
      //AfxMessageBox(_T("ASCII file"));
      if (ar.Read(lpszText, nLen) != nLen)
      {
         LocalUnlock(hText);
         LocalFree(hText);
         AfxThrowArchiveException(CArchiveException::endOfFile);
      }
      pChar = (BYTE*)lpszText;
      pTChar = (_TCHAR*)lpszText;
      for (i=(long)nLen-1; i>=0; i--)
         pTChar[i] = pChar[i];
      pTChar[nLen] = _T('\0');
      break;
   case FILE_ULITTLEEND:
      //AfxMessageBox(_T("Tagged Unicode file"));
      // Skip the header bytes
      ar.Read(head, 2);
      nLen = nLen - 2;
   case FILE_ULITTLEENDNOTAG:
      //AfxMessageBox(_T("Unicode file"));
      if (ar.Read(lpszText, nLen) != nLen)
      {
         LocalUnlock(hText);
         LocalFree(hText);
         AfxThrowArchiveException(CArchiveException::endOfFile);
      }
      pTChar = (_TCHAR*)lpszText;
      pTChar[nLen/sizeof(_TCHAR)] = _T('\0');
      break;
   case FILE_UBIGEND:
      //AfxMessageBox(_T("Tagged Reverse Unicode file"));
      // Skip the header bytes
      ar.Read(head, 2);
      nLen = nLen - 2;
   case FILE_UBIGENDNOTAG:
      //AfxMessageBox(_T("Reverse Unicode file"));
      if (ar.Read(lpszText, nLen) != nLen)
      {
         LocalUnlock(hText);
         LocalFree(hText);
         AfxThrowArchiveException(CArchiveException::endOfFile);
      }
      BYTE x;
      for (i=0; i<(long)nLen; i+=2)
      {
         x = lpszText[i];
         lpszText[i] = lpszText[i+1];
         lpszText[i+1] = x;
      }
      pTChar = (_TCHAR*)lpszText;
      pTChar[nLen/sizeof(_TCHAR)] = _T('\0');
      break;
   default:
      AfxMessageBox(_T("Unknown file type"));
   }

   HLOCAL hOldText = GetEditCtrl().GetHandle();
   ASSERT(hOldText != NULL);
   LocalFree(hOldText);
   GetEditCtrl().SetHandle((HLOCAL)(UINT)(DWORD)hText);
   Invalidate();
   ASSERT_VALID(this);
}

void CPEditView::WriteToArchive(CArchive& ar)
//   From MFC source for CEditView in viewedit.cpp
// Write just the text to an archive, no length prefix.
{
   long i;
   BYTE  head[2];
   CEditDoc *pDoc = (CEditDoc*)GetDocument();

   ASSERT_VALID(this);
   const _TCHAR* lpszText = LockBuffer();
   ASSERT(lpszText != NULL);
   UINT nLen = GetBufferLength();
   BYTE* pByte = (BYTE*)lpszText;
   int imb;

   try   {

   switch(pDoc->GetFileType())
   {
   case FILE_ASCII:
      imb = IDYES;
      for (i=0; i<(long)nLen; i++)
         if (pByte[2*i+1] != 0)
         {
            imb = AfxMessageBox(
               _T("File has non-ASCII characters, saving as ASCII will lose characters")
               _T("Save Anyway?"), MB_YESNO);
            break;
         }
      if (imb == IDYES)
         for (i=0; i<(long)nLen; i++)
            ar.Write(&pByte[2*i], 1);
      break;
   case FILE_ULITTLEEND:
      // Whether the file had the beginning tag or not, we're going
      // to put one in.  If we change our minds on this point, simply
      // move this second case statement down past the header write.
      // Same with big endian write below.
   case FILE_ULITTLEENDNOTAG:
      head[0] = 0xff;
      head[1] = 0xfe;
      ar.Write(head, 2);
      ar.Write(lpszText, nLen*sizeof(_TCHAR));
      break;
   case FILE_UBIGEND:
   case FILE_UBIGENDNOTAG:
      head[0] = 0xfe;
      head[1] = 0xff;
      ar.Write(head, 2);
      for (i=0; i<2*(long)nLen; i+=2)
      {
         ar.Write(&pByte[i+1], 1);
         ar.Write(&pByte[i], 1);
      }
      break;
   default:
      AfxMessageBox(_T("Invalid file type for save"));
   }

   } catch(...)
   {
      UnlockBuffer();
      throw;
   }
   UnlockBuffer();
   ASSERT_VALID(this);
}

void CPEditView::OnActivateView(BOOL bActivate, CView* pActivateView, CView* pDeactiveView) 
{
   // add custom code here
   
   CEditView::OnActivateView(bActivate, pActivateView, pDeactiveView);
}

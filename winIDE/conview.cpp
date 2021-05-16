// conview.cpp : implementation of the CConView class
//

#include "stdafx.h"
#include "resource.h"
#include "cpwide.h"
#include "mainfrm.h"
#include "conview.h"
#include "prodoc.h"
#include "proprog.h"
#include <ctype.h>


#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CConView

IMPLEMENT_DYNCREATE(CConView, CEditView)

BEGIN_MESSAGE_MAP(CConView, CEditView)
   //{{AFX_MSG_MAP(CConView)
   ON_COMMAND(ID_EDIT_PASTE, OnEditPaste)
   ON_WM_CREATE()
   ON_WM_SETFOCUS()
   ON_WM_KILLFOCUS()
   ON_WM_CREATE()
   ON_WM_KEYDOWN()
   //}}AFX_MSG_MAP
   // Standard printing commands
   ON_WM_CHAR()
   ON_COMMAND(ID_FILE_PRINT, CEditView::OnFilePrint)
   ON_COMMAND(ID_FILE_PRINT_PREVIEW, CEditView::OnFilePrintPreview)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CConView construction/destruction

CConView::CConView()
{
   keybuf[0] = EOS;
   kbWrite = 0;
   kbRead = 0;
   lineRead = FALSE;
   bDone = FALSE;
   maxLines = MAXVIEWLINES;
   deltaLines = maxLines / 10;
   ndelLines = 0;
   getchB = FALSE;
   bGettingChars = FALSE;
   quoting = 0;
   m_iOB = 0;
   m_szOutBuf[0] = EOS;
   m_iTopLine = 0;
}

CConView::~CConView()
{
}

int CConView::OnCreate(LPCREATESTRUCT lpcs)
{
   if (CEditView::OnCreate(lpcs) != 0)
      return -1;

//   m_font.CreateStockObject(ANSI_FIXED_FONT);
//   m_font.CreateStockObject(SYSTEM_FIXED_FONT);
//   m_font.CreateStockObject(OEM_FIXED_FONT);
   SetFont(&(theApp.m_pMFW->m_font));

   return 0;
}
/////////////////////////////////////////////////////////////////////////////
// CConView drawing

void CConView::OnDraw(CDC* pDC)
{
   CProDoc* pDoc = GetDocument();
   ASSERT_VALID(pDoc);

   // TODO: add draw code for native data here
}

/////////////////////////////////////////////////////////////////////////////
// CConView printing

BOOL CConView::OnPreparePrinting(CPrintInfo* pInfo)
{
   FlushBuf();
   // default CEditView preparation
   return CEditView::OnPreparePrinting(pInfo);
}

void CConView::OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo)
{
   // Default CEditView begin printing.
   CEditView::OnBeginPrinting(pDC, pInfo);
}

void CConView::OnEndPrinting(CDC* pDC, CPrintInfo* pInfo)
{
   // Default CEditView end printing
   CEditView::OnEndPrinting(pDC, pInfo);
}

/////////////////////////////////////////////////////////////////////////////
// CConView diagnostics

#ifdef _DEBUG
void CConView::AssertValid() const
{
   CEditView::AssertValid();
}

void CConView::Dump(CDumpContext& dc) const
{
   CEditView::Dump(dc);
}

CProDoc* CConView::GetDocument() // non-debug version is inline
{
   ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CProDoc)));
   return (CProDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CConView message handlers


//////////////////////////////////////////////////////////////////
// Other Functions

void CConView::Empty()
{
//   GetEditCtrl().SetWindowText(_T(""));
//   GetEditCtrl().SetSel(0,-1);      // select everything
//   GetEditCtrl().Clear();
}

void CConView::PutC(unsigned int c)
{
   _TCHAR buf[2];
   
   buf[0] = (_TCHAR)c;
   buf[1] = EOS;
   PutS(buf);
}

void CConView::PutS(_TCHAR * lpszString)   // Add a string to view
{
   _TCHAR *pStr;
   int   iTopLine;
   
   pStr = lpszString;

   while (*pStr)
   {
      if (*pStr == NL || m_iOB >= OUTBUFSIZE-4)
      {
         m_szOutBuf[m_iOB++] = _T('\r');
         m_szOutBuf[m_iOB++] = NL;
         m_szOutBuf[m_iOB] = EOS;
         GetEditCtrl().ReplaceSel(m_szOutBuf);
         if (ndelLines++ > deltaLines)
            TrimLines();
         iTopLine = GetEditCtrl().GetFirstVisibleLine();
         if (iTopLine > m_iTopLine)
         {
            GetEditCtrl().LineScroll(SCROLLINC);
            m_iTopLine = GetEditCtrl().GetFirstVisibleLine();
         }
         m_iOB = 0;
         pStr++;
      }
      else if (*pStr == _T('\r'))
         pStr++;
      else
         m_szOutBuf[m_iOB++] = *pStr++;
   }
}

void CConView::FlushBuf()
{
   if (m_iOB == 0) return;
   m_szOutBuf[m_iOB] = EOS;
   GetEditCtrl().ReplaceSel(m_szOutBuf);
   m_iOB = 0;
}

_TCHAR CConView::GetC()               // Read line from view
{
   MSG   msg;
   _TCHAR c;

   //SetFocus();
   FlushBuf();   
   bGettingChars = TRUE;

   // There is a problem here, if the user closes the listener window
   // while this loop is running, the code returns to nowhere causing
   // a GPF.  So we disable the close button.
   theApp.m_pMFW->PrologGetOn();
   while (!lineRead && GetMessage (&msg, NULL, 0, 0))
   {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
   }
   theApp.m_pMFW->PrologGetOff();

   c = (int) keybuf[kbRead++];
   if (kbRead >= kbWrite-1)
   {
      kbRead = kbWrite = 0;
      keybuf[0] = EOS;
      lineRead = FALSE;
   }
   bGettingChars = FALSE;
   return c;
}

void CConView::UnGetC()
{
   if ( kbWrite>0 && kbRead>0 ) kbRead--;
}

_TCHAR CConView::GetCh()
{
   MSG   msg;

   SetFocus();   
   FlushBuf();
   getchChar = EOS;
   getchB = TRUE;
   
   bGettingChars = TRUE;
   theApp.m_pMFW->PrologGetOn();
   while (getchB && GetMessage (&msg, NULL, 0, 0))
   {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
   }
   theApp.m_pMFW->PrologGetOff();
   bGettingChars = FALSE;
   return getchChar;
}

#define CTRL_V 0x16
#define CTRL_C 0x03
#define CTRL_X 0x18

void CConView::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags)
{
   if (nChar == CTRL_C && nFlags == 0x146)
   {
      // Its a break, not a copy so just enter the 0x03 for
      // the engine to recognize as a break.  We are using the
      // 0x146 flag setting which was empirically determined
      // for the development machine to mean the break key.
      // Who knows if it works on other keyboards.
      ;
   }
   else
   {
      // Its a character, but it might be a control sequence for
      // cutting or pasting.  If so, don't process the character.
      CEditView::OnChar(nChar, nRepCnt, nFlags);
      if (nChar == CTRL_V)
      {
         // For reasons unknown to me, when we take over the message
         // loop the edit window no longer handles CTRL_V etc., but
         // rather passes them in as characters here. Let me be more
         // precise, it does paste the data, but doesn't call our
         // OnEditPaste override, as it does when the editmenu is
         // used instead.  This is only true when we're in our loop
         // to read characters.  So, the kludge that works is to
         // just call PasteData() from here.
         PasteData();
         return;
      }
      if (nChar == CTRL_C || nChar == CTRL_X)
         return;
   }

   if (bDone)
   {
      AfxMessageBox(_T("Program has finished"));
      return;
   }
   if (getchB)
   {
      getchChar = nChar;               // in case there's a getch()
      getchB = FALSE;
   }
   else
   {
      // Check for quoting for determining end of line.
      // Note that to be consistent with the Prolog reader,
      // the $ symbol is allowed in atom names and 0'x indicates
      // a character
      if (nChar == _T('$') || nChar == _T('"') || nChar == _T('\'') || nChar == _T('`') )
      {
         if (nChar == _T('$') && quoting == 0 &&
            kbWrite > 0 && isalnum(keybuf[kbWrite-1])) ;
         else if (nChar == _T('\'') && quoting == 0 &&
            kbWrite > 0 && keybuf[kbWrite-1] == '0') ;
         else if (quoting == 0) quoting = nChar;
         else if (quoting == nChar) quoting = 0;
      }

      switch (nChar)         // process the _TCHARacter
      {
      case _T('\r'):
         keybuf[kbWrite++] = NL;
         keybuf[kbWrite++] = EOS;
         lineRead = TRUE;
         break;
      case _T('\b'):
         if (kbWrite <= 0) return;        // don't allow backspace past 0
         kbWrite--;
         switch (keybuf[kbWrite])     // undo quote processing
         {
         case _T('$'):
            if (quoting == 0 && isalnum(keybuf[kbWrite-1])) break;
         case _T('"'):
         case _T('\''):
         case _T('`'):
            if (quoting == 0) quoting = keybuf[kbWrite];
            else if (quoting == (UINT)keybuf[kbWrite]) quoting = 0;
         }
         break;
      default:
         keybuf[kbWrite++] = nChar;
      }
      if (kbWrite >= INBUFSIZE)
      {
         AfxMessageBox(_T("Line buffer overflow"));
         kbWrite = 0;
         keybuf[0] = EOS;
      }
   }
   if (lineRead == TRUE)
      OnLineRead();
}

void CConView::TrimLines()
{
   int nLines, iExcess, iLineIndex;

   ndelLines = 0;
   nLines = GetEditCtrl().GetLineCount();
   if (nLines <= maxLines)
      return;
   iExcess = nLines - maxLines;
   iLineIndex = GetEditCtrl().LineIndex(iExcess);
   GetEditCtrl().SetSel(0, iLineIndex, TRUE);
   GetEditCtrl().ReplaceSel(_T(""));
   iLineIndex = GetEditCtrl().GetLineCount();
   iLineIndex = GetEditCtrl().LineIndex(iLineIndex-1);
   GetEditCtrl().SetSel(iLineIndex, iLineIndex, FALSE);
   m_iTopLine = GetEditCtrl().GetFirstVisibleLine();
   return;
}

void CConView::OnEditPaste()
{
   CEditView::OnEditPaste();
   PasteData();
   //GetEditCtrl().Paste();
}

void CConView::PasteData()
{
   HANDLE hClipData;
   
   if (bDone) {
      AfxMessageBox(_T("Program has finished"));
      return;
   }

   if (!OpenClipboard()) {
      AfxMessageBox(_T("Cannot open the clipboard"));
      return;
   }
   
#ifdef _UNICODE
   if (NULL == (hClipData = GetClipboardData(CF_UNICODETEXT))) {
#else
   if (NULL == (hClipData = GetClipboardData(CF_TEXT))) {
#endif
      CloseClipboard();
      return;
   }
   
   CString sPaste = (_TCHAR *)GlobalLock(hClipData);
   GlobalUnlock(hClipData);
   CloseClipboard();

   _tcscpy(&keybuf[kbWrite], (const _TCHAR*)sPaste);
   kbWrite += sPaste.GetLength();

   return;
}


   /////////////////////////////////////////////////////////////////////////////
// CListenFrame

IMPLEMENT_DYNCREATE(CConFrame, CMDIChildWnd)

CConFrame::CConFrame()
{
}

CConFrame::~CConFrame()
{
}

BEGIN_MESSAGE_MAP(CConFrame, CMDIChildWnd)
   //{{AFX_MSG_MAP(CConFrame)
   ON_WM_CLOSE()
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CListenFrame message handlers


void CConFrame::OnClose()
{
   if (theApp.m_pMFW->PrologGet())
   {
      AfxMessageBox(_T("Let Prolog query finish first (Ctrl-Break will stop it)"));
      return;
   }
   
   CMDIChildWnd::OnClose();
}

void CConView::OnSetFocus(CWnd* pOldWnd) 
{
   CEditView::OnSetFocus(pOldWnd);

   g_pConView = this;
}

void CConView::OnKillFocus(CWnd* pNewWnd) 
{
   CEditView::OnKillFocus(pNewWnd);
   
   theApp.CheckChangedFiles();
}

void CConView::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
   // TODO: Add your message handler code here and/or call default
   
   CEditView::OnKeyDown(nChar, nRepCnt, nFlags);
}

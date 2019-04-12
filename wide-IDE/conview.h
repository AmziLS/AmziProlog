// conview.h : interface of the CConView class
//
/////////////////////////////////////////////////////////////////////////////

const int INBUFSIZE=512;
const int OUTBUFSIZE=128;
const int MAXVIEWLINES=1500;
const int SCROLLINC=10;

class CProDoc;

class CConView : public CEditView
{
protected: // create from serialization only
   DECLARE_DYNCREATE(CConView)

// Attributes
protected:
   _TCHAR  keybuf[INBUFSIZE];   // a line of input
   int   kbWrite;      // write index in key buffer
   int   kbRead;         // read index in key buffer
   BOOL  lineRead;      // new line in buffer flag
   _TCHAR  getchChar;      // the character for a getch
   BOOL  bDone;         // when the programs over
   int   maxLines;      // maximum number of lines in view
   int   deltaLines;      // number of lines to cut at a time
   int   ndelLines;      // number of lines to delta
   BOOL  getchB;      // getch flag
   BOOL  bGettingChars;  // in a Prolog getc
   UINT  quoting;      // active quoting char (' " $)
   _TCHAR  m_szOutBuf[OUTBUFSIZE]; // buffered output
   int   m_iOB;     // current pointer to outbuf
   int     m_iTopLine;  // last top visible line number
   CFont m_font;
   void PasteData();    // Paste clipboard to keybuffer

public:
   CProDoc* GetDocument();

// Operations
public:
   void Empty();                  // Empty contents of view
   void PutC(unsigned int c);               // Add a character to view
   void PutS(_TCHAR far * lpszString);   // Add a string to view
   _TCHAR GetC();
   void UnGetC();
   _TCHAR GetCh();
   void FlushBuf();   // flush the output buffer

// Implementation
public:
   CConView();
   virtual ~CConView();
   virtual void OnDraw(CDC* pDC);  // overridden to draw this view
   void OnChar(UINT nChar, UINT nRepCnt, UINT nFlags);
   virtual void OnLineRead() {};
#ifdef _DEBUG
   virtual void AssertValid() const;
   virtual void Dump(CDumpContext& dc) const;
#endif

protected:
   void TrimLines();
   // Printing support
   virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
   virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
   virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);

// Generated message map functions
protected:
   //{{AFX_MSG(CConView)
   afx_msg void OnEditPaste();
   afx_msg int OnCreate(LPCREATESTRUCT lpcs);
   afx_msg void OnSetFocus(CWnd* pOldWnd);
   afx_msg void OnKillFocus(CWnd* pNewWnd);
   afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in conview.cpp
inline CProDoc* CConView::GetDocument()
   { return (CProDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////
// CConFrame frame

class CConFrame : public CMDIChildWnd
{
   DECLARE_DYNCREATE(CConFrame)
protected:
   CConFrame();         // protected constructor used by dynamic creation

// Attributes
public:

// Operations
public:

// Implementation
protected:
   virtual ~CConFrame();

   // Generated message map functions
   //{{AFX_MSG(CConFrame)
   afx_msg void OnClose();
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////


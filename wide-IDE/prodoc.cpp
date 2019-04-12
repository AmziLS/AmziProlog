// prodoc.cpp : implementation of the CProDoc class
//

#include "stdafx.h"
#include "prodoc.h"
#include "conview.h"
#include "proprog.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

//extern "C" PRED_INIT winPreds[];

////////////////////////////////////////////////////
// Other Functions


/////////////////////////////////////////////////////////////////////////////
// CProDoc

IMPLEMENT_DYNCREATE(CProDoc, CDocument)

BEGIN_MESSAGE_MAP(CProDoc, CDocument)
   //{{AFX_MSG_MAP(CProDoc)
      // NOTE - the ClassWizard will add and remove mapping macros here.
      //    DO NOT EDIT what you see in these blocks of generated code!
   //}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CProDoc construction/destruction

CProDoc::CProDoc()
{
}

CProDoc::~CProDoc()
{
}

void CProDoc::Serialize(CArchive& ar)
{
   // Only use archive for writing out console to file
   if (ar.IsStoring() )
      ((CConView*)m_viewList.GetHead())->SerializeRaw(ar);
}

/////////////////////////////////////////////////////////////////////////////
// CProDoc diagnostics

#ifdef _DEBUG
void CProDoc::AssertValid() const
{
   CDocument::AssertValid();
}

void CProDoc::Dump(CDumpContext& dc) const
{
   CDocument::Dump(dc);
}
#endif //_DEBUG

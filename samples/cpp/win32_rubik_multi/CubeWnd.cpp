// CubeWnd.cpp : implementation file
//

#include "stdafx.h"
#include "inc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

BEGIN_MESSAGE_MAP(CCubeWnd, CMDIChildWnd)
	//{{AFX_MSG_MAP(CCubeWnd)
	ON_WM_PAINT()
	//}}AFX_MSG_MAP
	ON_WM_CREATE()
	ON_WM_MOUSEMOVE()
	ON_BN_CLICKED(ID_SOLVE, OnSolve)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CCubeMDIChildWnd

CCubeWnd::CCubeWnd()
{
	m_pRubik = new Rubik(this);

	m_bSolving = FALSE;

	m_brush[0].CreateSolidBrush(RGB(255,0,0));
	m_brush[1].CreateSolidBrush(RGB(0,255,0));
	m_brush[2].CreateSolidBrush(RGB(0,0,255));
	m_brush[3].CreateSolidBrush(RGB(255,255,0));
	m_brush[4].CreateSolidBrush(RGB(255,0,255));
	m_brush[5].CreateSolidBrush(RGB(0,255,255));

	CRect r(0,0,EDGE,EDGE);

	int si, sx, sy, sxo, syo;
	int i,j;
	int d;
	d = EDGE/10;
	sxo = syo = EDGE;

	si = 0; sx = sxo + 3*EDGE + 2*d; sy = syo + d;
	for (i=0; i<3; i++)
		for (j=0; j<3; j++)
			m_cube[si+3*i+j] = r + CPoint(sx + j*EDGE, sy + i*EDGE);

	si = 9; sx = sxo + d; sy = syo + 3*EDGE + 2*d;
	for (i=0; i<3; i++)
		for (j=0; j<3; j++)
			m_cube[si+3*i+j] = r + CPoint(sx + j*EDGE, sy + i*EDGE);

	si = 18; sx = sxo + 3*EDGE + 2*d;
	for (i=0; i<3; i++)
		for (j=0; j<3; j++)
			m_cube[si+3*i+j] = r + CPoint(sx + j*EDGE, sy + i*EDGE);

	si = 27; sx = sxo + 6*EDGE + 3*d;
	for (i=0; i<3; i++)
		for (j=0; j<3; j++)
			m_cube[si+3*i+j] = r + CPoint(sx + j*EDGE, sy + i*EDGE);

	si = 36; sx = sxo + 3*EDGE + 2*d; sy = syo + 6*EDGE + 3*d;
	for (i=0; i<3; i++)
		for (j=0; j<3; j++)
			m_cube[si+3*i+j] = r + CPoint(sx + j*EDGE, sy + i*EDGE);

	si = 45; sy = syo + 9*EDGE + 4*d;
	for (i=0; i<3; i++)
		for (j=0; j<3; j++)
			m_cube[si+3*i+j] = r + CPoint(sx + j*EDGE, sy + i*EDGE);
}

int CCubeWnd::OnCreate( LPCREATESTRUCT cs )
{
	m_solveButton.Create( _T("Solve"),
		BS_PUSHBUTTON | WS_CHILD | WS_VISIBLE,
		CRect(5, 10, 60, 40),
		this, ID_SOLVE );

	CMDIChildWnd::OnCreate(cs);

	return 0;
}

void CCubeWnd::OnMouseMove(UINT i, CPoint p)
{
	if (m_bSolving)
		AfxGetApp()->RestoreWaitCursor();
}


void CCubeWnd::OnSolve()
{
	AfxGetApp()->BeginWaitCursor();
	m_bSolving = TRUE;
	m_pRubik->SolveThread();
}

void CCubeWnd::Done()
{
	m_bSolving = FALSE;
	AfxGetApp()->EndWaitCursor();
}

void CCubeWnd::OnPaint() 
{
	CPaintDC dc(this); // device context for painting
	
	for (int i=0; i<54; i++)
		dc.FillRect(m_cube[i], &m_brush[m_pRubik->m_side[i]]);

	m_solveButton.UpdateWindow();
}

BOOL CCubeWnd::DestroyWindow()
{
	//OnPrepareToClose();

	return CMDIChildWnd::DestroyWindow();
}



//------------------------------------------
// Rubik.cpp - Cube class implementation
//

#include "stdafx.h"
#include "inc.h"

/////////////////////////////////////////////////////////////////
// Extended Prolog predicates
//

TF EXPFUNC p_bugmsg(VOIDptr pr)
{
	return ((Rubik*)pr)->p_bugmsg();
}

TF EXPFUNC p_cube_print(VOIDptr pr)
{
	return ((Rubik*)pr)->p_cube_print();
}

TF EXPFUNC p_wrfield(VOIDptr pr)
{
	return ((Rubik*)pr)->p_wrfield();
}

TF EXPFUNC p_disp_hist(VOIDptr pr)
{
	return ((Rubik*)pr)->p_disp_hist();
}

int g_tilemap[55] = {0,
	40, 31, 22, 4, 13, 49, 38, 33, 26,
	44, 35, 47, 36, 17, 24, 42, 15, 45,
	8, 27, 20, 2, 29, 53, 6, 11, 18,
	0, 9, 51, 30, 23, 32, 50, 14, 21,
	12, 48, 37, 25, 43, 46, 7, 19, 1,
	52, 41, 34, 39, 16, 5, 28, 3, 10};

Rubik::Rubik(CCubeWnd *pcw)
{
	//g_Doc = this;

	//LEngine *pe = new LEngine;

	m_pCubeWnd = pcw;

	for (int i=0; i<54; i++)
		m_side[i] = i%6;

	m_stage = 0;

	try
	{
		Init("rubik");

		AddPred("bugmsg", 1, &::p_bugmsg, this);
		AddPred("cube_print", 1, &::p_cube_print, this);
		AddPred("wrfield", 2, &::p_wrfield, this);
		AddPred("disp_hist", 1, &::p_disp_hist, this);

		Load("rubik");
	}
	catch (CLSException &e)
	{
		char msg[512];
		e.GetMsg(msg, 511);
		AfxMessageBox(msg);
	}
	
	m_solving = FALSE;
}

UINT RubikSolve(LPVOID pParam)
{
	((Rubik*)pParam)->Solve();
	((Rubik*)pParam)->Done();
	return 0;
}

void Rubik::SolveThread()
{
	AfxBeginThread(RubikSolve, this);
}

void Rubik::Solve()
{
	TERM t;

	if (m_solving == TRUE)
		return;

	m_solving = TRUE;
	
	try
	{
		if ( TRUE == ExecStr(&t, "wininit") )
		{
			if ( FALSE == ExecStr(&t, "solve(random)") )
				AfxMessageBox("can't solve");
		}
		else
			AfxMessageBox("wininit error");
	}
	catch (CLSException &e)
	{
		char msg[512];
		e.GetMsg(msg, 511);
		AfxMessageBox(msg);
	}

	m_solving = FALSE;
}

void Rubik::Done()
{
	m_pCubeWnd->Done();
}

TF Rubik::p_bugmsg()
{
	char buf[256];

	GetParm(1, cSTR, buf);
	//AfxMessageBox(buf);
	return TRUE;
}

// cubeprint/1 is called from Prolog with a cube
// structure, cube/54, that has the color values
// for each tile.  The cube/54 arguments are mapped
// to the correct tiles for display in the view.

TF Rubik::p_cube_print()
{
	TERM  cube;
	char  side[7];
	int   j;

	GetParm(1, cTERM, &cube);
	for (int i=1; i<=54; i++)
	{
		GetArg(cube, i, cSTR, side);
		switch(side[0])
		{
		case 'F': j=0; break;
		case 'R': j=1; break;
		case 'U': j=2; break;
		case 'B': j=3; break;
		case 'L': j=4; break;
		case 'D': j=5; break;
		default:
			AfxMessageBox("Bad side given to cubeprint");
		}
		m_side[g_tilemap[i]] = j;
	}
	m_pCubeWnd->Invalidate();
	// GDI batches commands as an optimization, which makes for
	// a really boring cube display, as the cube solves so quickly
	// only the final state gets displayed.  GdiFlush() causes
	// the GDI buffer to be flushed, so all of the intermediate
	// states get displayed as well.
	GdiFlush();

	return TRUE;
}

TF Rubik::p_wrfield()
{
	char field[40];

	GetParm(1, cSTR, field);
	if (0 == strcmp("stage", field))
	{
		//lsGetParm(eid, 2, cINT, &(g_Doc->m_stage));
	}
	else
		return TRUE;

	//g_Doc->UpdateAllViews(NULL);
	return TRUE;
}

TF Rubik::p_disp_hist()
{
	char hist[80];

	GetParm(1, cSTR, hist);
//	AfxMessageBox(hist);

	//g_Doc->m_history.AddTail(hist);

	//g_Doc->UpdateAllViews(NULL);

	return TRUE;
}


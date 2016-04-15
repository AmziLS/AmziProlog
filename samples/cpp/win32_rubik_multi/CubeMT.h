// CubeMT.h : main header file for the CUBEMT application
//
// CubeMT is an example of the multithreaded release 4.0 version
// of Amzi! Prolog + Logic Server.  It is a sample that is based
// on the Microsoft MTMDI sample, which spawns multiple threads
// in child windows of an MDI application.  In their case, the
// windows have bouncing balls.  In this case, the windows have
// Rubik's cubes that keep scrambling and solving themselves.

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CCubeMTApp:
// See CubeMT.cpp for the implementation of this class
//

class CCubeMTApp : public CWinApp
{
public:
	CCubeMTApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CCubeMTApp)
	public:
	virtual BOOL InitInstance();
	virtual int ExitInstance();
	//}}AFX_VIRTUAL

// Implementation

	//{{AFX_MSG(CCubeMTApp)
	afx_msg void OnAppAbout();
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

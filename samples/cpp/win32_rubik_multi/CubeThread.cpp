// CubeThread.cpp : implementation file
//

#include "stdafx.h"
#include "inc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CCubeThread

// A single global event handle is used to synchronize the termination
// of the application and the termination of bounce threads.  This
// synchronization avoids false memory leak detection of CBounceThread
// objects that did not have time to be auto-destroyed before the
// application terminates.
//
// The reason that m_hEventBounceThreadKilled cannot be a per-object member
// variable of the CBounceThread object is because the event handle must
// be referred to by the delete operator.   By the time the delete operator
// is called, the member variable no longer can be referenced.  The reason
// that it is permissable to use a global event, instead of per thread events,
// is that each CBounceWnd window is destroyed in sequence, one after another.
// Therefore, each CBounceThread is destroyed in sequence, one after another.

HANDLE CCubeThread::m_hEventCubeThreadKilled;

IMPLEMENT_DYNCREATE(CCubeThread, CWinThread)

CCubeThread::CCubeThread()
{
}

CCubeThread::CCubeThread(HWND hwndParent)
	: m_hwndParent(hwndParent)
{
}

CCubeThread::~CCubeThread()
{
}

void CCubeThread::operator delete(void* p)
{
	// The exiting main application thread waits for this event before completely
	// terminating in order to avoid a false memory leak detection.  See also
	// CBounceWnd::OnNcDestroy in bounce.cpp.

	SetEvent(m_hEventCubeThreadKilled);

	CWinThread::operator delete(p);
}

BOOL CCubeThread::InitInstance()
{
	//CWnd* pParent = CWnd::FromHandle(m_hwndParent);
	//CRect rect;
	//pParent->GetClientRect(&rect);

	//BOOL bReturn = m_wndCube.Create(_T("CubeWnd"),
	//	WS_CHILD | WS_VISIBLE, rect, pParent);

	// It is important to set CWinThread::m_pMainWnd to the user interface
	// window.  This is required so that when the m_pMainWnd is destroyed,
	// the CWinThread is also automatically destroyed.  For insight into
	// how the CWinThread is automatically destroyed when the m_pMainWnd
	// window is destroyed, see the implementation of CWnd::OnNcDestroy
	// in wincore.cpp of the MFC sources.

	//if (bReturn)
	//	m_pMainWnd = &m_wndCube;

	//return bReturn;
	return TRUE;
}

int CCubeThread::ExitInstance()
{
	// TODO:  perform any per-thread cleanup here
	return CWinThread::ExitInstance();
}

BEGIN_MESSAGE_MAP(CCubeThread, CWinThread)
	//{{AFX_MSG_MAP(CCubeThread)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CCubeThread message handlers

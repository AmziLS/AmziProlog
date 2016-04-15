// CubeThread.h : header file
//



/////////////////////////////////////////////////////////////////////////////
// CCubeThread thread

class CCubeThread : public CWinThread
{
	DECLARE_DYNCREATE(CCubeThread)
protected:
	CCubeThread();           // protected constructor used by dynamic creation
public:
	CCubeThread(HWND hwndParent);
	void operator delete(void* p);

// Attributes
public:
	static HANDLE m_hEventCubeThreadKilled;

protected:
	HWND m_hwndParent;
	CCubeWnd m_wndCube;



// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CCubeThread)
	public:
	virtual BOOL InitInstance();
	virtual int ExitInstance();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CCubeThread();

	// Generated message map functions
	//{{AFX_MSG(CCubeThread)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

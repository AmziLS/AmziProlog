// CubeWnd.h : header file
//

const int EDGE = 20;

class Rubik;

class CCubeWnd : public CMDIChildWnd
{
// Attributes
public:
	CRect     m_cube[54];
	Rubik    *m_pRubik;
	CButton   m_solveButton;
	BOOL      m_bSolving;

	enum {ID_SOLVE = 1};

private:
	CBrush m_brush[6];

public:
	CCubeWnd();

	void Done();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CCubeWnd)
	public:
	virtual BOOL DestroyWindow();
	//}}AFX_VIRTUAL

// Implementation
protected:
	// message handlers
	//{{AFX_MSG(CCubeWnd)
	//}}AFX_MSG
	afx_msg void OnPaint();
	afx_msg int OnCreate( LPCREATESTRUCT );
	afx_msg void OnSolve();
	afx_msg void OnMouseMove(UINT i, CPoint p);

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//-------------------------------------
// Rubik.h - Cube class description
//

class CCubeWnd;

class Rubik : public CLogicServer
{
// Extended Predicates
private:
	BOOL    m_solving;

public:
	TF p_bugmsg();
	TF p_cube_print();
	TF p_wrfield();
	TF p_disp_hist();

// Attributes
public:
	int m_side[54];
	int m_stage;
//	CStringList m_history;
	CCubeWnd *m_pCubeWnd;

public:
	Rubik() {};
	Rubik(CCubeWnd *);
	~Rubik() {};

	void Solve();
	void SolveThread();
	void Done();
};

//----------------------------------------------------------------
// proprog.h - header file for Prolog program
//

#include "amzi.h"

class CConView;

TF EXPFUNC p_keyb(VOIDptr vp);

extern CConView* g_pConView;

class CProProg : public CLogicServer, public CObject
{
protected:
   CConView*  m_pConView;
   
public:
   CProProg();
   ~CProProg();
   void AttachView(CConView* pCV);
   virtual BOOL Initialize(const _TCHAR* pszPathName);
   BOOL Run();
   //BOOL Close();
   int Error(CLSException&);
   TF p_keyb();   // Keyboard extended predicate
};


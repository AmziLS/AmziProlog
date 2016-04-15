/*
CHELLO.H - header file for Hello Class
*/

#include "logicserver.h"

using namespace std;

class CHello : public LogicServer
{
public:
   CHello() {};
   ~CHello();
   void init();
   void run();

private:
   RC   m_rc;

   void error(LSException &E);
   void msg_out(string msg);
};


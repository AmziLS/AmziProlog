/*
CHELLO.H - header file for Hello Class
*/

#include "stdio.h"
#include "logicserver.h"

using namespace std;

// A bug causes segmentation faults when CLSExceptions are
// thown as a class under Gnu C++, so exceptions are thrown
// as pointers under Gnu instead.
#ifdef GNU
#define LS_EXCEPTION LSException *E
#else
#define LS_EXCEPTION LSException &E
#endif
class CHello
{
private:
   LogicServer ls;
public:
   CHello() {};
   ~CHello();
   TF   init();
   void run();

private:
   RC   m_rc;

   void error(LS_EXCEPTION);
   void msg_out(string msg);
};


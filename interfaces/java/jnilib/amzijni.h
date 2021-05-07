/* Linked list of extended predicates */

const int MAXMSG = 100000; // Maximum error msg length

typedef uintC ajptr;  // an cell-sized int (64 on x64 systems) used as a pointer

class JExtPred
{
public:
   JNIEnv*    jenv;
   jint       jenv_type;
   jclass     jcl;
   jmethodID  jmeth;
   char*      jmeth_name;
   char*      jclass_name;
   jobject    jobj;
   jlong      lseng;
   JExtPred*  pnext;

   ~JExtPred();
};

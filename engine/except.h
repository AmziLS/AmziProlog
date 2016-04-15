//-*-C++-*-
/*
 *   except.h - Exception class
 *
 * 1999/12/23 Ray added badprepE
 *
 */

#ifndef EXCEPT_H
#define EXCEPT_H

//#include "lstring.h"

const int MAXMSG = 2000;  // If an error message is longer than this, it will
                  // create a disaster on platforms that don't have a snprintf.
class LExHandler;
class LString;

/*

enum ExType
{                                            // The exception types.
   WARNING,
   LERROR,
   LINIT,
   API,
   LOAD,
   EXEC,
   READ,
   FATAL,
   ABORT,
   INTERNAL,
   UNKNOWN,
   NOMORE
};
*/

typedef enum
{                          // The exception types, use C style declaration 
                           // because this will be used in C interface as well.
   BADENG,
   ABORT,
   INTERNAL,
   FATAL,
   INIT,
   API,
   LOAD,
   EXEC,
   READ,
	ARITH,
   SECURITY,
   UNKNOWN
} ExType;

typedef int ErrNo;    // The err numbers, associated with format in except.cpp.

// Initialization Errors

#define unknowniniE      100
#define nologfileE       101
#define openlogE         102
#define lsxinitE         103
#define lsxnoprocE       104
#define lsxloadE         105
#define lsxnomodE        106
#define lsxnodllE        107
#define lsxwinitE        108
#define lsxdebugE        109
#define lsxsysdebugE     110

// API Errors

#define noloadE          200
#define noninitE         201
#define afterloadE       202
#define argrangeE        203
#define arglrangeE       204
#define notcstrE         205
#define notcintE         206
#define notcfltE         207
#define notcadrE         208
#define badctypeE        209
#define badgetfaE        210
#define badlistE         211
#define badstreamE       212
#define badstrlenE       213
#define badtypeE         214
#define nonullE          215
#define badhosttypeE     216
#define extE             217

// Load Errors

#define loadfileE        300
//#define max_load_atomE   301
#define compverE         302
#define labelsE          303
#define localatomsE      304
#define destbufE         305
#define badfixupS        306
#define checksumE        307
#define filemaxE         308
#define bufmismatS       309
#define latomS           310
#define longatomS        311
#define longcodeS        312
#define manyvarS         313
#define badbytecntE      314
#define labjumpE         315
#define add_compiledE    316
#define disconE          317
#define loadloadE        318
#define latentE          319
#define redefinitionE    320
#define syspredE         321
#define free_xplE        322

// I/O Errors

#define stringlE         400
#define charE            401
#define hexE             402
#define maxvarrE         403
#define rdbufE           404
#define pstackE          405
#define delimE           406
#define randE            407
#define inargE           408
#define abuffE           409
#define rdnotatomE       410
#define parsetypeS       411
#define stringioE        412
#define handleE          413
#define unopenedE        414
#define wr_buf_overE     415
#define curstrE          416
#define iofE             417
#define pathlenE         418
#define eofE             419
#define fopenE           420
#define ioE              421
#define maxfileE         422
#define oopsE            423
#define modsurfeitE      424
#define streamsurfeitE   425
#define escseqE          426
#define syntaxE          427
#define permE            428
#define long_macroE      429
#define aliasE           430
#define setinputE        431
#define setoutputE       432
#define reposE           433
#define invalid_sopE     434
#define longlineE        435
#define prevopenE        436
#define no_cinE          437
#define outputdepthE     438
#define commentE         439
#define balanceE      440

// Arithmetic Errors

#define arithopE         500
#define arithargE        501
#define zero_divideE     502
#define type_error       503
#define calculationE     504
#define number_castE     505
#define number_domainE   506
#define number_indexE    507
#define number_sizeE     508

// Execution Errors

#define sysassertE      1000
#define userexitE       1001
#define userfatalE      1002
#define userabortE      1003
#define badopcodeS      1004
#define badrefS         1005
#define badtagS         1006
#define bigchunkS       1007
#define hbadtermS       1008
#define heapoverS       1009
#define memallocS       1010
#define stroverS        1011
#define thingS          1012
#define wrongwriteS     1013
#define outofmemE       1014
#define sysretractE     1015
#define heapE           1016
#define heapgcE         1017
#define alignE          1018
#define manyretracE     1019
#define trailE          1020
#define choiceE         1021
#define instanceE       1022
#define typeE           1023
#define natomsE         1024
#define maxvarE         1025
#define opopE           1026
#define sysargE         1027
#define intargE         1028
#define breakE          1029
#define localE          1030
//#define variE           1031
#define badopE          1032
#define argE            1033
#define badthrowtagE    1034
#define execE           1035
#define cuttagE         1036
#define cutdE           1037
#define stringoverE     1038
#define faultytowerE    1039
#define badbipE         1040
#define badxpredE       1041
#define atomtablesizeE  1042
#define badprepE        1043
#define retractE        1044
#define instantiation_error 1045
#define abolishE        1046
#define indexE          1047
#define heapofloE       1048
#define heapufloE       1049
#define struntermE      1050
#define nestedexecE     1051
#define badgoalE        1052
#define badindexedE     1053
#define unknown_modeE   1054
#define unknown_flagE   1055
#define maxmodsE        1056
//#define locked_featureE 1057
#define cyclicE         1058
#define choice_debugE   1059
#define undefinedE      1060

#define securityE       1200
#define no_local_amziE  1201
#define expired_xplE    1202
#define locked_featureE 1203
#define registryE       1204
#define not_activeE     1205
#define reg_corruptE    1206
#define eval_expiredE   1207

#define stubE           8888
#define internalE       8889

#define unknownE        9999

struct ErrFmt
{        // The structure that ties error numbers and format strings together.
   ErrNo   err;
   ExType  type;
   aCHAR  *fmt;
};

class ErrFmts
{                                 // Amzi error formats
private:
   static ErrFmt Fmts[];          // The format messages are in a static array
public:
   static ErrFmt *GetFmt(ErrNo);  // given error number, find the right string 
};

const int READTEXTLEN = 70;                   // LEN + TAIL = 80
const int READTEXTTAIL = 10;
const int NEARHERELEN = 15;

class LExcept
{     // The basic excptn class.  An excptn has a message, err number and type.
private:
//   The LString classes cause a problem in an exception, 
//   because C++ sometimes winds up deleting the exception twice,
//   once from where thrown and once from where caught.  
//   So make them pointers and NULL them on destruction.
   LString* m_msg;
   LString* m_apicall;
   LString* m_predinfo;
   LString* m_callstack;
   LString* m_readfilename;
   LString* m_readtext;
   LString* m_loadfilename;

   ErrNo    m_err;
   ExType   m_extype;
   LBOOL    m_bExecErr;
   LBOOL    m_bReadErr;
   intC     m_readlineno;
   LBOOL    m_bLoadErr;
public:
   LExcept();
   LExcept(ExType extype, ErrNo err, LString msg);
   LExcept( const LExcept& );
   LExcept(ErrNo, ...);
   ~LExcept();

   friend Lostream & operator<<( Lostream &, const LExcept& );
   LExcept& operator=( const LExcept & );

   void AddExecInfo(STRptr func, ARITY ar);
   void AddExecCallStack(LString sB);
   void AddReadInfo(LString, int, long, LString);
   void AddLoadInfo(LString loadfile);
   void AddToMsg(LString more);

   int GetRC()
   {   return m_err; }

   LBOOL IsExec()
   {   return m_bExecErr; }
   LBOOL IsRead()
   {   return m_bReadErr; }
   LBOOL IsLoad()
   {   return m_bLoadErr; }

   STRptr GetMsg();
   ExType GetType()
   {   return m_extype; }
   STRptr GetAPICall();
   STRptr GetPredInfo();
   STRptr GetCallStack();
   STRptr GetReadFileName();
   STRptr GetReadText();
   intC GetReadLineno()
   {   return m_readlineno; }
   STRptr GetLoadFile();
   void SetAPICall( const LString s);

   void SetType(ExType et)
   {   m_extype = et; }
};

const int MSGLEN=120;

class CLSException
{
private:
   LExcept *m_px;

public:
   DLLExport CLSException(LExcept &ex);
   DLLExport CLSException(CLSException &lsex);
   DLLExport virtual ~CLSException();
   //DLLExport TF IsExec();
   //DLLExport TF IsLoad();
   //DLLExport TF IsRead();
   DLLExport int GetRC();
   DLLExport void GetMsg(aCHAR*, int);
   DLLExport ExType GetType();
   //DLLExport STRptr GetAPICall();
   //DLLExport STRptr GetPredInfo();
   //DLLExport STRptr GetCallStack();
   DLLExport void GetReadFileName(aCHAR*, int);
   DLLExport void GetReadBuffer(aCHAR*, int);
   DLLExport void GetCallStack(aCHAR*, int);
   //DLLExport STRptr GetLoadFile();
   DLLExport intC GetReadLineno();
#ifdef _UNICODE
   DLLExport void GetMsg(char*, int);
   DLLExport void GetReadFileName(char*, int);
   DLLExport void GetReadBuffer(char*, int);
   DLLExport void GetCallStack(char*, int);
#endif
};

/*
// An engine maintains a stack of
// exceptions for review if necessary.  LExNode
// is a helper node class.
class LExNode
{
friend class LExStack;
private:
   LExcept *m_pex;
   LExNode *m_next;
   LExNode *m_prev;

   LExNode(LExcept *pex, LExNode *next, LExNode *prev)
   {   m_pex = pex; m_next = next; m_prev = prev; }
   ~LExNode ()   { delete m_pex; }             // Delete the Exception as well.
};

class LExStack
{
private:
   LExNode *m_first;
   LExNode *m_last;
public:
   LExStack() { m_first = NULL; m_last = NULL; }
   ~LExStack();
   void AddFirst(LExcept*);
   void AddLast(LExcept*);
   LExcept *GetFirst();
   LExcept *GetLast();
   LExcept *PeekFirst();
   LExcept *PeekLast();
   LBOOL   IsEmpty();
};
*/

class LExHandler
{         // The exception handler is notified of error and warning conditions 
private:                  // and decides whether to throw the exception or not.
   LEngine *m_peng;
   LFLAG    m_warnerr;    // Warnings can be considered errors or information.
   //LExStack m_exstack;
public:
   //LExHandler() : m_exstack() { m_warnerr = LOFF; }
   LExHandler(LEngine *peng) { m_warnerr = LOFF; m_peng = peng; }
   ~LExHandler() {};

   void Error(ErrNo, ...);
   void SetWarningErrors(LFLAG onoff) { m_warnerr = onoff; }

   //LString GetMsg();
   //RC      GetRC();
   //ExType  GetType();
   //LString GetExecInfo();
   //LString GetReadInfo();
   //intC    GetReadLineno();
   //void    Pop();
   //LBOOL   IsExcept();   // Is there an exception

   TF p_err_abort(void);
   TF p_err_fatal(void);
   TF p_errZexec(void);
};

#endif //EXCEPT_H

   /****************************************************************************

LSODBC.C - ODBC Extended Predicates for the Amzi! Logic Server

Copyright (c) 1995 by Amzi! inc.

Usage:

db_init - Initialize the DB environment.  This predicate is called
   only once, as only one DB environment is necessary to support
   multiple connections.

db_free - Closes the DB environment.  One call to this predicate
   balances the call to db_init.

db_connect(HDBC, DataSource, UserID, Password) - Connect to the named
   data source with user ID and password.  Unify HDBC with the handle
   of the connection.

db_disconnect(HDBC) - Disconnect the HDBC connection.

****************************************************************************/

#include <windows.h>
#include <stdio.h>
#ifndef _WIN32
#include <malloc.h>
#include <string.h>
#include <stdlib.h>
#endif

#include "sql.h"
#include "sqlext.h"
#include "odbcss.h"  // SQL Server definitions, note one used in connect
#include "amzi.h"

#define x_DUMPLOG  /* enable a logfile for dumping */

#ifdef _DUMPLOG
#define DUMPLOG(s) query_dump(s)
#define DUMPCLOSE  fclose(dumpfile) 
#else
#define DUMPLOG(s)
#define DUMPCLOSE
#endif


/* Global Data */
/* ----------- */

HENV g_hEnv;    /* ODBC environment */
HDBC g_hDBC;    /* The current connection */
HSTMT g_hSTMT;  /* The current statment */
ENGid g_eid;    /* The current LS engine */
BOOL g_bEnvOpen = FALSE;  /* Initialization flag */
RETCODE g_RC;   /* ODBC return code   */
RC g_lsRC;      /* Logic server return code */
SDWORD g_ColSize;
SDWORD g_ParmSize;


BOOL errchk(char* s);

/* Utility functions */
/* ----------------- */

#define ERROR_CHK(s) \
   if (g_RC != SQL_SUCCESS && g_RC != SQL_SUCCESS_WITH_INFO) \
   { \
      errchk(s); \
      if (g_RC == SQL_ERROR) goto ODBC_ERROR; \
   } \


void messout(char *s)
{
   MessageBox(NULL, s, "lsODBC Message", MB_OK);
//   lsErrRaise(g_eid, s);  /* try this */
}


/* Data structures for maintaining queries */
/* --------------------------------------- */

// This enum is similar to the amzi4.h pTYPE, but is just
// used inside this module.  It allows for the definitions
// of other types that will map back to Prolog structures,
// like date and time.
typedef enum
{
   pdATOM,
   pdINT,
   pdSTR,
   pdFLOAT,
   pdDOUBLE,
   pdSTRUCT,
   pdLIST,
   pdTERM,
   pdADDR,
   pdVAR,
   pdWSTR,
   pdWATOM,
   pdDATE,
   pdTIME
} pdTYPE;

/* In order to support multiple nested queries, it is necessary
   to save information about the SQL query.  The following section
   defines structures and functions that perform the service of
   saving the key information needed about a query, so that, on
   backtracking, the correct query can be reinstated.

   This includes saving the parameters, which are the Prolog values
   specified in the query, and the output columns, which define the
   Prolog types that will be returned as answers.
   */

typedef struct _parm
{
   struct _parm*  next;
   cTYPE  ctype;
   union
   {
      char* s;
      long  i;
      float f;
      double g;
		DATE_STRUCT* d;
		TIME_STRUCT* t;
   };
} PARM;

typedef struct _col
{
   struct _col*   next;
   pdTYPE  pdtype;
   union
   {
      char* s;
      long  i;
      float f;
      double g;
      DATE_STRUCT* d;
      TIME_STRUCT* t;
   };
} COL;

typedef struct _query
{
   struct  _query*  next;
   HSTMT   hstmt;
   PARM*   plist;
   COL*    clist;
} QUERY;

QUERY*   query_new();
void     query_del(QUERY*);
void     query_delall();
char*    query_addstrparm(QUERY*, char*);
long*    query_addintparm(QUERY*, long);
float*   query_addfloatparm(QUERY*, float);
double*  query_adddoubleparm(QUERY*, double);
char*    query_addstrcol(QUERY*, pdTYPE, long);
long*    query_addintcol(QUERY*);
float*   query_addfloatcol(QUERY*);
double*  query_adddoublecol(QUERY*);
DATE_STRUCT* query_adddatecol(QUERY*);
TIME_STRUCT* query_addtimecol(QUERY*);
#ifdef _DUMPLOG
void   query_dump();
#endif


/* Functions supporting query structures */
/* ------------------------------------- */

QUERY* QList = NULL;

QUERY* query_new()
{
   QUERY* pq;

   pq = (QUERY*)malloc(sizeof(QUERY));
   pq->hstmt = NULL;
   pq->plist = NULL;
   pq->clist = NULL;

   pq->next = QList;
   QList = pq;
// printf("created query %p\n", pq);

   return pq;
}

void query_del(QUERY* pq)
{
   PARM*  pparm;
   COL*   pcol;
   QUERY* px;

// printf("deleting query %p\n", pq);
   SQLFreeStmt(pq->hstmt, SQL_DROP);
   pq->hstmt = NULL;

   pparm=pq->plist;
   while(pparm != NULL)
   {
      pq->plist = pparm->next;
      if (pparm->ctype == cSTR)
         free(pparm->s);
      free(pparm);
      pparm = pq->plist;
   }

   pcol=pq->clist;
   while(pcol != NULL)
   {
      pq->clist = pcol->next;
      if (pcol->pdtype == pdATOM || pcol->pdtype == pdSTR)
         free(pcol->s);
      free(pcol);
      pcol = pq->clist;
   }

   if (QList == pq)
      QList = pq->next;
   else
   {
      px = QList;
      while(px != NULL)
      {
         if (px->next = pq)
         {
            px->next = pq->next;
            break;
         }
         messout("Broken LSODBC Query Chain");
      }
   }
   free(pq);
}

void query_delall()
{
   QUERY* pq;
   int    i=0;
   //char   mess[20];

   //DUMPLOG("query_delall begin");
   //messout("query_delall begin");

   pq = QList;
   while (pq != NULL)
   {
//  printf("deleting all query %p\n", pq);
      query_del(pq);
      pq = QList;
      i++;
   }
   
   //sprintf(mess, "Deleted %d queries", i);
   //messout(mess);
   //DUMPLOG("query_delall end");
}

char* query_addstrparm(QUERY* pq, char* sBuf)
{
   PARM*  pparm;
   char*  s;

   pparm = (PARM*)malloc(sizeof(PARM));
   pparm->next = pq->plist;
   pq->plist = pparm;

   pparm->ctype = cSTR;
   s = strdup(sBuf);
   pparm->s = s;

   return s;
}

long* query_addintparm(QUERY* pq, long i)
{
   PARM*  pparm;

   pparm = (PARM*)malloc(sizeof(PARM));
   pparm->next = pq->plist;
   pq->plist = pparm;

   pparm->ctype = cINT;
   pparm->i = i;

   return &(pparm->i);
}

float*  query_addfloatparm(QUERY* pq, float f)
{
   PARM*  pparm;

   pparm = (PARM*)malloc(sizeof(PARM));
   pparm->next = pq->plist;
   pq->plist = pparm;

   pparm->ctype = cFLOAT;
   pparm->f = f;

   return &(pparm->f);
}

double*  query_adddoubleparm(QUERY* pq, double g)
{
   PARM*  pparm;

   pparm = (PARM*)malloc(sizeof(PARM));
   pparm->next = pq->plist;
   pq->plist = pparm;

   pparm->ctype = cDOUBLE;
   pparm->g = g;

   return &(pparm->g);
}

char* query_addstrcol(QUERY* pq, pdTYPE pdtype, long len)
{
   COL*  pcol;
   char* s;

   pcol = (COL*)malloc(sizeof(COL));
   pcol->next = pq->clist;
   pq->clist = pcol;

   pcol->pdtype = pdtype;

   s = (char*)malloc((size_t)len+1);
   pcol->s = s;

   return s;
}

long* query_addintcol(QUERY* pq)
{
   COL*  pcol;

   pcol = (COL*)malloc(sizeof(COL));
   pcol->next = pq->clist;
   pq->clist = pcol;

   pcol->pdtype = pdINT;

   return &(pcol->i);
}

float*  query_addfloatcol(QUERY* pq)
{
   COL*  pcol;

   pcol = (COL*)malloc(sizeof(COL));
   pcol->next = pq->clist;
   pq->clist = pcol;

   pcol->pdtype = pdFLOAT;

   return &(pcol->f);
}

double*  query_adddoublecol(QUERY* pq)
{
   COL*  pcol;

   pcol = (COL*)malloc(sizeof(COL));
   pcol->next = pq->clist;
   pq->clist = pcol;

   pcol->pdtype = pdDOUBLE;

   return &(pcol->g);
}

DATE_STRUCT* query_adddatecol(QUERY* pq)
{
   COL*  pcol;
   DATE_STRUCT* pd;

   pcol = (COL*)malloc(sizeof(COL));
   pcol->next = pq->clist;
   pq->clist = pcol;

   pcol->pdtype = pdDATE;

   pd = (DATE_STRUCT*)malloc(sizeof(DATE_STRUCT));
   pcol->d = pd;

   return pd;
}

TIME_STRUCT* query_addtimecol(QUERY* pq)
{
   COL*  pcol;
   TIME_STRUCT* pt;

   pcol = (COL*)malloc(sizeof(COL));
   pcol->next = pq->clist;
   pq->clist = pcol;

   pcol->pdtype = pdTIME;

   pt = (TIME_STRUCT*)malloc(sizeof(TIME_STRUCT));
   pcol->t = pt;

   return pt;
}

#ifdef _DUMPLOG

FILE* dumpfile=NULL;

void query_dump(char* s)
{
   QUERY*  pq;
   PARM*   pparm;
   COL*    pcol;

   if (dumpfile == NULL)
      if ( NULL == (dumpfile = fopen("c:\\temp\\lsodbc.log", "w")) )
      {
         messout("Unable to open dump file");
         return;
      }

   fprintf(dumpfile, "\n----- Query dump for: %s -----\n\n", s);

   pq = QList;
   while (pq != NULL)
   {
      fprintf(dumpfile, "Q %p, next %p, hstmt %p, plist %p, clist %p\n",
            pq, pq->next, pq->hstmt, pq->plist, pq->clist);
      pparm = pq->plist;
      while (pparm != NULL)
      {
         switch(pparm->ctype)
         {
         case cSTR:
            fprintf(dumpfile, "  Parm %p, next %p, cSTR, s \"%s\"\n",
                  pparm, pparm->next, pparm->s);
            break;
         case cINT:
            fprintf(dumpfile, "  Parm %p, next %p, cINT, i %ld\n",
                  pparm, pparm->next, pparm->i);
            break;
         default:
            fprintf(dumpfile, "Invalid parm type %d\n", pparm->ctype);
         }
         pparm = pparm->next;
      }
      pcol = pq->clist;
      while (pcol != NULL)
      {
         switch(pcol->pdtype)
         {
         case pdATOM:
         case pdSTR:
            fprintf(dumpfile, "  Col %p, next %p, pdtype s%d, s \"%s\"\n",
                  pcol, pcol->next, pcol->pdtype, pcol->s);
            break;
         case pdINT:
            fprintf(dumpfile, "  Col %p, next %p, pdtype i%d, i %ld\n",
                  pcol, pcol->next, pcol->pdtype, pcol->i);
            break;
         default:
            fprintf(dumpfile, "Invalid col type %d\n", pcol->pdtype);
         }
         pcol = pcol->next;
      }
      pq = pq->next; 
   }
   fprintf(dumpfile, "\n----- No more queries -----\n");
   fflush(dumpfile);
   messout("Something dumped");
}

#endif  /* _DUMPLOG */


/* Predicate Definitions */
/* --------------------- */

TF EXPFUNC p_db_init(ENGid);
TF EXPFUNC p_db_free(ENGid);
TF EXPFUNC p_db_connect(ENGid);
TF EXPFUNC p_db_disconnect(ENGid);
TF EXPFUNC p_db_query(ENGid);
TF EXPFUNC p_db_fetch(ENGid);
TF EXPFUNC p_db_freeq(ENGid);
TF EXPFUNC p_db_freeall(ENGid);

PRED_INIT dbPreds[] =
{
   {"db_init", 0, p_db_init},
   {"db_free", 0, p_db_free},
   {"db_connect", 4, p_db_connect},
   {"db_disconnect", 1, p_db_disconnect},
   {"db_query", 5, p_db_query},
   {"db_fetch", 2, p_db_fetch},
   {"db_freeq", 1, p_db_freeq},
   {"db_freeall", 0, p_db_freeall},
   {NULL, 0, NULL}
};

/* Main DLL Entry Point */
/* -------------------- */

#if defined(_WIN32)

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD dwReason, LPVOID lpRes)
{
   switch(dwReason)
   {
      case DLL_PROCESS_ATTACH:
         break;
      case DLL_THREAD_ATTACH:
         break;
      case DLL_THREAD_DETACH:
         query_delall();
         DUMPCLOSE;
         break;
      case DLL_PROCESS_DETACH:
         query_delall();
         DUMPCLOSE;
         break;
   }
   return TRUE;
}

#else

int _pascal LibMain( HANDLE hInstance, WORD wDataSeg, WORD wHeapSize, LPSTR lpszCmdLine)
{
   return 1;
}

int _pascal _export WEP(int param)
{ 
   query_delall();
   DUMPCLOSE;
   return 1;
}

#endif

/* Required LSX Initialization Function */
/* ------------------------------------ */

#if defined(_WIN32)
__declspec(dllexport) RC EXPFUNC InitPreds(ENGid eid, void* p)
#else
RC EXPFUNC InitPreds(ENGid eid, void* p)
#endif
{
   RC rc;
   char buf[200];
   char msg[100];

   rc = lsInitPreds(eid, dbPreds);
   if (rc)
   {
      lsGetExceptMsg(eid, msg, 100);  // get the error message and display it!
      sprintf(buf, "Error Loading aODBC Predicates: %s", msg);
      MessageBox(NULL, buf, "aODBC Error", MB_OK);
   }
//   else
//      MessageBox(NULL, "aODBC Predicates Loaded", "aODBC Info", MB_OK);
   return 0;
}


/* Predicate Implementations */
/* ------------------------- */

BOOL errchk(char* s)
{
   UCHAR szSqlState[256];
   SDWORD pfNativeError;
   UCHAR szErrorMsg[SQL_MAX_MESSAGE_LENGTH];
   SWORD pcbErrorMsg;
   char buf[SQL_MAX_MESSAGE_LENGTH + 40];

   switch (g_RC)
   {
      case SQL_SUCCESS:
//         messout("success");
         break;
      case SQL_SUCCESS_WITH_INFO:
         if (SQL_NO_DATA_FOUND != SQLError(g_hEnv, g_hDBC, g_hSTMT,
               szSqlState, &pfNativeError, szErrorMsg,
               SQL_MAX_MESSAGE_LENGTH-1, &pcbErrorMsg))
            messout((char*)szErrorMsg);
         else
            messout("SQL success with info, no additional information available");
         break;
      case SQL_NO_DATA_FOUND:
         messout("no data found"); break;
      case SQL_ERROR:
         strcpy(buf, s);
         strcat(buf, "\n");
         if (SQL_NO_DATA_FOUND != SQLError(g_hEnv, g_hDBC, g_hSTMT,
               szSqlState, &pfNativeError, szErrorMsg,
               SQL_MAX_MESSAGE_LENGTH-1, &pcbErrorMsg))
         {
            strcat(buf, szSqlState);
            strcat(buf, " ");
            strcat(buf, (char*)szErrorMsg);
         }
         else
            strcat(buf, "SQL error, no additional information available");
         messout(buf);
         break;
      case SQL_INVALID_HANDLE:
         messout("invalid handle"); break;
      case SQL_STILL_EXECUTING:
         messout("still executing"); break;
      case SQL_NEED_DATA:
         messout("need data"); break;
      default:
         messout("something strange"); break;
   }
   return TRUE;
}

TF lserror()
{
   char buf[256];

   if (g_lsRC == OK)
      return TRUE;
   
   lsGetExceptMsg(g_eid, buf, 255);
   messout(buf);
   return FALSE;
}

TF EXPFUNC p_db_init(ENGid eid)
{
   if (g_bEnvOpen) return TRUE;

   g_RC = SQLAllocEnv(&g_hEnv);
   g_hDBC = SQL_NULL_HDBC;
   g_hSTMT = SQL_NULL_HSTMT;
   ERROR_CHK("SQLAllocEnv");

   g_bEnvOpen = TRUE;
   return TRUE;
ODBC_ERROR:
   return FALSE;
}


TF EXPFUNC p_db_free(ENGid eid)
{
   if (!g_bEnvOpen) return TRUE;
   g_RC = SQLFreeEnv(g_hEnv);
   g_hDBC = SQL_NULL_HDBC;
   g_hSTMT = SQL_NULL_HSTMT;
   ERROR_CHK("SQLFreeEnv");

   g_bEnvOpen = FALSE;
   return TRUE;
ODBC_ERROR:
   return FALSE;
}


TF EXPFUNC p_db_connect(ENGid eid)
{
   HDBC hdbc;
   char szDataSource[80];
   char szUserID[40];
   char szPassword[40];

   if (pVAR != lsGetParmType(eid, 1))
   {
      lsErrRaise(eid, "db_connect instantiation error: arg 1 must be var");
      return FALSE;
   }
   g_lsRC = lsGetParm(eid, 2, cSTR, szDataSource);
   if (g_lsRC != OK) goto LS_ERROR;
   g_lsRC = lsGetParm(eid, 3, cSTR, szUserID);
   if (g_lsRC != OK) goto LS_ERROR;
   g_lsRC = lsGetParm(eid, 4, cSTR, szPassword);
   if (g_lsRC != OK) goto LS_ERROR;

   g_RC = SQLAllocConnect(g_hEnv, &hdbc);
   /* set up error handler */
   g_hDBC = hdbc;
   g_hSTMT = SQL_NULL_HSTMT;
   g_eid = eid;
   ERROR_CHK("SQLAllocConnect");

   // Added this line to get around erroneous messages:
   // S1010:[Microsoft][ODBC SQL Server Driver]Function sequence error 
   // Fix from MS Knowledge Article 179226
   // SQL_PRESERVE_CURSORS - note new name in actual call
   // This only applies to SQL Server, so hopefully, nothing bad will
   // happen if it fails.

   g_RC = SQLSetConnectOption(hdbc, SQL_COPT_SS_PRESERVE_CURSORS, NULL);

   g_RC = SQLConnect(hdbc,
         (UCHAR*)szDataSource, SQL_NTS,
         (UCHAR*)szUserID, SQL_NTS,
         (UCHAR*)szPassword, SQL_NTS);
   ERROR_CHK("SQLConnect");

   lsUnifyParm(eid, 1, cADDR, &hdbc);
   return TRUE;
LS_ERROR:
   return(lserror());
ODBC_ERROR:
   return FALSE;
}

TF EXPFUNC p_db_disconnect(ENGid eid)
{
   HDBC hdbc;

   g_lsRC = lsGetParm(eid, 1, cADDR, &hdbc);
   if (g_lsRC != OK) goto LS_ERROR;
   
   /* set up error handler */
   g_hDBC = hdbc;
   g_hSTMT = SQL_NULL_HSTMT;
   g_eid = eid;

   g_RC = SQLDisconnect(hdbc);
   ERROR_CHK("SQLDisconnect");

   g_RC = SQLFreeConnect(hdbc);
   ERROR_CHK("SQLFreeConnect");

   errchk("lsODBC");
   return TRUE;   
LS_ERROR:
   return(lserror());
ODBC_ERROR:
   return FALSE;
}

TF EXPFUNC p_db_query(ENGid eid)
/* db_query/5
   ex.
   db_query(HConnect, PQuery,
      $select mother, father from person where name = ?$,
      ['Someones Name'],
      [string, string]). */
{
   HSTMT   hstmt;
   HDBC    hdbc;
   char*   sQ = NULL;
   int     iParm = 1;
   int     iCol = 1;
   TERM    tParms, tCols;
   char*   sBuf = NULL;
   char    sType[80];
   TERM    t;
   pTYPE   ptyp;
   QUERY*  pq;
   char*   s;
   long    len;
   long*   pl;
   float*  pf;
   double* pd;
   long    i;
   //float   f;
   double  g;
   DATE_STRUCT* pdate;
   TIME_STRUCT* ptime;
   char   sMsg[512];
#ifdef _DEBUG
char xbuf1[512], xbuf2[512];
#endif

   if (pVAR != lsGetParmType(eid, 2))
   {
      lsErrRaise(eid, "db_query instantiation error: arg 2 must be var");
      return FALSE;
   }

   /* create stmt structure */

   pq = query_new();

   /* add stmt handle to structure */

   g_lsRC = lsGetParm(eid, 1, cADDR, &hdbc);
   if (g_lsRC != OK) goto LS_ERROR;

/* #ifdef _DEBUG
xtest(hdbc);
#endif */

   g_RC = SQLAllocStmt(hdbc, &hstmt);
   /* set up error handler */
   g_hDBC = hdbc;
   g_hSTMT = hstmt;
   g_eid = eid;
   ERROR_CHK("SQLAllocStmt");

   pq->hstmt = hstmt;

   /* get query string from Prolog */

   sQ = (char*)malloc(lsStrParmLen(eid, 3)+1);
   g_lsRC = lsGetParm(eid, 3, cSTR, sQ);
   if (g_lsRC != OK) goto LS_ERROR;

   /* prepare SQL query */

   //g_RC = SQLPrepare(hstmt, (UCHAR*)sQ, SQL_NTS);
   //errchk("lsODBC");

   /* get parameter list from Prolog */

   g_lsRC = lsGetParm(eid, 4, cTERM, &tParms);
   if (g_lsRC != OK) goto LS_ERROR;

#ifdef _DEBUG
lsTermToStrQ(eid, tParms, xbuf1, 500);
#endif

   /* walk input list */

   while (OK == lsPopList(eid, &tParms, cTERM, &t))
   {
      /* bind parameter for each */
      ptyp = lsGetTermType(eid, t);
      switch(ptyp)
      {
      case pATOM:
      case pSTR:
         sBuf = (char*)malloc(lsStrTermLen(eid, t) + 1);
         g_lsRC = lsGetTerm(eid, t, cSTR, sBuf);
         if (g_lsRC != OK) goto LS_ERROR;
         len = strlen(sBuf) + 1;
         s = query_addstrparm(pq, sBuf);
         g_ParmSize = SQL_NTS;
         g_RC = SQLBindParameter(hstmt, (UWORD)iParm, SQL_PARAM_INPUT, SQL_C_CHAR,
               SQL_CHAR, (UDWORD)len, 0, s, (SDWORD)len, &g_ParmSize);
         ERROR_CHK("SQLBindParameter");
         break;
      case pINT:
         g_lsRC = lsGetTerm(eid, t, cLONG, &i);
         if (g_lsRC != OK) goto LS_ERROR;
         pl = query_addintparm(pq, i);
         g_ParmSize = 0;
         g_RC = SQLBindParameter(hstmt, (UWORD)iParm, SQL_PARAM_INPUT, SQL_C_SLONG,
               SQL_INTEGER, 4, 0, pl, 4, &g_ParmSize);
         ERROR_CHK("SQLBindParameter");
         break;
      case pFLOAT:
         g_lsRC = lsGetTerm(eid, t, cDOUBLE, &g);
         if (g_lsRC != OK) goto LS_ERROR;
         pd = query_adddoubleparm(pq, g);
         g_ParmSize = 0;
         g_RC = SQLBindParameter(hstmt, (UWORD)iParm, SQL_PARAM_INPUT, SQL_C_DOUBLE,
               SQL_DOUBLE, 8, 0, pd, 8, &g_ParmSize);
         ERROR_CHK("SQLBindParameter");
         break;
      default:
         messout("Unsupported Prolog type for ODBC input");
      }
      iParm++;
   }

   /* get column list from Prolog */

   g_lsRC = lsGetParm(eid, 5, cTERM, &tCols);
   if (g_lsRC != OK) goto LS_ERROR;
#ifdef _DEBUG
lsTermToStrQ(eid, tCols, xbuf2, 500);
#endif
   /* walk output list */

   while (OK == lsPopList(eid, &tCols, cSTR, sType))
   {
      /* bind col for each */
      switch(sType[0])
      {
      case 'a':
         len = 1 + atol(&sType[1]);
         s = query_addstrcol(pq, pdATOM, len);
         g_ColSize = SQL_NO_TOTAL;
         g_RC = SQLBindCol(hstmt, (UWORD)iCol, SQL_C_CHAR, s,
               len, &g_ColSize);
         ERROR_CHK("SQLBindCol");
         break;        
      case 's':
         len = 1 + atol(&sType[1]);
         s = query_addstrcol(pq, pdSTR, len);
         g_ColSize = SQL_NO_TOTAL;
         g_RC = SQLBindCol(hstmt, (UWORD)iCol, SQL_C_CHAR, s,
               len, &g_ColSize);
         ERROR_CHK("SQLBindCol");
         break;        
      case 'i':
         pl = query_addintcol(pq);
         g_ColSize = 0;
         g_RC = SQLBindCol(hstmt, (UWORD)iCol, SQL_C_SLONG, pl,
               4, &g_ColSize);
         ERROR_CHK("SQLBindCol");
         break;
      case 'f':
         pf = query_addfloatcol(pq);
         g_ColSize = 0;
         g_RC = SQLBindCol(hstmt, (UWORD)iCol, SQL_C_FLOAT, pf,
               4, &g_ColSize);
         ERROR_CHK("SQLBindCol");
         break;
      case 'g':
         pd = query_adddoublecol(pq);
         g_ColSize = 0;
         g_RC = SQLBindCol(hstmt, (UWORD)iCol, SQL_C_DOUBLE, pd,
               8, &g_ColSize);
         ERROR_CHK("SQLBindCol");
         break;
      case 'd':
         pdate = query_adddatecol(pq);
         g_ColSize  = 0;  // ignored for date
         g_RC = SQLBindCol(hstmt, (UWORD)iCol, SQL_C_DATE, pdate,
               6, &g_ColSize);
         ERROR_CHK("SQLBindCol");
         break;
      case 't':
         ptime = query_addtimecol(pq);
         g_RC = SQLBindCol(hstmt, (UWORD)iCol, SQL_C_TIME, ptime,
               6, &g_ColSize);
         ERROR_CHK("SQLBindCol");
         break;
      default:
         sprintf(sMsg,
            "Unsupported Prolog type '%c' for ODBC output", sType[0]);
         messout(sMsg);
      }
      iCol++;
   }

   /* execute query and return pointer to query */

   //g_RC = SQLExecute(hstmt);
   //errchk("lsODBC");

   g_RC = SQLExecDirect(hstmt, (UCHAR*)sQ, SQL_NTS);
   ERROR_CHK("SQLExecDirect");

   lsUnifyParm(eid, 2, cADDR, &pq);
   if (sQ != NULL) free(sQ);
   if (sBuf != NULL) free(sBuf);
   return TRUE;
LS_ERROR:
   if (sQ != NULL) free(sQ);
   if (sBuf != NULL) free(sBuf);
   return(lserror());
ODBC_ERROR:
   if (sQ != NULL) free(sQ);
   if (sBuf != NULL) free(sBuf);
   return FALSE;
}

TF EXPFUNC p_db_fetch(ENGid eid)
{
   QUERY* pq;
   HSTMT  hstmt;
   TERM   tcols;
   TERM   t;
   COL*   pCol;

#ifdef _DEBUG
TERM xt1, xt2;
char xbuf1[512], xbuf2[512];
#endif

   if (pVAR != lsGetParmType(eid, 2))
   {
      lsErrRaise(eid, "db_fetch instantiation error: arg 2 must be var");
      return FALSE;
   }

   g_lsRC = lsGetParm(eid, 1, cADDR, &pq);
   if (g_lsRC != OK) goto LS_ERROR;
   hstmt = pq->hstmt;
   
   /* set up error handler */
   g_hSTMT = hstmt;
   g_eid = eid;


   if ( SQL_NO_DATA_FOUND == (g_RC = SQLFetch(hstmt)) )
   {
      DUMPLOG("no data found");
      query_del(pq);
      return FALSE;
   }
   ERROR_CHK("SQLFetch");
   DUMPLOG("found data");

   g_lsRC = lsMakeList(eid, &tcols);
   if (g_lsRC != OK) goto LS_ERROR;

   for (pCol = pq->clist; pCol != NULL; pCol = pCol->next)
   {
      switch(pCol->pdtype)
      {
      case pdATOM:
         g_lsRC = lsMakeAtom(eid, &t, pCol->s);
         if (g_lsRC != OK) goto LS_ERROR;
         break;
      case pdSTR:
         g_lsRC = lsMakeStr(eid, &t, pCol->s);
         if (g_lsRC != OK) goto LS_ERROR;
         break;
      case pdINT:
         g_lsRC = lsMakeInt(eid, &t, pCol->i);
         if (g_lsRC != OK) goto LS_ERROR;
         break;
      case pdFLOAT:
         g_lsRC = lsMakeFloat(eid, &t, pCol->f);
         if (g_lsRC != OK) goto LS_ERROR;
         break;
      case pdDOUBLE:
         g_lsRC = lsMakeFloat(eid, &t, pCol->g);
         if (g_lsRC != OK) goto LS_ERROR;
         break;
      case pdDATE:
         g_lsRC = lsMakeFA(eid, &t, "date", 3);
         if (g_lsRC != OK) goto LS_ERROR;
         lsUnifyArg(eid, &t, 1, cSHORT, &(pCol->d->year));
         lsUnifyArg(eid, &t, 2, cSHORT, &(pCol->d->month));
         lsUnifyArg(eid, &t, 3, cSHORT, &(pCol->d->day));
         break;
      case pdTIME:
         g_lsRC = lsMakeFA(eid, &t, "time", 3);
         if (g_lsRC != OK) goto LS_ERROR;
         lsUnifyArg(eid, &t, 1, cSHORT, &(pCol->t->hour));
         lsUnifyArg(eid, &t, 2, cSHORT, &(pCol->t->minute));
         lsUnifyArg(eid, &t, 3, cSHORT, &(pCol->t->second));
         break;
      default:
         messout("Unsupported Prolog type for ODBC fetch");
      }
      g_lsRC = lsPushList(eid, &tcols, t);
      if (g_lsRC != OK) return(lserror());
   }

/* #ifdef _DEBUG
lsGetParm(eid, 2, cTERM, &xt1);
lsTermToStrQ(eid, xt1, xbuf1, 500);
lsTermToStrQ(eid, xt2, xbuf2, 500);
#endif */

   lsUnifyParm(eid, 2, cTERM, &tcols);

   return TRUE;
LS_ERROR:
   return(lserror());
ODBC_ERROR:
   return FALSE;
}


TF EXPFUNC p_db_freeq(ENGid eid)
{
   QUERY* pq;

   g_lsRC = lsGetParm(eid, 1, cADDR, &pq);
   if (g_lsRC != OK) goto LS_ERROR;
   query_del(pq);
   return TRUE;
LS_ERROR:
   return(lserror());
}

TF EXPFUNC p_db_freeall(ENGid eid)
{
   query_delall();  // get rid of any open queries first
   return TRUE;
}


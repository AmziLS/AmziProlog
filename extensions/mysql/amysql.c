// amysql.cpp : Defines the entry point for the DLL application.
//

#include "amzi.h"

#ifdef MSWIN  // from amzi.h
#include <windows.h>
#endif

#include	<stdio.h>
#include	<string.h>

#include <stdlib.h>
#include <malloc.h>

#include	<mysql.h>

/* built-in predicates, callable from Prolog */
/* ----------------------------------------- */

/* function prototypes */

TF EXPFUNC pMySQL_Init(ENGid);
TF EXPFUNC pMySQL_InitNetwork(ENGid);
TF EXPFUNC pMySQL_Connect(ENGid);
TF EXPFUNC pMySQL_Error(ENGid);
TF EXPFUNC pMySQL_Query(ENGid);
TF EXPFUNC pMySQL_Close(ENGid);

TF EXPFUNC pInitMySQL(ENGid);

/* extended predicate table definitions */

PRED_INIT arrayPreds[] = 
{
   {"mysql_init", 2, pMySQL_Init},
   {"mysql_init", 4, pMySQL_InitNetwork},
   {"mysql_connect", 1, pMySQL_Connect},
   {"mysql_error", 2, pMySQL_Error},
   {"mysql_reverse_query", 2, pMySQL_Query},
   {"mysql_close", 0, pMySQL_Close},

   {NULL, 0, NULL}
};

#ifdef MSWIN   // definition picked up from amzi.h

/* DLL Entry and Exit Points */
/* ------------------------- */

BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD dwReason, LPVOID lpRes)
{
	switch(dwReason)
	{
		case DLL_PROCESS_ATTACH:
		case DLL_THREAD_ATTACH:
		case DLL_THREAD_DETACH:
		case DLL_PROCESS_DETACH:
			break;
	}
	return TRUE;
}

/* Required LSX Initialization Function */
/* ------------------------------------ */

__declspec(dllexport) RC EXPFUNC InitPreds(ENGid eid, void* p)
{
	RC rc;
	char buf[80];

	rc = lsInitPreds(eid, arrayPreds);
	if (rc)
	{
		wsprintf(buf, "Error #%d loading amysql predicates", rc);
		MessageBox(NULL, buf, "amysql error", MB_OK);
	}
	//else
		//MessageBox(NULL, "amysql predicates loaded", "amysql info", MB_OK);
	return 0;
}
#endif   // MSWIN

#ifdef UNIX   // from amzi.h

// An LSX must be initialized by registering its predicates with the
// Prolog engine that loaded it. InitPreds is the entry point that
// the engine will call to learn the predicates defined in the LSX.

RC EXPFUNC InitPreds(ENGid eid, void* p)
{
	RC rc;
	char buf[256];

	rc = lsInitPreds(eid, arrayPreds);
	if (rc) {
      		sprintf(buf, "Error #%d loading amysql predicates", rc);
      		lsErrRaise(eid, buf);
        }
	return 0;
}
#endif

// A common data area since we aren't supporting nested queries.
MYSQL        * myData = NULL;

TF EXPFUNC pMySQL_Init(ENGid eid)
// mysql_init( USER, PASSWORD )
{
   char  user[512];
   char  password[512];
   char  errmsg[4096];

   lsGetParm(eid, 1, cSTR, user);
   lsGetParm(eid, 2, cSTR, password);

   myData = mysql_init((MYSQL*) 0);
   if (myData == NULL)
   {
      lsErrRaise(eid, "MySQL Initialization Failed");
      return FALSE;
   }

   if (NULL == mysql_real_connect( myData, NULL, user, password, NULL, MYSQL_PORT, NULL, 0 ) )
   {
      strncpy(errmsg, "MySQL Connection Failed: ", 4096);
      strncat(errmsg, mysql_error(myData), 4000);
      lsErrRaise(eid, errmsg);
      return FALSE;
   }

   return TRUE;
}

TF EXPFUNC pMySQL_InitNetwork(ENGid eid)
// mysql_init( USER, PASSWORD, HOSTNAME, PORT )
{
   char  user[512];
   char  password[512];
   char  host[4096];
   char  buf[100];
   char  errmsg[4096];
   int port;

   lsGetParm(eid, 1, cSTR, user);
   lsGetParm(eid, 2, cSTR, password);
   lsGetParm(eid, 3, cSTR, host);
   lsGetParm(eid, 4, cINT, &port);
   if (port == 0)
      port = MYSQL_PORT;
/*
   MessageBox(NULL, user, "user", MB_OK);
   MessageBox(NULL, password, "password", MB_OK);
   MessageBox(NULL, host, "host", MB_OK);
   itoa(port, buf, 10);
   MessageBox(NULL, buf, "port", MB_OK);
*/
   myData = mysql_init((MYSQL*) 0);
   if (myData == NULL)
   {
      lsErrRaise(eid,  "MySQL Network Initialization Failed");
      return FALSE;
   }

   if (NULL == mysql_real_connect( myData, host, user, password, NULL, port, NULL, 0 ) )
   {
      strncpy(errmsg, "MySQL Network Connection Failed: ", 4096);
      strncat(errmsg, mysql_error(myData), 4000);
      lsErrRaise(eid, errmsg);
      return FALSE;
   }

   return TRUE;
}

TF EXPFUNC pMySQL_Connect(ENGid eid)
// mysql_connect(DB)
{
   char db[512];
   char errmsg[4096];

   lsGetParm(eid, 1, cSTR, db);

   if ( mysql_select_db( myData, db ) < 0 )
   {
      strncpy(errmsg, "MySQL Select DB Failed: ", 4096);
      strncat(errmsg, mysql_error(myData), 4000);
      lsErrRaise(eid, errmsg);
      return FALSE;
   }

   return TRUE;
}

TF EXPFUNC pMySQL_Error(ENGid eid)
// mysql_error(num, msg)
{
   lsUnifyParm(eid, 1, cINT, mysql_errno(myData));
   lsUnifyParm(eid, 2, cASTR, mysql_error(myData));
   return TRUE;
}


// mysql_reverse_query(SQL, ROWS)
//
// Perform an SQL query and store the result as a list of lists.
// The first list is the headers.  The remaining lists contain the
// values for each row.
//
// The list of rows is reversed, but the values in each row is not.
// This predicate is designed to be called by a Prolog predicate,
// mysql_query/2, that reverses the list to proper order and performs
// any required field value conversions, like string to number.
TF EXPFUNC pMySQL_Query(ENGid eid)
{
   MYSQL_RES    * result ;
   MYSQL_FIELD  * fields;
   MYSQL_ROW    row ;
   TERM         tList;
   TERM         tRowList;
   TERM         tRowHead;
   char         * sql;
   char         msg[5000];
   int          i;
   int          num_fields;
   int			sql_length;

   // get the SQL query text
   sql_length = lsStrParmLen(eid, 1);
   sql = malloc(sql_length + 100);
   lsGetParm(eid, 1, cSTR, sql);

   // do the query
   if ( mysql_query(myData, sql) )
   {
      strcpy(msg, "MySQL Query Failed: ");
      // this next line should be in there, but I'm just saying
      // that, haven't tested it.
      //strncat(msg, mysql_error(myData), 4000);
      strncat(msg, sql, 4000);
      lsErrRaise(eid, msg);
	  free(sql);
      return FALSE;
   }

   // Save the result.
   result = mysql_store_result( myData );

   if (result == 0)
   {
      free(sql);
      return lsUnifyParm(eid, 2, cATOM, "[]");
   }

   // Create the list of lists, where each list will be a row.
   lsMakeList(eid, &tList);
   // Create the first row which is actually the headers.
   lsMakeList(eid, &tRowList);
   // Gather the fields in an array so they can be processed
   // backwards because the list gets built from back to front.
   num_fields = mysql_num_fields(result);
   fields = mysql_fetch_fields(result);
   // Make the first row of header names.
   for(i = num_fields-1; i >= 0; i--)
   {
      lsMakeStr(eid, &tRowHead, fields[i].name);
      lsPushList(eid, &tRowList, tRowHead);
   }
   // Push the header row on the list of lists which, unlike
   // the rows, will be built backwards and reversed on the
   // Prolog side of things.  This is because we might want
   // to add a feature that just gets the first N rows someday.
   lsPushList(eid, &tList, tRowList);
   // Start working through the rows.  All values will come
   // in as strings.  These are sorted out on the Prolog side
   // as well, making numbers and atoms when appropriate.
   while(row = mysql_fetch_row(result))
   {
      lsMakeList(eid, &tRowList);
      for (i = num_fields-1; i >= 0; i--)
      {
         // Replace NULL values and empty strings with "null",
         // all others come in as string values.
         if (row[i] == NULL || strlen(row[i]) == 0)
            lsMakeStr(eid, &tRowHead, "null");
         else
            lsMakeStr(eid, &tRowHead, row[i]);
         // Add the new field value to the row list.
         lsPushList(eid, &tRowList, tRowHead);
      }
      // Add the new row to the list of lists.
      lsPushList(eid, &tList, tRowList);
   }

   // Unify the list of lists with the second argument.
   lsUnifyParm(eid, 2, cTERM, &tList);
   mysql_free_result(result);
   free(sql);
   return TRUE;
}

TF EXPFUNC pMySQL_Close(ENGid eid)
{
   mysql_close(myData);
   return TRUE;
}

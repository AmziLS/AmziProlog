/* xarray.c -
*
*  Source code for a sample LSX, implementing simple
*  array handling predicates for Prolog.
*
* $Log: xarray.c,v $
* Revision 1.1.1.1  2003/09/11 02:15:15  dennis
* Starting release 7.0
*
* Revision 1.1.1.1  2002/03/15 18:35:22  dennis
* moved samples to src directory
*
* Revision 1.1.1.1  2000/12/29 02:17:38  dennis
* moved to a6 directory
*
* Revision 1.8  2000/06/04 20:43:02  mary
* Updated for Linux
*
* Revision 1.7  2000/02/14 04:03:59  dennis
* xarray building on Solaris, need to make sure LSXs use
* "C" linkage for InitPreds, otherwise we get mangled name.
*
* Revision 1.6  2000/02/13 01:04:25  dennis
* fixed load message
*
* Revision 1.5  2000/02/12 13:19:13  dennis
* update xarray for Solaris, problem with LSX still
*
* Revision 1.4  2000/01/27 10:01:45  dennis
* Samples updates
*
* Revision 1.3  2000/01/26 02:13:53  dennis
* Added Linux build to xarray, xarray-5-0-1
*
* Revision 1.2  2000/01/25 10:15:21  dennis
* fixed comment header
*
*/

#include "amzi.h"

#ifdef MSWIN  // from amzi.h
#include <windows.h>
#endif

#include <stdlib.h>
#include <malloc.h>

/* built-in predicates, callable from Prolog */
/* ----------------------------------------- */

/* function prototypes */

TF EXPFUNC pMakeArray(ENGid);
TF EXPFUNC pArrayElem(ENGid);
TF EXPFUNC pDeleteArray(ENGid);

/* extended predicate table definitions */

PRED_INIT arrayPreds[] = 
{
   {"make_array", 2, pMakeArray},
   {"array_elem", 3, pArrayElem},
   {"delete_array", 1, pDeleteArray},
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
		wsprintf(buf, "Error #%d loading xarray predicates", rc);
		MessageBox(NULL, buf, "xarray error", MB_OK);
	}
	else
		MessageBox(NULL, "xarray predicates loaded", "xarray info", MB_OK);
	return 0;
}
#endif   // MSWIN

#ifdef UNIX   // from amzi.h

// An LSX must be initialized by registering its predicates with the
// Prolog engine that loaded it. InitPreds is the entry point that
// the engine will call to learn the predicates defined in the LSX.

#ifdef LINUX
RC EXPFUNC InitPreds(ENGid eid, void* p)
#else
extern "C" RC EXPFUNC InitPreds(ENGid eid, void* p)
#endif
{
	RC rc;
	char buf[80];

	rc = lsInitPreds(eid, arrayPreds);
	if (rc)
		printf("Error #%d loading xarray predicates", rc);
	else
		printf("xarray predicates loaded\n");
	return 0;
}
#endif

/* make_array(Addr, Size)
     Addr is a variable instantiated with the address of the new array
     Size is integer size of array
     
   You might want to change the malloc to some other allocation routine
   for different environments. */

TF EXPFUNC pMakeArray(ENGid eid)
{
   long *iArray;
   int  iSize;
   TERM t;

   lsGetParm(eid, 2, cINT, &iSize);                  /* get size of array */
   iArray = (long*) malloc(iSize * sizeof(long)); 
   lsMakeAddr(eid, &t, iArray);		        /* send back address of array */
   lsUnifyParm(eid, 1, cTERM, &t);            

   return TRUE;
}

/* array_elem(Addr, Index, Elem)
     Addr is address of array
     Index is integer index into array
     Elem if instantiated is used to set array element,
          if not is given the value of the element */

TF EXPFUNC pArrayElem(ENGid eid)
{
   long *iArray;
   int   i, iElem;
   TERM  t;
   pTYPE pt;

   lsGetParm(eid, 1, cADDR, &iArray);             /* get address of array */
   lsGetParm(eid, 2, cINT, &i);                   /* get index of element */
   pt = lsGetParmType(eid, 3);      /* figure out type of third parameter */
   
   if (pt == pINT)                    /* third parameter was instantiated */
   {
      lsGetParm(eid, 3, cINT, &iElem);                   /* get its value */
      iArray[i] = iElem;                           /* put it in the array */
   }
   else if (pt == pVAR)                 /* third parameter was a variable */
   {
      lsMakeInt(eid, &t, iArray[i]);     /* fill its value from the array */
      lsUnifyParm(eid, 3, cTERM, &t);
   }
   else
      return FALSE;                       /* third parameter wasn't right */

   return TRUE;
}

TF EXPFUNC pDeleteArray(ENGid eid)
{
   long *iArray;

   lsGetParm(eid, 1, cADDR, &iArray);
   free(iArray);

   return TRUE;
}

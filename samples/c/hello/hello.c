/*---------------------------------------------------------------------
** HELLO.C - The simplest Amzi! Logic Server API (LSAPI)
**           sample program
**
** Calls the compiled Prolog file, HELLO.XPL and queries its only
** predicate, hello/2, where the arguments are:
**   1- A string with the callers name, and
**   2- A returned string with a greeting.
**
** Microsoft VC++ users can use the VC++ project to build and
** run the program.  It is a console application linked with
** AMZI.LIB for Logic Server functions.
**
** Borland C++ users should link with AMZIB.LIB.
*/

#include <stdio.h>
#include <stdlib.h>
#include "amzi.h"

ENGid CurEng;

void error()
/* Report on errors and quit. */
{
	char buf[120];

	lsGetExceptMsg(CurEng, buf, 120);
	printf("Logic Server error #%d %s", lsGetExceptRC(CurEng), buf);
	exit(0);
}

int main()
{
   char buf[120];
   TERM t;    /* a Prolog term */
   RC   rc;   /* LSAPI return code */
   TF   tf;   /* LSAPI true/false/err return codes */

/* Initialize and load the compiled Prolog program */

   if (rc = lsInit(&CurEng, "hello"))
   {
      printf("Prolog initialization error: %d\n  %s", (int)rc, buf);
      return -1;
   }
   else
      printf("\nProlog initialized\n");

   if (lsLoad(CurEng, "hello"))
      error();
   else
      printf("Hello loaded\n\n");

/* Build a query term and call it */

   tf = lsCallStr(CurEng, &t, "hello($C Programmer$, X)");

/* If the query succeeded print up the results */

   if (tf)
   {
      lsGetArg(CurEng, t, 2, cSTR, buf);
      printf("%s\n\n", buf);
   }
   else
      printf("Hello failed\n");

   lsClose(CurEng);
   printf("Prolog successfully completed\n");

   return 0;
}


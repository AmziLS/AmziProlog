/*	pets.c

	A simple example showing showing the use
   of embedded Prolog for identification.

	pets.pro

	pet(dog) :- sound(woof).
	pet(cat) :- sound(meow).
	pet(duck) :- sound(quack).
	*/

#include <stdio.h>
#include <stdlib.h>
#include "amzi.h"

/* One place to process Logic Server Exceptions. */
void error(ENGid);

int main()
{
	ENGid e1;
	TERM t;
	char buf[80];
	TF  tf;

	/*	Initialize Logic Server.  A non-zero
		return indicates a failure to open. */
	if ( lsInit(&e1, "") )
	{
		printf("Failure initializing engine one\n");
		exit(0);
	}

	/*	Load a copy of pets.xpl in the Logic Server. */
	if ( lsLoad(e1, "pets") )
		error(e1);

	/*	Add one sound to the Logic Server */
	if ( lsAssertaStr(e1, "sound(woof)") )
		error(e1);

	/*	Query the Logic Server */
	if ( TRUE == (tf = lsExecStr(e1, &t, "pet(X)")) )
	{
		lsGetArg(e1, t, 1, cSTR, buf);
		printf("Pet is a %s\n", buf);
	}
	else if (tf != FALSE)
		error(e1);


	/*	Close the Logic Server. */
	lsClose(e1);

	return(0);
}

void error(ENGid e)
/* Report on errors and quit. */
{
	char buf[120];

	lsGetExceptMsg(e, buf, 120);
	printf("Logic Server error #%d %s", lsGetExceptRC(e), buf);
	exit(0);
}

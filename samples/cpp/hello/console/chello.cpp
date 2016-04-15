/*

CHELLO.CPP - A Class for Hello

This class, which is derived from the Logic Server class,
CLogicServer, calls the compiled Prolog file, HELLO.XPL and
queries its only predicate, hello/2.  The arguments are:

	1- A string with the callers name, and
	2- A returned string with a greeting.

To use it you need HELLO.XPL which is the compiled
version of HELLO.PRO.  It is already in the \AMZI directory,
so, assuming \AMZI is on your path, the program will find
it without problem.

If you do have load errors on HELLO.XPL, check the path and
make sure HELLO.XPL is somewhere on it.

*/

#include "chello.h"
#include "stdio.h"
#include <string>
#ifdef _CONSOLE
#include <iostream>
#include <cstdio>
#endif

using namespace std;

TF CHello::init()
{
	try
	{
		// Initialize the Logic Server engine
		ls.Init("");
		msg_out("Logic Server initialized");

		// Load the compiled Prolog program, hello.xpl
		ls.Load("hello");
		msg_out("HELLO.XPL loaded");
		return TRUE;
	}
	catch(LS_EXCEPTION)
	{
		error(E);
		return FALSE;
	}
}

CHello::~CHello()
{
	// Close the Logic Server engine

	try { ls.Close(); }
	catch(LS_EXCEPTION) { error(E); }

	msg_out("Logic Server successfully closed");
}

void CHello::run()
{
	TERM   t;
	string   buf;

	try
	{
		// Build a query term and call it
		t = ls.ExecStr("hello($Windows C++ Programmer$, X)");

		// If the query succeeded print up the results
		if (t != 0)
		{
			buf = ls.TermToStr(t);
		   //buf = ls.GetStrArg(t, 2);
			msg_out(buf);
		}
		else
			msg_out("Hello failed");
	}
	catch(LS_EXCEPTION) { error(E); }
}

void CHello::error(LS_EXCEPTION)
{
	string buf;
	string msg;
   char num[100];
   int  rc;

	// Get Logic Server error number and message
#ifdef GNU
   rc = E->GetRC();
	msg = E->GetMsg();
#else
   rc = E.GetRC();
	msg = E.GetMsg();
#endif

   sprintf(num, "%d", rc);
	buf = "Logic Server Exception: " + string(num) + "\n" + msg;
	msg_out(buf);
}

void CHello::msg_out(string msg)
{
#ifdef _CONSOLE
	cout << msg << '\n';
	//printf("%s\n", msg);
#else
   MessageBox(NULL, msg.c_str(), "Hello Message", MB_OK);
#endif
}

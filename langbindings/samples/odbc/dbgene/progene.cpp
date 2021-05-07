//---------------------------------------------------------------------
// Prolog Geneology Program Interface
//

#include "stdafx.h"
#include "progene.h"
#include "dbgene.h"

CProGene::CProGene()
// The constructor initializes the Prolog engine and loads
// the Prolog program.
{
	try
	{
		Init("dbgene");
		AddLSX("aodbc", NULL);  // Load the LSX file
		Load("dbgene");

		// Normally strings sent to Prolog are scanned for escape sequences,
		// signified by a \ character, such as \n for newline.  Ordinarily
		// this is useful, but it's a pain when dealing with DOS file names.
		// This code turns that processing off.
		tf = ExecStr(&t, "set_mode(string_esc,off)");
	}
	catch(CLSException &E) { PrologError(E); }

}

CProGene::~CProGene()
// The destructor closes the Prolog engine, freeing all resources
{
	FamClose();
	Close();
}

BOOL CProGene::Open(const char * sDB)
// Open a family ODBC database.
{
	wsprintf(buf, "db_open('%s')", sDB);

	try	{ tf = ExecStr(&t, buf); }
	catch(CLSException &E) { PrologError(E); }

	if (tf == FALSE)
	{
		wsprintf(buf, "Unable to open %s", sDB);
		AfxMessageBox(buf);
		return FALSE;
	}
	strcpy(dbname, sDB);
	return TRUE;
}

BOOL CProGene::FamClose()
// Close the open ODBC database.
{
	try { ExecStr(&t, "db_close"); }
	catch(CLSException &E) { PrologError(E); }

	return TRUE;
}

BOOL CProGene::Persons(CListBox *lb)
// Gather all of the persons into a listbox.  Since the persons are
// stored as individual facts, this code uses the following API
// construct to backtrack through all the records.
//
//	 tf = Call(&t);
//	 while(tf) {
//		 ...process term after call...
//		 tf = Redo();
//		 }
{
	char Surname[NAMELEN];
	char Midname[NAMELEN];
	char Name[NAMELEN];
	int  PID;

	try
	{
		tf = CallStr(&t,"fullname(Pid, Surname, Midname, Name)");
		if (tf == FALSE)
		{
			AfxMessageBox("No family database loaded");
			return FALSE;
		}
		while(tf)
		{
			GetArg(t, 1, cINT, &PID);
			GetArg(t, 2, cSTR, Surname);
			GetArg(t, 3, cSTR, Midname);
			GetArg(t, 4, cSTR, Name);
			wsprintf(buf, "%3d: %s %s %s", PID, Name, Midname, Surname);
			lb->AddString(buf);
			tf = Redo();
		}
		return TRUE;
	}
	catch(CLSException &E) { PrologError(E); }
}

BOOL CProGene::Relationships(CListBox *lb)
// Gather all of the relations into a listbox.  Since the relations
// are stored as a Prolog list, this code uses the API list-handling
// functions to get all the of list members.  The construct is
//
//	Call(&t);	 // call the term
//	GetArg(...	// get the argument which is the resulting list
//	while (OK == PopList(&tList, ... )  // get head of list
//		...process retrieved head of list
{
	TERM  tList;

	try
	{
		tf = CallStr(&t, "relations(X)");
		if (tf)
			GetArg(t, 1, cTERM, &tList);
		else
		{
			AfxMessageBox("Failed to find relations list");
			return FALSE;
		}
		while (OK == PopList(&tList, cSTR, buf))
			lb->AddString(buf);
		return TRUE;
	}
	catch(CLSException &E) { PrologError(E); }

}

BOOL CProGene::Answers(CListBox *lb, char * relation, char * person)
// Put the answers to a query about an individual and relationship
// into a listbox.  It builds the query for the relationship, calls
// it and backtracks looking for all persons who meet the relationship
// requirement.
{
	int pid;
	char surname[NAMELEN];
	char midname[NAMELEN];
	char name[NAMELEN];

	if (NULL == strtok(person, ":"))
		return FALSE;
	pid = atoi(person);	

	wsprintf(buf, "query(%s(X,%d), PID, Surname, Midname, Name)", relation, pid);

	try
	{
		tf = CallStr(&t, buf);
		if (tf == FALSE)
		{
			wsprintf(buf, "%s has no %ss", person, relation);
			lb->AddString(buf);
			return FALSE;
		}
		while (tf)
		{
			GetArg(t, 2, cINT, &pid);
			GetArg(t, 3, cSTR, surname);
			GetArg(t, 4, cSTR, midname);
			GetArg(t, 5, cSTR, name);
			wsprintf(buf, "%3d: %s %s %s", pid, name, midname, surname);
			lb->AddString(buf);
			tf = Redo();
		}
		return TRUE;
	}
	catch(CLSException &E) { PrologError(E); }
}

int CProGene::PrologError(CLSException &E)
// Process a Prolog exception.
{
	char msg[1024] = "";
	char msgtxt[256];
	char buf[256];

	E.GetMsg(msgtxt, 255);
	sprintf(msg, "Logic Server Exception: %d %s\n", (int)E.GetRC(), msgtxt );
	ExType x = E.GetType();
	switch( x )
	{
	case INTERNAL:
		strcat(msg, "Internal error, notify Amzi! tech support\n");
		strcat(msg, "Engine shut down\n");
		break;
	case ABORT:
		strcat(msg, "Abort error - Prolog Engine shut down\n");
		break;
	case FATAL:
		strcat(msg, "Prolog resource exhausted, stacks and heaps reset\n");
		break;
	case READ:
		sprintf(buf, "Read error on line: %d\n", E.GetReadLineno());
		strcat(msg, buf);
	default:
		break;
	}

	AfxMessageBox(msg);
	if (x == INTERNAL || x == ABORT)
		return -1;
	else
		return 0;
}

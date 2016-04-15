//-------------------------------------------------------------------
// pets.cpp
//
// A simple example showing an interaction with an
// expert system.
//
// The expert system identifies pets based on sound:
//
//	pets.pro
//
//	pet(dog) :- sound(woof).
//	pet(cat) :- sound(meow).
//	pet(duck) :- sound(quack).
//
// To use it, first a sound/1 fact is asserted, then
// a query is made to pet(X) to determine the type
// of pet.
//
// Compile and link the pets.pro before using it.
// >acmp pets
// >alnk pets pets
//
// In C++ a class is defined that provides the service
// of pet identification to the rest of the application.
// That class wraps the Logic Server as part of its
// implementation.
//
// To test the error handling, rename pets.xpl to something
// else and try again.  You should see the missing .xpl
// file message.
//

#include <iostream>
#include <cstdio>
#include <string>
#include "logicserver.h"

using namespace std;

class petID
{
private:
   LogicServer ls;   // wrap Logic Server in class
public:
	petID();
	~petID();
	void SetSound(string);
	string GetPet();
};

int main()
{
	try
	{
		petID p1;
		string buf;

		p1.SetSound("woof");

		buf = p1.GetPet();
		cout << "pet is a " << buf << '\n';
	}
#ifdef GNU
	// A bug causes segmentation faults when CLSException
	// objects are thrown in Gnu C++, so pointers to the
	// object are thrown instead.  See amzi.h for details.
	catch(LSException *e)
	{
		string errmsg = e->GetMsg();
		cout << "Logic Server Exception " << e->GetRC()
			<< ": " << errmsg << '\n';
		return -1;
	}
#else
	catch(LSException &e)
	{
		string errmsg = e.GetMsg();
		cout << "Logic Server Exception " << e.GetRC()
			<< ": " << errmsg << '\n';
		return -1;
	}
#endif
	return 0;
}

petID::petID()
{
   // Initialize the Logic Server and load the
   // pets.xpl file.

	ls.Init("");
	ls.Load("pets");
}

petID::~petID()
{
	// Close the Logic Server
	ls.Close();
}

void petID::SetSound(string sound)
{
	// put the sound into a string for a Prolog fact
   string buf("sound(" + sound + ")");
	ls.AssertaStr(buf);
}

string petID::GetPet()
{
	TERM t;

	// query for the pet and if successful get
	// the argument to for the caller
   t = ls.ExecStr("pet(X)");
	if ( t )
		return ls.GetStrArg(t, 1);
	else
		return "unknown";
}


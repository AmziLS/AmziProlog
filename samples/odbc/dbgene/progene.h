//---------------------------------------------------------------------
// wgene.hpp - header file for wgene application demo
//

#include "amzi.h"

const int NAMELEN=60;
const int BUFLEN=120;

//---------------------------------------------------------------------
// Prolog Interface to GENE.PRO
//
// The class ProGene is derived from the C++ wrapper around the Logic
// Server API.  It extends that class by adding the particular Prolog
// program, GENE.PRO, and member functions that access GENE.PRO.
// This way, the rest of the C++ program has access to the services of
// the Prolog program only through the well-defined interface to the
// ProGene class.
//

class CProGene : public CLogicServer
{
private:
   TERM     t;
   RC       rc; 
   TF       tf;
   char     buf[BUFLEN];
   char     dbname[BUFLEN];

public:
           CProGene();
           ~CProGene();
   BOOL    Open(const char *);    // Open a family DB
   BOOL    FamClose();            // Close the current family DB
   BOOL    Persons(CListBox *);   // Gather all the persons in a list box
   BOOL    Relationships(CListBox *);  // Gather the relationships in a list box
   BOOL    Answers(CListBox *, char * relation, char * name);
private:
   int    PrologError(CLSException &E);
};


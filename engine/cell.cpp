/*****************************************************************\
*
* cell.cpp - the basic Prolog cell
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: cell.cpp,v $
* Revision 1.2  2003/12/10 22:29:04  dennis
* built dll with copy all feature for the dynamic db for now
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.21  2002/12/11 17:29:42  dennis
* new build for 6-3-2, using visual studio .net
*
* Revision 1.20  2002/12/06 17:37:36  dennis
* a6-3-2, finally got rid of mscw tagging of heap cells to maintain
* state of dynamic clause backtracking, put backtracking info in
* the choice point structure instead.  much cleaner.  retract can now
* just analyze the control stack to see if a retract is safe.  heapgc
* no longer has to worry about those wierd cells.
*
* Revision 1.19  2002/07/04 16:20:25  dennis
* support academic registration
*
* Revision 1.18  2002/05/15 16:59:07  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.17  2002/03/25 22:24:06  dennis
* Added read/write_binary predicates to replace fwrite, also repositioning
* for binary files. Note that even though the standard says there's a read
* and write position, they are one and the same.
*
* Revision 1.16  2002/03/04 04:22:53  dennis
* changed sorted iterator erase again, as gcc follows the stl standard,
* which is dumb but standard, and ms, hate to see them as the good guys,
* does the right thing, which is not standard
*
* Revision 1.15  2002/02/13 03:19:59  dennis
* reals changed to have a LReal class, moved to file of same name,
* math functions moved out of termsvc and into lmath.cpp/h, eval rewritten
* to reflect various options for numbers, lexcept modified so anyone, even
* non-engine objects, can throw LExcept objects.
*
* Revision 1.14  2002/02/04 17:20:58  dennis
* New lreal wrapper class on reals, start of number options:
* decimals: real/float, floats: single/double.  Created lmath.h/cpp
* but nothing in them yet.  Work to be done on sorting out mixed mode
* arithmetic for different options.  Also - casts removed, as they
* were causing problems.
*
* Revision 1.13  2002/01/28 06:29:19  dennis
* changes for parsing numbers, handling different options
*
* Revision 1.12  2002/01/06 20:31:27  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.11  2001/11/09 02:28:09  dennis
* Finished, I hope, sorted and indexed predicates. Needed to rethink
* how mscws worked, given the new itertors for moving from clause to
* clause.
*
* Revision 1.10  2001/10/19 01:37:59  dennis
* compiler bugs, still not found, but noted that X registers
* are really restricted to 255 because of flewrite in assemb.pro,
* should change some day.
*
* Revision 1.9  2001/10/13 02:58:12  dennis
* see/tell bugs, used to close function streams
*
* Revision 1.8  2001/10/05 17:07:01  ray
* Added real things. Fixed some bugs. Stopped adding '.' in prep
*
* Revision 1.7  2001/08/01 20:17:59  ray
* removed tswrite.cpp & sio.cpp, added streams.cpp sowrite.cpp
*
* Revision 1.6  2001/06/27 15:15:09  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.5  2001/04/01 15:54:52  ray
* Modified compiler and loader for fixed data.
*
* Revision 1.4  2001/03/25 15:29:50  ray
* Implemented fixed
* repaired #include
*
* Revision 1.3  2001/02/27 21:09:13  ray
* removed ? from properNames
* implemented integer/1 for reals
* repaired -ve reals error
*
* Revision 1.2  2001/02/21 04:46:42  dennis
* debugger working, updated documentation for 6.1
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.13  2000/12/19 20:35:27  ray
* Modified lex for '.' operators
* Added float_real/2 (bilateral)
*
* Revision 1.12  2000/10/30 15:11:45  ray
* Extended compiler, loader and linker for real data.
* Added arithmetic primitives for rational data in alib.pro
*
* Revision 1.11  2000/10/24 14:00:45  ray
* Added atom_chars.
* Performed cosmetic surgery on termsvc.
*
* Revision 1.10  2000/10/10 13:49:36  ray
* Replaced all assembly code with longlong data, on the assumption
* that all systems can define longlong.
*
* Revision 1.9  2000/10/03 02:03:01  dennis
* patched linux back to windows, both OK now
*
* Revision 1.8  2000/10/03 01:37:17  dennis
* Got it running under Linux, had to ifdef out reals for now
*
* Revision 1.7  2000/09/25 02:11:19  dennis
* first version of modules working, runs the modular version of
* duck world.  still needs import and export.  release 6.1.1
*
* Revision 1.6  2000/09/20 17:09:12  ray
* added casting operators to cell and rand to clean up code.
* added prolog flag 'modulo' and adapted arithmetic to use it.
* added fraction, realSpan and nth.
* added fast ** for reals.
*
* Revision 1.5  2000/09/15 21:42:24  dennis
* 12->13
*
* Revision 1.4  2000/08/14 02:05:37  ray
* Added Real class.
* No division yet.
* No error reporting yet.
*
* Revision 1.3  2000/05/14 03:52:32  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
* Revision 1.2  2000/03/28 01:05:15  dennis
* merged Ray's changes with bigdig.  bigdig is at point where
* new Cell class is used, but there are no modules in the system.
*
* Revision 1.1.2.3  2000/03/14 09:02:57  dennis
* getting further
*
* Revision 1.1.2.2  2000/03/13 06:07:44  dennis
* got cell, stash and termsvcs to compile
*
* Revision 1.1.2.1  2000/03/09 05:40:39  dennis
* reworked unify functions
*
*
\******************************************************************/

#include "inc.h"
#include "pch.h"

Real listToReal(TERM);

aCHAR * celltypes[] =
{
/*  0 */      aS("nil"),
/*  1 */      aS("reference"),
/*  2 */      aS("structure"),
/*  3 */      aS("list"),
/*  4 */      aS("atom"),
/*  5 */      aS("ddb_ref"),
/*  6 */      aS("string"),
/*  7 */      aS("mscw"),
/*  8 */      aS("rheap"),
/*  9 */      aS("stash"),
/* 10 */      aS("unused"),
/* 11 */      aS("mscw get"),
/* 12 */      aS("mscw retract"),
/* 13 */      aS("pointer"),
/* 14 */      aS("error"),
/* 15 */      aS("error"),

/* 16 */      aS("int"),
/* 17 */      aS("char"),

/* 18 */      aS("error"),
/* 19 */      aS("error"),
/* 20 */      aS("error"),
/* 21 */      aS("error"),
/* 22 */      aS("error"),
/* 23 */      aS("error"),
/* 24 */      aS("error"),
/* 25 */      aS("error"),
/* 26 */      aS("error"),
/* 27 */      aS("error"),
/* 28 */      aS("error"),
/* 29 */      aS("error"),
/* 30 */      aS("error"),
/* 31 */      aS("error"),

/* 32 */      aS("fixed"),
/* 33 */      aS("real"),
/* 34 */      aS("float")
};

Cell::Cell(GCReal *gcrr)
  { 
	 gcr = gcrr; 
	 setImod(0); 
	 setArity(0); 
	 setType(realS); 
  } 

Cell::Cell(GCDouble *gcdd)
  { 
	 gcd = gcdd; 
	 setImod(0); 
	 setArity(0); 
	 setType(doubleS); 
  } 

// Compare two constant cells.  Should be called with the
// types the same, so not checked here.
int Cell::operator==(Cell &c) 
{
   fixedC f2;

   switch (getType())
   {
   case atomS:
      return (a == c.a);
   case intS:
   case charS:
      return (i == c.i);
   case doubleS:
      return (*gcd == *(c.gcd));
   case singleS:
      return (f == c.f);
   case realS:
      return (gcr == c.gcr);
   case fixedS:
      f2 = c.getFixed();  // gcc for some reason needs this extra step
      return (getFixed() == f2);
   case strS:
      return (*(s->s) == *(c.s->s));
   //case dbrefS:
   //   return (db == c.db);
   default:
      return (getType() == c.getType() && getImod() == c.getImod() &&
         getArity() == c.getArity() && t == c.t);
   }
}

Lostream& operator<<( Lostream &os, Cell &c )
{
   //os << c.module << aS(":");
   os << celltypes[c.getType()] << SP;
   switch(c.getType())
   {
	  //   case nilT:                               // ray
   case unusedT:
   case rheapS:
      break;

   case refT:
   case strucT:
   case listT:
      os << c.t;
      break;

   case intS:
   case charS:
      os << c.i;
      break;

   case doubleS:
      os << c.gcd;
      break;
   case singleS:
      os << c.f;
      break;

   //case mscwS:
   //case dbrefS:
   //   os << c.db;
   //   break;

//   case strS:
//      os << c.s << aS(":") << *(c.s);
//      break;
   case strS:
      os << c.s->s << aS(":") << *(c.s->s);
      break;

   case atomS:
      os << c.a;
      break;

   case stashS:
      os << c.gst;
      break;

   default:
      os << aS("bad cell type");
   }
   return os;
}

//   walks along a term counting the number of cells needed to
//   contain it:
//
//   This errs on the large size since it follows references.
//   Consequently a substructure in the term which is referenced
//   more than once in the term will have that substructure counted
//   in its entirety twice. 
//
//   Note however that CopyTerm (for which Size() is needed) also
//   commits this sin - so don't fix this unless you fix CopyTerm()

int Cell::term_size()
{
   int size;
   try
   {
      size = _term_size(0);
   }
   catch(...)
   {
      throw LExcept(cyclicE);
   }
   return size;
}

int Cell::_term_size(int nests)
{
  register int count_cell;
  int ar;
  
  //if (nests > 20)
  //   throw LExcept(cyclicE);
  int nests2 = nests+1;

  TERM t = this;
  
  t = (t)->dref();
  switch(t->getType())
    {
    case strucT:
      count_cell = 2;                    // one for strucT, one for func info 
      t = t->getTerm();
      ar = t->getArity();
      while(ar--)
        count_cell += (++t)->_term_size(nests2);
      return(count_cell);
      
    case listT:
      count_cell = 0;
      while (t->IsList())
        {
          ++ count_cell;                    // list cell 
          t = t->getTerm();                 // cons 
          count_cell += (t)->_term_size(nests2);
          ++t;                              // cdr 
        }
      count_cell += (t)->_term_size(nests2);       // cdr 
      return(count_cell);

    default:                                // refs and consts
       return 1;
    }
  
  return(0);                                // a formality 
}

Real Cell::getReal()
{   return gcr->r; }

// use this when the caller might delete the real,
// as eval does.
Real Cell::getRealCopy()
{
   Real rr;
   LNEWX(rr, LReal(*(gcr->r)));
   return rr;
}

double Cell::getDouble()
{   return gcd->d; }

STRptr Cell::getStr()
{   return *(s->s); }

Stash *Cell::getStash()
{   return gst->st; }

LString * Cell::getLString()
{   return s->s; }

//DynamicClauseIterator *Cell::getDBRef()
//{   return db->pdci; }

//DynamicClauseIterator *Cell::getMSCWDBRef()
//{   return db->pdci; }

void Cell::gc_protect_plm(short file_ix)
{
   LASSERT(IsGCThing(), aS("Protecting non-gc thing"));
   gc->set_plm(file_ix);
}

//#ifdef LANDFILL
//{ LASSERT(IsGCThing(), aS("Protecting non-gc thing"));
// gc->protect(); }
//#else
//{  gc->protect(); }
//#endif

void Cell::setFixed(fixedC ff)
{ 
  i = ff.getFixedHI(); 
  descr = ff.getFixedLO(); 
}

fixedC Cell::getFixed()
{
   fixedC ff;
   ff.init_hilo(i, descr);
   return ff;
}

double Cell::forceDouble()                // cast cell to double
{
   switch(getType())
   {
   case intS:
   case charS:
      return (double)getInt();
      break;
   case singleS:
      return (double)getSingle();
      break;
   case doubleS:
      return getDouble();
      break;
   case realS:
      return getReal()->toDouble();
      break;
   case fixedS:
      return getFixed().toDouble();
      break;
   default:
      throw LExcept(number_castE, aS("to double"));
   }
}

int Cell::forceInt()                // cast cell to double
{
   switch(getType())
   {
   case intS:
   case charS:
      return getInt();
      break;
   case singleS:
      return (int)getSingle();
      break;
   case doubleS:
      return (int)getDouble();
      break;
   case realS:
      return getReal()->toInt();
      break;
   case fixedS:
      return getFixed().toInt();
      break;
   default:
      throw LExcept(number_castE, aS("to integer"));
   }
}





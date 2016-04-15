/****************************************************************************
*
* termsvc.cpp -- Term services
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: termsvc.cpp,v $
* Revision 1.14  2006/12/15 04:05:56  dennis
* undecapitalization of module names
*
* Revision 1.13  2005/11/08 02:50:34  dennis
* add ground/1 built-in
*
* Revision 1.12  2005/02/21 17:40:08  dennis
* vba addition
*
* Revision 1.11  2005/02/09 17:35:39  dennis
* ?var added
*
* Revision 1.10  2004/11/02 14:02:32  dennis
* numbers unify
*
* Revision 1.9  2004/04/12 18:09:02  dennis
* Allowed multiple lsx_loads to quietly continue
*
* Revision 1.8  2004/04/09 22:02:02  dennis
* fixed stack frame problem in adebug.lsx
*
* Revision 1.7  2004/03/27 19:18:22  dennis
* added is_cyclic and better cyclic checking in write
*
* Revision 1.6  2004/03/26 21:37:10  dennis
* implemented occurs check
*
* Revision 1.5  2004/01/15 20:29:47  dennis
* fixed nasty unifycopy gc bug
*
* Revision 1.4  2003/12/23 21:09:57  dennis
* speed improvement, added unifycopy function for clauseZdb, so it goes quicker,
* 30-40% improvement in rubik interpreted, 10-15% improvement compiled.
*
* Revision 1.3  2003/12/19 23:13:10  dennis
* intermediate changes, work in progress - DCG support in reader, ddb unify
*
* Revision 1.2  2003/12/10 22:29:04  dennis
* built dll with copy all feature for the dynamic db for now
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.62  2003/07/24 17:30:40  dennis
* support for debugging compiled code, reorg of alib etc to support that
*
* Revision 1.61  2002/12/06 17:37:36  dennis
* a6-3-2, finally got rid of mscw tagging of heap cells to maintain
* state of dynamic clause backtracking, put backtracking info in
* the choice point structure instead.  much cleaner.  retract can now
* just analyze the control stack to see if a retract is safe.  heapgc
* no longer has to worry about those wierd cells.
*
* Revision 1.60  2002/08/12 16:36:16  dennis
* a6-2-11 changes
*
* Revision 1.59  2002/06/23 20:01:30  dennis
* fixed some gc issues
*
* Revision 1.58  2002/06/02 04:15:42  dennis
* updated makefiles
*
* Revision 1.57  2002/06/02 03:50:57  dennis
* all the XStr forms of logic server calls call strterm and grow the
* heap, so made new ExecProve and CallProve that to the strterm inside
* so that the heap can rolled back to before the Exec/Call.  Important
* that this be done in the Prove, so that if heapgc is encountered,
* the new heap is used for the rollback.
*
* Revision 1.56  2002/05/15 16:59:09  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.55  2002/05/01 02:26:33  dennis
* minor fixes
*
* Revision 1.54  2002/04/09 17:31:18  ray
* corrected error in divide and mod arith
*
* Revision 1.53  2002/03/22 22:42:36  dennis
* took out calls to e_inittty and e_resettty to get logic server to
* work in background under Linux, but strangely, key$b still works
* as does the debugger which uses it...
*
* Revision 1.52  2002/03/21 01:25:10  dennis
* allow separator in file names to be either tilt on slash, converts
* to correct one for Unix or Windows
*
* Revision 1.51  2002/03/04 03:46:12  dennis
* fixed bug in sorted predicates
*
* Revision 1.50  2002/02/19 19:38:10  ray
* Reaffirmation
*
* Revision 1.49  2002/02/14 16:57:27  ray
* replaced divide and shift routines. Corrected LReal constructor calls
*
* Revision 1.48  2002/02/13 19:56:57  dennis
* fixed short bug, floatToReal, and real ** 1
*
* Revision 1.47  2002/02/13 03:20:00  dennis
* reals changed to have a LReal class, moved to file of same name,
* math functions moved out of termsvc and into lmath.cpp/h, eval rewritten
* to reflect various options for numbers, lexcept modified so anyone, even
* non-engine objects, can throw LExcept objects.
*
* Revision 1.46  2002/02/04 17:20:59  dennis
* New lreal wrapper class on reals, start of number options:
* decimals: real/float, floats: single/double.  Created lmath.h/cpp
* but nothing in them yet.  Work to be done on sorting out mixed mode
* arithmetic for different options.  Also - casts removed, as they
* were causing problems.
*
* Revision 1.45  2002/01/28 06:29:20  dennis
* changes for parsing numbers, handling different options
*
* Revision 1.44  2002/01/27 22:52:05  ray
* Corrections to power and fixed
*
* Revision 1.43  2002/01/20 20:48:06  ray
* revised real divide, printReal
*
* Revision 1.42  2002/01/06 20:31:28  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.41  2001/12/03 20:20:52  dennis
* fixed copyterm to allow infinite variables, not limited to maxvars.
*
* Revision 1.40  2001/10/27 03:24:37  dennis
* sorted dynamic predicates working, with optimized queries
* when they fit the right pattern, of vars last
*
* Revision 1.39  2001/10/26 01:28:16  dennis
* sorted predicates partially implemented
*
* Revision 1.38  2001/10/19 01:38:00  dennis
* compiler bugs, still not found, but noted that X registers
* are really restricted to 255 because of flewrite in assemb.pro,
* should change some day.
*
* Revision 1.37  2001/10/06 17:34:08  ray
* implemented power for fixed
*
* Revision 1.36  2001/10/06 03:08:00  dennis
* running on Linux mostly, compiling compiler at least
*
* Revision 1.35  2001/10/05 19:52:50  dennis
* changed real xor to rxor to make gcc happy
*
* Revision 1.34  2001/10/05 19:15:17  dennis
* string streams, function streams working, reals set up to not require
* m_peng, just passed in parameters as necessary
*
* Revision 1.33  2001/10/05 17:07:01  ray
* Added real things. Fixed some bugs. Stopped adding '.' in prep
*
* Revision 1.32  2001/10/02 16:05:21  dennis
* changed streams, cleaner interface to lex, reimplemented
* readers as buffer between the two
*
* Revision 1.31  2001/09/11 04:34:56  dennis
* cleaned up some io stuff, got consult working, etc.
*
* Revision 1.30  2001/08/01 20:18:00  ray
* removed tswrite.cpp & sio.cpp, added streams.cpp sowrite.cpp
*
* Revision 1.29  2001/07/21 00:39:47  dennis
* added garbage collector for strings and things
*
* Revision 1.28  2001/07/10 16:51:32  dennis
* added more memory leak checking tools, all clean so far
*
* Revision 1.27  2001/06/27 15:15:11  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.26  2001/04/15 14:58:09  ray
* modified primes to use vector<int>
* corrected preop '-'
*
* Revision 1.25  2001/04/02 19:09:51  ray
* Repaired arithmetic errors
*
* Revision 1.24  2001/04/01 15:54:52  ray
* Modified compiler and loader for fixed data.
*
* Revision 1.23  2001/03/28 21:46:11  ray
* repaired arithmetic bugs
*
* Revision 1.22  2001/03/28 15:07:21  ray
* char_code/2, number_chars/2 added
* 1 char macros proscribed
*
* Revision 1.21  2001/03/26 19:16:56  ray
* moved various string_ builtins to alib
* repaired arithmetic.
*
* Revision 1.20  2001/03/25 15:29:51  ray
* Implemented fixed
* repaired #include
*
* Revision 1.19  2001/03/13 20:05:18  dennis
* added meta$call to try and isolate meta calls.
*
* Revision 1.18  2001/02/28 02:22:39  dennis
* fixed some number bugs, some assert/retract bugs
*
* Revision 1.17  2001/02/27 21:09:13  ray
* removed ? from properNames
* implemented integer/1 for reals
* repaired -ve reals error
*
* Revision 1.16  2001/02/22 15:52:00  ray
* repaired rpoper names and proper quotes
*
* Revision 1.15  2001/02/21 04:46:43  dennis
* debugger working, updated documentation for 6.1
*
* Revision 1.14  2001/02/13 03:42:13  dennis
* trying to fix bug in lsExecStr, cleaning up user:
*
* Revision 1.13  2001/02/11 19:59:33  ray
* fixed reconsult bug
*
* Revision 1.12  2001/02/10 05:02:57  dennis
* fixed assert/retract gc bug
*
* Revision 1.11  2001/02/10 00:22:35  ray
* changed real descriptor layout (again)
*
* Revision 1.10  2001/02/08 22:56:45  dennis
* string bug fixes, modularized compiler and listener
*
* Revision 1.9  2001/02/07 16:48:07  ray
* made float_real bilateral, without using strings
*
* Revision 1.8  2001/02/05 03:11:44  dennis
* Fixed nasty heap GC bug, other minor fixes
*
* Revision 1.7  2001/02/02 15:22:31  ray
* corrected read(X)
* enabled arithmetic with char args
*
* Revision 1.6  2001/01/29 00:43:09  ray
* added is_integer/1 and is_fraction/1
*
* Revision 1.5  2001/01/21 23:02:35  ray
* Rewrote real divide
* Added real sqrt
*
* Revision 1.4  2001/01/11 01:49:23  dennis
* Implemented rest of import/export and metapredicates, working
* for test cases.
*
* Revision 1.3  2001/01/01 15:46:28  ray
* Added float to real promotion in expressions
*
* Revision 1.2  2000/12/29 17:00:41  dennis
* Added long long type for INT64 for Linux, minor glitches, now
* builds on Linux OK.
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.46  2000/12/28 16:30:36  dennis
* fixed nasty pets bug, true/false on CallInterp()
* and other changes, merged with ray's new stuff
* calling this a6-1-6
*
* Revision 1.45  2000/12/19 20:35:28  ray
* Modified lex for '.' operators
* Added float_real/2 (bilateral)
*
* Revision 1.44  2000/12/13 21:14:00  ray
* Added epsilon flag for reals.
*
* Revision 1.43  2000/11/30 15:47:29  ray
* made arg, nth and prime bilateral.
* made real length and exponent 12 bits for maximum range
*
* Revision 1.42  2000/11/10 17:09:18  ray
* Restricted real length to 255 in order to add max length, for protection
*
* Revision 1.41  2000/11/09 17:27:29  ray
* Added exceptions for prime primitives
*
* Revision 1.39  2000/10/31 16:44:41  ray
* integer overflows now automatically promote to real.
*
* Revision 1.38  2000/10/30 16:47:53  ray
* Corrections
*
* Revision 1.36  2000/10/24 14:00:45  ray
* Added atom_chars.
* Performed cosmetic surgery on termsvc.
*
* Allowed termsvc to admit there could be more than two arith data types
*
* Revision 1.35  2000/10/22 17:54:13  ray
* Made char lists and char integers produce char type
* Made string-integer test for non-digi* Revision 1.34  2000/10/20 12:40:52  ray
* Moved =:= out of alib to accomodate reals.
* Various repairs and enhancements
*
* Revision 1.34  2000/10/20 12:40:52  ray
* Moved =:= out of alib to accomodate reals.
* Various repairs and enhancements
*
* Revision 1.33  2000/10/14 22:14:05  ray
* redefined GD for very long arrays, and named the descriptor substructure 
* and enhanced all references
*
* Revision 1.32  2000/10/12 19:05:14  ray
* Corrected mult for real.
* Added clock for integer calculation of time in milliseconds.
* Added run time printout to alis
*
* Revision 1.31  2000/10/10 13:49:36  ray
* Replaced all assembly code with longlong data, on the assumption
* that all systems can define longlong.
*
* Revision 1.30  2000/10/07 17:57:09  ray
* deleted realSpan, modified nth, added packLong (for compiler),
* made lload load reals
* added inverse/3 for modulo divide.
*
* Revision 1.29  2000/10/03 02:03:01  dennis
* patched linux back to windows, both OK now
*
* Revision 1.28  2000/10/03 01:37:18  dennis
* Got it running under Linux, had to ifdef out reals for now
*
* Revision 1.27  2000/10/01 17:02:12  dennis
* cleaned up bigdig comments
*
* Revision 1.26  2000/10/01 16:20:04  dennis
* cleaned up modules, ddb, got defined, abolish and friends working
*
* Revision 1.25  2000/09/20 17:09:12  ray
* added casting operators to cell and rand to clean up code.
* added prolog flag 'modulo' and adapted arithmetic to use it.
* added fraction, realSpan and nth.
* added fast ** for reals.
*
* Revision 1.26  2000/09/06 20:07:32  ray
* Added ** for Real, corrected integer div
*
* Revision 1.25  2000/09/05 20:24:29  ray
* Added real divide
*
* Revision 1.24  2000/09/02 02:10:20  dennis
* new version of get$db replacing dbrefgen for getting clauses
* from dynamic database
*
* Revision 1.23  2000/09/01 21:06:32  ray
* Added // mod divu divs modu mods >> << to Real
* Still no /
*
* Revision 1.22  2000/08/27 01:57:03  ray
* corrected list_real
*
* Revision 1.21  2000/08/26 00:32:08  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.20  2000/08/21 20:36:53  ray
* reversed the order of gigadigits in real array
* real arithmetic installed for add, subtract, multiply.
*
* Revision 1.19  2000/08/14 02:05:38  ray
* Added Real class.
* No division yet.
* No error reporting yet.
*
* Revision 1.18  2000/05/15 11:24:48  dennis
* fixed some minor bugs, started modules
*
* Revision 1.17  2000/05/14 03:52:35  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
* Revision 1.16  2000/05/11 00:44:01  ray
* open, close, get_char, put_char, get_code, put_code, write_term, write_canonical, current_input, current_output, compound.
*
* Revision 1.15  2000/05/04 21:47:56  ray
* log, log10, xor, round, floor, ceiling, truncate/1, realpart, fractionpart
*
* Revision 1.14  2000/04/21 02:49:06  ray
*
* Added current_prolog_flags
*
* Revision 1.13  2000/04/18 01:32:01  ray
*
* Added builtin stream_attrs/1 for users and error reporting.
*
* Revision 1.12  2000/04/12 18:59:07  ray
*
* Allowed termsvc to admit there could be more than two arith data types
*
* Revision 1.11  2000/04/02 23:54:45  ray
*
* Fixed + and - bug
*
* Revision 1.10  2000/03/28 23:47:52  dennis
* Changed all tabs to three spaces, and also changed Logic Server
* to use void* for TERM externally and cast to Cell* in LEngine
* implementation.
*
* Revision 1.9  2000/03/28 01:05:17  dennis
* merged Ray's changes with bigdig.  bigdig is at point where
* new Cell class is used, but there are no modules in the system.
*
* Revision 1.8  2000/03/06 05:34:29  ray
* *** empty log message ***
*
* Revision 1.7  2000/02/21 20:36:23  ray
* *** empty log message ***
*
* Revision 1.6.2.4  2000/03/13 06:07:45  dennis
* got cell, stash and termsvcs to compile
*
* Revision 1.6.2.3  2000/03/09 05:40:41  dennis
* reworked unify functions
*
* Revision 1.6.2.2  2000/03/08 04:12:02  dennis
* builtin.cpp compiles
*
* Revision 1.6.2.1  2000/02/26 20:56:13  dennis
* Removed local atoms from compiler, and old module support, so
* compiler and listener are all global for now.  Also made member/2
* and friends built-ins as well as the bug predicates.
*
* Revision 1.6  2000/02/12 12:35:20  dennis
* put ifdef _UNICODE around all Lsprintf's that used %s, so that
* they use %ls under _UNICODE builds.  yuck.
*
* Revision 1.5  2000/02/10 15:35:17  dennis
* changed Lsprintf to require second argument for length, in keeping
* with new swprintf, which differed from older more dangerous sprintf
*
* Revision 1.4  2000/02/03 04:25:32  dennis
* some minor porting clean ups
*
* Revision 1.3  2000/01/28 11:05:42  dennis
* Fixed 'listing' bug caused by cur$atom wanting a short int
* (as it should for an atom) but being fed a long one from the
* new for/4, which drives the cur_atom loop.
*
* Revision 1.2  2000/01/17 09:51:52  dennis
* original a5x modified for new directory structure and
* names, sans the 5, like alnk and alis
*
* 1999/07/11 Ray rewrote lexord for full integers
*
* 1999/07/08 Ray rewrote p_for for full integers
*
* 1998/12/26 Ray added 1st time thru boundary check to for,
* and allowed negative increment
*
* Set usecnt for doubleS. 1998/11/06 ray
*
* Revision 4.4  1998/06/13 14:01:23  ray
* redefined max & min to suit doubleS
*
* Revision 4.3  1998/06/11 16:26:42  ray
* max/min, same as Pete
*
* Revision 4.2  1998/01/18 23:15:34  pete
* Removed reference to unused argument shadowing a member variable.
*
* Revision 4.1  1998/01/15 02:27:31  pete
* Initial changes for solaris
*
*
****************************************************************************/

#include <vector>
#include <queue>
#include <cctype>
#include "inc.h"
#include "pch.h"

//using namespace std;  // NO! qualify all names with std::

// debugging flags
#ifdef LANDFILL
#define noBUG_UNIFY_COPY
#define noBUG_LEXORD
#define noBUG_COPY
#endif

static  aCHAR buf[80];

/*  defined in termsvc.h as well, but slightly different, which is right?
int signum(long n)
  { return n < 0 ? -1 : (n == 0 ? 0 : 1); }

int signum(fixedC n)
  { return (n.hi == 0) && (n.lo == 0) ? 0 : (n.hi & 0x80000000 ? -1 : 1); }

int signum(double n) 
  { return n < 0 ? -1 : (n == 0 ? 0 : 1); }
*/

//void floatfields(longlong llp, char &sign, int &exp, longlong &mant)
//{                                            // analyse prolog float (double)
//  sign = llp < 0 ? '-' : ' ';
//  exp = llp>>52;
//  exp = ((int)(llp >> 52) & 0x7ff) - 0x400;
//  mant = (llp & 0xfffffffffffff) | 0x10000000000000;
//}


//#ifdef REALS
//#endif
/*
extern Real floatToReal(double d)
{
  longlong *llp = (longlong *)&d;
  longlong significand;
  int exponent;
  char sign;
  if(d == 0)
    return newReal(0L);
  floatfields(*llp, sign, exponent, significand);
  Real expfactor = ::realTwo->pow(exponent-51);
  Real r = newReal(significand);
  r->setSign(sign);
  (r->mult(*expfactor))->assignTo(r);
  Real result = r->trimToLen(3);             // doubleS are <4 gigits
  return result;
}

extern Real newReal(const double d)
{                                            // promote a real
  return floatToReal(d);
}
*/


TermServices::TermServices(LEngine *peng)
{
   m_peng = peng;
   //vl = pvl = ptopvl = NULL;

   for (int i = 0; i < MAX_COUNTERS; i++)
      counters[i] = 0;

   //m_buflen = 0;
   //quoting = FALSE;
}

TermServices::~TermServices()
{
   //delete[] vl;
}

void TermServices::Init(intC bufsz)
{
   //LNEW(vl, VLIST[pHXL->GetMaxVars()], aS("miscellaneous"));

   //ptopvl = &vl[pHXL->GetMaxVars()-1];

   b_vars_first = true;   // only set on by ddb when finding sorted clauses
}

//--------------------------------
// Term Unification Functions
//

inline bool TermServices::cyclic_check(TERM t)
{
   size_t i;
   for(i=0; i<occurrences.size(); i++)
   {
      if (t == occurrences[i])
      {
         return TRUE;
      }
   }
   occurrences.push_back(t);
   return FALSE;
}

inline void TermServices::cyclic_pop()
{
   occurrences.pop_back();
}

bool TermServices::IsCyclic(TERM t)
{
   occurrences.clear();
   return is_cyclic(t);
}

bool TermServices::is_cyclic(TERM t)
{
   ARITY a;
   t = t->dref();

   switch(t->getType())
   {
   case strucT:
      t = t->getTerm();
      if( cyclic_check(t) )
         return true;
      a = t->getArity();
      while(a--)
      {
         if( is_cyclic(++t) )
            return true;
      }
      cyclic_pop();
      return false;
   case listT:
      if( cyclic_check(t) )
         return true;
      t = t->getListHead();
      if( is_cyclic(t) )
         return true;
      if( is_cyclic(++t) )
         return true;
      cyclic_pop();
      return false;
   default:
      return false;
   }

}

int TermServices::Unify(TERM t1, TERM t2)
{
   occurrences.clear();
   return unify_loop(t1, t2);
}

// return true if a cyclic redundancy exists, otherwise false
inline int TermServices::occurs_check(TERM t1, TERM t2)
{
   size_t i;
   for(i=0; i<occurrences.size(); i++, i++)
   {
      if (t1 == occurrences[i] && t2 == occurrences[i+1])
      {
         //pXCPT->Error(internalE, aS("occurs check"));
         return TRUE;
      }
   }
   occurrences.push_back(t1);
   occurrences.push_back(t2);
   return FALSE;
}

inline void TermServices::occurs_pop()
{
   occurrences.pop_back();
   occurrences.pop_back();
}

int TermServices::unify_loop(register TERM t1, register TERM t2)
{  // general purpose unification 
   ARITY   arity;
   TF      tf;

   t1 = (t1)->dref();
   t2 = (t2)->dref();

   // if occurs_check flag on, check t1/t2

   // If they're both structure/list/ref then here's the logic
   // for dealing with them.
   if (t1->getType() == t2->getType())
     switch(t1->getType())                   // both the same type
        {
        case strucT:
          t1 = t1->getTerm();
          t2 = t2->getTerm();
          if ( (t1->getAtom() == t2->getAtom()) && 
               (t1->getArity() == (arity = t2->getArity())) )
            {
              if(pSTATE->m_occurs_check && occurs_check(t1, t2))
                 return FALSE;
              while(arity--)
                if (! unify_loop(++t1, ++t2))
                {
                   if(pSTATE->m_occurs_check) occurs_pop();
                   return FALSE;
                }
              if(pSTATE->m_occurs_check) occurs_pop();
              return TRUE;
            }
          else
            return FALSE;
          
        case refT:                                // both unbound

          if (t2 <= t1)
            {
              t1->setTerm(t2);
              pCNTL->TrailBinding(t1);
            }
          else
            {
              t2->setTerm(t1);
              pCNTL->TrailBinding(t2);
            }
          return TRUE;
          
        case listT:
          if(pSTATE->m_occurs_check && occurs_check(t1, t2))
             return FALSE;
          t1 = t1->getListHead();
          t2 = t2->getListHead();
          if (! unify_loop(t1, t2))
          {
            if(pSTATE->m_occurs_check) occurs_pop();
            return FALSE;
          }
          tf = unify_loop(++t1, ++t2);
          if(pSTATE->m_occurs_check) occurs_pop();
          return tf;
          
        case realS:
          //return (((Real)*t1)->compare(*((Real)*t2))) == '=';
          //return t1->getReal()->compare(*(t2->getReal())) == '=';
           return *(t1->getReal()) == *(t2->getReal());
        
        case strS:
           if (pSTATE->m_vba)
              return 0 == t1->getLString()->Stricmp(*(t2->getLString()));
           else
              return *t1 == *t2;
        default:                             // both constants of the same type
          return *t1 == *t2;
        }
                                             
   else if (t2->IsRef())
     {                       // not the same type but unifiable if one's a ref 
       *t2 = *t1;
       pCNTL->TrailBinding(t2);
       return TRUE;
     }
  else if (t1->IsRef())
    {
      *t1 = *t2;
      pCNTL->TrailBinding(t1);
      return TRUE;
    }
  
  if (t1->IsConst() && t2->IsConst())  // both constants of different type
    {                                           // both constants
      int type2 = t2->getType();
      switch (t1->getType())
        {                                       // see if they can be unified
        case atomS:
          if (type2 == strS)
          {
             if (pSTATE->m_vba)
                return 0 == (t1->getAtom())->Stricmp(*(t2->getLString()));
             else
               return *(t2->getLString()) == *(t1->getAtom());
          }
          return FALSE;
          
        case strS:
          if (type2 == atomS)
          {
             if (pSTATE->m_vba)
                return 0 == (t1->getLString())->Stricmp(*(t2->getAtom()));
             else
               return *(t2->getAtom()) == *(t1->getLString());
          }
          return FALSE;

        case intS:
		  case charS:
          if (type2 == intS || type2 == charS)
            //return (intC)*t2 == (intC)*t1;
            return t2->getInt() == t1->getInt();
          else if (type2 == realS)
             return *t2->getReal() == t1->getInt();
          else if (type2 == fixedS)
             return t2->getFixed() == t1->getInt();
          else if (type2 == singleS)
             return (t2->getSingle() == (float)t1->getInt());
          else if (type2 == doubleS)
             return (t2->getDouble() == (double)t1->getInt());
          else
             return FALSE;

          //if(type2 == realS)
          //  {                                    // Real and long can unify
          //    Real r = t2->getReal();
          //    if(r->isUnitary())
          //      {                                // r is a long
          //        long i = t1->getInt();
          //        if(signum(i) == r->signum())   // must be same signs
          //          return i < 0 ? -i == r->getGd(1) : i == r->getGd(1);
          //      }
          //  }

        case realS:
          //if(type2 == intS)
          //  return ( t1->getReal()->compare(t2->getInt()) ) == '=';
           if (type2 == fixedS)
              return (*t1->getReal() == t2->getFixed());
           else if (type2 == intS || type2 == charS)
              return (*t1->getReal() == t2->getInt());
           else
            return FALSE;
        case fixedS:
           if (type2 == realS)
              return (*t2->getReal() == t1->getFixed());
           else if (type2 == intS || type2 == charS)
              return (t1->getFixed() == t2->getInt());
           else
            return FALSE;

        case doubleS:
           if (type2 == singleS)
              return t1->getDouble() == (double)t2->getSingle();
           else if (type2 == intS)
              return (t1->getDouble() == (double)t2->getInt());
           else
              return FALSE;

        case singleS:
           if (type2 == doubleS)
               return (t2->getDouble() == (double)t1->getSingle());
           else if (type2 == intS)
              return (t1->getSingle() == (float)t2->getInt());
           else
               return false;
          
        default:
          return FALSE;
        }
    }  
  return FALSE;
}

int TermServices::UnifyConst(TERM t1, TERM t2)
{
   t1 = (t1)->dref();
   if (t1->IsRef())
   {
      *t1 = *t2;
      pCNTL->TrailBinding(t1);
      return TRUE;
    }

  if (t1->IsConst())
    {
      if (t1->getType() == t2->getType())
      {
         if (pSTATE->m_vba && t1->getType() == strS)
            return 0 == t1->getLString()->Stricmp(*(t2->getLString()));
         else
            return *t1 == *t2;
      }
      
      // If both are const but different types, see if they can be unified
      int type2 = t2->getType();
      switch (t1->getType())
        {                                         // all cases return
        case atomS:
          if (type2 == strS)
          {
             if (pSTATE->m_vba)
                return 0 == (t1->getAtom())->Stricmp(*(t2->getLString()));
             else
               return *(t2->getLString()) == *(t1->getAtom());
          }
          return FALSE;

        case strS:
          if (type2 == atomS)
          {
             if (pSTATE->m_vba)
                return 0 == (t1->getLString())->Stricmp(*(t2->getAtom()));
             else
               return *(t2->getAtom()) == *(t1->getLString());
          }
          return FALSE;
          
        case intS:
		  case charS:
          if (type2 == intS || type2 == charS)
            //return (intC)*t2 == (intC)*t1;
            return t2->getInt() == t1->getInt();
          else if (type2 == fixedS)
             return t2->getFixed() == t1->getInt();
          else if (type2 == realS)
             return *t2->getReal() == t1->getInt();
          else if (type2 == singleS)
             return (t2->getSingle() == (float)t1->getInt());
          else if (type2 == doubleS)
             return (t2->getDouble() == (double)t1->getInt());
          else
             return FALSE;

        case realS:
          if (type2 == intS || type2 == charS)
            //return (((Real)*t1)->compare((intC)*t2)) == '=';
            //return t1->getReal()->compare(t2->getInt()) == '=';
            return *(t1->getReal()) == t2->getInt();
          else if (type2 == fixedS)
             return *(t1->getReal()) == t2->getFixed();
          else
            return FALSE;

        case fixedS:
           if (type2 == intS || type2 == charS)
              return t1->getFixed() == t2->getInt();
           else if (type2 == realS)
              return *(t2->getReal()) == t1->getFixed();
           else
              return FALSE;

        case doubleS:
           if (type2 == singleS)
              return t1->getDouble() == (double)t2->getSingle();
           else if (type2 == intS)
              return (t1->getDouble() == (double)t2->getInt());
           else
              return FALSE;

        case singleS:
           if (type2 == doubleS)
              return t2->getDouble() == (double)t1->getSingle();
           else if (type2 == intS)
              return (t1->getSingle() == (float)t2->getInt());
          else
              return FALSE;

        default:
          return FALSE;
        }                                       // end switch
    }                                           // end if
  return FALSE;
}


int TermServices::UnifyInt(intC val, TERM t)
{
   t = (t)->dref();

   if (t->IsRef())
   {
      t->setInt(val);
      pCNTL->TrailBinding(t);
      return TRUE;
   }
   else
   {
      switch (t->getType())
      {
      case intS:
      case charS:
         return t->getInt() == val;
         break;
      case fixedS:
         return t->getFixed() == val;
         break;
      case realS:
         return *(t->getReal()) == val;
         break;
      case singleS:
         return t->getSingle() == val;
         break;
      case doubleS:
         return t->getDouble() == val;
         break;
      default:
         return FALSE;
      }
   }
      //return ((t->IsInt() || t->IsChar())) && ((intC)*t == val);
      return ((t->IsInt() || t->IsChar())) && (t->getInt() == val);
}

int TermServices::UnifyAtom(PATOM a, TERM t)
{
   t = (t)->dref();

   if (t->IsRef())
   {
      t->setAtom(a);
      pCNTL->TrailBinding(t);
      return TRUE;
   }
   else
   {
      if (t->IsAtom())
         return *(t->getAtom()) == *a;
      else if (t->IsStr())
      {
         if (pSTATE->m_vba)
            return 0 == a->Stricmp(*(t->getLString()));
         else
            return *(t->getLString()) == *a;
      }
      else
         return FALSE;
   }
}

int TermServices::StrongUnify(TERM t1, TERM t2)
{
   occurrences.clear();
   return strong_unify(t1, t2);
}

int TermServices::strong_unify(TERM t1, TERM t2)
{                                   // strong unification, for the == operator
   ARITY   arity;
   TF   tf;
   //Real r;
   //char c;

   t1 = (t1)->dref();
   t2 = (t2)->dref();
   int type1 = t1->getType();
   int type2 = t2->getType();


   // If they're both structure/list/ref then here's the logic
   // for dealing with them.
   if (type1 == type2)
     switch(type1)
       {
       case realS:
         if (type2 == realS)
           return (t2->getReal())->compare(*(t1->getReal())) == '=';
         return FALSE;
       case strucT:
         t1 = t1->getTerm();
         t2 = t2->getTerm();
         if ( (t1->getAtom() == t2->getAtom()) && 
              (t1->getArity() == (arity = t2->getArity())) )
           {
             if(pSTATE->m_occurs_check && occurs_check(t1, t2))
                 return FALSE;
             while(arity--)
               if (! strong_unify(++t1, ++t2))
               {
                 if(pSTATE->m_occurs_check) occurs_pop();
                 return FALSE;
               }
             if(pSTATE->m_occurs_check) occurs_pop();
             return TRUE;
           }
         else
           return FALSE;
         
       case refT:  // both unbound
         return t1 == t2;
         
       case listT:
         if(pSTATE->m_occurs_check && occurs_check(t1, t2))
            return FALSE;
         t1 = t1->getListHead();
         t2 = t2->getListHead();
         if (! strong_unify(t1, t2))
         {
           if(pSTATE->m_occurs_check) occurs_pop();
           return(FALSE);
         }
         tf = strong_unify(++t1, ++t2);
         if(pSTATE->m_occurs_check) occurs_pop();
         return tf;
         
         // both constants of the same type
       case strS:
          if (pSTATE->m_vba)
            return 0 == t1->getLString()->Stricmp(*(t2->getLString()));
         else
            return *t1 == *t2;

       default:
         return *t1 == *t2;
       }
   
   // If both are constants of different types, see
   // if there the sort that can be unified
   if (t1->IsConst() && t2->IsConst())
   {
      return UnifyConst(t1,t2);
   }

   return(FALSE);
}

//--------------------------
// Arithmetic Functions
//


aCHAR *TermServices::TermVal(TERM x, aCHAR* termS, int tslen)
{
  TERM   t;
  aCHAR  term2S[80];
  
  if (x == NULL)
    {
      Lsprintf(termS, tslen, aS("NULL Term"));
      return termS;
    }
  
  t = (x)->dref();
  switch(t->getType())
    {
    case atomS:
#ifdef _UNICODE
     Lsprintf(termS, tslen, aS("ATOM %ls"), (STRptr)*(t->getAtom()));
#else
     Lsprintf(termS, tslen, aS("ATOM %s"), (STRptr)*(t->getAtom()));
#endif
      break;
      
    case intS:
    case charS:
      Lsprintf(termS, tslen, aS("INT %d"), t->getInt());
      break;
      
    /*
    case dbrefS:
      Lsprintf(termS, tslen, aS("DBREF @%8.8lX"), t);  // was double?
      break;
      */
      
    case strS:
#ifdef _UNICODE
      Lsprintf(termS, tslen, aS("STRING $%ls$"), t->getStr());
#else
      Lsprintf(termS, tslen, aS("STRING $%s$"), t->getStr());
#endif
      break;
      
    case doubleS:
      Lsprintf(termS, tslen, aS("FLOAT %g"), t->getDouble());
      break;
      
    case refT:
#ifdef _UNICODE
      Lsprintf(termS, tslen, aS("VAR %ls"), (aCHAR*)pHXL->cellname(t));
#else
      Lsprintf(termS, tslen, aS("VAR %s"), (aCHAR*)pHXL->cellname(t));
#endif
      break;
      
    case strucT:
      t = t->getTerm();
#ifdef _UNICODE
      Lsprintf(termS, tslen, aS("STRUCT %ls/%d"),
#else
      Lsprintf(termS, tslen, aS("STRUCT %s/%d"), 
#endif
               (STRptr)*(t->getAtom()), t->getArity());
      break;
      
    case listT: 
      t = t->getTerm();
#ifdef _UNICODE
      Lsprintf(termS, tslen, aS("LIST [%ls..."), TermVal(++t, term2S, tslen));
#else
      Lsprintf(termS, tslen, aS("LIST [%s..."), TermVal(++t, term2S, tslen));
#endif
      break;
      
    default:  
      Lsprintf(termS, tslen, aS("UNKNOWN TERM"));
      break;
    }
  
  return(termS);
}

bool TermServices::is_rule(TERM tt)
{
   TERM t = tt->dref();
   if (t->IsStruct() && t->getTerm()->getAtom() == pATAB->ifA)
      return true;
   else
      return false;
}


#ifdef LANDFILL

TF TermServices::p_landZfill(void)
{
   aCHAR buf[512];
   pWRIT->termWriteString(X(0), buf, 511, true);
   DUMP << buf << NL;
   return TRUE;
}

void TermServices::DumpWrite(TERM t)
{
   aCHAR buf[512];
   pWRIT->termWriteString(t, buf, 511, true);
   DUMP << buf;

}

void TermServices::DumpCell(TERM t)
{
   if (t == NULL)
   {
      DUMP << "NULL Term" << NL;
      return;
   }

   DUMP << HEXOUT(t);
   DUMP << SP;
   pHXL->DumpCellName(t);
   DUMP << SP << HEXOUT(t->getTerm());
   DUMP << SP << t->getModix();
   DUMP << SP << t->getArity();
   DUMP << SP << t->getType() << SP3;

   switch(t->getType())
   {
   case atomS:
      DUMP << "ATOM ";
      if (t->getAtom() == NULL)
         DUMP << "NULL Atom";
      else
         DUMP << PREDAR(t->getAtom(), t->getArity());
      break;

   case intS:
	case charS:
      DUMP << "INT " << t->getInt();
      break;

   /*
   case dbrefS:
      DUMP << "DBREF ";
      break;
      */

   case strS:
      DUMP << "STRING " << t->getStr();
      break;

   case doubleS:
      DUMP << "DOUBLE " << t->getDouble();
      break;

   case singleS:
      DUMP << "SINGLE " << t->getSingle();
      break;

#ifdef GNU
   // gcc complains about ambiguity, but there are no casts, just
   // constructors for cell from fixedC, and real from fixedC.
   case fixedS:
      DUMP << "FIXED " << " can't figure out how to get GCC happy with this";
      break;
#else
   case fixedS:
      DUMP << "FIXED " << t->getFixed();
      break;
#endif

   case realS:
      DUMP << "REAL " << t->getReal();
      break;

   case refT:
      if (t == t->getTerm())
         DUMP << "VAR ";
      else
         DUMP << "REF ";
      break;

   case strucT:
      DUMP << "STRUCTURE ";
      break;

   case listT: 
      DUMP << "LIST ";
      break;

   default:
      DUMP << "UNKNOWN TYPE: ";
      break;
   }
   DUMP << NL << FLUSH;
}

void TermServices::Dump(TERM t)
{
   DUMP << "---Term Heap Layout---" << NL;
   Dump(t, 0);
//   DUMP << "---Written---" << NL;
//   DumpWrite(t);
   DUMP << NL << "---" << NL;

}

void TermServices::Dump(TERM t, int ind)
{
   intC i;
   intC ind2;
   ARITY ar;

   if (t == NULL)
   {
      INDENT(m_peng->m_dump, ind);
      DUMP << "NULL Term" << NL;
      return;
   }

   INDENT(m_peng->m_dump, ind);
   DumpCell(t);

   ind2 = ind + 1;

   switch(t->getType())
   {
   case refT:
      if (t != t->getRef())
         Dump(t->getRef(), ind2);
      break;

   case strucT:
      t = t->getTerm();
      INDENT(m_peng->m_dump, ind2);
      DumpCell(t);
      ar = t->getArity();
      for (i=0; i<(intC)ar; i++)
      {
         Dump(++t, ind2);
      }
      break;

   case listT: 
      t = t->getTerm();
      INDENT(m_peng->m_dump, ind);
      DUMP << "HEAD " << t;
      Dump(++t, ind);
      break;

   default:
      break;
   }
}

#endif // LANDFILL

TERM TermServices::copy_to_heap(TERM ddb)
{
   //TERM h = pHXL->heapGETN( ddb->term_size() + 1 );  // bad mistake, not sure why...
   TERM h = pHXL->heapGETN( ddb->term_size() );
   TERM t = h+1;
   // used by unify_copy, so varcount is already set, so go direct
   // to copyterm2_
   copyterm2_(h, t, &t, ddb, GC_NO);
   return h;
}

int TermServices::Copy(TERM dest, TERM space, TERMptr pnewspace, 
                       TERM src, MARKGC markuse)
//   walks along a term counting the number of cells needed to
//   contain it:
//
//   This errs on the large size since it follows references.
//   Consequently a substructure in the term which is referenced
//   more than one in the term will have that substructure counted
//   in its entirety twice. 
//
//   Note however that CopyTerm (for which Size() is needed) also
//   commits this sin - so don't fix this unless you fix CopyTerm()
// returns number of distinct variables in term 
{
   //pvl = vl;
   varcount = 0;
#ifdef BUG_COPY
   DUMP << "==> Copy" << NL;
#endif
   copyterm2_(dest, space, pnewspace, src, markuse);
#ifdef BUG_COPY
   DUMP << "<== Copy" << NL;
#endif

   //return (int)(pvl - vl);            // returns number of variables in term 
   return varcount;
}

// Actually performs the non-trivial copying in CopyTerm()
// dest is where the current node is going to go, 
// space is the next free cell in copy space, pnewspace is
// set to point to the next free cell in copy space.
//
// MARKGC indicates what action to take with cells that
// point to GC objects. If the object is being created in
// non-heap space, then increment its use, if it's being
// copied to the heap, then, decrement if the original
// is destroyed, otherwise do nothing.  This all because
// thing_gc can look at the heap, but not non-heap use.
//
void TermServices::copyterm2_(TERM dest, TERM space, TERMptr pnewspace, 
                              TERM src, MARKGC markuse)
{
   TERM      x;
   int       count_cell;
   int i;
   //VLISTptr  pv;

   src = (src)->dref();

//	LASSERT( (*(int*)src != 0xdddddddd), aS("bad copyterm ref") );

//   if (*(int*)src == 0xdddddddd)
//      pXCPT->Error(internalE, aS("bad copyterm ref dddddddd"));

   switch(src->getType())
   {
   case refT:
      *pnewspace = space;                      // will not use any new space 
      //for(pv = &vl[0]; pv < pvl; ++pv)       // look through VLIST so far 
      for(i = 0; i < varcount; i++)
         //if (pv -> src_var == src)
         if (var_pairs[i].src_var == src)
         {
            //dest->setTerm(pv->dest_var);
            dest->setTerm(var_pairs[i].dest_var);
            return;
         }
         // must have run out of list - no match 
      dest->setUnbound();
#ifdef BUG_COPY
      DUMP << "varcount " << varcount;
      DUMP << " var_pairs.size() " << var_pairs.size() << NL;
#endif
      if (varcount >= (int)var_pairs.size())
         var_pairs.push_back(VarPair(src,dest));
      else
      {
         var_pairs[varcount].src_var = src;
         var_pairs[varcount].dest_var = dest;
      }
      varcount++;

      break;

      case strucT:
       //be careful that the functor/arity and argument cells 
       //for the structure are contiguous in the copy space 
         src = src->getTerm();
         count_cell= src->getArity();
         dest->setStruct(space);
         *space = *src;
         x = ++ space;                         // where first arg will go 
         dest = x + count_cell;                // use dest as space pointer 
         while(count_cell--)
            copyterm2_(x++, dest, &dest, ++src, markuse);
         *pnewspace = dest;
         break;

      case listT:         // have to guarantee that cons/cdrs are contiguous 
         while (src->IsList())
           {
             // dest always contains where list will be built.
             // space is the next free cell so:
             // list goes to dest, cons to space and cdr to space + 1 
             dest->setList(space);
             src = src->getTerm();                  // cons 
             copyterm2_(space, space+2, pnewspace, src, markuse);
             ++ src;                                // cdr 
             dest = space + 1;                      // continguous cdr !! 
             space = *pnewspace;
           }
         copyterm2_(dest, space, pnewspace, src, markuse); // copy cdr 
         break;
         
      default:                                      // various consts
         LASSERT( (src->getType() >=3 || src->getType() <= 20), 
						aS("bad copyterm const") );
         if (src->IsGCThing())
         {
            switch (markuse)
            {
            case GC_INC:
               src->getGCThing()->inc_dbuse();
               break;
            case GC_DEC:
               src->getGCThing()->dec_dbuse();
               break;
            case GC_NO:
               break;
            default:
               pXCPT->Error(internalE, aS("unknown markuse in copyterm"));
            }
         }
         //if (markuse != GC_NO && src->IsGCThing())
         //{
         //   if (markuse == GC_INC)
         //      src->getGCThing()->inc_dbuse();
         //   else  // GC_DEC
         //      src->getGCThing()->dec_dbuse();
         //}
         *dest = *src;
         *pnewspace = space;             // haven't used any more copy space 
         break;
      }
}

/* use hasVar instead
bool TermServices::Ground(TERM src)
{
   TERM      x;
   int       count_cell;

   src = (src)->dref();

   switch(src->getType())
   {
   case refT:
      return FALSE;
      break;

   case strucT:
      src = src->getTerm();
      count_cell= src->getArity();
      *x = *src;
      while(count_cell--)
         if (! Ground(++x)) return FALSE;
      break;

   case listT:
      while (src->IsList())
         {
            src = src->getTerm();                  // cons 
            if (! Ground(src)) return FALSE;
            ++ src;                                // cdr 
         }
      if (! Ground(src)) return FALSE; // copy cdr 
      break;
         
   default:                                      // various consts
      break;
   }
   return TRUE;
}
*/


// increment or decrement non-heap (db) use of any
// gc thing objects in a term.

void TermServices::UpdateGCThings(TERM src, MARKGC markuse)
{
   int       count_cell;

   src = (src)->dref();

//	LASSERT( (*(int*)src != 0xdddddddd), aS("bad update gc ref") );
   switch(src->getType())
   {
   case refT:
      break;

   case strucT:
      //be careful that the functor/arity and argument cells 
      //for the structure are contiguous in the copy space 
      src = src->getTerm();
      count_cell= src->getArity();
      while(count_cell--)
         UpdateGCThings(++src, markuse);
      break;

   case listT:         // have to guarantee that cons/cdrs are contiguous 
      while (src->IsList())
      {
          src = src->getTerm();                  // cons 
          UpdateGCThings(src, markuse);
          ++src;                                // cdr 
      }
      UpdateGCThings(src, markuse); // copy cdr 
      break;
         
   default:                                      // various consts
      LASSERT( (src->getType() >=3 || src->getType() <= 20), 
					aS("bad updateterm const") );
      if (src->IsGCThing())
      {
         switch (markuse)
         {
         case GC_INC:
            src->getGCThing()->inc_dbuse();
            break;
         case GC_DEC:
            src->getGCThing()->dec_dbuse();
            break;
         default:
            pXCPT->Error(internalE, aS("bad markuse in updateterm"));
         }
      }
      break;
   }
}

// When looking for dynamic database clauses, the terms sometimes need to
// be copied to the heap.  Called from DDB:p_clauseZdb().
TF TermServices::UnifyCopy(TERM head, TERM body, DynamicClause *pdc)
{
/*   
   TERM            dbhead, dbbody;
   TERM            h, dbt;
   TRAIL_INDEX     ti;

         dbt = pHXL->heapGETN(pdc->getSize());   
         h = dbt+1;
         pTSVC->Copy(dbt, h, &h, pdc->getCode(), GC_NO);
         //heap_copy = true;
//      }
//      else
//      {
//         dbt = pdc->getCode();
//         heap_copy = false;
//      }

      if (dbt->IsStruct()
          && dbt->getTerm()->getAtom() == pATAB->ifA)
      {
         dbhead = dbt->getTerm() + 1;
         dbbody = dbt->getTerm() + 2;
      }
      else
      {
         dbhead = dbt;
         dbbody = pATAB->TrueTerm;
      }

      //clause_number++;
      if (pTSVC->Unify(dbhead, head) && pTSVC->Unify(dbbody, body))
      {
         //LASSERT(b_unifycopy, aS("unify false when should be true"));
         return TRUE;
      }

      // unwind any variables that were bound during
      // the unification attempt
      pHXL->SetHTop(pCNTL->BTop()->HBc);
      ti = pCNTL->BTop()->TRc;
      pCNTL->Unwind(ti);

      return FALSE;

*/

   TRAIL_INDEX     ti;
   TERM dbhead, dbbody, ddb;

   ddb = pdc->getCode();
   varcount = 0;
   //TERM t = pHXL->heapGETN(pdc->getSize()+1);
   //TERM t = pHXL->heapGETN(2);
   //TERM t1 = t;
   //TERM h = t+1;

   if ( ddb->IsStruct() &&
         ddb->getTerm()->getAtom() == pATAB->ifA )
   {
      dbhead = ddb->getTerm() + 1;
      dbbody = ddb->getTerm() + 2;
   }
   else
   {
      dbhead = ddb;
      dbbody = pATAB->TrueTerm;
   }

   //if ( unify_copy2(head, t, h, &h, dbhead, GC_NO) )
   if ( unify_copy2(head, dbhead) )
   {
      //LASSERT( t1+(pdc->getSize()) <= h, aS("unify copy head went too far") );
      //t = h;
      //if ( unify_copy2(body, t, h+1, &h, dbbody, GC_NO) )
      if ( unify_copy2(body, dbbody) )
      {
         //LASSERT( t1+(pdc->getSize()) <= h, aS("unify copy body went too far"));
         return TRUE;
      }
   }

   // unwind any variables that were bound during
   // the unification attempt
   pHXL->SetHTop(pCNTL->BTop()->HBc);
   ti = pCNTL->BTop()->TRc;
   pCNTL->Unwind(ti);

   return FALSE;


}

TF TermServices::unify_copy2(TERM query, TERM ddb)
//TF TermServices::unify_copy2(TERM query, TERM heap, TERM space, TERMptr pnewspace, 
//                       TERM ddb, MARKGC markuse)
{
#ifdef BUG_UNIFY_COPY
 DUMP << NL;
 FILL("unify_copy2 query: ");
 pTSVC->DumpWrite(query);
 DUMP << NL;
 FILL("unify_copy2 ddb: ");
 pTSVC->DumpWrite(ddb);
 DUMP << NL;
#endif
   TERM q, d;
   ARITY arity;
   //TERM dest;
   TERM heap;

   query = query->dref();

   if ( ddb->IsRef() || query->IsRef() )
   {
      heap = copy_to_heap(ddb);
      //t1 = space,
      //copyterm2_(heap, space, &t1, ddb, markuse);
      //*pnewspace = t1;
#ifdef BUG_UNIFY_COPY
 DUMP << NL;
 FILL("var copy heap: ");
 pTSVC->Dump(heap);
 DUMP << NL;
 FILL("var copy query: ");
 pTSVC->Dump(query);
 DUMP << NL;
#endif
      return Unify(query, heap);
   }

   else if ( query->getType() == ddb->getType() )
   {
      switch( query->getType() )
      {
      case strucT:
         q = query->getTerm();
         d = ddb->getTerm();
         if ( (q->getAtom() == d->getAtom()) && 
              (q->getArity() == (arity = d->getArity())) )
         {
            //heap->setStruct(space);
            //*space = *ddb;
            //x = ++space;
            //dest = x + arity;
            while(arity--)
            {
               if (! unify_copy2(++q, ++d))
               //if (! unify_copy2())  // overwrote what it was
                  return FALSE;
            }
            //*pnewspace = dest;
            return TRUE;
         }
         else
            return FALSE;
          
      case listT:
         q = query->getTerm();
         d = ddb->getTerm();
         //if ( ! unify_copy2(t1, heap, space, pnewspace, t2, markuse) )
         if ( ! unify_copy2(q, d) )
            return FALSE;
         //space = *pnewspace;
         //heap = space;
         //if ( unify_copy2(++t1, heap, heap+1, pnewspace, ++t2, markuse) )
         if ( unify_copy2(++q, ++d) )
         {
            //space = *pnewspace;
            return TRUE;
         }
         else
            return FALSE;

      case realS:
         //*pnewspace = space;   //?
         return *(query->getReal()) == *(ddb->getReal());
      
      default:  // both constants of same type
         //*pnewspace = space;   //?
         return *query == *ddb;
      }
   }
   
   else if ( query->IsConst() && ddb->IsConst() )
   {
      //*pnewspace = space;  // nothing new copied
      return UnifyConst( query, ddb );
   }

   return FALSE;
}


// query_copy makes a copy of a term for use in posing queries against
// sorted dynamic predicates.  The copy has variables in all the arguments
// found past the first argument with a variable.  This allows for
// optimization of getting iterators from std::map.
TERM TermServices::query_copy(TERM t)
{
   TERM      in, out, x;
   ARITY     a;
   int       i;
   bool      var_found = false;

   in = t->dref();
   if (! in->IsStruct())
      return in;
   
   in = in->getTerm();
   a = in->getArity();
   out = pHXL->heapGETN(a+2);
   x = out+1;
   out->setStruct(x);
   x->setFA(in->getAtom(), a);
   for (i=0; i<(intC)a; i++)
   {
      in++; x++;
      if (!var_found && hasVar(in))
         var_found = true;
      if (var_found)
         x->setUnbound();
      else
         *x = *in;
   }
   return out;
}

bool TermServices::hasVar(TERM tin)
{
   TERM t;
   ARITY a;
   int i;

   t = tin->dref();

   if (t->IsRef())
      return true;
   if (t->IsStruct())
   {
      t = t->getTerm();
      a = t->getArity();
      for (i = 0; i < (intC)a; i++)
      {
         t++;
         if (hasVar(t))
            return true;
      }
   }
   if (t->IsList())
   {
      while(t->IsList())
      {
         t = t->getTerm();
         if (hasVar(t))
            return true;
         t++;
      }

      if( hasVar(t) )
         return true;

   }
   return false;
}

TF TermServices::p_copy_term()
{
   TERM x1 = X(0);
   TERM h = pHXL->heapGETN(x1->term_size() + 1);
   TERM t = h + 1;
   // we're copying to heap, no need for GC incrementing
   Copy(h, t, &t, x1, GC_NO);
   //Copy(h, t, &t, x1, GC_INC);
   return Unify(h, X(1));
}


//-----------------------------------
// Term Classification Functions
//

int TermServices::varsof_(TERM t, TERM mark_heap)
{                                               // does the work 
  int    arity;
  TERM    x;
  
  t = (t)->dref();
  
  switch( t->getType() )
    { 
    case strucT:
      t = t->getTerm();
      arity = t->getArity();
      ++t;
      while(arity --)
        varsof_(t++, mark_heap);
      return(TRUE);
      
    case listT:
      do
        {
          t = t->getTerm();                      // cons 
          varsof_(t, mark_heap);
          ++t;                                   // cdr 
        } while (t->IsList());
      varsof_(t, mark_heap);                     // vars of final cdr-cell 
      return(TRUE);
      
    case refT:
      for(x = mark_heap; x->IsList(); ++x)
        {
          x = x->getTerm();                      // cons 
          if (t == (x)->dref())
            return(TRUE);
        }
      // else x points to cdr terminator 
      // replace it by list cell 
      x->setList(x+1);
      ++x;
      pHXL->heapINC();
                                       
      x->setUnbound();
      Unify(x, t);                           // can't do simple var assignment 
                                       
      x++;
      x->setAtom(pATAB->nilA);               // nil-terminate the list 
      pHXL->heapINC();                       // so we can recognize success 
      return(TRUE);

    default:
       return TRUE;
    }
  return(0);                             // a formality 
}

ORDER_ TermServices::ordtype_(TERM x1)
{                                        // support for above 
   if (x1->IsNumber())
      return(ONUMBERS);
   if (x1->IsAtom())
      return(OATOMS);  
   if (x1->IsStr())
      return(OSTRINGS);
   if (x1->IsStruct())
      return(OSTRUCTS);
   if (x1->IsList())
      return(OLISTS);

   return OREFS;  // treat as a reference if none of the above
}

TERM TermServices::MakeAbstractList()
{
  TERM arg1, head, tail;

  arg1 = pHXL->heapGETN(3);
  head = arg1+1;
  tail = head+1;
  arg1->setList(head);
  head->setUnbound();
  tail->setUnbound();
  return arg1;
}

TERM TermServices::MakeStruc(PATOM functor, ARITY arity)
{
   TERM tstruc,t;

   tstruc = pHXL->heapGETN(arity + 2);
   //t = pHXL->heapGET();
   t = tstruc + 1;
   tstruc->setStruct(t);
   t->setFA(functor, arity);
   while (arity--)
   {
      //t = pHXL->heapGET();
      t++;
      t->setUnbound();
   }
   return tstruc;
}



TF TermServices::p_ground()
{
   TERM t;

   t = X(0);
   if (hasVar(t))
      return FALSE;
   else
      return TRUE;
}

TF TermServices::p_functor()
{                                             // functor(Term, Pred, Arity) 
   Cell      x, x2;
   TERM      arg1, arg2, arg3;
   int       err;
   intC      i;

   arg1 = (pHXL->XVar(0))->dref();
   if (arg1->IsAtom() || arg1->IsStruct() || arg1->IsList())
   {
      if (arg1->IsAtom())
        {
          x.setAtom(arg1->getAtom());
          x2.setInt(0);
        }
      else if (arg1->IsStruct())
        {
          arg1 = arg1->getTerm();                  // get functor 
          x.setAtom(arg1->getAtom());
          x2.setInt(arg1->getArity());
        }
      else
        {
          x.setAtom(pATAB->periodA);
          x2.setInt(2);
        }
      if (TRUE != (err = Unify(&x, pHXL->XVar(1))))
        return(err);
      return(Unify(&x2, pHXL->XVar(2)));
   } 

   arg2 = (pHXL->XVar(1))->dref();
   arg3 = (pHXL->XVar(2))->dref();
   if (arg3->IsInt() && arg2->IsAtom())
     {                                         // args 2 and 3 given
       i = arg3->getInt();
       if (i > (int)(pHXL->GetMaxVars() -1))
         pXCPT->Error(maxvarE);
       switch(i)
         {
         case 0:
           arg1 = pHXL->heapGET();
           arg1->setAtom(arg2->getAtom());
           break;
         case 2:
           if(pATAB->periodA == arg2->getAtom())
             arg1 = MakeAbstractList();
           else
             arg1 = MakeStruc(arg2->getAtom(), 2);
           break;
         default:
           arg1 = MakeStruc(arg2->getAtom(), (ARITY)i);
           break;
         }
      return(Unify(arg1, pHXL->XVar(0)));
     }
   else
     return(FALSE);
}

TF TermServices::p_strong_unify()
{                                         // X == Y 
   return(StrongUnify(pHXL->XVar(0), pHXL->XVar(1)));
}

TF TermServices::p_not_strong_unify()
{                                         // X \== Y 
   return(! StrongUnify(pHXL->XVar(0), pHXL->XVar(1)));
}

TF TermServices::p_univ()                 // Term =.. List 
{           // changed sysargE returns to FALSE to match Arity and AAIS Prolog 
   int   n, cnt;
   TERM  head, tail, arg1, func, t, x;

   x = X(0);
   if (x->IsAtom())
     {
       arg1 = pHXL->heapGETN(3);
       arg1->setList(arg1+1);
       (arg1 + 1)->setAtom(x->getAtom());
       (arg1 + 2)->setAtom(pATAB->nilA);
       return(Unify(arg1, pHXL->XVar(1)));
     }
   else if (x->IsStruct())
     {
       x = x->getTerm();
       cnt = x->getArity();
       n = 2*cnt + 3;
       arg1 = pHXL->heapGETN(n);
       head = arg1 + 1;
       arg1->setList(head);
       head->setAtom(x->getAtom());
       tail = head+1;
       for (n = 0; n < cnt; n++)
         {
           head += 2;
           tail->setList(head);
           *head = *(++x);
           tail += 2;
         }
       tail->setAtom(pATAB->nilA);
       return(Unify(arg1, pHXL->XVar(1)));
     }
   else if (x->IsList())
     {
       arg1 = pHXL->heapGETN(7);             // make a list ['.',head,tail] 

       arg1->setList(arg1 + 1);
       (arg1 + 1)->setAtom(pATAB->periodA);
       (arg1 + 2)->setList(arg1 + 3);
       x = x->getTerm();
       *(arg1 + 3) = *x;
       (arg1 + 4)->setList(arg1 +5);
       *(arg1 + 5) = *(++x);
       (arg1 + 6)->setAtom(pATAB->nilA);
       
       return(Unify(arg1, pHXL->XVar(1)));
     }
   
   x = (pHXL->XVar(1))->dref();
   if (x->IsList())
     {
		 x = x->getTerm();
       func = (x)->dref();
       if (! func->IsAtom())             // first element must be functor atom 
         return FALSE;
       x++;
       x = (x)->dref();                      // next list elem 
       if ( x->IsAtom() && (x->getAtom()==pATAB->nilA) )   // only one element 
         {
           t = pHXL->heapGET();
           t->setAtom(func->getAtom());
         }
       else if ( func->getAtom()==pATAB->periodA )
         {           // NOTE - no error checking yet, must be dot/2 to work!!! 
           t = pHXL->heapGETN(3);
           t->setList(t + 1);
           x = x->getTerm();
           *(t+1) = *((x))->dref();
           x++;
           x = (x)->dref();
           x = x->getTerm();
           *(t+2) = *((x))->dref();
         }
       else if (x->IsList())
         {
           t = pHXL->heapGETN(2);
           t->setStruct(t + 1);
           cnt = 0;
           while (x->IsList())
             {
               x = x->getTerm();
               *(pHXL->heapGET()) = *(x->dref());
               x++;
               x = (x)->dref();              // tail of list 
               cnt++;
             }
           if (! ( x->IsAtom() && (x->getAtom()==pATAB->nilA) )) 
             return FALSE;
           (t+1)->setFA(func->getAtom(), cnt);
         }
       else 
         return FALSE;
       
       return(Unify(t, pHXL->XVar(0)));
     }
   else
     return(FALSE);
}

TF TermServices::p_fixed_list()
{
  TERM arg1, arg2, result = NULL;
  fixedC f;
  Cell c;
  
  arg1 = (pHXL->XVar(0))->dref();
  arg2 = (pHXL->XVar(1))->dref();
  
  if (arg1->IsFixed())
	 {
		f = arg1->getFixed();
		result = pHXL->FixedToList(f);
		return(Unify(result, pHXL->XVar(1)));
	 }
  
  if (arg2->IsList())
    {
		f = pHXL->ListToFixed(arg2);
		//		result->setFixed(f);
                c.setFixed(f);
		return(Unify(&c, pHXL->XVar(0)));
	 }
  return FALSE;
}

TF TermServices::p_arg()
{
   register int i;
   TERM a1, a2;

   a1 = (pHXL->XVar(0))->dref();
   a2 = (pHXL->XVar(1))->dref();

   if (! a1->IsInt())
      pXCPT->Error(intargE);
   if (a2->IsStruct())
     {
       i = a1->getInt();
       a2 = a2->getTerm();
       if ( 0 < i && i <= (int) a2->getArity())
         {
           while (i--)
             ++a2;
           return(Unify(a2, pHXL->XVar(2)));
         } 
       else
         return(FALSE);
     }
   else if (a2->IsList())
     {
       i = a1->getInt();
       a2 = a2->getTerm();
       if (i == 1)
         return(Unify(a2, pHXL->XVar(2)));   // 1st arg of list is car 
       else if (i == 2)
         return(Unify(++a2, pHXL->XVar(2))); // 2nd is cdr 
       else
         return(FALSE);
     }
   
   return(0);                                // a formality 
}

TF TermServices::p_getZmodix()
{
   MODIX imod = X(0)->getModix();
   if (!pDDB->validModix(imod))
      pXCPT->Error(internalE, aS("Invalid Modix in X(0)"));
   return UnifyInt(imod, X(1));
}

// counter support - maintains 20 counters - format is
// cntr$( cntr_num, 1 = set/0 = read, value)
//
// Counter Usage:
//    0   User
//    1   User
//    2   User
//    3   User
//   16   pp

TF TermServices::p_cntr()
{
   intC          index;
   Cell          x;
   intC          val;
   TERM          t1, t2, t3;   
   
   t1 = (pHXL->XVar(0))->dref();
   t2 = (pHXL->XVar(1))->dref();
   t3 = (pHXL->XVar(2))->dref();
   
   if (! (t2->IsInt() && t1->IsInt()))
      pXCPT->Error(sysargE);
   index = t1->getInt();

   if (index >= MAX_COUNTERS)
      pXCPT->Error(instanceE, aS("arg1 must be between 0 and 99"));
   
   if (t2->getInt() == 1)                             // Set value 
     {
       if (t3->IsInt())
         SetCounter( index, t3->getInt() );
       else if (t3->IsDouble())
         SetCounter( index, (intC)(t3->getDouble()) );
       else if (t3->IsSingle())
         SetCounter( index, (intC)(t3->getSingle()) );
       else 
         pXCPT->Error(sysargE);
       return(TRUE);
     }
   else
     {
       val = GetCounter(index);
       x.setInt(val); 
       return(Unify(pHXL->XVar(2), &x));
     }
}

TF TermServices::p_for()                           // full ints
{      // for(I, Lower, Upper, Step) - must be used in repeat, for$(I, L, U, S)
  TERM h, t1, t2, t3;
  TF   tf;

  intC start, limit, step, valueh;

  t2 =(pHXL->XVar(2))->dref();
  t3 = (pHXL->XVar(3))->dref();
  limit = t2->getInt();
  step  = t3->getInt();

  if ((pCNTL->BTop() -> flags) & BF)              
    {                                              // not 1st time through
      h = (pCNTL->BTop() -> HBc - 1);              // get the last index 
      h->setInt(h->getInt() + step);
    } 
  else                                             
    {                                              // 1st time through 
      h = pHXL->heapGET();                         // get a cell 
      pCNTL->BTop()-> HBc = pHXL->HTop();          // reserve it 
      t1 = (pHXL->XVar(1))->dref();
      start = t1->getInt();
      h->setInt(start);
    }

  valueh = h->getInt();
  tf = step >= 0 ? valueh <= limit : valueh >= limit;
  if(tf && Unify(h, pHXL->XVar(0)))
    return(TRUE);

  pCNTL->FCut(); 
  return(FALSE); 
}


TF TermServices::p_char()
{ return X(0)->IsChar();}

TF TermServices::p_number()
{ return X(0)->IsNumber();}

TF TermServices::p_atom()
{ return X(0)->IsAtom();}

//TF TermServices::p_dbref()
//{ return X(0)->IsDBRef();}

TF TermServices::p_varsof()
{                                               // varsof(Term, List_of_Vars)
   TERM mark_heap; 
   // mark_heap is used as place holder in the heap from where
   // we will build a list of the vars in the argument 
   mark_heap = pHXL->HTop();
   (pHXL->HTop())->setList(pHXL->HTop() +1);
   pHXL->heapINC();
   *(pHXL->HTop()) = pATAB->NilCell;
   pHXL->heapINC();
   *(pHXL->HTop()) = pATAB->NilCell;
   pHXL->heapINC();
   varsof_(pHXL->XVar(0), mark_heap);
   if (pHXL->HTop() == 2 + mark_heap)             // no vars
     {
       *(mark_heap) = pATAB->NilCell;
       pHXL->SetHTop(mark_heap+1);
     }
   else
      mark_heap += 2;
   return(Unify(mark_heap, pHXL->XVar(1)));
}


TF TermServices::p_memberv()
// is_member/2 used by compiler and exported for general use
// it tests once, no backtracking, no var unification,
// note, good for open lists!
//  is_member( X, [H|_]) :- X == H, !.
//  is_member( X, [_|T]) :- is_member(X, T).
{
   TERM   t1, t2;

   t1 = (pHXL->XVar(0))->dref();
   t2 = (pHXL->XVar(1))->dref();

   if (! t2->IsList())
      return(FALSE);

   do 
   {
      t2 = t2->getTerm();
      if (StrongUnify(t1, t2))
         return(TRUE);
      t2 = (++t2)->dref(); 
   }  while (t2->IsList());

   return(FALSE);
}

TF TermServices::p_gensym()
// gensym/2 - create new atoms
{
   TERM x0;
   PATOM root;
   PATOM nextsym;
   std::map<PATOM, int>::iterator mi;
   int i;
   aCHAR buf[15];
   Cell c;

   x0 = X(0);
   if (! x0->IsAtom())
      pXCPT->Error(sysargE);
   root = x0->getAtom();

   mi = gensyms.find(root);
   if (mi != gensyms.end())
   {
      i = ++(mi->second);
   }
   else
   {
      i = 1;
      gensyms.insert(std::pair<PATOM,int>(root, i));
   }

   Lsprintf(buf, 14, aS("%d"), i);
   LString newstr(*root);
   newstr += buf;
   nextsym = pATAB->EnterAtom(newstr);
   c.setAtom(nextsym);

   return UnifyConst(X(1), &c);
}



/*
bool TermServices::less(TERM t1, TERM t2, bool vars_equal)
{
   int icmp = compare(t1, t2, vars_equal);
   return icmp < 0;
}
*/

// compare in standard sort order two terms.  Return
// int like strcmp. Used both by @< type
// predicates and for sorted dynamic predicates.
int TermServices::compare(TERM t1, TERM t2, bool vars_equal)
// return negative for  t1<t2, positive t1>t2, 0 t1=t2
{
#ifdef BUG_LEXORD
 DUMP << "==>TermServices::less" << NL;
 //Dump(t1);
 DumpWrite(t1);
 DUMP << SP2;
 //Dump(t2);
 DumpWrite(t2);
 //DUMP << NL;
 DUMP << "<==TermServices::less" << NL;
 DUMP << FLUSH;
#endif
   TERM x0, x1;
   x0 = t1->dref();
   x1 = t2->dref();
   int i;
   int icmp;

   ORDER_ rank0 = ordtype_(x0);
   ORDER_ rank1 = ordtype_(x1);
   double  rand0, rand1;
   int arity;

   // normally, vars sort first, but when finding
   // the last clause possible in sorted predicates,
   // we need to compare assuming vars are the last
   // thing, so this is temporarily turned off by
   // dynamic sorted predicate getIterator.
   // Note that it has to be set indirectly via
   // the setVarsFirst() function because compare() is
   // called from within the definition of the <
   // operator in modules.

   if (! b_vars_first)
   {
      if (rank0 == OREFS)
      {
         if (rank1 == OREFS)
            return 0;
         else
            return 1;
      }
      if (rank1 == OREFS)
         return -1;
   }

   // in VBA mode, or Asheville Syntax, atoms & strings compare as expected
   if (pSTATE->m_vba)
   {
      if ( (rank0 == OATOMS && rank1 == OSTRINGS) )
         return x0->getAtom()->Stricmp(*(x1->getLString()));
	  if ( (rank1 == OATOMS && rank0 == OSTRINGS) )
         return x0->getLString()->Stricmp(*(x1->getAtom()));
   }

   if (rank0 < rank1)
      return -1;
   if (rank0 > rank1)
      return 1;

   switch(rank0)
   {
   // a reference is less than other terms, but equal
   // to other references
   case OREFS:
      if (vars_equal)
         return 0;
      else
      {
         if (x0 == x1)
            return 0;
         else if (x0 < x1)
            return -1;
         else
            return 1;
      }
   case ONUMBERS:
      rand0 = x0->forceDouble();
      rand1 = x1->forceDouble();
      if (rand0 == rand1)
         return 0;
      else if (rand0 < rand1)
         return -1;
      else
         return 1;
   case OATOMS:
      return Lstrcmp(*(x0->getAtom()), *(x1->getAtom()));
   case OSTRINGS:
      if (pSTATE->m_vba)
         return x0->getLString()->Stricmp(*(x1->getLString()));
      else
         return Lstrcmp(x0->getStr(), x1->getStr());
   case OSTRUCTS:
      x0 = x0->getTerm();
      x1 = x1->getTerm();
      icmp = Lstrcmp(*(x0->getAtom()), *(x1->getAtom()));
      if (icmp != 0)
         return icmp;
      arity = x0->getArity();
      if (arity < x1->getArity())
         return -1;
      if (arity > x1->getArity())
         return 1;
      for (i = 0; i < arity; i++)
      {
         x0++; x1++;
         icmp = compare(x0,x1,vars_equal);
         if (icmp != 0)
            return icmp;
      }
      return 0;
   case OLISTS:
      x0 = x0->getListHead();
      x1 = x1->getListHead();
      icmp = compare(x0,x1,vars_equal);
      if (icmp != 0)
         return icmp;
      x0++; x1++;
      return compare(x0,x1,vars_equal);

   default:
      pXCPT->Error(internalE, aS("bad term type comparing terms"));
      //return 0;
   }

   return 0;
}

TF TermServices::p_is_cyclic()
{
   if (IsCyclic(X(0)))
      return TRUE;
   else
      return FALSE;
}

//--------------------------------------------------
// Stashing Functions
//
// reserve$/1 - create a named stash (list)
// stash$/2 - add a term to the end of the stash (list)
// get$/2 - get the full list term from the stash, and clear it
// peek$/2 - peek at the full list term on the stash
// stash$free/1 - free the stash, as does get$/2
//

TF TermServices::p_reserve_heap(void)
// reserve$/1
{
   Cell c;
   TERM t = (pHXL->XVar(0))->dref();
   if (! t->IsRef()) 
      pXCPT->Error(instanceE, aS("reserve$/1 arg not a variable"));

   Stash *st;
   LNEW(st, Stash(m_peng), aS("stash"));
   //c.setStash(new Stash(m_peng));
   c.setStash(pGCTH->make_stash(st));
   Unify(&c, pHXL->XVar(0));
   return TRUE;
}

TF TermServices::p_stash_term(void)
// stash$/2
{
   TERM t0 = (pHXL->XVar(0))->dref();
   if (! t0->IsStash())
      pXCPT->Error(instanceE, aS("stash$/2 arg1 not a stash"));

   Stash *st = t0->getStash();
   st->addTerm((pHXL->XVar(1))->term_size());
   TERM h = st->getHead();
   TERM ht = st->getHeadTerm();
   // copying to non heap storage, inc gc counts
   Copy(h, ht, &ht, pHXL->XVar(1), GC_INC); 
   return TRUE;
}

TF TermServices::p_get_term(void)
{
   TERM t0 = (pHXL->XVar(0))->dref();
   if (! t0->IsStash())
      pXCPT->Error(sysargE);
   Stash *st = t0->getStash();

   TERM x = st->getTerm();
   TERM h = pHXL->heapGET();
   TERM t = pHXL->heapGETN((x)->term_size());

   // no longer on stash, so dec GC Things
   Copy(h, t, &t, x, GC_DEC);
   st->clearTerm();
   return Unify(pHXL->XVar(1), h);
}

TF TermServices::p_peek_term(void)
{
   TERM t0 = (pHXL->XVar(0))->dref();
   if (! t0->IsStash())
      pXCPT->Error(sysargE);
   Stash *st = t0->getStash();

   TERM x = st->getTerm();
   TERM h = pHXL->heapGET();
   TERM t = pHXL->heapGETN((x)->term_size());

   // copying to heap, no need for gc
   Copy(h, t, &t, x, GC_NO);
   return Unify(pHXL->XVar(1), h);
}

TF TermServices::p_stash_free()
{
   TERM t0 = (pHXL->XVar(0))->dref();
   if (! t0->IsStash())
      pXCPT->Error(sysargE);
   Stash *st = t0->getStash();
   //delete st;
   st->clearTerm();  // gc will delete later if necessary
   return TRUE;
}



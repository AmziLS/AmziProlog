/***************************************************************************\
*
* ddb.cpp -- Dynamic Database methods
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: ddb.cpp,v $
* Revision 1.16  2007/05/16 02:15:38  dennis
* a7-6-8
*
* Revision 1.15  2006/12/19 04:58:46  dennis
* module names are just atoms
*
* Revision 1.14  2006/12/18 17:56:15  dennis
* another newmod
*
* Revision 1.13  2006/12/15 21:00:30  dennis
* mods and caps stuff
*
* Revision 1.12  2006/12/15 04:05:55  dennis
* undecapitalization of module names
*
* Revision 1.11  2005/08/04 21:05:39  dennis
* sync logging
*
* Revision 1.10  2005/07/28 13:33:19  dennis
* minor bug fixes
*
* Revision 1.9  2005/04/25 23:55:37  dennis
* merged sync and apitrace changes
*
* Revision 1.8  2004/05/23 21:56:06  dennis
* fixed dcg debug bug
*
* Revision 1.7  2004/02/01 13:25:56  mary
* Fix predicate_info to not return blank results, and some predicates were
* documented incorrectly.
*
* Revision 1.6  2004/01/30 16:33:44  mary
* Added argument and description strings to PredicateHead and alib.
* Added predicate_info and predicate$enginfo to return this information.
*
* Revision 1.5  2003/12/23 21:09:57  dennis
* speed improvement, added unifycopy function for clauseZdb, so it goes quicker,
* 30-40% improvement in rubik interpreted, 10-15% improvement compiled.
*
* Revision 1.4  2003/12/10 22:29:04  dennis
* built dll with copy all feature for the dynamic db for now
*
* Revision 1.3  2003/10/14 01:45:09  dennis
* keys/license stuff in engine
*
* Revision 1.2  2003/10/03 03:57:35  dennis
* fixed, broke, fixed, broke, fixed I hope the source debugger
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.57  2003/09/11 02:07:44  dennis
* fixed memory leak problem with dynamic iterators
*
* Revision 1.56  2003/06/13 18:37:01  dennis
* date_time and other fixes
*
* Revision 1.55  2002/12/26 17:44:02  dennis
* Changes to listener to make it easier to access listener from IDE.
*
* Revision 1.54  2002/12/11 17:29:42  dennis
* new build for 6-3-2, using visual studio .net
*
* Revision 1.53  2002/12/06 17:37:36  dennis
* a6-3-2, finally got rid of mscw tagging of heap cells to maintain
* state of dynamic clause backtracking, put backtracking info in
* the choice point structure instead.  much cleaner.  retract can now
* just analyze the control stack to see if a retract is safe.  heapgc
* no longer has to worry about those wierd cells.
*
* Revision 1.52  2002/12/02 18:25:12  dennis
* Converted to Visual Studio .NET.
*
* Revision 1.51  2002/11/26 16:50:40  dennis
* thanksgiving updates
*
* Revision 1.50  2002/09/27 00:27:23  dennis
* fix another retract pulls the rug out from goal bug
*
* Revision 1.49  2002/07/20 00:57:29  dennis
* debugger bug fixes, and some retract bugs fixed
*
* Revision 1.48  2002/06/23 20:01:29  dennis
* fixed some gc issues
*
* Revision 1.47  2002/06/19 17:11:01  dennis
* fixed debugger to not display integer modules, use atom instead
*
* Revision 1.46  2002/06/02 03:50:56  dennis
* all the XStr forms of logic server calls call strterm and grow the
* heap, so made new ExecProve and CallProve that to the strterm inside
* so that the heap can rolled back to before the Exec/Call.  Important
* that this be done in the Prove, so that if heapgc is encountered,
* the new heap is used for the rollback.
*
* Revision 1.45  2002/05/15 16:59:07  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.44  2002/05/08 16:14:54  dennis
* final a6-1-78 build, OK for chinese lsapi calls
*
* Revision 1.43  2002/04/25 03:42:22  dennis
* more documentation, logicbase.htm, and some fiddling with sources
*
* Revision 1.42  2002/04/19 19:41:43  dennis
* fixed retract bug with sorted/indexed clauses, implemented abolish for
* those types as well
*
* Revision 1.41  2002/04/02 22:52:43  dennis
* Moved the hotel two feet to the right, changing arity and xi
* in .plm files to be 2 bytes rather than 1.
*
* Revision 1.40  2002/03/07 04:37:43  dennis
* updated tutorial and duckworld samples
*
* Revision 1.39  2002/03/04 03:46:12  dennis
* fixed bug in sorted predicates
*
* Revision 1.38  2002/02/19 19:38:09  ray
* Reaffirmation
*
* Revision 1.37  2002/01/06 20:31:27  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.36  2001/12/08 03:48:21  dennis
* fixed bugs with reconsult, added unicode support
*
* Revision 1.35  2001/12/06 16:43:20  dennis
* fixed heapgc bug and allowed for unlimited vars in copyterm
*
* Revision 1.34  2001/11/10 00:36:20  dennis
* fixed reset bug in streams, so reset OK and IDE can post errors.
*
* Revision 1.33  2001/11/09 02:28:09  dennis
* Finished, I hope, sorted and indexed predicates. Needed to rethink
* how mscws worked, given the new itertors for moving from clause to
* clause.
*
* Revision 1.32  2001/10/30 01:01:03  dennis
* sorted and indexed clauses supported in dynamic database
*
* Revision 1.31  2001/10/27 03:24:37  dennis
* sorted dynamic predicates working, with optimized queries
* when they fit the right pattern, of vars last
*
* Revision 1.30  2001/10/24 05:06:34  dennis
* changed linked list of clauses in ddb to use STL list, so links
* no longer in clause, allowing for different types of iterators on
* clauses
*
* Revision 1.29  2001/10/13 05:06:10  dennis
* speeded up system metapredicates with hard-wired properties
*
* Revision 1.28  2001/10/13 02:58:12  dennis
* see/tell bugs, used to close function streams
*
* Revision 1.27  2001/10/02 16:05:20  dennis
* changed streams, cleaner interface to lex, reimplemented
* readers as buffer between the two
*
* Revision 1.26  2001/08/05 19:11:20  dennis
* made unload work for .plm files again
*
* Revision 1.25  2001/07/24 02:52:47  dennis
* discontiguous/multifile working again for 6.1
*
* Revision 1.24  2001/07/21 00:39:46  dennis
* added garbage collector for strings and things
*
* Revision 1.23  2001/07/10 16:51:31  dennis
* added more memory leak checking tools, all clean so far
*
* Revision 1.22  2001/06/27 15:15:09  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.21  2001/04/16 05:21:14  dennis
* hacked together some fixes for sio/lex to be better friends,
* merged other changes, added new samples
*
* Revision 1.20  2001/04/02 21:50:12  dennis
* got debugger working again
*
* Revision 1.19  2001/03/25 15:29:50  ray
* Implemented fixed
* repaired #include
*
* Revision 1.18  2001/03/16 00:29:06  dennis
* compiled metapredicates
*
* Revision 1.17  2001/03/14 02:20:39  dennis
* minor meta$convert improvements
*
* Revision 1.16  2001/03/13 22:04:03  dennis
* changed is$code
*
* Revision 1.15  2001/03/13 20:05:17  dennis
* added meta$call to try and isolate meta calls.
*
* Revision 1.14  2001/03/09 20:13:59  dennis
* consolidating for KW delivery, fixed jni memory leak
*
* Revision 1.13  2001/02/28 02:22:38  dennis
* fixed some number bugs, some assert/retract bugs
*
* Revision 1.12  2001/02/22 18:03:08  dennis
* fixed srcbuf and destbuf, so not needed
*
* Revision 1.11  2001/02/21 04:46:42  dennis
* debugger working, updated documentation for 6.1
*
* Revision 1.10  2001/02/10 05:02:57  dennis
* fixed assert/retract gc bug
*
* Revision 1.9  2001/02/08 22:56:45  dennis
* string bug fixes, modularized compiler and listener
*
* Revision 1.8  2001/02/05 03:11:43  dennis
* Fixed nasty heap GC bug, other minor fixes
*
* Revision 1.7  2001/01/30 16:47:28  dennis
* Made, after many trials, alib into amzi_system module.
*
* Revision 1.6  2001/01/11 03:39:00  dennis
* Made metapredicates more efficient with is$meta/3
*
* Revision 1.5  2001/01/11 01:49:23  dennis
* Implemented rest of import/export and metapredicates, working
* for test cases.
*
* Revision 1.4  2001/01/06 03:46:02  dennis
* listener working with import/export
*
* Revision 1.3  2001/01/05 06:05:54  dennis
* fixed minor discrepancies
*
* Revision 1.2  2000/12/29 22:39:48  dennis
* added first cut at a system module
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.9  2000/12/28 16:30:35  dennis
* fixed nasty pets bug, true/false on CallInterp()
* and other changes, merged with ray's new stuff
* calling this a6-1-6
*
* Revision 1.8  2000/11/30 15:47:28  ray
* made arg, nth and prime bilateral.
* made real length and exponent 12 bits for maximum range
*
* Revision 1.7  2000/10/01 16:20:03  dennis
* cleaned up modules, ddb, got defined, abolish and friends working
*
* Revision 1.6  2000/09/28 03:24:39  dennis
* debugger working for command line listener
*
* Revision 1.5  2000/09/27 01:42:03  dennis
* implemented listing, needed current_predicate, predicate_property,
* and current_module
*
* Revision 1.4  2000/09/25 02:11:19  dennis
* first version of modules working, runs the modular version of
* duck world.  still needs import and export.  release 6.1.1
*
* Revision 1.3  2000/09/15 21:42:24  dennis
* 12->13
*
* Revision 1.2  2000/09/02 02:10:19  dennis
* new version of get$db replacing dbrefgen for getting clauses
* from dynamic database
*
* Revision 1.1  2000/08/26 20:24:10  dennis
* added missing module files
*
* Revision 1.2  2000/05/15 11:24:47  dennis
* fixed some minor bugs, started modules
*
* Revision 1.1  2000/05/14 03:52:32  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
*
\***************************************************************************/

#include "inc.h"
#include "pch.h"

// debugging flags
#ifdef LANDFILL
#define noBUG_DB

#ifdef BUG_DB
#define noBUG_NEW_COPY
#define noBUG_MODULES
#define noBUG_OPS
#define noBUG_DBDIAG
#define noBUG_CLAUSE
#define noBUG_ASSRET
#define noBUG_CALL
#define noBUG_INTERP
#define noBUG_REFGEN
#define noBUG_ISCODE
#define MODTEST(IMOD) IMOD > SYSTEM_MODULE
#endif

#endif

// Find the components of a goal in a term which
// might be in any one of a number of forms with
// an optional m:
// m:f/a  which is  :(m,/(f,a))
// m:f(a) which is  :(m, f(a))
// m:f(a) :- b,c.  which is  :-(:(m,f(a)),','(b,c))
// m:[a,b]
// or just plain a, in which case find an arity that fits.  
// This should be an interim case.
Goal::Goal(LEngine *m_peng, TERM t)
{
   PROBE_FUNCTION_START("Goal constructor");

   PATOM a;
   TERM targ;
   explicit_modix = true;
   
   TERM terr;  // used for error reporting
   terr = t;
   t = t->dref();
   head = t;   // for now
   if (t->IsStruct())
   {
      t = t->getTerm();
      a = t->getAtom();
      if (a == pATAB->ifA)  // ":-"
      {
         targ = (t+1)->dref();  // head of clause
         head = targ;
         if (targ->IsStruct())
         {
            t = targ->getTerm();
            a = t->getAtom();
            if (a == pATAB->colonA)
            {
               targ = (t+1)->dref();  // the module name
               if (targ->IsAtom())
               {
                  a = targ->getAtom();
                  imod = pDDB->getModuleIX(a);
               }
               else if (targ->IsInt())
                  imod = (MODIX)targ->getInt();
               else
               {
                  aCHAR buf[128];
                  pWRIT->termWriteString(terr, buf, 127, true);
                  pXCPT->Error(badgoalE, buf);
               }

               targ = (t+2)->dref();
               modterm = head = targ;  // not sure this is right for modterm.
               if (targ->IsStruct())
               {
                  t = targ->getTerm();
                  functor = t->getAtom();
                  arity = t->getArity();
               }
               else
               {
                  functor = targ->getAtom();
                  arity = 0;
               }
            } // end :- with head = structure :/2
            else
            {
               functor = a;
               arity = t->getArity();
               imod = pDDB->getCurrentMIX();
               explicit_modix = false;
               modterm = t;
            } // end :- with head = structure not :/2
         } // end :- with head = structure
         else if (targ->IsAtom())  // head is just an atom
         {
            functor = targ->getAtom();
            arity = 0;
            imod = pDDB->getCurrentMIX();
            explicit_modix = false;
            modterm = targ;
         }
         else
         {
            aCHAR buf[128];
            pWRIT->termWriteString(terr, buf, 127, true);
            pXCPT->Error(badgoalE, buf);
        }
      }  // end :-
      else if (a == pATAB->colonA)            // ":"
      {
         targ=(t+1)->dref();
         if (targ->IsAtom())
         {
            a = targ->getAtom();              // the module name
            imod = pDDB->getModuleIX(a);
         }
         else if (targ->IsInt())
            imod = (MODIX)targ->getInt();
         // we allow variable mods sometimes, like in goals
         // but not in heads, and this is called in both cases,
         // so let the goals slip through here.
         //else if (targ->IsVar())
         //   imod = -1;
         else
         {
            aCHAR buf[128];
            pWRIT->termWriteString(terr, buf, 127, true);
            pXCPT->Error(badgoalE, buf);
         }
//            LASSERT(false, aS("invalid goal in Goal constructor"));

         targ=(t+2)->dref();
         modterm = head = targ;
         if (targ->IsStruct())
         {
            t = targ->getTerm();
            a = t->getAtom();
            if (a == pATAB->divA)
            {
               targ = (t+1)->dref();
               functor = targ->getAtom();
               targ = (t+2)->dref();
               arity = (ARITY)targ->getInt();
            }
            else
            {
               functor = a;
               arity = t->getArity();
            }
         }
         else if (targ->IsList())
         {
            functor = pATAB->periodA;
            arity = 2;
         }
         else if (targ->IsAtom())
         {
            functor = targ->getAtom();
            arity = 0;
         }
         else
         {
            aCHAR buf[128];
            pWRIT->termWriteString(terr, buf, 127, true);
            pXCPT->Error(badgoalE, buf);
         }
      }
      else if (a == pATAB->divA)              // "/"
      {
         t++;
         functor = t->getAtom();
         t++;
         arity = (ARITY)t->getInt();
         imod = pDDB->getCurrentMIX();
         explicit_modix = false;
      }
      else                                // no module specified, normal f(a,b)
      {
         functor = a;
         arity = t->getArity();
         imod = pDDB->getCurrentMIX();
         explicit_modix = false;
         modterm = t;
      }
   }
   else if (t->IsAtom())
   {
      functor = t->getAtom();
      arity = 0;
      imod = pDDB->getCurrentMIX();
      explicit_modix = false;
      modterm = t;
   }
   else if (t->IsList())
   {
      functor = pATAB->periodA;
      arity = 0;
      imod = pDDB->getCurrentMIX();
      explicit_modix = false;
      modterm = t;
   }
   else
   {
      aCHAR buf[128];
      pWRIT->termWriteString(terr, buf, 127, true);
      pXCPT->Error(badgoalE, buf);
   }

   PROBE_FUNCTION_END("Goal constructor");
}

DDB::DDB(LEngine *peng)
{
   m_peng = peng;

   for (MODIX i=0; i<N_MODULES; i++)
      modules[i] = NULL;
}

DDB::~DDB()
{
   for (MODIX i=0; i<N_MODULES; i++)
   {
      delete modules[i];
   }  // brackets for LDEL macro
}

void DDB::Init()
{        // Where all the built-in predicates go, as well as the code in alib.
	  //new ModClassic(SYSTEM_MODULE, aS("amzi_system"), m_peng);
   LNEW(modules[SYSTEM_MODULE], 
//cim	  ModClassic(SYSTEM_MODULE, aS("amzi_system"), m_peng), aS("module"));
	  ModClassic(SYSTEM_MODULE, pATAB->amzi_systemA, m_peng), aS("module"));
   modules[SYSTEM_MODULE]->Init();
   // Each application has a default module named 'user'.
      //new ModClassic(USER_MODULE, aS("user"), m_peng);
   LNEW(modules[USER_MODULE],
//cim          ModClassic(USER_MODULE, aS("user"), m_peng), aS("module"));
          ModClassic(USER_MODULE, pATAB->userA, m_peng), aS("module"));
   modules[USER_MODULE]->Init();
   current_imod = USER_MODULE;
   nmods = 2;
}

/*cim
MODIX DDB::NewModule(PATOM modatom)
{
	STRptr modname = modatom->get_display_name(m_peng);
	return NewModule(modname);
}

MODIX DDB::NewModule(STRptr modname)
{
   MODIX imod;

   imod = getModuleIX(modname);
   if (imod != UNDEFINED_MODULE)
      return imod;

   imod = nmods;
   if (nmods >= N_MODULES)
      pXCPT->Error(maxmodsE);
   LNEW(modules[imod],ModClassic(imod, modname, m_peng), aS("module"));
   modules[imod]->Init();
   nmods++;

#ifdef BUG_MODULES
 FILL("DDB::NewModule " << modname);
 DUMP << "current modules: " << NL;
 for (int i=0; i<nmods; i++)
  DUMP << "  module[" << i << "] = " << modules[i]->getModuleNameA()->get_display_name(m_peng) << NL;
 DUMP << FLUSH;
#endif
   return imod;
}
*/

MODIX DDB::NewModule(PATOM modnameA)
{
   MODIX imod;

   imod = getModuleIX(modnameA);
   if (imod != UNDEFINED_MODULE)
      return imod;

   imod = nmods;
   if (nmods >= N_MODULES)
      pXCPT->Error(maxmodsE);
   LNEW(modules[imod],ModClassic(imod, modnameA, m_peng), aS("module"));
   modules[imod]->Init();
   nmods++;

#ifdef BUG_MODULES
 FILL("DDB::NewModule " << modnameA->get_display_name(m_peng));
 DUMP << "current modules: " << NL;
 for (int i=0; i<nmods-1; i++)
  DUMP << "  module[" << i << "] = " << modules[i]->getModuleNameA() << NL;
 DUMP << FLUSH;
#endif
   return imod;
}



/*cim
MODIX   DDB::getModuleIX(STRptr s)
{                 // Get Module IX from a string. Returns -1 if no such module.
   for (MODIX i = 0; i < nmods; i++)
   {
      if (pSTATE->m_vba)
	  {
         if ( 0 == LString(s).Stricmp(LString(modules[i]->getModuleName())) )
			 return i;
	  }
	  else
	  {
         if (Lstrcmp(modules[i]->getModuleName(), s) == 0)
            return i;
	  }
   }
   return UNDEFINED_MODULE;
}
*/

MODIX   DDB::getModuleIX(PATOM a)
{                 // Get Module IX from a string. Returns -1 if no such module.
   for (MODIX i = 0; i < nmods; i++)
   {
      if (a == modules[i]->getModuleNameA())
		  return i;
   }
   return UNDEFINED_MODULE;
}


RC DDB::Assert(int az, TERM t)
{
   MODIX imod;
   t = t->dref();
   if ( t->IsVar() )
      pXCPT->Error(instanceE, aS("Cannot assert a variable"));

   Goal g(m_peng, t);
   pATAB->ValidGoal(g.functor); // If its a special key, throw an error

   if (g.explicit_modix)
   {
      t = g.modterm;
      imod = g.imod;
   }
   else
      imod = USER_MODULE;

#ifdef BUG_ASSRET
if (MODTEST(g.imod))
{
  DUMP << NL << "Asserting: " << MODPREDAR(g.imod, g.functor, g.arity) 
		 << SP << SP << FLUSH;
  pTSVC->Dump(t);
  DUMP << ".end." << NL << FLUSH;
}
#endif
   return modules[imod]->Assert(az, g, t);
}

RC DDB::Assert(MODIX imod, int az, TERM t)
{
   t = t->dref();
   if ( t->IsVar() )
      pXCPT->Error(instanceE, aS("Cannot assert a variable"));

   Goal g(m_peng, t);
   pATAB->ValidGoal(g.functor);         // If its a special key, throw an error

#ifdef BUG_ASSRET
if (MODTEST(imod))
{
  DUMP << NL << "--- Asserting---" << NL;
  pTSVC->Dump(t);
  DUMP << NL << "in Prolog:" << NL << FLUSH;
  pTSVC->DumpWrite(t);
  DUMP << NL << FLUSH;
  DUMP << "Goal" << NL;
  DUMP << MODPREDAR(imod, g.functor, g.arity) << NL;
  DUMP << ".end." << NL << FLUSH;
}
#endif
   return modules[imod]->Assert(az, g, t);
}

RC DDB::Retract(TERM t)
{
   PredicateHead  *pph;
   DynamicClause  *pdc = NULL;
   DynamicClauseIterator  *pdci;
   TRAIL_INDEX     ti;

   RC rc;

   Goal g(m_peng, t);

#ifdef BUG_ASSRET
if (MODTEST(g.imod))
{
  DUMP << NL << "Retracting: " << MODPREDAR(g.imod, g.functor, g.arity) << 
	 SP << SP << FLUSH;
  pTSVC->Dump(t);
  DUMP << ".end." << NL << FLUSH;
}
#endif
   // find the head of the predicate, which will
   // lead to the dynamic clauses if they exist.
   pph = getPredicateHead(g.imod, g.functor, g.arity);
   if (pph == NULL || !pph->IsDynamic())
      return FALSE;

   //pdc = pph->getDynamicPredicate()->getFirst();      
   pdci = pph->getDynamicPredicate()->getIterator(g.head);      
   //while (pdc != NULL)
   while (! pdci->isDone())
   {
      pdc = pdci->getClause();
      //if (pTSVC->Unify(pdc->getCode(), g.head))
      if (pTSVC->Unify(pdc->getCode(), t))
         break;

      // unwind any variables that were bound during
      // the unification attempt
      pHXL->SetHTop(pCNTL->BTop()->HBc);
      ti = pCNTL->BTop()->TRc;
      pCNTL->Unwind(ti);

      //pdc = pdc->next();
      pdci->setNext();
   }

   // if none was found, trim the trail and cut
   // past the preceding 'repeat' and fail.
   //if (pdc == NULL)
   //if (pdci->isDone())
   //   rc = FALSE;
   rc = pdci->isDone() ? FALSE : TRUE;
   remove_ddb(g.imod, pdci);

   delete pdci;   // don't need iterator anymore
   return rc;
}


//--------------------------------
// Interpreted Call functions
//

// called from pcode.c when the WAM code call calls call/1
//   X[0] therefore has the argument to call/1, or the thing to be called
// As this is a meta-predicate, the argument is checked to see if
// it is :/2, to resolve the module to call.  If not, then the
// current runtime context is used, which is the first argument
// in the codeptr.  The code contains:
//    modix, pred(call), pred(call), arity, [numvars if call, not exec]

CODEptr DDB::DoCall(CODEptr cp, TF from_call)
{                                           // cp has functor, arity, numperms 
   //CLAUSE_BLKptr  ci;
   //PRED_BLKptr    pi;
   PredicateHead *pph;
   TERM           t, t2;
   //TERM           x;
   ARITY          ar;
   PATOM          pred, interpreter;
   int            result;
   int            CCLEN;
   MODIX          imod;
   MODIX          code_imod;

   CCLEN = cdMODIX_L + 2 * cdATOM_L + cdSMINT_L + cdSINT_L;
   
   t = X(0);
   Goal g(m_peng, t);
   pred = g.functor;
   ar = g.arity;
   imod = g.imod;

#ifdef BUG_CALL
   DUMP << "DoCall: " << MODPREDAR(imod,pred,ar);
   if (g.explicit_modix)
      DUMP << " explicit_modix" << NL;
   else
      DUMP << " no explicit_modix" << NL;
   DUMP << "  code_call_modix = " << *(cdMODIXptr)cp << NL;
//   DUMP << "  term:  " << NL;
//   pTSVC->Dump(t);
   DUMP << FLUSH;
#endif
   // no explicit module specification
   code_imod = (MODIX)*(cdMODIXptr)cp;
   //if (! g.explicit_modix)
   //   imod = code_imod;
   // careful here, added this without being too sure,
   // if code_imod is the system module, then the chances
   // are good, we should really use the user module as our
   // context... the calls to call without explicit modix,
   // but from system, are the findalls, and calls, nots, onces,
   // of the world, with a call(X) as a goal, so user is usually
   // what they want.
   if (! g.explicit_modix)
   {
      if (code_imod == SYSTEM_MODULE)
         imod = USER_MODULE;
      else
         imod = code_imod;
   }

   pEXEC->SetCurFA(pred, ar);

   interpreter = (pSTATE->m_trace) ?       // tracing ? 
         pATAB->call_d_dollarA :           // use debugger (call$d) 
         pATAB->call_dollarA;              // use call$i 

   //pph = getPredicateHead(imod, pred, ar);
   pph = getVisiblePredicate(imod, pred, ar);

   // if no predicate, then the call simply fails.  Is this
   // correct?  old version called interpreter anyway to
   // let it fail. - no that was wrong, call interpreter
   // because the term might be a ',' or ';' or some other
   // such thing that needs the interpreter to break apart.

#ifdef BUG_CALL
   DUMP << "  code_imod = " << code_imod;
   DUMP << "  interpreter = " << interpreter;
   DUMP << NL;
   if (pph)
      DUMP << "  pph: " << *pph << NL;
   else
      DUMP << "  pph: NULL" << NL << FLUSH;
#endif

   // run$code is in the debugger, and it calls db$call in
   // alib, but for some reason, whoa is run$code and
   // code_imod is system module, so that's what we'll
   // go with.
   if ( pph
        &&
        pph->IsCompiled()
        &&
        ! pph->IsMeta() )
   {
      if (from_call)
         pEXEC->SetCP( CCLEN + cp ); 
      // skip func, arity, numperms 
		// this is where args are copied to registers
      if (ar && ! g.explicit_modix)
         pHXL->CopyToX( t->getTerm()+1, ar );
      else if (ar && g.explicit_modix)
      {
         t2 = (t->getTerm()+2)->dref();
         pHXL->CopyToX( t2->getTerm()+1, ar );
      }
#ifdef BUG_CALL
   DUMP << "  using compiled code, args:";
   for (int i = 0; i < ar; i++)
   {
      DUMP << NL << "  X[" << i << "] ";
      pTSVC->Dump(pHXL->XVar(i));
   }
   DUMP << NL << FLUSH;
#endif
      return pph->getCompiledPredicate()->getCode();
   }

   else if (pph && pph->IsMeta())
   {
      if (from_call)
         pEXEC->SetCP( CCLEN + cp );

      if (! g.explicit_modix)
      {   // pop the current module on if not already there, as call needs it.
         if (imod == SYSTEM_MODULE)     // default to user if caller is system
            imod = USER_MODULE;
         X(0)->setModix(imod);
      }
#ifdef BUG_CALL
   DUMP << "  using meta$call" << NL << FLUSH;
#endif
      return getCompiledPredicate(SYSTEM_MODULE, 
											 pATAB->metaZcallA, 1)->getCode();
   }
   else if (pph == NULL || pph->IsDynamic())
   {
      if (from_call)
         pEXEC->SetCP( CCLEN + cp );

      // pop the current module on if its not already there, as
      // call needs it.
      if (! g.explicit_modix)
      {
         // default to user if caller is system
         if (imod == SYSTEM_MODULE)
            imod = USER_MODULE;
         X(0)->setModix(imod);
      }
#ifdef BUG_CALL
   DUMP << "  using interpreter" << NL << FLUSH;
#endif
      return getCompiledPredicate(SYSTEM_MODULE, interpreter, 1)->getCode();
   }

   else if (pph->IsEscape())
   {
      if (ar && ! g.explicit_modix)
         pHXL->CopyToX( t->getTerm()+1, ar );
      else if (ar && g.explicit_modix)
      {
         t2 = (t->getTerm()+2)->dref();
         pHXL->CopyToX( t2->getTerm()+1, ar );
      }

      try
      {
         result = pph->getEscapePredicate()->execute();
      } 
      catch (LExcept &pE)
      {
         throw(pE);
      }

      if (result == TRUE)
      {
         if (from_call)
            return(CCLEN + cp);
         else   // fake out a proceed
            return(pEXEC->GetCP());
      }
      else if (result == FALSE)
         return(pEXEC->failure());
   }
   else
      pXCPT->Error(internalE, aS("corrupt PredicateHead in DoCall"));

   return NULL;   // a dummy
}

CODEptr DDB::DoCallMeta(CODEptr cp, TF from_call)
{
   //CLAUSE_BLKptr  ci;
   //PRED_BLKptr    pi;
   PredicateHead *pph;
   TERM           t, t2;
   //TERM           x;
   ARITY          ar;
   PATOM          pred, interpreter;
   int            result;
   int            CCLEN;
   MODIX          imod;
   MODIX          code_imod;

   CCLEN = cdMODIX_L + 2 * cdATOM_L + cdSMINT_L + cdSINT_L;
   // cp has functor, arity, numperms 
   t = X(0);
   Goal g(m_peng, t);
   pred = g.functor;
   ar = g.arity;
   imod = g.imod;

#ifdef BUG_CALL
   DUMP << "DoCall: " << MODPREDAR(imod,pred,ar);
   if (g.explicit_modix)
      DUMP << " explicit_modix" << NL;
   else
      DUMP << " no explicit_modix" << NL;
   DUMP << "  code_call_modix = " << *(cdMODIXptr)cp << NL;
//   DUMP << "  term:  " << NL;
//   pTSVC->Dump(t);
   DUMP << FLUSH;
#endif
   // no explicit module specification
   code_imod = (MODIX)*(cdMODIXptr)cp;
   if (! g.explicit_modix)
      imod = code_imod;

   pEXEC->SetCurFA(pred, ar);

   interpreter = (pSTATE->m_trace) ?       // tracing ? 
         pATAB->call_d_dollarA :           // use debugger (call$d) 
         pATAB->call_dollarA;              // use call$i 

   //pph = getPredicateHead(imod, pred, ar);
   pph = getVisiblePredicate(imod, pred, ar);

   // if no predicate, then the call simply fails.  Is this
   // correct?  old version called interpreter anyway to
   // let it fail. - no that was wrong, call interpreter
   // because the term might be a ',' or ';' or some other
   // such thing that needs the interpreter to break apart.

#ifdef BUG_CALL
   DUMP << "  code_imod = " << code_imod;
   DUMP << "  interpreter = " << interpreter;
   DUMP << NL;
   if (pph)
      DUMP << "  pph: " << *pph << NL;
   else
      DUMP << "  pph: NULL" << NL << FLUSH;
#endif

   // run$code is in the debugger, and it calls db$call in
   // alib, but for some reason, whoa is run$code and
   // code_imod is system module, so that's what we'll
   // go with.
   if ( pph
        &&
        pph->IsCompiled()
        &&
        ! pph->IsMeta() )
   {
      if (from_call)
         pEXEC->SetCP( CCLEN + cp ); 
      // skip func, arity, numperms 

      if (ar && ! g.explicit_modix)
         pHXL->CopyToX( t->getTerm()+1, ar );
      else if (ar && g.explicit_modix)
      {
         t2 = (t->getTerm()+2)->dref();
         pHXL->CopyToX( t2->getTerm()+1, ar );
      }
#ifdef BUG_CALL
   DUMP << "  using compiled code, args:";
   for (int i = 0; i < ar; i++)
   {
      DUMP << NL << "  X[" << i << "] ";
      pTSVC->Dump(pHXL->XVar(i));
   }
   DUMP << NL << FLUSH;
#endif
      return pph->getCompiledPredicate()->getCode();
   }

   else if (pph && pph->IsMeta())
   {
      if (from_call)
         pEXEC->SetCP( CCLEN + cp );

      if (! g.explicit_modix)
      {// pop the current module on if its not already there, as call needs it.
         if (imod == SYSTEM_MODULE)
            imod = USER_MODULE;          // default to user if caller is system

         X(0)->setModix(imod);
      }
#ifdef BUG_CALL
   DUMP << "  using meta$call" << NL << FLUSH;
#endif
      return getCompiledPredicate(SYSTEM_MODULE, 
											 pATAB->metaZcallA, 1)->getCode();
   }
   else if (pph == NULL || pph->IsDynamic())
   {
      if (from_call)
         pEXEC->SetCP( CCLEN + cp );

      // pop the current module on if its not already there, as
      // call needs it.
      if (! g.explicit_modix)
      {
         // default to user if caller is system
         if (imod == SYSTEM_MODULE)
            imod = USER_MODULE;
         X(0)->setModix(imod);
      }
#ifdef BUG_CALL
   DUMP << "  using interpreter" << NL << FLUSH;
#endif
      return getCompiledPredicate(SYSTEM_MODULE, interpreter, 1)->getCode();
   }

   else if (pph->IsEscape())
   {
      if (ar && ! g.explicit_modix)
         pHXL->CopyToX( t->getTerm()+1, ar );
      else if (ar && g.explicit_modix)
      {
         t2 = (t->getTerm()+2)->dref();
         pHXL->CopyToX( t2->getTerm()+1, ar );
      }

      try
      {
         result = pph->getEscapePredicate()->execute();
      } 
      catch (LExcept &pE)
      {
         throw(pE);
      }

      if (result == TRUE)
      {
         if (from_call)
            return(CCLEN + cp);
         else                                  // fake out a proceed
            return(pEXEC->GetCP());
      }
      else if (result == FALSE)
         return(pEXEC->failure());
   }
   else
      pXCPT->Error(internalE, aS("corrupt PredicateHead in DoCall"));

   return NULL;                                // a dummy
}

// A special version of DoCall, called when we know
// the argument does not need meta$convert processing

CODEptr DDB::DoCallNoMeta(CODEptr cp, TF from_call)
{
   PredicateHead *pph;
   TERM           t, t2;
   ARITY          ar;
   PATOM          pred, interpreter;
   int            result;
   int            CCLEN;
   MODIX          imod;
   MODIX          code_imod;

   CCLEN = cdMODIX_L + 2 * cdATOM_L + cdSMINT_L + cdSINT_L;
   // cp has functor, arity, numperms 
   t = X(0);
   Goal g(m_peng, t);
   pred = g.functor;
   ar = g.arity;
   imod = g.imod;

#ifdef BUG_CALL
   DUMP << "DoCallNoMeta: " << MODPREDAR(imod,pred,ar);
   if (g.explicit_modix)
      DUMP << " explicit_modix" << NL;
   else
      DUMP << " no explicit_modix" << NL;
   DUMP << "  code_call_modix = " << *(cdMODIXptr)cp << NL;
   DUMP << FLUSH;
#endif
   // no explicit module specification
   code_imod = (MODIX)*(cdMODIXptr)cp;
   if (! g.explicit_modix)
      imod = code_imod;

   pEXEC->SetCurFA(pred, ar);

   interpreter = (pSTATE->m_trace) ?       // tracing ? 
         pATAB->call_d_dollarA :           // use debugger (call$d) 
         pATAB->call_dollarA;              // use call$i 

   pph = getVisiblePredicate(imod, pred, ar);

#ifdef BUG_CALL
   DUMP << "  code_imod = " << code_imod;
   DUMP << "  interpreter = " << interpreter;
   DUMP << NL;
   if (pph)
      DUMP << "  pph: " << *pph << NL;
   else
      DUMP << "  pph: NULL" << NL << FLUSH;
#endif

   if ( pph
        &&
        pph->IsCompiled() )
   {
      if (from_call)
         pEXEC->SetCP( CCLEN + cp ); 
      // skip func, arity, numperms 

      if (ar && ! g.explicit_modix)
         pHXL->CopyToX( t->getTerm()+1, ar );
      else if (ar && g.explicit_modix)
      {
         t2 = (t->getTerm()+2)->dref();
         pHXL->CopyToX( t2->getTerm()+1, ar );
      }
#ifdef BUG_CALL
   DUMP << "  using compiled code, args:";
   for (int i = 0; i < ar; i++)
   {
      DUMP << NL << "  X[" << i << "] ";
      pTSVC->Dump(pHXL->XVar(i));
   }
   DUMP << NL << FLUSH;
#endif
      return pph->getCompiledPredicate()->getCode();
   }

   else if (pph == NULL || pph->IsDynamic())
   {
      if (from_call)
         pEXEC->SetCP( CCLEN + cp );

      if (! g.explicit_modix)
      {                        // pop the current module on, as call needs it.
         if (imod == SYSTEM_MODULE)
            imod = USER_MODULE;       // caller is system, so default to user 
         X(0)->setModix(imod);
      }
#ifdef BUG_CALL
   DUMP << "  using interpreter" << NL << FLUSH;
#endif
      return getCompiledPredicate(SYSTEM_MODULE, interpreter, 1)->getCode();
   }

   else if (pph->IsEscape())
   {
      if (ar && ! g.explicit_modix)
         pHXL->CopyToX( t->getTerm()+1, ar );
      else if (ar && g.explicit_modix)
      {
         t2 = (t->getTerm()+2)->dref();
         pHXL->CopyToX( t2->getTerm()+1, ar );
      }

      try
      {
         result = pph->getEscapePredicate()->execute();
      } 
      catch (LExcept &pE)
      {
         throw(pE);
      }

      if (result == TRUE)
      {
         if (from_call)
            return(CCLEN + cp);
         else   // fake out a proceed
            return(pEXEC->GetCP());
      }
      else if (result == FALSE)
         return(pEXEC->failure());
   }
   else
      pXCPT->Error(internalE, aS("corrupt PredicateHead in DoCall"));

   return NULL;                                // a dummy
}

//      call the interpreter - called from Prove (Ocall or Oexe).
//
//      called if we can't get compC clause for pred/ar.
//      cp contains the code sequence present at the Ocall/Oexec
//
//        i.e. Functor/Arity  (ATOM / CODE)

CODEptr DDB::CallInterp(CODEptr cp, TF from_call)
{

   TERM       t, tx, t2;
   PATOM      interpreter;
   ARITY      ar;
   PATOM      func;
   uintCH     i;
   int        CCLEN;
   MODIX      imod;

   CCLEN = cdMODIX_L + 2 * cdATOM_L + cdSMINT_L + cdSINT_L;

   interpreter = (pSTATE->m_trace) ?       // tracing ? 
         pATAB->call_d_dollarA :           // use debugger (call$d) 
         pATAB->call_dollarA;              // use call$i 

   // we will be calling call$x(TERM) where we have to construct
   // TERM from cp and arity Xi registers

   ar = (ARITY) *(cdSMINTptr)(cp + cdMODIX_L + 2 * cdATOM_L);
   imod = (MODIX)*(cdMODIXptr)(cp);
   func = *(cdATOMptr)(cp + cdMODIX_L);

   tx = pHXL->heapGET();
   tx->setFA(pATAB->colonA, 2);
   (pHXL->heapGET())->setInt(imod);
   t2 = pHXL->heapGET();
   if (ar) 
   {                                       // a structure 
      t = pHXL->heapGET();
      t->setFA(func, ar);
      t2->setStruct(t);
      pEXEC->SetCurFA(func, ar);
      for(i = 0; i < ar; ++i)
         *(pHXL->heapGET()) = *(pHXL->XVar(i));
   }
   else
   {
      t2->setAtom(func);
   }
   (pHXL->XVar(0))->setStruct(tx);


#ifdef BUG_INTERP
   DUMP << "Interpreter: " << interpreter;
   DUMP << "  predicate: " << MODPREDAR(imod,func,ar) << NL;
   DUMP << "  term: ";
   pTSVC->Dump(X(0));
   DUMP << NL;
#endif

   if (from_call)
      pEXEC->SetCP( CCLEN + cp );
   // change the call() to a call$i() or call$d()

   //return(m_atomtable[interpreter].pinfo->pclause->code.pbuf);
   return getCompiledPredicate(SYSTEM_MODULE, interpreter, 1)->getCode();
//   
}

void DDB::UnloadFile(int fileix)
{
   for (int i=0; i < nmods; i++)
      modules[i]->UnloadFile(fileix);
}

//---------------------
// predicates
//

//--------------------
// Module predicates
//

// For now we're assuming module definitions are not nested.
TF DDB::p_moduleZ()
{  // The module directive module$(+atom)
   TERM tmod = X(0);
   if (tmod->IsVar())
      pXCPT->Error(instanceE, aS("arg1 must be instantiated"));
   if (! tmod->IsAtom())
      pXCPT->Error(typeE, aS("arg1 must be an atom"));

//   MODIX imod = NewModule( *(tmod->getAtom()) );
   MODIX imod = NewModule( tmod->getAtom() );
//   MODIX imod = NewModule( tmod->getAtom()->get_display_name(m_peng) );
#ifdef BUG_MODULES
 FILL("p_moduleZ: setting current_imod to: " << imod);
#endif
   current_imod = imod;
   return TRUE;
}

// Assume for now module definitions not nested.
TF DDB::p_end_moduleZ()
{                                // The end_module directive end_module$(+atom)
#ifdef BUG_MODULES
 FILL("p_end_moduleZ: setting current_imod to: " << USER_MODULE);
 modules[current_imod]->Dump();
#endif
   current_imod = USER_MODULE;
   return TRUE;
}

TF DDB::p_remove_module()
{
   TERM tmod = X(0);
   if (tmod->IsVar())
      pXCPT->Error(instanceE, aS("arg1 must be instantiated"));
   if (! tmod->IsAtom())
      pXCPT->Error(typeE, aS("arg1 must be an atom"));

   return FALSE;
}

TF DDB::p_import_1(void)
{
   TERM tmod = X(0);
   if (tmod->IsVar())
      pXCPT->Error(instanceE, aS("arg1 must be instantiated"));
   if (! tmod->IsAtom())
      pXCPT->Error(typeE, aS("arg1 must be an atom"));

   AddImportModule((tmod->getAtom()));
   return TRUE;
}

// used by ARulesXL to dynamically import a module to a rule set module
TF DDB::p_importZmod(void)
{
   TERM imod = X(0);
   TERM tmod = X(1);
   if (tmod->IsVar())
      pXCPT->Error(instanceE, aS("arg1 must be instantiated"));
   if (! tmod->IsAtom())
      pXCPT->Error(typeE, aS("arg1 must be an atom"));

   MODIX modix = getModuleIX((imod->getAtom()));
   if (modix == UNDEFINED_MODULE)
      modix = NewModule( imod->getAtom() );
//      modix = NewModule( *(imod->getAtom()) );

   AddImportModule(modix, (tmod->getAtom()));
   return TRUE;
}

TF DDB::p_importZ2(void)
{
   TERM tmod = X(0);
   TERM tpred = X(1);
   TERM tarity = X(2);

   if (tmod->IsVar())
      pXCPT->Error(instanceE, aS("arg1 must be instantiated"));
   if (! tmod->IsAtom())
      pXCPT->Error(typeE, aS("arg1 must be an atom"));

   if (tpred->IsVar())
      pXCPT->Error(instanceE, aS("arg2 must be instantiated"));
   if (! tpred->IsAtom())
      pXCPT->Error(typeE, aS("arg2 must be an atom"));

   if (tarity->IsVar())
      pXCPT->Error(instanceE, aS("arg3 must be instantiated"));
   if (! tarity->IsInt())
      pXCPT->Error(typeE, aS("arg3 must be an integer"));

   MODIX modix = getModuleIX((tmod->getAtom()));
   if (modix == UNDEFINED_MODULE)
      modix = NewModule(tmod->getAtom());
//      modix = NewModule( *(tmod->getAtom()) );

   AddImportPredicate(
         modix,
         tpred->getAtom(),
         (ARITY)tarity->getInt() );
   return TRUE;
}

TF DDB::p_exportZ(void)
{
   TERM tpred = X(0);
   TERM tarity = X(1);

   if (tpred->IsVar())
      pXCPT->Error(instanceE, aS("arg2 must be instantiated"));
   if (! tpred->IsAtom())
      pXCPT->Error(typeE, aS("arg2 must be an atom"));

   if (tarity->IsVar())
      pXCPT->Error(instanceE, aS("arg3 must be instantiated"));
   if (! tarity->IsInt())
      pXCPT->Error(typeE, aS("arg3 must be an integer"));

   MarkExportPredicate(tpred->getAtom(), (ARITY)tarity->getInt());

   return TRUE;
}

TF DDB::p_setZmeta(void)
{
   TERM tpred = X(0);
   TERM tarity = X(1);

   if (tpred->IsVar())
      pXCPT->Error(instanceE, aS("arg2 must be instantiated"));
   if (! tpred->IsAtom())
      pXCPT->Error(typeE, aS("arg2 must be an atom"));

   if (tarity->IsVar())
      pXCPT->Error(instanceE, aS("arg3 must be instantiated"));
   if (! tarity->IsInt())
      pXCPT->Error(typeE, aS("arg3 must be an integer"));

   MarkMetaPredicate(tpred->getAtom(), (ARITY)tarity->getInt());

   return TRUE;
}

TF DDB::p_setZdiscontiguous()
// A latent expression, set by compiler to let compiler flag
// a discontiguous predicate
{
   TERM tpred = X(0);
   TERM tarity = X(1);

   if (tpred->IsVar())
      pXCPT->Error(instanceE, aS("arg2 must be instantiated"));
   if (! tpred->IsAtom())
      pXCPT->Error(typeE, aS("arg2 must be an atom"));

   if (tarity->IsVar())
      pXCPT->Error(instanceE, aS("arg3 must be instantiated"));
   if (! tarity->IsInt())
      pXCPT->Error(typeE, aS("arg3 must be an integer"));

   MarkDisconPredicate(tpred->getAtom(), (ARITY)tarity->getInt());

   return TRUE;
}

TF DDB::p_setZsorted()
// A latent expression, indicating a sorted predicate
{
   TERM tpred = X(0);
   TERM tarity = X(1);

   if (tpred->IsVar())
      pXCPT->Error(instanceE, aS("arg2 must be instantiated"));
   if (! tpred->IsAtom())
      pXCPT->Error(typeE, aS("arg2 must be an atom"));

   if (tarity->IsVar())
      pXCPT->Error(instanceE, aS("arg3 must be instantiated"));
   if (! tarity->IsInt())
      pXCPT->Error(typeE, aS("arg3 must be an integer"));

   MarkSortedPredicate(tpred->getAtom(), (ARITY)tarity->getInt());

   return TRUE;
}

TF DDB::p_setZindexed()
// A latent expression, indicating an indexed predicate
// the argument is a map functor(1,0,1,0) where 1s are
// indexed arguments.  Because it is being read, it
// applies to the current module.
{
   modules[current_imod]->AddIndexedPredicate(X(0));
   return TRUE;
}

// Return the loading module, used by the compiler
TF DDB::p_loading_module(void)
{
   Cell c;
   c.setAtom(getModuleNameA(current_imod));
   return pTSVC->UnifyConst(X(0), &c);
}

TF DDB::p_moduleZindex(void)
{
   TERM nameT;
   MODIX imod;
   Cell c;

   nameT = X(0);

   if (nameT->IsVar())
   {
      imod = (MODIX)X(1)->getInt();
      if (validModix(imod))
      {
         c.setAtom(getModuleNameA(imod));
         return pTSVC->UnifyConst(X(0), &c);
      }
   }

   return FALSE;
}

//------------------------------
// Dynamic DB predicates
//

TF DDB::p_assertZ()
{
   TERM  mod;
   MODIX imod;

   mod = X(0);

   if (mod->IsVar())
      imod = getCurrentMIX();
   else 
      imod = (MODIX)(mod->IsInt() ? mod->getInt() : getModuleIX((mod->getAtom())));

   if (imod == UNDEFINED_MODULE)
      return FALSE;

   Assert(imod, X(1)->getInt(), X(2));
   return TRUE;
}

TF DDB::p_debug_data()
{
  unsigned int   hsize, ssize, lsize;
  int            err;  
  
  hsize = (unsigned int) (pHXL->heapUSE() >> 2);
  
  ssize = (long *) pCNTL->ETop() > (long *) pCNTL->BTop() ?
	 (unsigned int)( (long)(pCNTL->ETop()) - (long)(pCNTL->GetStack()) ) >> 2 :
	 (unsigned int)( (long)(pCNTL->BTop()) - (long)(pCNTL->GetStack()) ) >> 2;
  
  lsize = (unsigned int) (pHXL->localUSE() >> 2);
  
  if (TRUE != (err = pTSVC->UnifyInt(hsize, pHXL->XVar(0))))
    return(err);
  if (TRUE != (err = pTSVC->UnifyInt(ssize, pHXL->XVar(1))))
    return(err);
  if (TRUE != (err = pTSVC->UnifyInt(lsize, pHXL->XVar(2))))
    return(err);
  if (TRUE != (err = pTSVC->UnifyInt(pCNTL->TRTop(), pHXL->XVar(3))))
    return(err);
  
  return(TRUE);
}

// P is predicate in form MOD:NAME/ARITY
TF DDB::p_abolish()
{   // abolish$(M:F/A) - removes a dynamic predicate
   TERM   t = X(0);
   PATOM  pred;
   ARITY  ar;
   MODIX  imod;

   // get the goal components
   Goal g(m_peng, t);
   pred = g.functor;
   ar = g.arity;
   imod = g.imod;

#ifdef BUG_ASSRET
if (MODTEST(g.imod))
  DUMP << "Abolishing: " << MODPREDAR(g.imod, g.functor, g.arity) << NL << FLUSH;
#endif

   if (! g.explicit_modix)
      pXCPT->Error(instanceE, aS("arg must be module qualified"));
   if (imod < 0)
      pXCPT->Error(instanceE, aS("module specifier must be bound to existing module"));
   modules[imod]->Abolish(pred, ar);

   return TRUE;
}

// succeeds if compiled or escape or extended predicate.
// used by interpreter and debugger to see if a pred is built-in.
// second argument used to flag
// metapredicates, for the interpreter's use.
TF DDB::p_iscode()
{                                              // is$code(Mod, Goal)
   PATOM          pred;
   ARITY          ar;
   TERM           t;
   PredicateHead *pph;
   MODIX          imod;

   t = X(0);

   LASSERT((t->IsInt() || t->IsAtom()), aS("p_iscode - bad module spec"));
	imod = (MODIX)(t->IsInt() ? t->getInt() : pDDB->getModuleIX((t->getAtom())));

   if (imod == UNDEFINED_MODULE)
      return FALSE;

   t = X(1);
   if (t->IsStruct())
   {
      t = t->getTerm();
      LASSERT(t->IsAtom(), aS("p_iscode, bad predicate"));
      pred = t->getAtom();
      ar = t->getArity();
   }
   else if (t->IsAtom())
   {
      pred = t->getAtom();
      ar = 0;
   }
   else
   {
      aCHAR tstr[256];
      pWRIT->termWriteString(t, tstr, 255, true);
      pXCPT->Error(badgoalE, tstr);
   }

   if (NULL == (pph = getVisiblePredicate(imod, pred, ar)))
      return(FALSE);

   if (pph->IsCompiled() || pph->IsEscape() || pph->IsExtended() )
      return(TRUE);

   return FALSE;
}

// succeeds if compiled or escape or extended predicate.
// used by interpreter and debugger to see if a pred is built-in.
// second argument used to flag
// metapredicates, for the interpreter's use.
TF DDB::p_isZcode()
{  // is$code(Mod, Goal, CodeFlag, MetaFlag)
   PATOM          pred;
   ARITY          ar;
   TERM           t;
   PredicateHead *pph;
   MODIX          imod;
   Cell           c;

   t = X(0);

   LASSERT((t->IsInt() || t->IsAtom()), aS("p_iscode - bad module spec"));
	imod = (MODIX)(t->IsInt() ? t->getInt() : pDDB->getModuleIX((t->getAtom())));

   if (imod == UNDEFINED_MODULE)
      return FALSE;

   t = X(1);
   if (t->IsStruct())
   {
      t = t->getTerm();
      LASSERT(t->IsAtom(), aS("p_iscode, bad predicate"));
      pred = t->getAtom();
      ar = t->getArity();
   }
   else if (t->IsAtom())
   {
      pred = t->getAtom();
      ar = 0;
   }
   else
   {
      aCHAR tstr[256];
      pWRIT->termWriteString(t, tstr, 255, true);
      pXCPT->Error(badgoalE, tstr);
   }

   if (NULL == (pph = getVisiblePredicate(imod, pred, ar)))
   {
	   if(pSTATE->m_undefined_predicate == LERROR)
	   {
			if (imod != SYSTEM_MODULE)
			{
				PATOM modA = modules[imod]->getModuleNameA();
				if (modA != pATAB->amzi_listenerA &&
					modA != pATAB->amzi_compilerA &&
					modA != pATAB->amzi_debuggerA)
					pXCPT->Error(undefinedE, (aCHAR*)pred->get_display_name(m_peng), ar);
			}
		}
		return(FALSE);
   }

   if ( pph->IsCompiled() || pph->IsEscape() || pph->IsExtended() )
   {
      if (! pTSVC->UnifyInt(1, X(2)))
         return FALSE;
   }
   else
   {
      if (! pTSVC->UnifyInt(0, X(2)))
         return FALSE;
   }

   if (pph->IsMeta())
   {
      c.setAtom(getModuleNameA(pph->getModuleIX()));
      return pTSVC->UnifyConst(X(3), &c);
   }
   else
      return pTSVC->UnifyInt(0, X(3));

   return(TRUE);
}

TF DDB::p_definedZ()
{                          // defined(TERM) TRUE if term is defined in any way.
   TERM t;

   t = X(0);

   if (t->IsVar())
      pXCPT->Error(instanceE, aS("arg must be atom"));

   Goal g(m_peng, t);

   if (! g.explicit_modix)
      pXCPT->Error(instanceE, aS("arg 1 must be module qualified"));

   return modules[g.imod]->IsDefined(g.functor, g.arity);
}

// This is a backtracking predicate, meaning it
// must be called in a repeat, call loop.  
// The FCut() jumps back over the repeat.
TF DDB::p_currentZmodule(void)
{    // current$module(M) - returns all module names as atoms on backtracking.
   TERM x0, hmark;
   MODIX imod;
   Cell c;

   x0 = X(0);
   if (x0->IsVar())
   {
      // if first call, ie not backtracking, then
      // get a heap cell to use to store the current
      // module index, for use in following passes.
      if ( !(pCNTL->BTop()->flags & BF) )
      {
         hmark = pHXL->heapGET();
         pCNTL->BTop()->HBc = pHXL->HTop();
         hmark->setInt(0);
         imod = 0;
      }
      // if backtracking, get the module index from
      // the heap, where we left it last time.
      else
      {
         hmark = pCNTL->BTop()->HBc - 1;
         imod = (MODIX)(1 + hmark->getInt());
      }

      if (imod >= nmods)
      {                            // we're done, so FCut past repeat and fail
         pCNTL->FCut();
         return FALSE;
      }

      c.setAtom(modules[imod]->getModuleNameA());
      pTSVC->UnifyConst(x0, &c);
      hmark->setInt(imod);
      return TRUE;
   }
   else if (x0->IsAtom())
   {
      if ( pCNTL->BTop()->flags & BF )
      {                         // backtracking in this case, so just fail
         pCNTL->FCut();
         return FALSE;
      }
      for (imod=0; imod < nmods; imod++)
         if ( x0->getAtom() == modules[imod]->getModuleNameA() )
            return TRUE;
   }
   else
   {
      pXCPT->Error(typeE, aS("arg1 must be atomic module name"));
   }
   pCNTL->FCut();
   return FALSE;
}

// PTR - var on first call, returned value of PredIter for next
// MODULE - input module name
// NAME - output name of first predicate
// ARITY - output arity of first predicate
//
// Note this is called from alib in implementing current_predicate
// in a recursive repeat like loop.
TF DDB::p_getZpred(void)
{                                       // get$pred(PTR, MODULE, NAME, ARITY)
   TERM modT, ppiT;
   MODIX imod;
   PredIter *ppi = NULL;
   LPredicate lp;
   Cell c;
   TF tf;

   modT = X(1);
   if (modT->IsAtom())
      imod = getModuleIX((modT->getAtom()));
   else if (modT->IsVar())
      pXCPT->Error(instanceE, aS("arg2 must be a module name"));
   else
      pXCPT->Error(typeE, aS("arg2 must be an atom module name"));

   // just fail if a bad mod name
   if (! validModix(imod))
      return FALSE;

   ppiT = X(0);
   if (ppiT->IsVar())  // first call
   {
      LNEW(ppi, PredIter, aS("temporary"));   // deleted below
      c.setPtr(ppi);
      pTSVC->UnifyConst(X(0), &c);
      lp = modules[imod]->get_first_pred(ppi);
   }
   else
   {
      ppi = (PredIter*) (ppiT->getPtr());
      lp = modules[imod]->get_next_pred(ppi);
   }

   while (! lp.IsNull() )
   {
      (*ppi)++;
      c.setAtom(lp.functor);
      tf = pTSVC->UnifyConst(X(2), &c);
      if (tf)
      {
         c.setInt(lp.arity);
         tf = pTSVC->UnifyConst(X(3), &c);
      }

      if (tf)
         return TRUE;
      else
         lp = modules[imod]->get_next_pred(ppi);
   }

   delete ppi;
   return FALSE;
}


TF DDB::p_predicateZproperty(void)
{
   TERM goalT, propT;
   TERM listarg, t;
   Cell c;
   MODIX imod;
   bool import_module = false;

   goalT = X(0);

   Goal g(m_peng, goalT);
   imod = g.imod;

   PredicateHead *ph = modules[imod]->
         getPredicateHead(g.functor, g.arity);

   // it might be imported from a module, rather than
   // being explicitly imported.
   if (ph == NULL)
   {
      ph = modules[imod]->getVisiblePredicate(g.functor, g.arity);
      if (ph == NULL)
         return FALSE;
      import_module = true;
      imod = ph->getModuleIX();
   }

   propT = X(1);
//   if (propT->IsVar())
//   {
      // create a list of properties
   t = listarg = pHXL->heapGET();
   t->setList(t+1);

   while (ph->IsImported())
   {
      t = pHXL->heapGETN(4);
      t->setStruct(t+2);
      (t+1)->setList(t+4);
      (t+2)->setFA(pATAB->imported_fromA, 1);
      imod = ph->getImportPredicate()->getModix();
      (t+3)->setAtom( pDDB->getModuleNameA(imod) );
      ph = modules[imod]->getPredicateHead(g.functor, g.arity);
   }

   // was imported, but from :- import(module), not
   // specified directly.
   if (import_module)
   {
      t = pHXL->heapGETN(4);
      t->setStruct(t+2);
      (t+1)->setList(t+4);
      (t+2)->setFA(pATAB->imported_fromA, 1);
      (t+3)->setAtom( pDDB->getModuleNameA(imod) );
   }

   t = pHXL->heapGETN(4);
   t->setStruct(t+2);
   (t+1)->setList(t+4);
   (t+2)->setFA(pATAB->defined_inA, 1);
   (t+3)->setAtom(getModuleNameA(imod));

   if (ph->IsCompiled())
   {
      t = pHXL->heapGETN(2);
      t->setAtom(pATAB->staticA);
      (t+1)->setList(t+2);
   }
   if (ph->IsDynamic())
   {
      t = pHXL->heapGETN(2);
      t->setAtom(pATAB->dynamicA);
      (t+1)->setList(t+2);
   }
   if (ph->IsBuiltIn())
   {
      t = pHXL->heapGETN(2);
      t->setAtom(pATAB->built_inA);
      (t+1)->setList(t+2);
   }
   if (ph->IsExported())
   {
      t = pHXL->heapGETN(2);
      t->setAtom(pATAB->exportedA);
      (t+1)->setList(t+2);
   }
   if (ph->IsMeta())
   {
      // the caller in alib will fill in mode indicator
      t = pHXL->heapGETN(4);
      t->setStruct(t+2);
      (t+1)->setList(t+4);
      (t+2)->setFA(pATAB->metapredicateA, 1);
      (t+3)->setUnbound();
   }
   if (ph->IsExtended())
   {
      t = pHXL->heapGETN(2);
      t->setAtom(pATAB->extendedA);
      (t+1)->setList(t+2);
   }
   (t+1)->setAtom(pATAB->nilA);
   return pTSVC->Unify(X(1), listarg);
//   }
}

// M:F/A
// ARGS_INFO_STRING
// DESC_INFO_STRING
TF DDB::p_predicateZenginfo(void)
{
   TERM goalT;
   MODIX imod;
   Cell  args, desc;

   goalT = X(0);

   Goal g(m_peng, goalT);
   imod = g.imod;

   PredicateHead *ph = modules[imod]->
         getPredicateHead(g.functor, g.arity);

   // it might be imported from a module, rather than
   // being explicitly imported.
   if (ph == NULL)
   {
      ph = modules[imod]->getVisiblePredicate(g.functor, g.arity);
      if (ph == NULL)
         return FALSE;
   }

   if (!ph->IsBuiltIn())
      return FALSE;

   args.setStr( pGCTH->make_string(ph->getArgsStr()) );
   desc.setStr( pGCTH->make_string(ph->getDescStr()) );
   if (pTSVC->Unify(&args,X(1)))
      return(pTSVC->Unify(&desc,X(2)));
   else
      return FALSE;
}

TF DDB::p_isZmeta(void)
{                                 // is$meta(MOD, GOAL, DEFINING_MOD)
   TERM  modT;
   MODIX imod;
   Cell  c;

   modT = X(0);
   if (modT->IsAtom())
      imod = getModuleIX( (modT->getAtom()) );
   else if (modT->IsInt())
      imod = (MODIX)modT->getInt();
   else
      return FALSE;

   if (imod == UNDEFINED_MODULE)
      return FALSE;

   Goal g(m_peng, X(1));

   PredicateHead *pph = 
	  modules[imod]->getVisiblePredicate(g.functor, g.arity);
   if (pph == NULL)
      return FALSE;

   if (pph->IsMeta())
   {
      c.setAtom(getModuleNameA(pph->getModuleIX()));
      return pTSVC->UnifyConst(X(2), &c);
   }
   else
      return FALSE;
}

// Gets a clause whose head unifies with HEAD, and
// returns BODY.  On backtracking, gets next one.
//
// Note that this is used by calls to the dynamic db,
// via rb$ which uses clause.
//
// Note that it uses FCut to get out of backtracking loop,
// so this MUST be called with a repeat first.  Like,
// ..., repeat, clause$db(...), ...  The FCut causes control
// to skip over the repeat.
//
// MOD - the module name (optional)
// HEAD - the head in form pred(arg1, arg2, ...)
// BODY - the body of the clause, 'true' if no body
// N - the clause number, used by the debugger - note
//   this currently only goes to 256, as its a byte

//#define NEW_COPY_DDB

TF DDB::p_clauseZdb()
{ // clause$db(MOD, HEAD, BODY, N)
   TERM            mod, head, body;
   //TERM            dbhead, dbbody;
   //TERM            h, dbt;
   //TRAIL_INDEX     ti;

   PATOM           pred;
   ARITY           ar;
   MODIX           imod;

   PredicateHead  *pph;
   DynamicClause  *pdc;
   DynamicClauseIterator *pdci;

   int             clause_number;
   bool            first_time;
   bool            heap_copy = false;

   Cell            c;

#ifdef xBUG_CLAUSE
 FILL(" heap top and BTop->HBc ");
 pHXL->DumpCellName(pHXL->HTop());
 pHXL->DumpCellName(pCNTL->BTop()->HBc);
 FILL(" &?&?&? ");
#endif
   head = X(1);
   body = X(2);

#ifdef BUG_CLAUSE
 DynamicPredicate *pdp;
 PredicateHead *xpph;
 PATOM xpred;
 ARITY xar;
 MODIX ximod;
#endif
#ifdef BUG_CLAUSE
// aCHAR buf[256];
// pTSVC->termWriteString(head, buf, 255, true);
 DUMP << NL << NL << "==>p_clauseZdb() " << NL;
 DUMP << "Head of query clause: " << NL;
 pTSVC->DumpWrite(head);
 DUMP << NL;
 pTSVC->Dump(head);
 DUMP << NL;
#endif
//#ifdef BUG_NEW_COPY
// bool b_unifycopy;
//#endif

   if (!(pCNTL->BTop()->flags & BF))
   {// not backtracking, so init a database entry for normal backtracking search.
      first_time = true;

      mod = X(0);

      if (mod->IsVar())
         pXCPT->Error(instanceE, aS("arg 1 must be module"));
		imod = (MODIX)(mod->IsAtom() ? getModuleIX((mod->getAtom())) : mod->getInt());

      if (imod == UNDEFINED_MODULE)
      {
         pCNTL->FCut();
         return FALSE;
      }

      if (head->IsStruct())
      {
         pred = head->getTerm()->getAtom();
         ar = head->getTerm()->getArity();
      }
      else if (head->IsAtom())
      {
         pred = head->getAtom();
         ar = 0;
      }
      else
         pXCPT->Error(instanceE, aS("head must be atom or structure"));

      // find the head of the predicate, which will
      // lead us to the dynamic clauses if they exist.
      pph = getVisiblePredicate(imod, pred, ar);
      if (pph == NULL || !pph->IsDynamic())
      {
#ifdef BUG_CLAUSE
 DUMP << "  no such predicate" << NL;
#endif
 // don't believe we'll ever get here anymore since is$code is called before clause$db
 // and it fails if there is no predicate.  but best to keep it in case that logic changes.
         pCNTL->FCut();  // skip back over 'repeat'
         return(FALSE);  // and fail
      }

      clause_number = 0;  // for the debugger
      //pdc = pph->getDynamicPredicate()->getFirst();
      pdci = pph->getDynamicPredicate()->getIterator(head);
      if (pdci == NULL)
      {
         pCNTL->FCut();  // skip back over 'repeat'
#ifdef BUG_CLAUSE
 DUMP << "<==p_clauseZdb() no clauses " << NL;
#endif
         return(FALSE);  // and fail
      }
#ifdef BUG_CLAUSE
 DUMP << "  first clause " << MODPREDAR(imod,pred,ar) << " pdc = " << pdci->getClause() << NL;
#endif
   }
   // if backtracking, then get the last db pointer from
   // the heap where it was put last time through.  this
   // was the pdc used for calls, but the next in line
   // for retracts.
   else
   {
      first_time = false;

      //mscw = pCNTL->BTop()->HBc - 1;
      //clause_number = mscw->getMSCWClauseNumber();
      //pdci = mscw->getMSCWDBRef();
      //imod = mscw->getModix();

      //pdci = pCNTL->BTop()->pdci;
      pdci = pCNTL->BTop()->gc_pdci->pdci;
      clause_number = pCNTL->BTop()->clause_number;
      imod = pCNTL->BTop()->imod;

      //if (! pdci->isDone())
      //{
//#ifdef BUG_CLAUSE
// DUMP << "clearing previous for: " << pdci->getPrev() << NL << FLUSH;
//#endif
     //    pdci->getClause()->clearNextMSCW(mscw);
     //    if (! pdci->isFirst()) pdci->getPrev()->clearInUseMSCW(mscw);
         //clause_number++;
     // }
#ifdef BUG_CLAUSE
 DUMP << "  next clause # " << clause_number << " pdc = ";
 if (pdci->isDone())
 {
   DUMP << "is done" << NL;
 }
 else
 {
   DUMP << pdci->getClause() << NL;
 }
#endif
   }

#ifdef BUG_CLAUSE
// FILL(NL << "BEFORE");
 if (pdci->getClause())
 {
  pdp = pdci->getDynamicPredicate();
  xpph = pdp->getPredicateHead();
  xpred = xpph->getName();
  xar = xpph->getArity();
  ximod = imod;
  FILL("p_clauseZdb starting loop for: " << MODPREDAR(ximod,xpred,xar));
//  predicate_dump(pdp);
//  mscw_heap_walk();
 }
 else
  FILL("pdc is NULL");
// FILL(NL << "end BEFORE" << NL);
#endif

   //while ((pdc = pdci->getClause()) != NULL)
   while (! pdci->isDone())
   {
// the following line used to tests 'hits' to see if indexing works
//if (imod == USER_MODULE) std::cout << "!";
      // look for a clause that unifies with the target
      // if there are variables, we unfortunately have to
      // copy the term to the heap because it might be a
      // recursive clause and we need fresh variables each time.
      // Also, if its the last clause, we're going to trim
      // the choice point and we won't know it needs to be protected
      // from retracts.
      pdc = pdci->getClause();
      if (pdc->isDeleted())
      {
         pdci->setNext();
         continue;
      }

//#ifdef COPY_DDB_TERMS
      // Here's the whole problem.  If we don't copy a term to the heap
      // before unifying, then we have variables on the heap unified
      // with non-heap terms in the DDB.  Clearly it's quicker if we
      // can do that, and we'd like to be able to avoid the copy whenever
      // possible.  But I just don't see when we can do that.
      //
      // If the variable in the query term unifies with a structure
      // in the DDB, then it will have pointers in the DDB.  If the
      // clause is then deleted, the pointers will be garbage.
      //
      // remove_ddb checks to see if the clause is in use on the heap,
      // and that works well, except that if a cut was issued, then
      // the control stack no longer has the backtracking points.
      //
      // if there are variables in the clause, then it always needs to
      // copied to the heap first, so there really isn't much we can to
      // in terms of optimizing not copying to the heap, so I think
      // we just need to copy them all and be done with it.
      //
      // Alan used to avoid the copies for non-var clauses, but then
      // he never deleted them either, and didn't have a working GC
      // for the dynamic db.
//      if (true)
//#else
//      if (pdc->getNumVars() > 0 || pdci->isLast())
//#endif
//      {


//#ifdef NEW_COPY_DDB
#ifdef BUG_NEW_COPY
 DUMP << NL << NL;
 FILL("Unify Copy head: ");
 pTSVC->DumpWrite(head);
 DUMP << NL;
 FILL("Unify Copy body: ");
 pTSVC->DumpWrite(body);
 DUMP << NL;
 FILL("DDB clause: ");
 pTSVC->DumpWrite(pdc->getCode());
 DUMP << NL;
#endif
      clause_number++;
      if ( pTSVC->UnifyCopy(head, body, pdc) )
         break;
     //    b_unifycopy = true;
     // else
     //    b_unifycopy = false;
      // unwind any variables that were bound during
      // the unification attempt
      //pHXL->SetHTop(pCNTL->BTop()->HBc);
      //ti = pCNTL->BTop()->TRc;
      //pCNTL->Unwind(ti);

/*
#else
         dbt = pHXL->heapGETN(pdc->getSize());   
         h = dbt+1;
         pTSVC->Copy(dbt, h, &h, pdc->getCode(), GC_NO);
         heap_copy = true;
//      }
//      else
//      {
//         dbt = pdc->getCode();
//         heap_copy = false;
//      }

#ifdef xBUG_CLAUSE
 pTSVC->termWriteString(dbt, buf, 255, true);
 FILL("p_clauseZdb looking at clause " << clause_number << "  " << buf);
 pTSVC->termWriteString(head, buf, 255, true);
 FILL("  to match: " << buf << " :- ");
 pTSVC->termWriteString(body, buf, 255, true);
 FILL("      " << buf);
#endif

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

      clause_number++;
      if (pTSVC->Unify(dbhead, head) && pTSVC->Unify(dbbody, body))
      {
         //LASSERT(b_unifycopy, aS("unify false when should be true"));
         break;
      }
      //LASSERT(! b_unifycopy, aS("unify copy true when should be false"));

      // unwind any variables that were bound during
      // the unification attempt
      pHXL->SetHTop(pCNTL->BTop()->HBc);
      ti = pCNTL->BTop()->TRc;
      pCNTL->Unwind(ti);

#endif
*/
      //pdc = pdc->next();
      pdci->setNext();
   }
   //if (pdc == NULL)
   if (pdci->isDone())
   {           // trim the trail and cut past the preceding 'repeat' and fail.

#ifdef BUG_CLAUSE
 DUMP << "  no match found" << NL;
 DUMP << "<==p_clauseZdb()" << NL;
#endif
      pCNTL->TrimTrail();
      pCNTL->FCut();
      //if (mscw)
      //   mscw->setUnused();
      if (first_time)  // haven't made into a gc object yet
         delete pdci;
      return FALSE;
   }

#ifdef BUG_CLAUSE
 DUMP << "  match found" << NL;
#endif

   MODIX nextmod = 
	  pdci->getClause()->getDynamicPredicate()->getPredicateHead()->getModuleIX();
   c.setAtom(pDDB->getModuleNameA(nextmod));

   pTSVC->UnifyConst(X(4), &c);

   pTSVC->UnifyInt(clause_number, pHXL->XVar(3));

//    Took out this optimization because it meant the last
//    clause found would not be represented by a mscw, and
//    the mscw is used on retract to determine if its OK
//    to delete the clause.
//    
//    Put it back in because it didn't work without it.  Besides,
//    its a fairly common case of a singleton fact which might
//    get asserted and retracted a lot. Instead, put in fix
//    of copying last clause to heap always.

   // if this was the last one, trim the choicepoint
   //if (NULL == pdc->next())
   if (pdci->isLast())
   {
      pCNTL->TrimTrail();
      pCNTL->FCut();
      pdci->setNext();
      //if (mscw)
      //   mscw->setUnused();
      if (first_time)   // didn't create a gc object yet
         delete pdci;
   }
   // if there's more to come, create an mscw cell on
   // the heap if this is the first time, otherwise
   // update the old one.

   else // not the last one
   {
      if (first_time)
      {
         //mscw = pHXL->heapGET();
         //pCNTL->BTop()->HBc = pHXL->HTop();
         //mscw->setMSCW(pGCTH->make_dbiter(pdci), imod, clause_number);
         //if (!heap_copy)
         //   mscw->setInUseMSCW();
         //pCNTL->BTop()->pdci = pdci;
         pCNTL->BTop()->gc_pdci = pGCTH->make_dbiter(pdci);
         pCNTL->BTop()->clause_number = clause_number;
         pCNTL->BTop()->imod = imod;
      }
      //if (!heap_copy)
      //   pdci->getClause()->setInUseMSCW(mscw);

      pdci->setNext();  // set for next, not this
      //pdci->getClause()->setNextMSCW(mscw);
      //mscw->setMSCWClauseNumber(clause_number);
#ifdef BUG_CLAUSE
 DUMP << "more to come " << " with pdc " << pdc << NL;
#endif

   }

#ifdef BUG_CLAUSE
 FILL("predicate status after found clause:");
 xpph = getPredicateHead(ximod, xpred, xar);
 if (xpph)
 {
  pdp = xpph->getDynamicPredicate();
  if (pdp) //predicate_dump(pdp);
   pdp->Dump();
//  mscw_heap_walk();
 }
 else
 {
  FILL("predicate head NULL");
 }
 DUMP << "<==p_clauseZdb()" << NL << NL << FLUSH;
#endif

 return TRUE;
}

//
// Gets a clause that unifies with TERM from the ddb
// based on the KEY.  On backtracking gets next clause
// that unifies with TERM.  Fails when no more.
//
// Note that this is used by calls to the dynamic db,
// via rb$ which uses clause, and by retract for retracting
// individual clauses.
//
// Note that it uses FCut to get out of backtracking loop,
// so this MUST be called with a repeat first.  Like,
// ..., repeat, get$db(...), ...  The FCut causes control
// to skip over the repeat.
//
// MOD - the module, or var if default
// HEAD - a goal of the form f(_,...)
// TERM - the term the clause is unified with.
TF DDB::p_retractZdb()
{                                            // retract$db(MOD, HEAD, TERM)
   TERM            mod, head, term;
   TERM            h, dbt;
   //TERM            mscw = NULL;
   TERM            heap_top;
   TRAIL_INDEX     ti;

   PATOM           pred;
   ARITY           ar;
   MODIX           imod;

   PredicateHead  *pph;
   DynamicClause  *pdc = NULL;
   DynamicClauseIterator *pdci;

   bool            first_time;

#ifdef xBUG_ASSRET
 DUMP << NL << NL << "==>p_retractZdb()" << NL;
 DynamicPredicate *pdp;
 PredicateHead *xpph;
 PATOM xpred;
 ARITY xar;
 MODIX ximod;
#endif

#ifdef xBUG_ASSRET
 FILL(" heap top and BTop->HBc ");
 pHXL->DumpCellName(pHXL->HTop());
 pHXL->DumpCellName(pCNTL->BTop()->HBc);
 FILL(" %#%#%# ");
#endif
   term = X(2);
#ifdef xBUG_ASSRET
 FILL(" heap top and BTop->HBc ");
 pHXL->DumpCellName(pHXL->HTop());
 pHXL->DumpCellName(pCNTL->BTop()->HBc);
 FILL(" %#%#%# ");
#endif

   if (!(pCNTL->BTop()->flags & BF))
   {// not backtracking, so init a database entry 4 normal backtracking search.
      first_time = true;
      mod = X(0);
      head = X(1);

      if (mod->IsVar())
         pXCPT->Error(instanceE, aS("arg 1 must be module"));
      else 
		  imod = (MODIX)(mod->IsAtom() ? getModuleIX((mod->getAtom())) : mod->getInt());

      if (imod == UNDEFINED_MODULE)
      {
         pCNTL->FCut();
         return FALSE;
      }

      if (head->IsStruct())
      {
         pred = head->getTerm()->getAtom();
         ar = head->getTerm()->getArity();
      }
      else
      {
         pred = head->getAtom();
         ar = 0;
      }

#ifdef xBUG_ASSRET
  if (MODTEST(imod))
   DUMP << NL << "DB Retract: " << MODPREDAR(imod, pred, ar) << NL;
#endif
      // find the head of the predicate, which will
      // lead us to the dynamic clauses if they exist.
      pph = getPredicateHead(imod, pred, ar);
      if (pph == NULL || !pph->IsDynamic())
      {
         pCNTL->FCut();  // skip back over 'repeat'
#ifdef xBUG_ASSRET
 DUMP << "<==p_retractZdb() not a dynamic predicate" << NL;
#endif
         return(FALSE);  // and fail
      }

      pdci = pph->getDynamicPredicate()->getIterator(head);      
      if (pdci == NULL)
      {
         pCNTL->FCut();  // skip back over 'repeat'
#ifdef xBUG_ASSRET
 DUMP << "<==p_retractZdb() no clauses" << NL;
#endif
         return(FALSE);  // and fail
      }
   }
   // if backtracking, then get the last db pointer from
   // the heap where it was put last time through.  this
   // was the next in line
   else
   {
      first_time = false;

      //mscw = pCNTL->BTop()->HBc - 1;
      //pdci = mscw->getMSCWDBRef();
      //imod = mscw->getModix();
      //pdci = pCNTL->BTop()->pdci;
      pdci = pCNTL->BTop()->gc_pdci->pdci;
      imod = pCNTL->BTop()->imod;

#ifdef xBUG_ASSRET
   DUMP << NL << "DB Retract backtracking, pdc = " << pdci->getClause() << NL;
#endif

      //if (pdc)
     // if (! pdci->isDone())
     //    pdci->getClause()->clearNextMSCW(mscw);
   }

#ifdef xBUG_ASSRET
 FILL(NL << "BEFORE");
 if (pdci->getClause())
 {
  pdp = pdci->getClause()->getDynamicPredicate();
  xpph = pdp->getPredicateHead();
  xpred = xpph->getName();
  xar = xpph->getArity();
  ximod = imod;
  //predicate_dump(pdp);
  if (MODTEST(ximod))
  {
    FILL(NL << "BEFORE");
    pdp->Dump();
  }
  //mscw_heap_walk();
 }
 else
  FILL("pdc is NULL");
 //FILL(NL << "end BEFORE" << NL);
#endif

   while (! pdci->isDone())
   {  // look for a clause that unifies with the target term
      // also, for retracts, must copy before deleting.
#ifdef xBUG_ASSRET
if (MODTEST(imod))
{
 FILL("  *** retract looking for this term: ***");
 pTSVC->Dump(term);
 FILL("          *** ");
}
#endif
      pdc = pdci->getClause();
      if (pdc->isDeleted())
      {
         pdci->setNext();
         continue;
      }
#ifdef xBUG_ASSRET
 FILL(" heap top and BTop->HBc ");
 pHXL->DumpCellName(pHXL->HTop());
 pHXL->DumpCellName(pCNTL->BTop()->HBc);
 FILL(" %#%#%# ");
#endif
      // a nasty bug - for some reason, not well understood,
      // HTop is higher than BTop()->HBc when retract is called
      // with a rule (:-) as the term to retract.  When the
      // the heap was reset to BTop()->HBc, in that case, the
      // term itself was smushed.  So a hack for now, save the
      // real heap_top and use that for the reset after failure.
      heap_top = pHXL->HTop();
      dbt = pHXL->heapGETN(pdc->getSize());   
      h = dbt+1;
#ifdef xBUG_ASSRET
 FILL("  *** still the term we're looking for is: ***");
 pTSVC->Dump(term);
 FILL("          *** ");
#endif
      // the following is a GC Thing event, decrement the counts for
      // gcthings in the clause to be deleted, no I don't think so,
      // we'll have to do the GC_DEC after we know we're really
      // getting rid of this one.
      //pTSVC->Copy(dbt, h, &h, pdc->getCode(), GC_DEC);
      pTSVC->Copy(dbt, h, &h, pdc->getCode(), GC_NO);

#ifdef xBUG_ASSRET
 if (MODTEST(ximod))
 {
 FILL("  *** retract unifying: ***");
 pTSVC->Dump(term);
 pTSVC->Dump(dbt);
 FILL("          *** ");
 }
#endif

      if (pTSVC->Unify(dbt, term))
         break;

      // unwind any variables that were bound during
      // the unification attempt

      // see comment above, this is hack to fix a not
      // well understood bug.
      pHXL->SetHTop(heap_top);
      //pHXL->SetHTop(pCNTL->BTop()->HBc);
      ti = pCNTL->BTop()->TRc;
#ifdef xBUG_ASSRET
 FILL("  *** retract failed to unify: ***");
 pTSVC->Dump(term);
 pTSVC->Dump(dbt);
 FILL("          *** ");
#endif
      pCNTL->Unwind(ti);
#ifdef xBUG_ASSRET
 FILL("  *** after unwinding: ***");
 pTSVC->Dump(term);
 pTSVC->Dump(dbt);
 FILL("          *** ");
#endif

      //pdc = pdc->next();
      pdci->setNext();
   }

   //if (pdc == NULL)
   if (pdci->isDone())
   { // none was found, so trim trail and cut past preceding 'repeat' and fail.

      pCNTL->TrimTrail();
      pCNTL->FCut();
#ifdef xBUG_ASSRET
 DUMP << "<==p_retractZdb() nothing found" << NL;
#endif
      //if (mscw)
      //   mscw->setUnused();
      if (first_time)   // not saved in gc object yet
         delete pdci;
      return FALSE;
   }

   //if (NULL == pdc->next())
   if (pdci->isLast())
   {                        // this was the last one, so trim the choicepoint
      pCNTL->TrimTrail();
      pCNTL->FCut();
      remove_ddb(imod, pdci);
      //if (mscw)
      //   mscw->setUnused();
      if (first_time)   // not saved in gc object yet
         delete pdci;
   }
   else
   {                           // there's more to come, 
      if (first_time)
		{                      // create an mscw cell on the heap
         //mscw = pHXL->heapGET();
         //pCNTL->BTop()->HBc = pHXL->HTop();
         //mscw->setMSCW(pGCTH->make_dbiter(pdci), imod, 0);  // not using clause number
         //pCNTL->BTop()->pdci = pdci;
         pCNTL->BTop()->gc_pdci = pGCTH->make_dbiter(pdci);
         pCNTL->BTop()->imod = imod;
      }
      // retracts save next entry in mscw, gets
      // save current one, this allows a pdc
      // to know whether its in use or not.
#ifdef xBUG_ASSRET
 DUMP << "this is who we're retracting: " << NL;
 pdc = pdci->getClause();
 pdc->Dump();
#endif
      remove_ddb(imod, pdci);  // note this sets pdci to next
      //pdci->setNext();
      //mscw->setMSCWretract(pdc->next(), imod);
      //mscw->setMSCWretract(pdci, imod);
      //pdci->getClause()->setMSCWretract(mscw);
#ifdef xBUG_ASSRET
 pdc = pdci->getClause();
 pdc->Dump();
#endif
      //pdci->getClause()->setNextMSCW(mscw);
   }

#ifdef xBUG_ASSRET
 FILL(NL << "AFTER");
 xpph = getPredicateHead(ximod, xpred, xar);
 if (xpph)
 {
  pdp = xpph->getDynamicPredicate();
  if (pdp) //predicate_dump(pdp);
   pdp->Dump();
  //mscw_heap_walk();
 }
 else
  FILL("predicate head NULL");
 FILL(NL << "end AFTER" << NL);
#endif

#ifdef xBUG_ASSRET
 DUMP << "<==p_retractZdb() got one" << NL;
#endif
   return TRUE;
}

#ifdef LANDFILL

TF DDB::p_dumpdb()
{  // For dumping from Prolog
   Dump();
   return TRUE;
}

void DDB::Dump()
{
   DUMP << "current modules: " << NL;
   for (int i=0; i<nmods; i++)
   {
      DUMP << "  module[" << i << "] = " << modules[i]->getModuleNameA()->get_display_name(m_peng) << NL;
      modules[i]->Dump();
   }
   DUMP << FLUSH;
}

/*
void DDB::mscw_heap_walk()
{
   TERM h;
   aCHAR buf[128];
   DynamicClause *pdc;
   CHOICE_POINTptr Bt;

   DUMP << "--- MSCW Heap Walk ---" << NL;
//   for (h=pHXL->GetHeap(); h<pHXL->HTop(); h++)
   for (Bt = pCNTL->BTop(); Bt != NULL; Bt = Bt->Bc)
   {
      h = Bt->HBc - 1;
   //for (h=pHXL->GetHeap(); h<pCNTL->BTop()->HBc; h++)
      if (h->IsMSCW())
      {
         DUMP << "h = " << h << " pdc = ";
         if (h->getMSCWDBRef()->isDone())
         {
            pdc = NULL;
            DUMP << "is done" << NL << FLUSH;
         }
         else
         {
            pdc = h->getMSCWDBRef()->getClause();
            DUMP <<  pdc << NL << FLUSH;
         }
         if (pdc) 
			  pWRIT->termWriteString(pdc->getCode(), buf, 127, true);
         else 
			  buf[0] = 0;
         DUMP << SP2 << buf << NL << FLUSH;
      }
   }
   DUMP << "---" << NL;
}
*/

void DDB::predicate_dump(DynamicPredicate *pdp)
{
   DynamicClause *pdc;
   PredicateHead *pph;
   aCHAR buf[128];

   pph = pdp->getPredicateHead();
   DUMP << "--- Predicate Dump ---" << NL;
   DUMP << pph << ": " << pph->getName() << "/" << pph->getArity() << NL;
   //pdc = pdp->getFirst();
   DynamicClauseIterator *dci = pdp->getIterator(NULL);
   //while(pdc)
   while(pdc = dci->getClause())
   {
      DUMP << "clause: " << pdc << ": " << *pdc << NL;
      pWRIT->termWriteString(pdc->getCode(), buf, 127, true);
      DUMP << buf << NL;
      //pdc=pdc->next();
      dci->setNext();
   }
   DUMP << "---" << NL;
}

#endif

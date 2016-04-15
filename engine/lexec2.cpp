/****************************************************************************
*
* lexec2.cpp --  support predicates for pcode interpreter
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: lexec2.cpp,v $
* Revision 1.5  2006/08/10 13:31:49  dennis
* fix to async
*
* Revision 1.4  2004/09/27 17:43:34  dennis
* 7.0.24 - no major changes I hope
*
* Revision 1.3  2004/03/26 21:37:10  dennis
* implemented occurs check
*
* Revision 1.2  2003/09/23 20:54:34  dennis
* more security stuff in linker and engine
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.13  2003/09/11 02:07:44  dennis
* fixed memory leak problem with dynamic iterators
*
* Revision 1.12  2003/08/21 18:24:02  dennis
* latest fixes to debug64
*
* Revision 1.11  2002/12/06 17:37:36  dennis
* a6-3-2, finally got rid of mscw tagging of heap cells to maintain
* state of dynamic clause backtracking, put backtracking info in
* the choice point structure instead.  much cleaner.  retract can now
* just analyze the control stack to see if a retract is safe.  heapgc
* no longer has to worry about those wierd cells.
*
* Revision 1.10  2002/05/15 16:59:08  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.9  2001/10/20 03:00:13  dennis
* fixed really old bug in put_unsafe_value.  a pathological eotek
* example uncovered the bug.  What's interesting is the implementation we
* had is correct according to Ait Kaci, and other WAM sources, but not
* according to Warren, who, alone, indicates the need for trail binding
* when pointing the local to a safe place on the heap.
*
* Revision 1.8  2001/10/02 16:05:21  dennis
* changed streams, cleaner interface to lex, reimplemented
* readers as buffer between the two
*
* Revision 1.7  2001/07/21 00:39:46  dennis
* added garbage collector for strings and things
*
* Revision 1.6  2001/06/27 15:15:10  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.5  2001/04/01 15:54:52  ray
* Modified compiler and loader for fixed data.
*
* Revision 1.4  2001/02/13 03:42:13  dennis
* trying to fix bug in lsExecStr, cleaning up user:
*
* Revision 1.3  2001/02/06 04:06:34  dennis
* fixed throw bug, added puctuation error checking to alib.
*
* Revision 1.2  2001/02/05 03:11:44  dennis
* Fixed nasty heap GC bug, other minor fixes
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.12  2000/09/28 03:24:40  dennis
* debugger working for command line listener
*
* Revision 1.11  2000/09/25 02:11:19  dennis
* first version of modules working, runs the modular version of
* duck world.  still needs import and export.  release 6.1.1
*
* Revision 1.10  2000/09/01 21:06:32  ray
* Added // mod divu divs modu mods >> << to Real
* Still no /
*
* Revision 1.9  2000/08/26 00:32:06  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.8  2000/05/23 03:37:43  dennis
* changed name of various variables that used to be called mod this
* and that for keeping track of load modules.  Those are now 'file' variables
* and 'mod' is used for real modules.
*
* Revision 1.7  2000/05/14 03:52:34  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
* Revision 1.6  2000/04/21 02:49:06  ray
*
* Added current_prolog_flags
*
* Revision 1.5  2000/03/28 01:05:16  dennis
* merged Ray's changes with bigdig.  bigdig is at point where
* new Cell class is used, but there are no modules in the system.
*
* Revision 1.4.2.9  2000/03/22 22:36:40  dennis
* duckworld now working
*
* Revision 1.4.2.8  2000/03/17 21:20:07  dennis
* simple duck working
*
* Revision 1.4.2.7  2000/03/17 03:43:31  dennis
* hello.xpl running right at this point
*
*
****************************************************************************/


/* ----- Includes -------------------------------------------------------- */

#include "inc.h"
#include "pch.h"

#ifdef LANDFILL
#define noBUG_TRACE
#define noBUG_CODSUP
#ifdef BUG_TRACE
#define TRACE(s) DUMP << "TR " << s << NL << FLUSH;
#else
#define TRACE(s)
#endif
#endif

#ifdef LANDFILL
void CODE_HEADER::Dump(LEngine *m_peng)
{
   DUMP << "Code Header " << this << SP << MODPREDAR(mod_ix, pred_atom, pred_arity);
}
#endif

/* ----- Internal Function Definitions ----------------------------------- */

int LExec::choice_point(TF tme, CODEptr pcode)
{
   // sets up a new choice point for next failure.
   CHOICE_POINTptr chpt;
   intCH NTV;
   //CLAUSE_BLKptr ci;
   CompiledPredicate *ci;
   int result;
   // returns continuation offset in the code stream (ie: new code_ptr)

   NTV = (intCH)*((cdSINTptr)(pcode + cdSINT_L));

   chpt = pCNTL->NewChoicePoint(NTV);           // new choice point

   chpt->NTVc = NTV;
   chpt->Ec   = pCNTL->ETop();                  // environment
   chpt->CPc  = CP;                             // continuation
#ifdef xBUG_CODE
   errDebugMsg2("  new B, CPc <- %lp\n", "  new B, CPc <-  %p\n", CP); 
#endif
   chpt->HBc   = pHXL->HTop();                  // heap
   chpt->Lc    = pHXL->LTop();                  // local
   chpt->TRc   = pCNTL->TRTop();                // trail
   chpt->Bc    = pCNTL->BTop();                 // latest ch pnt (backtrack to)
   chpt->flags = 0;
   chpt->Tagc  = NULL;
   chpt->Whoc  = Who;                           // code header
   //chpt->pdci = NULL;
   chpt->gc_pdci = NULL;
   chpt->clause_number = 0;
   chpt->imod = 0;
   //B = chpt;
   //CpCells(chpt+1, X, NTV);

   //pHXL->CopyFromX((CELLptr)(chpt+1), NTV);
   pHXL->CopyFromX((Cell *)(chpt+1), NTV);

   /* choice points are only created by try_me_elses, which link the
   clauses one to the next, and trys which are sequential at the end
   of the compiled code as part of a switch statement.
   tme (arg 1) is true if called by try_me_else.

   for try_me_else, CLc, what to try on backtracking is set to the
   argument with the offset to the next clause, while the code_ptr,
   returned, is the next op code.

   for try, CLc is the next instruction and the code_ptr is set by the
   offset to the clause to be tried.  */


#ifdef xBUG_CODSUP
   errDebugMsg("CHOICE:  B = %p\n", (STACK_ELMptr)B - Stack);
   errDebugMsg("        Bc = %p\n", (STACK_ELMptr)(B->Bc) - Stack);
   //   cdDebugChoice();
#endif

   chpt->Discon  = 0;                             // reset Discon
   if(Who)        // Transfer the extension block addr to Discon in the new chpt
      //if(ci = Who->block)
         //if(ci->info & extended)                   // Discontiguous code exists?
            //chpt->Discon = ci->clink;                // transfer it to new ch pt 
      if(ci = Who->block)
         if(ci->IsContinued())                   // Discontiguous code exists?
            chpt->Discon = ci->getNext();                // transfer it to new ch pt 

   chpt->CLc = tme ? pcode + *(cdSINTptr)pcode :  // next clause code
         (CODEptr) ((2 * cdSINT_L) + pcode); 
   result = tme ? 2 * cdSINT_L : *(cdSINTptr) pcode;

#ifdef xBUG_CODSUP
   if (bugFlag)
      if (tme)
         errDebugMsg("New B: %5u tryelse CLc = %p  %s/%d\n",
               (STACK_ELMptr)B-Stack, B->CLc, Cur_goal, Cur_arity);
      else
         errDebugMsg("New B: %5u try     CLc = %p  %s/%d\n",
               (STACK_ELMptr)B-Stack, B->CLc, Cur_goal, Cur_arity);
#endif
   pCNTL->SetBTop(chpt);            // top of control stack is new choice point
   return result;
}

CODEptr LExec::failure()
{
   // reset trailed cells then reset state; return next byte code to be processed
   CODEptr code_ptr;
   CHOICE_POINTptr  B;
   //CLAUSE_BLKptr ci;

   //TRAIL_INDEX     i, tr_top;
   //TERM            t;
   //TRAIL_CELLptr   tc;

#ifdef BUG_CODSUP
   FILL("FAILURE: " << Cur_goal << "/" << Cur_arity);
//   cdDebugChoice();
#endif
   B = pCNTL->BTop();

   // might have been pseudo-cut by debug64
   while (B->debug64_cut)
      B = pCNTL->PopBTop();

#ifdef BUG_TRACE
//   if (codePLMTraceB)
 Who = B->Whoc;
 if (Who)
 {
  if (Who->pred_atom == (cdATOM)(pATAB->semiA))
  {
//   plmtrace_msg(aS("OR Retry:\n"));
   TRACE("OR Retry");
  }
  else
  {
    Port = aS("FAIL");
    plm_trace();
    setwho();
  }
 }
#endif
#ifdef BUG_CODSUP
 int i;
 FILL("LEXEC:    failure before");
 for (i=0; i < B->NTVc; i++)
    pTSVC->Dump(pHXL->XVar(i));
#endif
   pCNTL->Unwind(B->TRc);                       // reset trailed cells

   //pHXL->CopyToX((CELLptr)(B+1),  B->NTVc);
   pHXL->CopyToX((Cell *)(B+1),  B->NTVc);
                              // make chpt fields true, don't pop chpt
#ifdef BUG_CODSUP
 FILL("LEXEC:    failure after");
 for (i=0; i < B->NTVc; i++)
    pTSVC->Dump(pHXL->XVar(i));
 pCNTL->Dump();
#endif

   pHXL->SetLTop(B->Lc);                        // restore local chpt 
   pHXL->SetHTop(B->HBc);                       // restore heap
   pCNTL->SetETop(B->Ec); 
   CP = B->CPc;                                 // restore continuation pntr 
#ifdef xBUG_CODSUP
//   errDebugMsg2("  failing CP <- %lp from B\n", 
//                "  failing CP <-  %p from B\n", CP); 
#endif
   code_ptr = B->CLc;                           // get next clause code 
   if(B->flags & TRUSTED)                       // If current chpt is trusted, 
   {
#ifdef BUG_CODSUP
 FILL("LEXEC: Popping trusted choice point");
#endif
      B = pCNTL->PopBTop();                      // pop it off
   }
   B->flags |= BF;                              // set backtracking 
   return(code_ptr) ;
}
      
int  LExec::put_unsafe(CODEptr pcode)
{
   TERM xi, yh;
   //TERM PushRef();

   /* bigdig
   for(yh = pCNTL->ETop()->Le + *(cdSMINTptr)pcode;   // deref
   //yh->IsRef() && ! yh->IsUnbound();
   yh->IsRef() && ! yh->IsUnbound();
   yh = yh->getTerm());
   */
   yh = ( pCNTL->ETop()->Le + *(cdSMINTptr)pcode )->dref();

   xi = yh;

   // ! this had a nasty bug fixed by the trailbinding line
   // below.  Interesting that the coding of put_unsafe is correct
   // according to three different WAM texts, but Warren's paper
   // indicates that trail binding is necessary when setting yh.
   // The bug that led to this was a pathological program that
   // put and unsafe y (local), pointed it to the heap, and then
   // changed the heap at the last minute when trail would no longer
   // care about it.  This trailbinding avoids that scenario.

   if (yh->IsRef())  // is a self-ref after Drf above
   {
      if (yh >= (pCNTL->ETop()->Le))
      {
         xi = pHXL->heapPushRef();
         //pTSVC->putVAL(yh, xi);
         pCNTL->TrailBinding(yh);  // see above!
         yh->setTerm(xi);
      }
   }
   //pTSVC->putVAL(* (TERMptr) (1 + pcode), pTSVC->TValue(xi));
   // trying to put the value from xi into 1+pcode as a Cell**
   //*(* (TERMptr) (1 + pcode)) = *(xi->getTerm());
   //(* (TERMptr) (1 + pcode))->setTerm(xi->getTerm());
   *(*(TERMptr)(1+pcode)) = *(xi);
#ifdef BUG_CODSUP
         FILL("LEXEC:    put_unsafe y,x");
         pTSVC->Dump(pCNTL->ETop()->Le + *(cdSMINTptr)pcode);
         pTSVC->Dump(*(TERMptr)(1+pcode));
#endif
   return(0);                                       // a formality 
}

CODEptr LExec::switch_on_cons(CODEptr code)
{
   register int size;
   register int i;
   TERM x;
//   TF short_cons;
//   aBYTE x_sub_type;

   size = * (cdSINTptr) code;
   code += cdSINT_L;
   x = (pHXL->GetX())->dref();
//   x_sub_type = (aBYTE) pTSVC->SubType(x);

//   short_cons = ! pTSVC->TisThing(x);

   for(i = 1; i <= size; ++i)
     {
       if (pTSVC->UnifyConst(x, (TERM) code)) 
         break;
       code += CELL_L + cdSINT_L;            // ! CONSTANT !! LABEL ! 
     }
   if (i > size)                             // no match 
     return((CODEptr) failure());

   code += CELL_L;
   return(code + * (cdSINTptr) (code));      // code + Label 
}

CODEptr LExec::switch_on_struc(CODEptr code)
{
   int      size;
   int      i;
   TERM      x;
   int      offset;

    size = * (cdSINTptr) code;
    code += cdSINT_L;
    x = pHXL->GetX()->dref()->getTerm();
    for(i = 1; i <= size; ++i)
      {
        if (x->getArity() == (ARITY) * (cdSMINTptr) (cdATOM_L +  code))
          if (x->getAtom() == (PATOM) * (cdATOMptr) code)
            break;
        code += cdATOM_L + cdSMINT_L + cdSINT_L; // !NAME!ARITY!LABEL! 
      }
    if (i > size)                                 // no match 
      return((CODEptr) failure());

   offset = (int) * (cdSINTptr) (code + cdATOM_L + cdSMINT_L);
   code += cdATOM_L + cdSMINT_L;
   return(code + offset);                         // code + LABEL 
}

//----------------------
// Cut tag functions
//

CHOICE_POINTptr   LExec::FindTag(TERM t, TF bcuton)
/*   find most recent choice point  whose Tag unifies with t,
   unify and return the choice point
   
   We have a problem if this is being executed from within a 
   NewProve  since we dare not trim the choicepoint back to before
   the NewProve (which has its own "artificial choicepoint" whose
   B->Clc is the special op code Oexit). So, what we do is look for
   this opcode. If we find it we trim back to this point, and then 
   WE PLANT THE ARGUMENT of the cut_tag immediately after this opcode
   (we left room for this in the NewProve). When we exit from the 
   Prove in NewProve we look to see if there is a term wedged after
   the Exit opcode. If there is then we continue to cut tag before
   return from NewProve. This is what is affectionately known as a
   hack.

   Because we need to turn tags on and off to allow backtracking
   into the catch/3 predicate, a choicepoint boolean flag, BTAG has
   been added.  Because we turn them on and off, sometime we need
   to find a turned on one, and sometimes a turned off.  cuton = TRUE
   means find a turned on one. */

   // Hmm, this is messy.  We can get in all sorts of trouble by
   // skipping back over a ls_call and resuming Prolog execution
   // on the other side.  I'm thinking we need to simply say there
   // is not catcher if there isn't one on this side of ls_call.
   // Let's try it and see if anything bad happens... 2004Mar24
{
   CHOICE_POINTptr   b;
   TERM  t2;

   t2 = t;
                           
   // b = pCNTL->BTop();
   // while(b)
   for(b = pCNTL->BTop(); b;  b = b->Bc)          // ray
       {                         // find out if the tagged choice point exists 
#ifdef _xDEBUG
    b->Dump(m_eo);
#endif
    if (bcuton == (b->flags & BTAG))           // (BTAG is 1)
      if(b->Tagc)
        if (pTSVC->Unify(t2, b->Tagc))
          break;
    //  b = b->Bc;
       }
     
     if (b==NULL)                           // if it didn't, trigger an error 
       return(NULL);
                        // if it did, return either the choice point or the
                        // phony NewProve choice point, which ever comes first 
     
     for(b = pCNTL->BTop(); b; b = b->Bc)         // ray
       {
         if (bcuton == (b->flags & BTAG))         // (BTAG is 1)
           if(b->Tagc)
             if (pTSVC->Unify(t, b->Tagc))
               return(b);

           if (*(cdOPptr)(b->CLc)==Oexit)
 // this is in for experiment
             return(NULL);
 // this is out for experiment
 //          {                 // put the cuttag term after the Oexit op code
 //              *(TERM)((CODEptr)(b->CLc) + cdOP_L) = *t;
 //              return(b);
//             }
       }
     return(NULL);
}

int LExec::p_tag()
{                                              // Sets B -> Tagc to  X[0] 
   TERM t;

#ifdef _xDEBUG
   aCHAR s3[80];
   TERM x0;
   x0 = pHXL->XVar(0);
   x0 = (x0)->dref();
   pTSVC->TermVal(x0, s3, 79);
#endif
   
   t = pHXL->heapGET();
   //pTSVC->putVAL( (t), pTSVC->LValue((pHXL->XVar(0))) )->dref();
   *t = *((pHXL->XVar(0)))->dref();
#ifdef _xDEBUG
   aCHAR s[80];
   aCHAR* s2;
   s2 = pTSVC->TermVal(pHXL->XVar(0), s, 79);
   //s2 = pTSVC->TermVal( pTSVC->LValue((pHXL->XVar(0))), s, 79 )->dref();
   TERM t2 = t;
   t2 = (t2)->dref();
   s2 = pTSVC->TermVal(t2, s, 79);
   Cur_goal = *(Cur_goala);
#endif
   pCNTL->BTop()->Tagc = t;
   pCNTL->BTop()->flags |= BTAG;
   return(TRUE);
}

TF LExec::p_deZtag()
/* de$tag - mark tag X[0] as disabled for now, after exitting the
   the call of a catch.  Leave the tag so it can be turned back on
   on backtracking. */
{
   CHOICE_POINTptr      t;

   if (NULL != (t = FindTag((pHXL->XVar(0))->dref(), TRUE)))
      t->flags &= ~BTAG;
//      t->Tagc = NULL;

   return(TRUE);
}

TF LExec::p_reZtag()
{                                            // re$tag - turn tag back on. 
   CHOICE_POINTptr t;

   if (t = FindTag((pHXL->XVar(0))->dref(), FALSE))
      t->flags |= BTAG;
//      t->Tagc = NULL;

   return(TRUE);
}

TF LExec::p_cuttag()
{            // eliminate all B's for which B -> HBc > FindTag(pHXL->XVar(0))
   CHOICE_POINTptr t;

   t = FindTag((pHXL->XVar(0))->dref(), TRUE);
   if (NULL == t)
      return(FALSE);

   pCNTL->SetBTop(t);                         //B = t;
   return(TRUE);
}

/* cut$tag/1 - a variation on cut_tag/1 used in implementing the ISO
   standard catch/throw predicates.  cut$tag/1 signals a system error
   if the tag being looked for is not in the control stack above the
   entry-level choice point, be it Prove() or NewProve(). */
TF LExec::p_cutZtag()
{
   CHOICE_POINTptr  pb;
   TERM  t;

   t = (pHXL->XVar(0))->dref();

   if (NULL == (pb = FindTag(t, TRUE)))
	  {                                 // no prolog catcher
      //aCHAR *pbuf = new aCHAR[80];
      aCHAR buf[1000];
      pWRIT->termWriteString(t, buf, 1000, TRUE); // puts term into buf for C

      // remove '{sys}thrown'(T) for amzilib portion of implementation
      TERM t2 = pHXL->heapGETN(3);
      (t2)->setStruct( t2+1);
      PATOM systhrownA;
      systhrownA = pATAB->EnterAtom(aS("{sys}thrown"));
      //pTSVC->putFUNC(t2+1, systhrownA, 1);
      (t2+1)->setFA(systhrownA, 1);
      (t2+2)->setTerm( t);
      pDDB->Retract(t2);

      pXCPT->Error(badthrowtagE, buf);
   }

   //B = t;
   pCNTL->SetBTop(pb);
   return(TRUE);
}

LBOOL LExec::throwPrologError(LExcept E)
{
  PATOM errortypeA;
  PATOM throwerrorA;
  LString buf;
  TERM errortypeT;
  
  // Create a term with exec error info
  // error(system_error(RC), MSG)
  // error(syntax_error(RC), MSG+READBUFFER)
  // Get error type atom.
  if (E.GetType()==READ)
	 errortypeA = pATAB->EnterAtom(aS("syntax_error"));
  else
	 errortypeA = pATAB->EnterAtom(aS("system_error"));
  
  errortypeT = pHXL->heapGET();
  errortypeT->setAtom( errortypeA);
  
  // Get message buffer, which includes read buffer for
  // errors that occurred during a read.
  //if (E.IsRead())
  //   buf = (LString)(E.GetMsg()) + aS("\n") + E.GetReadText();
  //else
  //   buf = E.GetMsg();
  //pPSTR->strEnterString(buf, &ps);
  // Create list of attr=val pairs for exception
  TERM attrlistT, head, tail;
  PATOM attrA, eqA, valA;
  TERM attrvalT;
  //STRptr msg;
  
  eqA = pATAB->EnterAtom(aS("="));
  // Start the list
  attrlistT = pHXL->heapGETN(3);
  head = attrlistT + 1;
  tail = attrlistT + 2;
  (attrlistT)->setList( head);
  
  // type = TYPE
  attrvalT = pHXL->heapGETN(4);
  (attrvalT)->setStruct( attrvalT + 1);
  //pTSVC->putFUNC(attrvalT + 1, eqA, 2);
  (attrvalT + 1)->setFA(eqA, 2);
  attrA = pATAB->EnterAtom(aS("type"));
  // bigdigattrvalT + 2.setAtom( attrA);
  (attrvalT + 2)->setAtom(attrA);
  switch(E.GetType())
	 {
	 case(BADENG):   valA = pATAB->EnterAtom(aS("badeng")); break;
	 case(ABORT):    valA = pATAB->EnterAtom(aS("abort")); break;
	 case(INTERNAL): valA = pATAB->EnterAtom(aS("internal")); break;
	 case(FATAL):    valA = pATAB->EnterAtom(aS("fatal")); break;
	 case(SECURITY): valA = pATAB->EnterAtom(aS("security")); break;
	 case(INIT):     valA = pATAB->EnterAtom(aS("init")); break;
	 case(API):      valA = pATAB->EnterAtom(aS("api")); break;
	 case(LOAD):     valA = pATAB->EnterAtom(aS("load")); break;
	 case(EXEC):     valA = pATAB->EnterAtom(aS("exec")); break;
	 case(READ):     valA = pATAB->EnterAtom(aS("read")); break;
	 default:        valA = pATAB->EnterAtom(aS("unknown"));
	 }
  // bigdig attrvalT + 3.setAtom( valA);
  (attrvalT + 3)->setAtom(valA);
  (head)->setTerm( attrvalT);
  // Next list element
  head = pHXL->heapGETN(2);
  (tail)->setList( head);
  tail = head + 1;
  
  // rc = RC
  attrvalT = pHXL->heapGETN(4);
  (attrvalT)->setStruct( attrvalT + 1);
  //pTSVC->putFUNC(attrvalT + 1, eqA, 2);
  (attrvalT + 1)->setFA(eqA, 2);
  attrA = pATAB->EnterAtom(aS("rc"));
  // bigdig attrvalT + 2.setAtom( attrA);
  (attrvalT+2)->setAtom(attrA);
  //pTSVC->putINT(attrvalT + 3, E.GetRC());
  (attrvalT + 3)->setInt(E.GetRC());
  (head)->setTerm( attrvalT);
  // Next list element
  head = pHXL->heapGETN(2);
  (tail)->setList( head);
  tail = head + 1;
  
  // message = MSG
  attrvalT = pHXL->heapGETN(4);
  (attrvalT)->setStruct( attrvalT + 1);
  //pTSVC->putFUNC(attrvalT + 1, eqA, 2);
  (attrvalT + 1)->setFA(eqA, 2);
  attrA = pATAB->EnterAtom(aS("message"));
  // bigdig attrvalT + 2.setAtom( attrA);
  (attrvalT +2)->setAtom(attrA);
  //pPSTR->strEnterString(E.GetMsg(), &msg);
  // bigdig attrvalT + 3->setStr( msg);
  (attrvalT +3)->setStr(pGCTH->make_string(E.GetMsg()));
  (head)->setTerm( attrvalT);
  // Next list element
  head = pHXL->heapGETN(2);
  (tail)->setList( head);
  tail = head + 1;
  
  // predicate = functor/arity
  attrvalT = pHXL->heapGETN(4);
  (attrvalT)->setStruct( attrvalT + 1);
  //pTSVC->putFUNC(attrvalT + 1, eqA, 2);
  (attrvalT + 1)->setFA(eqA, 2);
  attrA = pATAB->EnterAtom(aS("predicate"));
  // bigdig attrvalT + 2.setAtom( attrA);
  (attrvalT + 2)->setAtom(attrA);
  //pPSTR->strEnterString(E.GetPredInfo(), &msg);
  //attrvalT + 3->setStr( msg);
  (attrvalT + 3)->setStr(pGCTH->make_string(E.GetPredInfo()));
  (head)->setTerm( attrvalT);
  // Next list element
  head = pHXL->heapGETN(2);
  (tail)->setList( head);
  tail = head + 1;
  // callstack = CALLSTACK
  attrvalT = pHXL->heapGETN(4);
  (attrvalT)->setStruct( attrvalT + 1);
  //pTSVC->putFUNC(attrvalT + 1, eqA, 2);
  (attrvalT + 1)->setFA(eqA, 2);
  attrA = pATAB->EnterAtom(aS("callstack"));
  // bigdig attrvalT + 2.setAtom( attrA);
  (attrvalT +2)->setAtom(attrA);
  //pPSTR->strEnterString(E.GetCallStack(), &msg);
  //attrvalT + 3->setStr( msg);
  (attrvalT +3)->setStr(pGCTH->make_string(E.GetCallStack()));
  (head)->setTerm( attrvalT);
  
  if (E.IsRead())
	 {
		// Next list element
      head = pHXL->heapGETN(2);
      (tail)->setList( head);
      tail = head + 1;
		
      // read_buffer = READ_BUFFER
      attrvalT = pHXL->heapGETN(4);
      (attrvalT)->setStruct( attrvalT + 1);
      //pTSVC->putFUNC(attrvalT + 1, eqA, 2);
      (attrvalT + 1)->setFA(eqA, 2);
      attrA = pATAB->EnterAtom(aS("read_buffer"));
      // bigdig attrvalT + 2.setAtom( attrA);
      (attrvalT +2)->setAtom(attrA);
      //pPSTR->strEnterString(E.GetReadText(), &msg);
      //attrvalT + 3->setStr( msg);
      (attrvalT +3)->setStr(pGCTH->make_string(E.GetReadText()));
      (head)->setTerm( attrvalT);
                                               // Next list element
      head = pHXL->heapGETN(2);
      (tail)->setList( head);
      tail = head + 1;

      // read_file = read file name
      attrvalT = pHXL->heapGETN(4);
      (attrvalT)->setStruct( attrvalT + 1);
      //pTSVC->putFUNC(attrvalT + 1, eqA, 2);
      (attrvalT + 1)->setFA(eqA, 2);
      attrA = pATAB->EnterAtom(aS("read_file"));
      // bigdig attrvalT + 2.setAtom( attrA);
      (attrvalT +2)->setAtom(attrA);
      //pPSTR->strEnterString(E.GetReadFileName(), &msg);
      //attrvalT + 3->setStr( msg);
      (attrvalT +3)->setStr(pGCTH->make_string(E.GetReadFileName()));
      (head)->setTerm( attrvalT);
                                               // Next list element
      head = pHXL->heapGETN(2);
      (tail)->setList( head);
      tail = head + 1;

      // line_number = lineno
      attrvalT = pHXL->heapGETN(4);
      (attrvalT)->setStruct( attrvalT + 1);
      //pTSVC->putFUNC(attrvalT + 1, eqA, 2);
      (attrvalT + 1)->setFA(eqA, 2);
      attrA = pATAB->EnterAtom(aS("line_number"));
      // bigdig attrvalT + 2.setAtom( attrA);
      (attrvalT +2)->setAtom(attrA);
      //pPSTR->strEnterString(E.GetReadFileName(), &msg);
      //pTSVC->putINT(attrvalT + 3, E.GetReadLineno());
      (attrvalT +3)->setInt(E.GetReadLineno());
      head->setTerm( attrvalT);
   }

                                               // End the list
   tail->setAtom(pATAB->nilA);

           // Build full error structure, such as error(syntax_error(RC), MSG)
   TERM t = pHXL->heapGETN(4);
   t->setStruct( t+1);
   throwerrorA = pATAB->EnterAtom(aS("error"));
   //pTSVC->putFUNC(t+1, throwerrorA, 2);
   (t+1)->setFA(throwerrorA, 2);
   (t+2)->setTerm( errortypeT);
   (t+3)->setTerm( attrlistT);

                                  // See if it unifies with any tags FindTag()
   CHOICE_POINTptr pb = FindTag(t, TRUE);
   if (pb == NULL)
      return LFALSE;

                             // assert '{sys}thrown'(Term) for amzilib catch/3
   TERM t2 = pHXL->heapGETN(3);
   t2->setStruct( t2+1);
   PATOM systhrownA;
   systhrownA = pATAB->EnterAtom(aS("{sys}thrown"));
   //pTSVC->putFUNC(t2+1, systhrownA, 1);
   (t2+1)->setFA(systhrownA, 1);
   (t2+2)->setTerm(t);
   pDDB->Assert(SYSTEM_MODULE, 'z', t2);

   pCNTL->SetBTop(pb);                        // Set new B

   // return LBOOL of true indicating:
   // caller, prove, should fail and carryon
   return LTRUE;
}




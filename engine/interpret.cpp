/****************************************************************************
*
* interpret.cpp -- An interpreter
*
* Copyright (c) 1992-2002 by Amzi! inc.  All Rights Reserved.
*
* $Log: interpret.cpp,v $
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.6  2002/05/15 16:59:08  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.5  2002/01/20 20:48:05  ray
* revised real divide, printReal
*
* Revision 1.4  2001/10/27 03:24:37  dennis
* sorted dynamic predicates working, with optimized queries
* when they fit the right pattern, of vars last
*
* Revision 1.3  2001/04/16 05:21:14  dennis
* hacked together some fixes for sio/lex to be better friends,
* merged other changes, added new samples
*
* Revision 1.2  2001/04/02 21:50:13  dennis
* got debugger working again
*
* Revision 1.1  2001/03/26 02:29:35  dennis
* Ray's fixed numbers in.
*
*
*****************************************************************************/

#include "inc.h"
#include "pch.h"

// debugging flags
#ifdef LANDFILL
#define noBUG_TERP
#endif

/*
This all needs to be better thought out -
The tough part is the interplay between
intepreted prove and compiled prove.  They can
call each other.  The correct solution is
two prove loops that recursively call each
other on the C-stack.  Both maintain the
same control stack, so we have proves on
top of proves.
*/

void DDB::debug_out(STRptr port, TERM t)
{
   aCHAR buf[512];

   pTSVC->termWriteString(t, buf, 511, true);
   Lprintf(aS("%s - %s\n"), port, buf);
}

TF DDB::p_interpretZdb()
{
   return interpret_first();
}

TF DDB::interpret_first()
{
   TERM   t;
   TERM   f;
   TF     tf;

   t = X(0);

   if (t->IsStruct())
   {
      f = t->getTerm();
      // a goal list, prove the first one
      // interpret the rest
      if (f->getAtom() == pATAB->commaA)
      {
         pHXL->SetXRef(0, ++f);
         tf = i_prove();
         if (tf)
         {
            pHXL->SetXRef(0, ++f);
            tf = interpret_first();
         }
         return tf;
      }
      // some single goal
      tf = i_prove();
      return tf;
   }
   else
      return FALSE;
}

TF DDB::i_prove()
// X0 has the term
{
   TERM            head, body;
   TERM            dbhead, dbbody;
   TERM            h, dbt;
   TERM            mscw;
   TERM            t, x0;
   Cell           *XI;
   TRAIL_INDEX     ti;

   PATOM           pred;
   ARITY           ar;
   MODIX           imod;

   PredicateHead  *pph;
   DynamicClause  *pdc;

   int             clause_number;
   bool            first_time;
   Cell            c;
   bool            debug;

   debug = true;

   t = X(0);

   Goal g(m_peng, t);
   imod = g.imod;
   pred = g.functor;
   ar = g.arity;
   head = g.head;
   body = pHXL->heapGET();
   body->setUnbound();

   if (debug)
   {
      debug_out(aS("call"), t);
   }

   pph = getVisiblePredicate(imod, pred, ar);
   if (pph == NULL)
   {
      return FALSE;
   }

   clause_number = 1;                      // for the debugger
   pdc = pph->getDynamicPredicate()->getFirst();
   
   while (pdc != NULL)
   {                       // look for a clause that unifies with the target
      // if there are variables, we unfortunately have to
      // copy the term to the heap because it might be a
      // recursive clause and we need fresh variables each time.
      // Also, if its the last clause, we're going to trim
      // the choice point and we won't know it needs to be protected
      // from retracts.
      if (pdc->getNumVars() > 0 || pdc->next() == NULL)
//      if (true)
      {
         dbt = pHXL->heapGETN(pdc->getSize());   
         h = dbt+1;
         pTSVC->Copy(dbt, h, &h, pdc->getCode());
      }
      else
         dbt = pdc->getCode();

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

      if (pTSVC->Unify(dbhead, head) && pTSVC->Unify(dbbody, body))
      {
         if (dbbody == pATAB->TrueTerm)
            break;
         else
         {
            XI = pHXL->SaveX();
            x0 = pHXL->heapGETN(4);
            x0->setStruct(x0+1);
            (x0+1)->setFA(pATAB->colonA, 2);
            (x0+2)->setAtom(pDDB->getModuleNameA(imod));
            (x0+3)->setTerm(dbbody);
            pHXL->SetXRef(0, x0);
            if (TRUE == p_interpretZdb())
               break;
            pHXL->RestoreX(XI);
         }
      }

      // unwind any variables that were bound during
      // the unification attempt
      pHXL->SetHTop(pCNTL->BTop()->HBc);
      ti = pCNTL->BTop()->TRc;
      pCNTL->Unwind(ti);

      pdc = pdc->next();
      clause_number++;
   }

   // if none was found, trim the trail and cut
   // past the preceding 'repeat' and fail.
   if (pdc == NULL)
   {
      //pCNTL->TrimTrail();
      //pCNTL->FCut();

      if (debug)
      {
         debug_out(aS("fail"), t);
      }

      return FALSE;
   }
   else
   {                                    // if there's more to come
      if (pdc->next())
      {
         pCNTL->choice_point()
      }
      pdc->setMSCW(mscw);
   }

   if (debug)
   {
      debug_out(aS("exit"), t);
   }
   return TRUE;
}

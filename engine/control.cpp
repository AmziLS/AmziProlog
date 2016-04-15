/****************************************************************************

control.cpp -- control stack and trail


Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.

* 1999/11/11 Ray reduced TrailTop by 1 to eliminate destructor crash
****************************************************************************/

#include "inc.h"
#include "pch.h"

#ifdef LANDFILL
#define noBUG_CONTROL
#define noBUG_DEBUG64
#define noBUG_STACK_FULL
#endif

ControlStack::ControlStack(LEngine *peng)
{
   m_peng = peng;
   Stack = NULL;
   Trail = NULL;
}

ControlStack::~ControlStack()
{
   // normal deletes, because don't need to delete each
   // element of the array
   delete Stack;
   delete Trail;
}

void ControlStack::Init(uintC stacksize, uintC trailsize)
{
   StackLim = stacksize;
   TrailLim = trailsize;

   // add a few stack frames for error handling
   LNEW(Stack, STACK_ELM[StackLim + 10], aS("initialization"));
   //if (Stack == NULL)
   //   pXCPT->Error(outofmemE, aS("Control Stack"));
   // Cells must be on 4-byte boundary, make sure this is so.
   if ( (intC)Stack % sizeof(intC) != 0 )
      pXCPT->Error(alignE, aS("Control Stack"));

   LNEW(Trail, TRAIL_CELL[TrailLim], aS("initialization"));
   //if (Trail == NULL)
   //   pXCPT->Error(outofmemE, aS("Trail Stack"));
   // Cells must be on 4-byte boundary, make sure this is so.
   if ( (intC)Trail % sizeof(intC) != 0 )
      pXCPT->Error(alignE, aS("Trail"));

   TrailTop = TrailLim-1;                        // ray
   StackTop = Stack + StackLim;

   Reset();
   StackHW = STACK_ELMptr(B+1);
   TrailHW = TR;
}

void ControlStack::Reset()
{
   E = (ENVptr) Stack;
   E->Ee  = NULL;
   E->CPe = NULL;
   E->Be  = NULL;
   E->Whoe = NULL;
   E->Le  = pHXL->GetLocal();
   E->CutFlag = FALSE;

   TR = 0;

   B = (CHOICE_POINTptr)(E+1);
   B->Ec  = E;
   B->CPc = NULL;
   B->HBc = pHXL->HTop();
   B->Lc  = pHXL->LTop();
   B->TRc = TR;
   B->Bc  = NULL;
   //B->pdci = NULL;
   B->gc_pdci = NULL;
   B->clause_number = 0;
   B->imod = 0;
   //B->BF  = FALSE;   //rrdc
   B->Tagc   = NULL;
   //B->bTag = FALSE;  //rrdc
   B->flags  = 0;      //rrdc
   B->Discon = NULL;   //rrdc
   B->Whoc   = NULL;
   //B->CLc  = exittagP;
   B->CLc = (CODEptr)pEXEC->GetExitCode();  // Defined in code module
   B->NTVc = 0;
   B->debug64_cut = false;
}

void ControlStack::TrailBinding(TERM t)
{
   // hmmm, if we're unifying directly in ddb, then we don't
   // want this test.  I wonder if it was important???
   if (  (t>=pHXL->GetHeap() && t<B->HBc) ||
         (t>=pHXL->GetLocal() && t<B->Lc) )
         // || (!pHXL->OnHXL(t)) )  won't happen - added to protect ddb variable cells
   {
      if (TR > TrailHW)
      {
         if ((TrailHW = TR) >= TrailTop-1)
            pXCPT->Error(trailE, TR, TrailTop);
      }
#ifdef BUG_CONTROL
 DUMP << "   binding: " << NL;
 pTSVC->Dump(t);
#endif
      Trail[TR++].reset_cell = t;
   }
}

// used in conjunction with codeFCut() -- see if we can trim the
//   trail back after an codeFCut().
//
//   Because we are cutting, some of the cells which had to be
//   trailed for the current choice point no longer need to be trailed.
//   So we find all the trail cells that refer to cells above the new
//   choice point and remove them.
//
//   Also now used from Ocut WAM code, for the same reason.  recursive
//   loops with cuts were growing the trail, calling this from Ocut
//   seems to have fixed the problem.

void ControlStack::TrimTrail()
{
  TERM h;
  TRAIL_INDEX it;
  
#ifdef BUG_TRAIL
  printf("\nTT:");
  it = E->Be->TRc;
  
  while(it < TR)
    {
      h = Trail[it++].reset_cell;
      if (E->Be->HBc <= h && h < H)       // a heap cell over HBc
        putchar('H');
      else 
        if (E->Be->Lc <= h && h < L)      // a local cell over Lc
          putchar('L');
        else
          putchar('x');
    }
  printf("\n");
#endif
  
  it = E->Be->TRc;
  
  while(it < TR)
    {
      h = Trail[it].reset_cell;
      if ( (E->Be->HBc <= h && h < pHXL->HTop()) ||   // a heap cell over HBc
           (E->Be->Lc  <= h && h < pHXL->LTop()) )    // a local cell over Lc
        {
          Trail[it].reset_cell = Trail[TR-1].reset_cell;
          TR--;
        }
      else
        it++;
    }
}

// unwind the trail back to tr_top
void ControlStack::Unwind(TRAIL_INDEX tr_top)
{
   TERM t;
   TRAIL_CELLptr tc = &Trail[TR];
   for (TRAIL_INDEX i = TR - 1; i >= tr_top; i--)
     {
       t = (--tc)->reset_cell;
       // bigdig pTSVC->putVAL(t, t);
#ifdef BUG_CONTROL
 DUMP << "unwinding: " << NL;
 pTSVC->Dump(t);
#endif
       t->setUnbound();
     }
   TR = tr_top;
}

CHOICE_POINTptr ControlStack::NewChoicePoint(intCH NTV)
{
   CHOICE_POINTptr chpt;
   STACK_ELMptr p;

   if ( (CHOICE_POINTptr) E > B )
      chpt = (CHOICE_POINTptr) (E + 1);
   else
   {
      chpt = B + 1;
      // bigdig chpt = (CHOICE_POINTptr)((STACK_ELMptr)chpt + B->NTVc);
      // add on the number of cells saved
      chpt = (CHOICE_POINTptr)((Cell*)chpt + B->NTVc);
   }
   p = (STACK_ELMptr)(chpt + 1) + NTV*(sizeof(Cell)/sizeof(STACK_ELM));
   if (p > StackHW )
      StackHW = p;
   if (p >= StackTop )
   {
#ifdef BUG_STACK_FULL
      Dump();
#endif
      if (pSTATE->m_debug64_cut == LON)
         pXCPT->Error(choice_debugE);
      else
         pXCPT->Error(choiceE);
   }

   // initialize the clause variables, which are only used
   // when backtracking over clauses.  Other initialization
   // is usually performed by caller
   //chpt->pdci = NULL;
   chpt->gc_pdci = NULL;
   chpt->clause_number = 0;
   chpt->imod = -1;
   chpt->debug64_cut = false;
   return chpt;
}

void ControlStack::trailPUSH(TERM t)
{
   if (TR < TrailTop)
     Trail[TR++].reset_cell = t;
   else
     pXCPT->Error(trailE, TR, TrailTop);
}

ENVptr ControlStack::NewEnv()
{
   ENVptr tos;
   STACK_ELMptr p;

   if ( E > (ENVptr) B )
      tos = E + 1;
   else
   {
      tos = (ENVptr) (B + 1);
      tos = (ENVptr) ((Cell*)tos + B->NTVc);
   }
   if ( (p = (STACK_ELMptr)(tos + 1)) > StackHW )
      if ( (StackHW = p) >= StackTop )
      {
#ifdef BUG_STACK_FULL
 Dump();
#endif
         if (pSTATE->m_debug64_cut == LON)
            pXCPT->Error(choice_debugE);
         else
            pXCPT->Error(choiceE);
      }
   return tos;
}

TF ControlStack::p_debugZstack()
{
   TERM stack_list = get_debug_stack();
   return pTSVC->Unify(stack_list, X(0));
}

TERM ControlStack::get_debug_stack()
{
#ifdef BUG_DEBUG64
   FILL("---> ControlStack::get_debug_stack()");
#endif
   CHOICE_POINTptr bX = pCNTL->BTop();
   ENVptr eX = pCNTL->ETop();
   int idepth = 0;                       // Build choice-pnt stack info
   int ibstr = 0;
   int maxbdepth = 12;                   // someday we'll parameterize this
   LString sB;
   const int bbuf_size = 256;
   LString sGoal;
   CODE_HEADERptr chWho;
   LFLAG fRB = LON;
   TERM stack_list, head, tail;
   stack_list = pHXL->heapGET();
   tail = stack_list;
   bool is_choice_point = true;
   TERM goal, functor, arg;
   int n_args;
   TERM x0;

   while (bX && eX)
   {
      if ((STACK_ELMptr)bX > (STACK_ELMptr)eX)
      {
         chWho = bX->Whoc;
         is_choice_point = true;
      }
      else
      {
         chWho = eX->Whoe;
         is_choice_point = false;
      }

      if (is_choice_point && chWho &&
            (chWho->pred_atom == pATAB->debugZcallA ||
             chWho->pred_atom == pATAB->debugZinfoA) )
      {
         head = pHXL->heapGETN(2);

         if (is_choice_point && chWho->pred_atom == pATAB->debugZcallA)
         {
            n_args = 5;
            x0 = (TERM)(bX+1);
            goal = pHXL->heapGETN(2 + n_args);   // one argument
            functor = goal+1;
            goal->setStruct(functor);
            functor->setFA(pATAB->goalA, n_args);
            arg = functor+1;
            arg->setTerm(x0);   // goal
            arg++;
            arg->setTerm(x0+1);  // file
            arg++;
            arg->setTerm(x0+2);  // line
            arg++;
            if ( (x0+4)->dref()->IsVar() )
            {
               arg->setTerm(x0+3);  // call/fail port
               arg++;
               arg->setTerm(x0+5);  // call/fail indent
            }
            else
            {
               arg->setTerm(x0+4);  // exit/redo port
               arg++;
               arg->setTerm(x0+6);  // exit/redo indent
            }
            head->setTerm(goal);
         }
         else if (is_choice_point && chWho->pred_atom == pATAB->debugZinfoA)
         {
            n_args = 7;
            x0 = (TERM)(bX+1);
            goal = pHXL->heapGETN(2 + n_args);   // one argument
            functor = goal+1;
            goal->setStruct(functor);
            functor->setFA(pATAB->clauseA, n_args);
            arg = functor+1;
            arg->setTerm(x0);     // head
            arg++;
            arg->setTerm(x0+1);   // file
            arg++;
            arg->setTerm(x0+2);   // line
            arg++;
            arg->setTerm(x0+3);   // port
            arg++;
            arg->setTerm(x0+4);   // vars
            arg++;
            arg->setTerm(x0+5);   // indent
            arg++;
            if (bX->debug64_cut)  // debug_cut
               arg->setInt(1);
            else
               arg->setInt(0);
            head->setTerm(goal);
         }

         tail->setList(head);
         tail = head+1;
      }

      if (is_choice_point)
         bX = bX->Bc;
      else
         eX = eX->Ee;
   }

   tail->setAtom(pATAB->nilA);
#ifdef BUG_DEBUG64
   FILL("<--- ControlStack::get_debug_stack()");
#endif

   return stack_list;
}

// LANDFILL Methods
#ifdef LANDFILL
void CHOICE_POINT::Dump(LEngine *m_peng)
{
#ifdef BUG_STACK_FULL
   DUMP << "Choice Point " << this << NL;
   DUMP << "  Whoc = ";
   if (Whoc == NULL)
      DUMP << "NULL";
   else
      Whoc->Dump(m_peng);
   DUMP << NL;
   TERM t = (TERM)(this+1);
   for (int i = 0; i < NTVc; i++)
     {
       DUMP << "  X[" << i << "] = ";
       pTSVC->DumpWrite(t);
       DUMP << NL;
       t++;
     }
#else
   DUMP << "Choice Point " << this << NL;
   DUMP << "  HBc  = " << HBc << NL;
   DUMP << "  mscw = ";
   //if ((HBc-1)->IsMSCW())
   //   pTSVC->Dump(HBc-1);
   DUMP << NL;
   DUMP << "  Lc   = " << Lc << NL;
   DUMP << "  Ec   = " << Ec << NL;
   DUMP << "  Bc   = " << Bc << NL;
   DUMP << "  CPc  = " << CPc << NL;
   DUMP << "  CLc  = " << CLc << NL;
   DUMP << "  Discon = " << Discon << NL;
   DUMP << "  Tagc = " << Tagc << NL;
   pTSVC->Dump(Tagc);
   DUMP << NL;
   DUMP << "  Whoc = ";
   if (Whoc == NULL)
      DUMP << "NULL" << NL;
   else
      Whoc->Dump(m_peng);
   DUMP << NL;
   DUMP << "  TRc  = " << (int)TRc << NL;
   DUMP << "  NTVc = " << (int)NTVc << NL;
   DUMP << "  flags: ";
   if(flags & BTAG)
      DUMP << "bTag ";
   if(flags & BF)
      DUMP << "BF ";
   if(flags & TRUSTED)
      DUMP << "TRUSTED ";
   DUMP << NL;

   TERM t = (TERM)(this+1);
   for (int i = 0; i < NTVc; i++)
     {
       DUMP << "  X[" << i << "] = ";
       pTSVC->Dump(t);
       DUMP << NL;
       t++;
     }
#endif
}


void ENV::Dump(LEngine * m_peng)
{
#ifdef BUG_STACK_FULL
   DUMP << "Environment " << this << NL;
   if (Whoe == NULL)
      DUMP << "NULL" << NL;
   else
      Whoe->Dump(m_peng);
   DUMP << NL;
#else
   DUMP << "Environment " << this << NL;
   DUMP << "  Le   = " << Le << NL;
   DUMP << "  Ee   = " << Ee << NL;
   DUMP << "  Be   = " << Be << NL;
   DUMP << "  CPe  = " << CPe << NL;
   DUMP << "  Whoe = ";
   if (Whoe == NULL)
      DUMP << "NULL" << NL;
   else
      Whoe->Dump(m_peng);
   DUMP << NL;
   DUMP << "  CutFlag  = " << (int)CutFlag << NL;
#endif
}

void ControlStack::Dump()
{
   CHOICE_POINTptr bt = B;
   ENVptr          et = E;

   DUMP << NL << "----- Begin Control Stack Dump -----" << NL;

   while(bt != NULL)
     {
       if ((STACK_ELMptr)bt > (STACK_ELMptr)et)
         {
           bt->Dump(m_peng);
           bt = bt->Bc;
         }
       else
         {
           et->Dump(m_peng);
           et = et->Ee;
         }
     }
   DUMP << "----- End Control Stack Dump -----" << NL;

}

TF ControlStack::p_dumpZcontrol()
{
   Dump();
   return TRUE;
}
#endif  // LANDFILL









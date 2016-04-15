/****************************************************************************
*
* control.h -- control stack and trail
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
****************************************************************************/

#ifndef CONTROL_H
#define CONTROL_H

// BTAG must be 1 !
#define    BTAG      0x1      // cuttag enabled flag
#define    BF        0x2      // backtracking flag
#define    TRUSTED   0x4      // continued block

typedef   intC  TRAIL_INDEX;

struct TRAIL_CELL
{
   TERM    reset_cell;
};

typedef TRAIL_CELL *      TRAIL_CELLptr;

typedef   uintC  STACK_ELM; // or should this be a Cell like it was?

typedef   STACK_ELM *          STACK_ELMptr;

struct CHOICE_POINT;

struct ENV 
{
   TERM            Le;                     // L when E was activated
   ENV             *Ee;
   CHOICE_POINT    *Be;
   CODE            *CPe;
   CODE_HEADERptr  Whoe;
   TF              CutFlag;
   intCH           filler;
public:
#ifdef LANDFILL
   void Dump(LEngine *m_peng);
#endif
};

typedef ENV *       ENVptr;

class CompiledPredicate;
class DynamicClauseIterator;

struct CHOICE_POINT
{
   TERM            HBc;                           // current heap
   TERM            Lc;                            // current local 
   ENV            *Ec;                           // current environment
   CHOICE_POINT   *Bc;                   // latest choice point (backtrack to)
   CODEptr         CPc;                           // continuation pointer
   CODEptr         CLc;                           // next clause to try
   CompiledPredicate *Discon;                        // Discontiguous code
   //DynamicClauseIterator *pdci;                    // if clause choice point, this is iterator
   GCDBIter       *gc_pdci;                       // iterator for clause choice points
   intCH           clause_number;                 // for debugger when clauses in choice point
   MODIX           imod;                          // module number for clauses
   TERM            Tagc;
   CODE_HEADERptr  Whoc;
   TRAIL_INDEX     TRc;                           // current trail
   intC            flags;
   intCH           NTVc;
   bool            debug64_cut;                   // cut by debug64, but still around for stack reporting
   //TF              BF;                          // backtracking flag
   //TF              bTag;                        // cuttag enabled flag

public:
#ifdef LANDFILL
   void Dump(LEngine *m_peng);
#endif
};

typedef CHOICE_POINT *        CHOICE_POINTptr;

class ControlStack
{
private:
   LEngine         *m_peng;
   uintC            StackLim;
   uintC            TrailLim;
   STACK_ELMptr     Stack;
   STACK_ELMptr     StackTop;
   STACK_ELMptr     StackHW;
   ENVptr           E;
   CHOICE_POINTptr    B;

   TRAIL_INDEX      TR;
   TRAIL_CELLptr    Trail;
   TRAIL_INDEX      TrailTop;
   TRAIL_INDEX      TrailHW;

public:
   ControlStack(LEngine *peng);
   ~ControlStack();

   void Init(uintC stacksize, uintC trailsize);
   void Reset();
   void TrailBinding(TERM t);
   void TrimTrail();
   //void ControlStack::trimMSCW(CHOICE_POINTptr oldB);
   void FCut()
   {   B = E->Be; }

   STACK_ELMptr GetStack()
   {   return Stack; }
   uintC GetStackHW()
   {   return (uintC)(StackHW - Stack + 25); }
   void ResetStackHW()
     {StackHW = (STACK_ELMptr)E > (STACK_ELMptr)B ? 
        (STACK_ELMptr)E : 
        (STACK_ELMptr)B; }
   TERM get_debug_stack();

   uintC controlSIZE()
   {   return StackLim; }
   uintC controlUSE()
   {   return (uintC)((STACK_ELMptr)E - Stack); }

   uintC trailSIZE()
   {   return TrailLim; }
   uintC trailUSE()
   {   return TR; }
   TRAIL_INDEX TRTop()
   {   return TR; }
   void SetTRTop(TRAIL_INDEX newTR)
   {   TR = newTR; }
   void Unwind(TRAIL_INDEX tr_top);
   void trailPUSH(TERM t);
   TRAIL_CELLptr trailTopCell()
   {   return &Trail[TR]; }
   uintC GetTrailHW()
   {   return (uintC)TrailHW + 5; }
   void ResetTrailHW()
   {   TrailHW = TR; }

   CHOICE_POINTptr BTop()
   {
      return B; }
   void SetBTop(CHOICE_POINTptr newB)
   {   B = newB;
       LASSERT( B != 0, aS("Null B from SetBTop"));
//#ifdef _DEBUG
//      if (B == 0) DebugBreak();
//#endif
   }

   CHOICE_POINTptr PopBTop()                   // pop choice pt
     {   B = B->Bc;
       LASSERT( B != 0, aS("Null B from PopBTop"));
//#ifdef _DEBUG
//     if (B == 0) DebugBreak();
//#endif
     return B;
     }
   
   ENVptr ETop()
   {   return E; }
   void SetETop(ENVptr newE)
   {   E = newE; }

   ENVptr PopETop()                             // pop environment
     {   
       E = E->Ee;
       return E;
     }
   CHOICE_POINTptr NewChoicePoint(intCH NTV);
   ENVptr NewEnv();

   TF p_debugZstack();

#ifdef LANDFILL
   void Dump();
   TF p_dumpZcontrol();
#endif
};

#endif //CONTROL_H









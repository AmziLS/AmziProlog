/****************************************************************************
*
* LExec -- the pcode interpreter
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
****************************************************************************/

#include "inc.h"
#include "pch.h"

#ifdef LANDFILL
#define noBUG_CODE

#ifdef BUG_CODE
#define BUG_WHOAMI
#define BUG_CALLS
#define BUG_TRACE
#define BUG_OPS
#define BUG_CUTD
#define BUG_HEADER
#define BUG_COVERCALL
#define BUG_ESCAPE
#define BUG_STACK
#define BUG_BREAK
#define BUG_CALLS_REDOS
#endif

#ifdef BUG_TRACE
#define ATRACE(s) DUMP << "TR " << s << NL << FLUSH;
#else
#define ATRACE(s)
#endif

#endif

#ifdef BUG_CODE
char  debug_predname[512];   // a normal char version for debuggers to view
#endif

//#ifdef WINDOWS
//#define LOOP_CHECK

#ifdef THREADED_EXEC_STR
#define LOOP_CHECK \
         if (loopcount++ > Slice) \
         { \
            if (pACTION->IsBreakSet()) pXCPT->Error(breakE); \
            if (p_break && p_break->execute() == LTRUE)  \
            { if (p_pause && p_pause->execute() == LTRUE) pause_flag = true; \
              else pXCPT->Error(breakE); } \
            loopcount = 0; \
         }
#else
#define LOOP_CHECK \
         if (loopcount++ > Slice) \
         { \
            if (p_break && p_break->execute() == LTRUE)  \
            { if (p_pause && p_pause->execute() == LTRUE) pause_flag = true; \
              else pXCPT->Error(breakE); } \
            loopcount = 0; \
         }
#endif

//#endif
//#ifdef WINDOWS
//if (winbreak()) \
//               pXCPT->Error(breakE); \
//#endif

LExec::LExec(LEngine *peng, TROPT trace)
{
   m_peng = peng;
   Cur_clause = 0;
   pause_flag = false;

//#ifdef WINDOWS
   Slice = EXEC_SLICE
//#endif

   Who_Or = Who_Nil = NULL_CODE_HEADER;

   Reset();
#ifdef WAMPROFILE
   for (wbi=0; wbi < N_WAMOPS; wbi++)
      bucket[wbi] = 0;
#endif

   switch(trace)
   {
   case LOFF:
      codePLMTraceB = FALSE;
      codeTraceInitB = FALSE;
      break;
   case LON:
      codePLMTraceB = TRUE;
      codeTraceInitB = TRUE;
      break;
   case LINIT:
      codePLMTraceB = FALSE;
      codeTraceInitB = TRUE;
      break;
   default:
      break;
   }
}

LExec::~LExec()
{
}

#if defined WINDOWS && ! defined DOSDLL

/* Let Windows do it's thing every 'Slice' predicate calls.  Called
 * from OWhoAmI WAM code logic.  Allows user to stop or control break
 * in Windows execution. 
 */

TF LExec::winbreak()
{
   MSG   msg;

   if(breaks)                                                  // ray
   while (PeekMessage(&msg, 0, 0, 0, PM_REMOVE))
   {
      if (msg.message==WM_KEYDOWN && msg.wParam==VK_CANCEL)
         return TRUE;
      TranslateMessage (&msg) ;
      DispatchMessage (&msg) ;
   }
   return FALSE;
}

#else

TF LExec::winbreak()
{
   return FALSE;
}
#endif                                     // DOSDLL 

//#endif


void LExec::Reset()
{
   CODEptr            exittagP;

   CP = NULL;

   exittagP = (CODEptr)ExitCode;
   *(cdOPptr)exittagP = Oexit;
   *(TERMptr)(exittagP + cdOP_L) = NULL;   // set up space for cut_tag term

   *(cdOPptr)ExitCode = Oexit;

   return;
}

int LExec::ProveMain()
{    // calls top/0, defined in alib.pro, which then calls main$, main, etc.
   CODE     goal[cdTOP_L];
   CODEptr   g;

   pSTATE->m_trace      = LOFF;
   //pSTATE->m_fileerrors = LON;
   //pSTATE->m_readerrors = LON;

   g = (CODEptr) goal;
   * (cdOPptr) g = Oexec;                      // goal[0] Call op
   g += cdOP_L;
   * (cdMODIXptr) g = SYSTEM_MODULE;
   g += cdMODIX_L;
   * (cdATOMptr) g = (cdATOM)pATAB->topA;          // goal[1,2] the 2 functors for call
   g += cdATOM_L;
   * (cdATOMptr) g = (cdATOM)pATAB->topA;
   g += cdATOM_L;
   * (cdSMINTptr) g = 0;                       // goal[3] the arity
   g += cdSMINT_L;
   * (cdSINTptr) g = 0;                     // goal[4] # of perms for this call

   return(Prove(goal, NULL));
}

int LExec::Prove(CODEptr pcode_ptr, CODEptr continuation_ptr)
{              // code ptr points to p-code -- returns TRUE/FALSE or Error (<0)
   register CODEptr code_ptr;
   //CLAUSE_BLKptr    ci;
   TERM             xi, ri;
   int              x_int, y_int;  // all purpose ints
   //PRED_BLKptr      pi;
   TF               CF = FALSE;
   register TERM    S;
   short            UM = READ_MODE;    // READ or WRITE mode
   ENVptr           tos;
   CODEptr          xcode;
   cdOP             op;
   int              ret_code;
   TF               exec_B = FALSE;
   CHOICE_POINTptr  B;       //rrdc
   //CHOICE_POINTptr  oldB;
   //CLAUSE_BLKptr    Discon;                     //rrdc
   CompiledPredicate *Discon;
   STRptr           clname;                     //rrdc
   //int arity;   // ray
   MODIX            imod;
   PATOM            pred;
   ARITY            arity;
   EscapePredicate *p_escape;
   EscapePredicate *p_break;   // optional user_break specified
   EscapePredicate *p_pause;   // optional user_pause specified (for debugger use only)
   PredicateHead   *ph;
#ifdef BUG_TRACE
   cdOP         nextop;
#endif
#ifdef BUG_CODE
   bool gotit = false;
#endif
   
   // Used to process errors in catch at end - moved because we
   // suspect the sB initialization might be getting duplicated
   // causing spurious results
       int idepth;                       // Build choice-pnt stack info
       int ibstr;
       int maxbdepth;                   // someday we'll parameterize this
       CHOICE_POINTptr bX;
       ENVptr eX;
       LString sB;
       LString sGoal;
       const int bbuf_size = 256;
       aCHAR bBuf[bbuf_size];
       CODE_HEADERptr chWho;
       aCHAR cBE;
       LFLAG fRB;
       TERM tHead, tList;
   // end of erro declarations


  // PLM_TRACE
  //PATOM            escape_predA;
  //ARITY            escape_arity;
  //CODEptr          escape_code_ptr;
//#ifdef WINDOWS
  int              loopcount=0;
//#endif
  
   P = code_ptr = pcode_ptr;
   CP = continuation_ptr;
   // set up "Near" initializations 
   S = pHXL->HTop();            // this sets S to H segment so we can QUICK it 
   tos = pCNTL->ETop();         // ditto 

   Who_Or.pred_atom = (cdATOM) (pATAB->semiA);

   ret_code = FALSE;
   pause_flag = false;

   ph = pDDB->getVisiblePredicate(USER_MODULE, pATAB->user_breakA, 0);
   if (ph && ph->IsEscape())
   {
      p_break = ph->getEscapePredicate();
   }
   else
      p_break = NULL;

   ph = pDDB->getVisiblePredicate(USER_MODULE, pATAB->user_pauseA, 0);
   if (ph && ph->IsEscape())
   {
      p_pause = ph->getEscapePredicate();
   }
   else
      p_pause = NULL;

   // why was this code here?
   //pi = pATAB->PredHead(*(cdATOMptr) code_ptr);        //rrdc
  
  // After an error, the handler may start the loop again, at this label.
start:
   try {
    PROBE_WAM_INIT;
   while(code_ptr)
   {
   loop:
      op = *((cdOPptr) code_ptr);
      code_ptr += cdOP_L;
#ifdef xBUG_STACK
      if (g_iErrCount > 0)  // somewhere along the line g_iErrCount got undefined, and is unused
         errDebugMsg("%p opcode %d: %s\n", code_ptr, (int)op, ops[op]);
#endif
#ifdef WAMPROFILE
      bucket[op]++;
#endif
#ifdef BUG_OPS
if (op <= 56)
{
 FILL("LEXEC: op " << op << " " << ops[op]);
}
else
{
 FILL("LEXEC: bad op " << op);
}
#endif
      PROBE_WAM(op);
      switch(op)
      {
      case Ounify_x_var:                          // Xi 
            //pTSVC->putVAL((* (TERMptr) code_ptr),
            //   (UM) ? pTSVC->LValue(S++) : (long)pHXL->heapPushRef() );
        if (UM == READ_MODE)
          *( *(TERMptr)code_ptr ) = *(S++);
        else
          ( *(TERMptr)code_ptr )->setTerm(pHXL->heapPushRef());
        code_ptr += PTR_L;
        continue;   
            
      case Ounify_x_val:                             // Xi 
        xi = * (TERMptr) code_ptr;
        code_ptr += PTR_L;
        goto unify_val;
        
      case Oput_y_val:
         ** (TERMptr) (code_ptr + cdSMINT_L) = 
               * (pCNTL->ETop()->Le + * (cdSMINTptr) code_ptr);
         code_ptr += cdSMINT_L + PTR_L;
         continue;
    
      case Olabel:
         Cur_clause = *(cdSINTptr)code_ptr;
         code_ptr += cdSINT_L;
#ifdef BUG_TRACE
//         if (codePLMTraceB)
//         {
 if (*Port == aS('F')) 
  Port = aS("REDO");
 plm_trace();
//         }
#endif
         continue;
            
      case Oget_struc:                             // functor, arity, Xi 
         xi = * (TERMptr) (code_ptr + cdATOM_L + 1);
         while (TRUE)
         {
            if (xi->IsRef())
            {
               if (xi->IsUnbound())                // unbound
               {  
                  //arity = (CELL)*(CODEptr)(cdATOM_L + code_ptr);
                  arity = (ARITY)*(CODEptr)(cdATOM_L + code_ptr);
                  ri = pHXL->heapGET();
                  //pTSVC->putVAL( xi, (CELL) ri | (CELL) strucT );
                  xi->setStruct(ri);
                  //pTSVC->putVAL( ri, (consT | atomS) |
                  //                ((CELL)*(cdATOMptr) code_ptr) << HALFCELL |
                  //                     arity << QUARTERCELL );
                  ri->setFA((PATOM)(*(cdATOMptr) code_ptr), arity);
                  UM = WRITE_MODE;
                  code_ptr += cdATOM_L + 1 + PTR_L;
                  goto trail;
               }
               else
               {
                  xi = xi->getTerm();
                  continue;
               }
            }
            else if (xi->IsStruct())
            {
               xi = xi->getTerm();
               if (xi->getArity() == (ARITY) * (code_ptr + cdATOM_L))
                  if (xi->getAtom() == (PATOM)(* (cdATOMptr) code_ptr))
                  {
                     S = ++xi;
                     UM = READ_MODE;
                     code_ptr += cdATOM_L + 1 + PTR_L;
                     break;
                  }
            }
            LOOP_CHECK;
            code_ptr = failure();
            break;
         }
         continue;
            
      case Oput_y_var:                          // Yi, Xi 
         ri = pCNTL->ETop()->Le + * (cdSMINTptr) code_ptr;
         code_ptr += cdSMINT_L;
         //pTSVC->putVAL(ri, ri);
         ri->setUnbound();
         //pTSVC->putVAL(* (TERMptr) code_ptr, ri);
         (* (TERMptr) code_ptr)->setTerm(ri);
         code_ptr += PTR_L;
         continue;
            
      case Owho_am_i:
         LOOP_CHECK;
         code_ptr -= cdOP_L;

         Who = (CODE_HEADERptr) code_ptr;
         clname = (STRptr)*(Who->pred_atom);     // Lprintf(aS("clname: %s\n"),clname);
#ifdef BUG_CODE
         if (wcstombs(debug_predname, clname, 512) >= 512)  // so debuggers see it easily
            debug_predname[511] = 0;
         if (0 == strcmp("user_debug_stack", debug_predname))
           {                        // so you can break on a specific predicate
             gotit = true;
           }
#endif
         Cur_modix = Who->mod_ix;
         Cur_goala = (PATOM)Who->pred_atom;
         Cur_arity = (ARITY)Who->pred_arity;
         PROBE_WHOAMI_HIT(Cur_goala, Cur_arity);

#ifdef BUG_WHOAMI
FILL("LEXEC: WhoAmI " << MODPREDAR(Cur_modix, Cur_goala, Cur_arity));
#endif

         code_ptr += sizeof(CODE_HEADER) / sizeof(CODE);
            
#ifdef BUG_TRACE
//         if (codePLMTraceB)
//         {
plm_debug_header();
 if (exec_B)
 {Port = aS("XCAL"); exec_B = FALSE;}
 else 
  Port = aS("CALL");

// call trace from here or wait for Olabel
 nextop = * (cdOPptr) code_ptr;
 if (! (nextop == Otry_me_else || nextop == Oswitch_on_term))
   plm_trace();
//         }
#endif
         continue;
            
      case Oget_x_var:
         ** (TERMptr) code_ptr = ** (TERMptr) (code_ptr + PTR_L);
         code_ptr += 2 * PTR_L;
         continue;
            
      case Oget_y_var:
         * (pCNTL->ETop()->Le + *(cdSMINTptr)code_ptr) = 
               ** (TERMptr) (code_ptr + cdSMINT_L);
         code_ptr += cdSMINT_L + PTR_L;
         continue;
            
      case Oget_x_val:
         if (! pTSVC->Unify(*(TERMptr) code_ptr, *(TERMptr)(code_ptr + PTR_L)))
         {
            LOOP_CHECK;
            code_ptr = failure();
         }
         else
            code_ptr += 2 * PTR_L;
         continue;
            
      case Oget_y_val:
         if (! pTSVC->Unify(pCNTL->ETop()->Le + * (cdSMINTptr) code_ptr, 
                            *(TERMptr) (code_ptr + cdSMINT_L)))
         {
            LOOP_CHECK;
            code_ptr = failure();
         }
         else
            code_ptr += cdSMINT_L + PTR_L;
         continue;
            
      case Oget_con:  //  Constant, Xi
#ifdef xBUG_CODE
         FILL("LEXEC:    unifying const");
         pTSVC->Dump(* (TERMptr) (code_ptr + CELL_L));
         pTSVC->Dump((TERM) code_ptr);
#endif
            if (pTSVC->UnifyConst(* (TERMptr) (code_ptr + CELL_L), 
                                  (TERM)   code_ptr))
              code_ptr += PTR_L + CELL_L;
            else
            {
               LOOP_CHECK;
              code_ptr = failure();
            }
            continue;
            
      case Oget_nil:                               // P[0] = Src Xi 
            if ( ! pTSVC->UnifyConst(*(TERMptr) code_ptr, pATAB->NilTerm))
            {
               LOOP_CHECK;
              code_ptr = failure();
            }
            else
              code_ptr += PTR_L;
            continue;
            
      case Oget_list:                              // P[0] = Xi 
            xi = * (TERMptr) code_ptr;
            code_ptr += PTR_L;
          get_list:
            while(TRUE)
              {
                if (xi->IsRef())
                  if (xi->IsUnbound())
                    {                              // build a list 
                      //pTSVC->putVAL( xi, (CELL) pHXL->HTop() | listT );
                      xi->setList(pHXL->HTop());
                      UM = WRITE_MODE;
                    trail:
                      if (xi < pHXL->GetX() ?      // come again?
                          (xi < (pCNTL->BTop()->HBc)) : 
                          (xi < (pCNTL->BTop()->Lc))
                          )
                        pCNTL->trailPUSH(xi);
                      goto loop;
                    }
                  else
                    xi = xi->getTerm();
                else 
                  if (xi->IsList())              // decompose a list 
                    {
                      S = xi->getTerm();
                      UM = READ_MODE;
                      break;
                    }
                  else
                    {
                       LOOP_CHECK;
                      code_ptr = failure();
                      break;
                    }
              }
            continue;
            
            /* puts */
      case Oput_x_var:                         // Xi, Xj 
            ri = pHXL->heapPushRef();
            //pTSVC->putVAL(* (TERMptr)(code_ptr + PTR_L), ri);
            (* (TERMptr)(code_ptr + PTR_L))->setTerm(ri);
            //pTSVC->putVAL(* (TERMptr) code_ptr, ri);
            (* (TERMptr) code_ptr)->setTerm(ri);
            code_ptr += 2 * PTR_L;
            continue;
            
      case Oput_x_val:                          // Xi, Xj 
         ** (TERMptr) (code_ptr + PTR_L) = ** (TERMptr) code_ptr;
         code_ptr += 2 * PTR_L;
         continue;
            
      case Oput_unsafe:                          // P[0] = Yi, P[1] = Xj  
         put_unsafe(code_ptr);
         code_ptr += cdSMINT_L + PTR_L;
         continue;
            
      case Oput_con:                             // Constant, Src Xi 
         **  (TERMptr) (code_ptr + CELL_L) = * (TERM) code_ptr;
         code_ptr += CELL_L + PTR_L;
         continue;
            
      case Oput_nil:                             // Xi 
         ** (TERMptr) code_ptr = pATAB->NilCell;
         //(* (TERMptr) code_ptr)->setNil();
         // bigdig (old bug?) code_ptr += CELL_L;
         code_ptr += PTR_L;
         continue;
            
      case Oput_struc:                           // functor, arity, Xi 
            ri = pHXL->heapGET();
            //pTSVC->putVAL( (* (TERMptr)(code_ptr + cdSMINT_L + cdATOM_L)), 
            //               (long) ri | strucT );
            (* (TERMptr)(code_ptr + cdSMINT_L + cdATOM_L))->setStruct(ri);
            //pTSVC->ATag(ri, consT | atomS);
            arity = (ARITY)*(code_ptr + cdATOM_L);
            //            if(arity >100)
            //              printf("yertiz");
            //pTSVC->putARITY(ri, arity);
            //pTSVC->AaValue(ri, * (cdATOMptr) code_ptr);
                                ri->setFA((PATOM)(* (cdATOMptr) code_ptr), arity);
            code_ptr += cdATOM_L + cdSMINT_L + PTR_L;
            UM = WRITE_MODE;
            continue;
            
      case Oput_list:    // Xi 
         //pTSVC->putVAL((*(TERMptr)code_ptr), (long)pHXL->HTop() | listT );
         (*(TERMptr)code_ptr)->setList(pHXL->HTop());
         code_ptr += PTR_L;
         UM = WRITE_MODE;
         continue;
            
      case Ounify_y_var:                         // Yi 
            //pTSVC->putVAL((pCNTL->ETop()->Le + * (cdSMINTptr) code_ptr ), 
            //  (UM) ? pTSVC->LValue(S++) : (CELL)pHXL->heapPushRef()
        if (UM == READ_MODE)
          *(pCNTL->ETop()->Le + * (cdSMINTptr) code_ptr ) =   *(S++);
        else
          (pCNTL->ETop()->Le + 
           * (cdSMINTptr) code_ptr )->setTerm(pHXL->heapPushRef());
            code_ptr += cdSINT_L;
            continue;
            
      case Ou_var_getlist:

            /* this is just a Ounifyar X followed by get_list X */
            /* for the scratch register */
            /* used to walk down lists, so condense it to reduce code size */
            
            //xi = (X + MaxVars - 1);
        xi = pHXL->XVar(pHXL->GetMaxVars() - 1);
        if (UM == WRITE_MODE)
          //pTSVC->putVAL(xi, pHXL->heapPushRef());
          xi->setTerm(pHXL->heapPushRef());
        else
          //pTSVC->putVAL( (xi), pTSVC->LValue(S++) );
          *(xi) = *(S++);
        goto get_list;
        
      case Ounify_y_val:                             // Yi 
        xi = pCNTL->ETop()->Le + * (cdSMINTptr)code_ptr;
        code_ptr += cdSMINT_L;
        goto unify_val;
        
      case Ounify_unsafe:                            // X index of Y index 
        xi = (Y_REG & (x_int = * (cdSMINTptr)code_ptr)) ?
          (pCNTL->ETop()->Le + (Y_MASK & x_int)) :
            pHXL->XVar(x_int);
        code_ptr += cdSMINT_L;
      unify_val:
        if (UM == READ_MODE)
          {
            if (! pTSVC->Unify(S++, xi))
            {
               LOOP_CHECK;
              code_ptr = failure();
            }
            continue;
          }
        
        while(TRUE)
          {
            if (xi->IsRef())
              if (xi->IsUnbound())
                {
                  if (xi < pHXL->GetX())
                    {
                      pHXL->SetHVal(*xi);
                      pHXL->heapINC();
                    }
                  else
                    {
                      //pTSVC->putVAL(xi, pHXL->heapPushRef());
                      xi->setTerm(pHXL->heapPushRef());
                      goto trail;
                    }
                  goto loop;
                }
              else
                {
                  xi = xi->getTerm();
                  continue;
                }
            else
              {
                pHXL->SetHVal(*xi);
                pHXL->heapINC();
                goto loop;
              }
          }
        
      case Ounify_con:                                // CELL 
        if (UM == WRITE_MODE)
       //pTSVC->putVAL( ((pHXL->heapGET())),  pTSVC->LValue((TERM) code_ptr) );
          *(pHXL->heapGET()) = *((TERM) code_ptr);
        else if (! pTSVC->UnifyConst(S++, (Cell *) code_ptr))
          {
             LOOP_CHECK;
            code_ptr = failure();
            continue;
          }
        code_ptr += CELL_L;
        continue;
            
      case Ounify_void:                                 // short integer 
        x_int = * (cdSINTptr) code_ptr;
        while(x_int--)
          {
            if (UM == WRITE_MODE)      
              pHXL->heapPushRef();
            else
              ++S;
          }
        code_ptr += cdSINT_L;
        continue;
        
      case Ounify_nil:
        if (UM == WRITE_MODE)
        //pTSVC->putVAL( ((pHXL->heapGET())), pTSVC->LValue(pATAB->NilTerm) );
          *(pHXL->heapGET()) = pATAB->NilCell;
        else
          {
            if (! pTSVC->UnifyConst(S++, pATAB->NilTerm))
              {
                 LOOP_CHECK;
                code_ptr = failure();
                continue;
              }
          }
        continue;
        
        /* sequencing codes */
        
      case Ofail:
        Cur_goala = pATAB->failA;
        Cur_arity = 0;
        LOOP_CHECK;
        code_ptr = failure();
        continue;
        
      case Ocall:
      case Omod_call:
         // modix, functor x 2, arity, NumPerms (short int *) 
         // if E is protected by a choice point ...       
        
         if ( pCNTL->ETop() > (ENVptr) pCNTL->BTop() )  // not protected 
            pHXL->SetLTop( pCNTL->ETop()->Le + 
                           *(cdSINTptr)(code_ptr + cdMODIX_L + 
                                        (2*cdATOM_L) + cdSMINT_L) );
         CF = FALSE;

         imod = (MODIX)*(cdMODIXptr)code_ptr;
         pred = (PATOM)(*(cdATOMptr)(code_ptr + cdMODIX_L));
         arity = (ARITY)*(cdSMINTptr)(code_ptr + cdMODIX_L + (2*cdATOM_L));

#ifdef BUG_CALLS
FILL("LEXEC:   call " << MODPREDAR(imod,pred,arity));
pHXL->Dump();
#endif

         // if the special case of call(X)
         if (pred == pATAB->callA)
         { 
            code_ptr = pDDB->DoCall(code_ptr, TRUE);
            continue;
         }

         if (pred == pATAB->call_nometaA)
         { 
            code_ptr = pDDB->DoCallNoMeta(code_ptr, TRUE);
            continue;
         }

         //else if ((pi = pATAB->PredHead(imod, *(cdATOMptr)code_ptr)) != NULL)
         //if (ph = pDDB->getPredicateHead(imod, pred, arity))
         if (ph = pDDB->getVisiblePredicate(imod, pred, arity))
           {
             if (ph->IsCompiled() &&
                 (
                  ! ph->IsMeta() || 
                  //imod == SYSTEM_MODULE || 
                  imod == USER_MODULE)
                 )
               {
                 CP = cdMODIX_L + (2 * cdATOM_L) + cdSMINT_L + cdSINT_L + 
                   code_ptr;
                 code_ptr = ph->getCompiledPredicate()->getCode();
                 continue;
               }
             else
               {
                 code_ptr = pDDB->CallInterp(code_ptr, TRUE);
                 continue;
               }
           }
         else
         {

#ifdef BUG_TRACE
//if (codePLMTraceB)
//plmtrace_msg(aS("\n*** - No Clauses for %s/%d "),
//* (cdATOMptr) code_ptr,
//* (cdSMINTptr)(code_ptr + (2 * cdATOM_L)) );
 ATRACE("No clauses for: " << MODPREDAR(imod,pred,arity) );
#endif
            LOOP_CHECK;
            code_ptr = failure();
         }
         continue;
        
        // AS CALL, except 4 bytes following op-code are now ptr to code 
        // directly, however the space reserved in the code for call local 
        // is the same as for call, that is two atoms, an arity and the 
        // number of perms, so the length adjustments reflect that.  
        // The direct pointer overrides the atom(s) at the beginning of 
        // the arguments

      case Ocall_direct:
        if ( pCNTL->ETop() > (ENVptr) pCNTL->BTop() )   // not protected 
          pHXL->SetLTop( pCNTL->ETop()->Le + 
                         *(cdSINTptr)(cdCODE_L + 2*cdATOM_L + cdSMINT_L + 
                                      code_ptr) );
        CF = FALSE;
        CP = cdCODE_L + 2 * cdATOM_L + cdSMINT_L + cdSINT_L + code_ptr;
        code_ptr = *(CODEhnd) code_ptr;
#ifdef BUG_CALLS
FILL("LEXEC:   call_direct " << MODPREDAR(
  ((CODE_HEADERptr)code_ptr)->mod_ix,
  ((CODE_HEADERptr)code_ptr)->pred_atom,
  ((CODE_HEADERptr)code_ptr)->pred_arity ));
pHXL->Dump();
#endif
        continue;

      case Oexec:                                       // functor, arity
      case Omod_exec:
         exec_B = TRUE;
         //            printf("Heap left: %d\n", pHXL->HeapLeft() );

         if ( pHXL->HeapPastBumper() )
            pHXL->GC((ARITY) *(cdSMINTptr)(code_ptr + cdMODIX_L + 2*cdATOM_L));

         CF = FALSE;
         //  Functor, Arity 
         imod = (MODIX)*(cdMODIXptr)code_ptr;
         pred = *(cdATOMptr)(code_ptr + cdMODIX_L);
         arity = (ARITY)*(cdSMINTptr)(code_ptr + cdMODIX_L + (2*cdATOM_L));

#ifdef BUG_CALLS
FILL("LEXEC:   exec " << MODPREDAR(imod,pred,arity));
pHXL->Dump();
#endif

         if (pATAB->callA == *(cdATOMptr) (code_ptr + cdMODIX_L))
           {
             code_ptr = pDDB->DoCall(code_ptr, FALSE);
           }
         else if (pATAB->call_nometaA == *(cdATOMptr) (code_ptr + cdMODIX_L))
           {
             code_ptr = pDDB->DoCallNoMeta(code_ptr, FALSE);
           }
         else 
           if ((ph = pDDB->getVisiblePredicate(imod, pred, arity)))
             {
               code_ptr = ph->IsCompiled() && (
                                               ! ph->IsMeta() || 
                                               //imod == SYSTEM_MODULE || 
                                               imod == USER_MODULE
                                               ) ?                 
                 ph->getCompiledPredicate()->getCode() :
                 pDDB->CallInterp(code_ptr, FALSE);
               continue;
             }
           else
           {
              LOOP_CHECK;
             code_ptr = failure();
           }
         
         if (code_ptr == NULL) 
            ret_code = TRUE;                // catch API calls ending in execs 
         continue;

      case Oexec_direct:
        // AS exec except first 4 bytes are ptr to code, but length is 
        //  still two atoms after the exec_local op code
        
        exec_B = TRUE;
        
        if ( pHXL->HeapPastBumper() )
          pHXL->GC((ARITY)*(cdSMINTptr)(code_ptr + cdMODIX_L + 2*cdATOM_L));
        CF = FALSE;
        code_ptr = *(CODEhnd) code_ptr;
#ifdef BUG_CALLS
FILL("LEXEC:   exec_direct " << MODPREDAR(
  ((CODE_HEADERptr)code_ptr)->mod_ix,
  ((CODE_HEADERptr)code_ptr)->pred_atom,
  ((CODE_HEADERptr)code_ptr)->pred_arity ));
pHXL->Dump();
#endif
        continue;

        
      case Oproceed:
        tos = pCNTL->ETop();                       //rrdc environment
        CF = FALSE;
#ifdef BUG_TRACE
//        if (codePLMTraceB)
//          {
 Port = aS("EXIT");
 plm_trace();
//          }
#endif
        if (CP)
          code_ptr = CP;
        else
          {
            P = code_ptr;
            ret_code = TRUE;
            code_ptr = NULL;
          }
        continue;                                  //rrdc


      case Oescape:                         // arg is EscapePredicate ptr 
         p_escape = *(cdESCAPEptr)code_ptr;

         Esc_predA = p_escape->getName();
         Esc_arity = p_escape->getArity();
#ifdef BUG_CALLS
DUMP << NL;
FILL("LEXEC:   escape " << p_escape << SP << PREDAR(Esc_predA,Esc_arity));
//pHXL->Dump();
#endif
         Cur_goala = Esc_predA;
         Cur_arity = Esc_arity;
         Esc_code_ptr = code_ptr;
         Esc_S = S;
         Esc_CF = CF;
         Esc_UM = UM;
         Esc_tos = tos;        
#ifdef BUG_TRACE
//         if (codePLMTraceB && FALSE)
//         {
//            plmtrace_msg(aS("[c]: %s/%d"),
//                  *(Esc_predA), Esc_arity);
//            plm_xs(Esc_arity);
//         }
 ATRACE("[c]: "<< PREDAR(Esc_predA, Esc_arity));
 pHXL->DumpX(Esc_arity);      
#endif

         PROBE_ESCAPE_START(Esc_predA, Esc_arity);
         x_int = p_escape->execute();      // try/catch this
         PROBE_ESCAPE_END(Esc_predA, Esc_arity);
 
         if (TRUE == x_int)
         {
             
#ifdef BUG_TRACE
//            if (codePLMTraceB && FALSE)
//            {
//               plmtrace_msg(aS("[c]EXIT: %s/%d"),
//                     *(Esc_predA), Esc_arity);
//               plm_xs(Esc_arity);
//            }
 ATRACE("[c]EXIT: " << PREDAR(Esc_predA, Esc_arity));
 pHXL->DumpX(Esc_arity);      
#endif
            code_ptr += cdESCAPE_L;
            continue;
         }
         else if (FALSE != x_int)
         {
            pXCPT->Error(badxpredE, 
               (STRptr)*(p_escape->getName()), 
               (int)p_escape->getArity(), 
               x_int);
         }

#ifdef BUG_TRACE
//         if (codePLMTraceB && FALSE)
//         {
//            plmtrace_msg(aS("\n[c]FAIL: %s/%d"),
//                  *(Esc_predA), Esc_arity);
//            plm_xs(Esc_arity);
//         }
 ATRACE("[c]FAIL: " << PREDAR(Esc_predA, Esc_arity));
 pHXL->DumpX(Esc_arity);      
#endif
         LOOP_CHECK;
         code_ptr = failure();
         continue;

      
      case Oalloc:  // Numvars (short) 
         tos = pCNTL->NewEnv();

         tos->Ee = pCNTL->ETop();
         tos->CPe = CP;
         tos->Le = pHXL->LTop();
         tos->CutFlag = CF;  
         tos->Be = pCNTL->BTop();
         tos->Whoe = Who;
         pCNTL->SetETop(tos);
         pHXL->localINC( *(cdSINTptr)code_ptr );
         code_ptr += cdSINT_L;
         continue;
        

      case Odealloc:
        CP = pCNTL->ETop()->CPe;
        if ( pCNTL->ETop() > (ENVptr)(pCNTL->BTop()) ) 
          pHXL->SetLTop( pCNTL->ETop()->Le );
        pCNTL->SetETop(pCNTL->ETop()->Ee);
        continue;
        
      case Ocut64:
        // The debug64_cut, used for debugging.  It doesn't
        // actually cut yet, just mark choicepoints as
        // having been cut.  See p_cut_debug64_env for the same
        // thing for interpreted code.

        if (pSTATE->m_debug64_cut == LON)  // drop through to normal cut if option is off
        {
            CHOICE_POINTptr bcut, bnow;

            bnow = pCNTL->BTop();

            pCNTL->SetBTop(pCNTL->ETop()->Be); 
            if (pCNTL->ETop()->CutFlag)
               pCNTL->SetBTop(pCNTL->BTop()->Bc);

            bcut = pCNTL->BTop();
            pCNTL->SetBTop(bnow);  // we don't really cut

            for (bnow = pCNTL->BTop(); bnow > bcut; bnow = bnow->Bc)
               bnow->debug64_cut = true;

            pCNTL->TrimTrail();
            CF = FALSE;
            continue;
        }

      case Ocut:
        //oldB = pCNTL->BTop();

        pCNTL->SetBTop(pCNTL->ETop()->Be); 
        if (pCNTL->ETop()->CutFlag)
          pCNTL->SetBTop(pCNTL->BTop()->Bc);

        // some of the choice points might have been
        // holding mscws for backtracking into clauses
        //pCNTL->trimMSCW(oldB);

        // added this line because trail was growing in recursive
        // loops with cuts before recursive call.  was written
        // for interpreted cuts, but seems to do the job here
        // as well.
        pCNTL->TrimTrail();
        CF = FALSE;
        continue;


      /*
      case Ocut:
        pCNTL->SetBTop(pCNTL->ETop()->Be); 
        if (pCNTL->ETop()->CutFlag)
          pCNTL->SetBTop(pCNTL->BTop()->Bc);

        // added this line because trail was growing in recursive
        // loops with cuts before recursive call.  was written
        // for interpreted cuts, but seems to do the job here
        // as well.
        pCNTL->TrimTrail();
        CF = FALSE;
        continue;
        */
        
        /* cutd was used only inside or branches (see cclause.pro), but
           it appears it was ill-conceived and that normal cuts will
           suffice.  This is a bit worrisome, but removal of cutd's
           seems to fix the problems created by cutd's.
           
           Those problems were that two in a row would crash, because
           the second one couldn't find the label on the stack, and
           that cutd's generated from nested or's didn't reach up
           far enough.
           
           Well, the answer is A->B;C and A\=B and not(A).  Each of
           these is implemented by the compiler using a cut and an
           or, but, of course, it needs to be a cutd, not a real
           cut.  The cutd allows the or branch to be treated as
           a separate entity, and backtracking can flow around it.
           
           But this is a real rat's nest.  If then elses (-> ;) and 
           not's should be opaque to cuts the user puts in, but where
           do we cut to in those cases?
           
           For now, we just try to catch the failing cutd before it walks
           off the choice point stack looking for a label that has already
           been cut.
           */
        
      case Ocutd:                                     // short 
        xcode = code_ptr + * (cdSINTptr) code_ptr;
        
        while(pCNTL->BTop()->CLc != xcode)
          {
            pCNTL->SetBTop(pCNTL->BTop()->Bc);
            if (pCNTL->BTop() == NULL)
              pXCPT->Error(cutdE);
          }
        pCNTL->SetBTop(pCNTL->BTop()->Bc);
        code_ptr += cdSINT_L;
        continue;
        
      case Otry:
        CF = TRUE;
        //B = pCNTL->BTop(); //reeves just looking
        code_ptr += choice_point(FALSE, code_ptr);
        //B = pCNTL->BTop(); //reeves just looking
        continue;
        
      case Otry_me_or_else:
        
#ifdef BUG_TRACE
// if (codePLMTraceB && FALSE)
 Who = &Who_Or;
#endif
      case Otry_me_else:
        CF = TRUE;
        code_ptr += choice_point(TRUE, code_ptr);
        continue;
        
      case Oretry_me_else:                    // short 
        CF = TRUE;
        //rrdc pCNTL->BTop()->CLc = code_ptr + * (cdSINTptr) code_ptr;
        // B = pCNTL->BTop(); //rrdc must declare B to see the BTop
        pCNTL->BTop()->CLc = code_ptr + * (cdSINTptr) code_ptr; //rrdc
        code_ptr += cdSINT_L;
        
#ifdef BUG_TRACE
 plm_trace();  
#endif
        continue;
        
      case Oretry:
        CF = TRUE;
        pCNTL->BTop()->CLc = cdSINT_L + code_ptr;
        code_ptr += * (cdSINTptr) code_ptr;
        continue;
        
      case Otrust_me_2_else:
        code_ptr += cdSINT_L;
        
      case Otrust_me_else:
        CF = FALSE;
        //rrdc pCNTL->SetBTop(pCNTL->BTop()->Bc);
        B = pCNTL->BTop();                  // the ch pnt
        Discon = pCNTL->BTop()->Discon;     // extensn CLAUSE_BLK (if any)
        if(Discon)                          // is clause blk extended?
          {                                 // yes, do not pop ch pt yet
            pCNTL->BTop()->flags |= TRUSTED;
            pCNTL->BTop()->CLc = 
              //Discon->code.pbuf;            // tell ch pnt next blk code
              Discon->getCode();            // tell ch pnt next blk code
            
            //pCNTL->BTop()->Discon =         // extensn block also extended?
            //  Discon->info & extended ? Discon->clink : 0; 
            pCNTL->BTop()->Discon =         // extensn block also extended?
              Discon->IsContinued() ? Discon->getNext() : NULL; 
          }
        else                                // last or only block
          //B = pCNTL->PopBTop();           // pop ch pnt
          pCNTL->PopBTop();                 // pop ch pnt
        continue;
        

      case Otrust:
        CF = FALSE;
        //B = pCNTL->BTop();                // the ch pnt
        Discon = pCNTL->BTop()->Discon;     // extensn CLAUSE_BLK (if any)
        if(Discon)                          // is clause blk extended?
          {                                 // yes, do not pop ch pt yet
            pCNTL->BTop()->flags |= TRUSTED;
            pCNTL->BTop()->CLc = 
              //Discon->code.pbuf;            // tell ch pnt next blk code
              Discon->getCode();            // tell ch pnt next blk code
            
            //pCNTL->BTop()->Discon =         // extensn block also extended?
            //  Discon->info & extended ? Discon->clink : 0; 
            pCNTL->BTop()->Discon =         // extensn block also extended?
              (Discon && Discon->IsContinued()) ? Discon->getNext() : NULL; 
          }
        else                                // last or only block
          //B = pCNTL->PopBTop();           //rrdc
          pCNTL->PopBTop();                 //rrdc
        //rrdc pCNTL->SetBTop(pCNTL->BTop()->Bc);
        code_ptr += * (cdSINTptr) code_ptr;
        continue;
        

      case Ogoto:
        code_ptr += * (cdSINTptr) code_ptr;
        continue;
        
        /* 
           switch_on_term is called at the beginning of a sequence of clauses.
           It is an indexing optimization based on the first argument, X[0].
           If X[0] is a variable, then the clauses are searched sequentially.
           The code_ptr is set to the next instruction.
           
           If it is not a variable, then its value is used to jump to the
           index table that in turn jumps to the right clause.
           
           args: struct offset (cdSINT), list offset (cdSINT), 
           constant offset (cdSINT)
           
           Note that Type() returns:
           0 reference,
           
           1 struct, 
           2 list, 
           3 constant
           So the three arguments are the offsets into the code 
           for the index for struct, list and constants.
           
           If the argument's value is 0, then the predicate fails. 
           */
        
      case Oswitch_on_term:
#ifdef BUG_TRACE
//        if (codePLMTraceB && FALSE)
//          plmtrace_msg(aS("[s]CALL: Switch\n"));
 ATRACE("[s]CALL: Switch\n");
#endif
        //xi = X;
        xi = pHXL->GetX();
        while(TRUE)
          {
            if (xi->IsRef())
              {                                   // reference
                if (xi->IsUnbound())
                  {                               // unbound
                    code_ptr += 3 * cdSINT_L;
                    goto loop;
                  }
                else
                  xi = xi->getTerm();             // bound
              }
            else
              {
                // y_int will hold offset based on type
                switch(xi->getType())
                  {
                  case strucT: y_int = 0; break;
                  case listT: y_int = 1; break;
                  default: y_int = 2;
                  }
                x_int =         // code_ptr + reduced index
                  //*(cdSMINTptr)(code_ptr+cdSINT_L*((int)xi->getType()-1));
                  *(cdSMINTptr)(code_ptr+cdSINT_L*(y_int));
                if (x_int == 0)
                  {
#ifdef BUG_TRACE
//                    if (codePLMTraceB && FALSE) 
//                      plmtrace_msg(aS("[s]FAIL: Switch\n"));
 ATRACE("[s]FAIL: Switch\n");
#endif
                    LOOP_CHECK;
                    code_ptr = failure();
                  }
                else
                  {
#ifdef BUG_TRACE
//                    if (codePLMTraceB && FALSE) 
//                      plmtrace_msg(aS("[s]EXIT: Switch\n"));
 ATRACE("[s]EXIT: Switch");
#endif
                    code_ptr +=                 // the arg index switch
                      //x_int + ((int)xi->getType()-1) * (cdSINT_L);
                      x_int + (y_int) * (cdSINT_L);
                  }
                // reeves
                //B = pCNTL->BTop();            // the ch pnt
                Discon = pCNTL->BTop()->Discon; // extensn CLAUSE_BLK(if any)
                goto loop;
              }
          }
        
      case Oswitch_on_cons:
        code_ptr = switch_on_cons(code_ptr);
        continue;
        
      case Oswitch_on_struc:
        code_ptr = switch_on_struc(code_ptr);
        continue;

      case Oexit:              // only created from codeNewProve and friends 
        code_ptr = NULL;
        ret_code = FALSE;
        continue;

      case Ono_op:
         continue;
            
        
      default:   
        pXCPT->Error(badopcodeS);
      }
   }                                         // end of big while loop 
   
   } // end of try
   catch (STRptr msg)
      {
         std::cout << "**** Error: " << msg << NL;
         exit(0);
      }
   
   catch (LExcept &pE)
     {
       pE.AddExecInfo(*(Cur_goala), Cur_arity);
       
       if (pE.GetType() == ABORT || pE.GetType() == INTERNAL)
         throw(pE);                          // Don't show Prolog serious errs
       
       idepth = 0;                       // Build choice-pnt stack info
       ibstr = 0;
       maxbdepth = 12;                   // someday we'll parameterize this
       bX = pCNTL->BTop();
       eX = pCNTL->ETop();
       sB = "";
       sGoal = "";
       fRB = LON;

      while (idepth < maxbdepth && bX && eX)
        {
          if ((STACK_ELMptr)bX > (STACK_ELMptr)eX)
            {
              chWho = bX->Whoc;
              cBE = aS('+');
            }
          else
            {
              chWho = eX->Whoe;
              cBE = aS('-');
            }      
          
          if (chWho)
            {
              sGoal = *(chWho->pred_atom);
              
              if(fRB == LON && -1 != sGoal.Strstr(aS("rb$")) )
                {
                  tList = &Cur_cStack;
                  while( tList->IsList() )
                    {       // can write long list of semics (800)
                      tHead = tList->getListHead();
                      pWRIT->termWriteString(tHead, bBuf, 255, TRUE);
                      sB += bBuf;
                      sB += aS(";");
                      idepth++;

#ifdef WINDOWS
                      if((0 == idepth % 100) && winbreak())        // ray
                        pXCPT->Error(breakE);
#endif
                  tList = (tHead+1)->dref();
               }
               fRB = LOFF;
            }
            else if ( 0 <= sGoal.Strchr(aS('$')) ) ;
            else
            {
#ifdef _UNICODE
                    Lsprintf(bBuf, bbuf_size, aS("%c %ls/%d"), cBE,
                             (aCHAR*) sGoal, chWho->pred_arity);     
#else
                    Lsprintf(bBuf, bbuf_size, aS("%c %s/%d"), cBE,
                             (aCHAR*) sGoal, chWho->pred_arity);     
#endif
                    sB += bBuf;
                    sB += aS(";");
                    idepth++;
                  }
            }
          if (cBE == aS('+'))
            bX = bX->Bc;
          else
            eX = eX->Ee;
        }       
      
      if (idepth == maxbdepth)
        sB += aS("...stack continues...");
      else
        sB += aS("--- top of stack ---"); 
      
/*
      try
      {
         TERM debug_stack = pHXL->heapGETN(3);
         debug_stack->setStruct(debug_stack+1);
         (debug_stack+1)->setFA(pATAB->exceptionZdebugZstackA, 1);
         (debug_stack+2)->setTerm(pCNTL->get_debug_stack());

         pDDB->Assert('a', debug_stack);
      }
      catch(...)
      {
         ;
      }
*/
      //pWRIT->termWriteString(stack_list, sB, 1022, true);

      pE.AddExecCallStack(sB);
      
      // Insert in Prolog catch/throw stream & see if program
      // wants to handle them.
      if (LFALSE == throwPrologError(pE))
        throw(pE);
      else
        {
          code_ptr = failure();
          goto start;
        }
   }
   
#ifdef WAMPROFILE
   wamprf = fopen("d:\\ax\\engine\\wamprf.txt", "w");
   for (wbi=0; wbi<N_WAMOPS; wbi++)
     fprintf(wamprf, "%d   %d\n", wbi, bucket[wbi]);
   fclose(wamprf);
#endif
   PROBE_WAM_STOP;
   return(ret_code);
}
   
/* New prove is a bit of a dog. It is used for two purposes
**      executing a latent expression in a compiled file,
**      implementing the error handler.
**
** The idea is that NewProve will execute its code stream (given) as its
** argument. It well then return the P-Code system to exactly where it was
** before new Prove was executed. This means that we have to
**         ** Save off ALL Xi registers since we dont know how many
**            may be trashed.
**
**         ** Save any Permanents on the stack (our pcode engine
**       does not tell you where the current top of permanent
**       stack is - the code itself is responsible for manipulating
**       the stack - L is just the local frame Base.
**
**         ** Give the NewProve environment a choice point of its own
**       (necessary in case the new code tags its choice point) but
**       guarantee that the code is executed on failure just returns
**       you from the Prove() with a failure - this is the purpose
**       of the EXIT op code
**
**         ** Save off the PCODE state
**
** A further wrinkle is what to do if we are executing a cut_tag in the new 
** environment. We dare not trim passed the choice point we build (otherwise
** we lose the EXIT opcode and we never leave the new Prove environment,
** totally screwing things up. So, what we do is leave a space for the 
** argument to the cut_tag in the buffer we made to contain our exit
** opcode in the new choice points alternate code buffer. When we return from
** the Prove we check this buffer. If there is a term in it we continue the
** choice point trimming
** 
*/

int LExec::NewProve(CODEptr pc)
  {                            // prove code stream at pc in a NEW ENVIRONMENT
   short              err;
   CHOICE_POINTptr    oldB, newcp;
   CODEptr            oldCP, oldP;
   CODE               exittag[cdEXIT_L];
   CODEptr            exittagP;

   //CELLptr XI = pHXL->SaveX();
   Cell *XI = pHXL->SaveX();
   exittagP = (CODEptr) exittag;
   oldCP = CP;
   oldB = pCNTL->BTop();
   oldP = P;

   *(cdOPptr)exittagP = Oexit;
   *(TERMptr)(exittagP + cdOP_L) = NULL;  // set up space for the cut_tag term

   newcp = pCNTL->NewChoicePoint(0);

   newcp->CLc    = (CODEptr) exittagP; 
   newcp->Tagc   = NULL;
   newcp->Whoc   = NULL;
   newcp->HBc    = pHXL->HTop();
   newcp->Lc     = pHXL->LTop();
   newcp->NTVc   = 0;
   newcp->Bc     = pCNTL->BTop();
   newcp->Ec     = pCNTL->ETop();   
   newcp->TRc    = pCNTL->TRTop();
   //newcp->pdci = NULL;
   newcp->gc_pdci = NULL;
   newcp->clause_number = 0;
   newcp->imod = 0;
   newcp->flags  = 0;  //rrdc
   newcp->Discon = 0; //rrdc
   //rrdc newcp->BF = FALSE;
   //rrdc newcp->bTag = FALSE;
   
   pCNTL->SetBTop(newcp);
   try
     {
       err = Prove(pc, NULL);
     }
   catch(...)
     {
       pHXL->RestoreX(XI);
       throw;
     }

   if ( (err == 0) && (*(TERMptr)(exittagP + cdOP_L)) )  
     {                                            // we were cut_tagging 

// We're thinking of forbidding cuttags from crossing API call and
// NewProve boundaries.  A cuttag is now used to implement catch/throw
// so the better thing to do here is catch the term, which for now
// we can't do so lets just flag an error.  The old code is left here
// as a reference. 

       pHXL->RestoreX(XI);
       pXCPT->Error(cuttagE);    
/*
      if (B == newcp) 
         B = newcp->Bc;
         
      if ( cp = FindTag((TERM)(exittagP + cdOP_L),TRUE) ) 
      {
         // have a care; if newcp was the tagged choice point
         // then skip one more since newcp doesn't really exist,
         // it was just somewhere to put our exit code 

         if (cp == newcp)
            B = cp->Bc;
         else
            B = cp;
      }
      else
         B = oldB;
*/
     }
   else
     pCNTL->SetBTop(oldB);
   
   pHXL->SetLTop( newcp->Lc );
   pCNTL->SetETop(newcp->Ec);
   pCNTL->SetTRTop(newcp->TRc);
   pHXL->SetHTop( newcp->HBc );
   CP = oldCP;
   P = oldP;
   

   pHXL->RestoreX(XI);
   return(err);
}

/* codeCallProve is used by the cover functions to allow a C program to
 * call a loaded Prolog program.  It is called with X[0] already set
 * to the term to be called.  It is the argument of callA.  
 */

int LExec::CallProve(STRptr s, TERMptr tp)
{
   int               rc;
   CHOICE_POINTptr   newcp;
   CODEptr           oldCP;
   CODE              goal[cdTOP_L];
   CODEptr           g;
   CODEptr           exittagP;

#ifdef BUG_COVERCALL
 DUMP << "************************" << NL;
 DUMP << "*** Call CallProve" << NL;
 DUMP << "***" << NL;
 pTSVC->Dump(*tp);
 DUMP << NL << FLUSH;
#endif
#ifdef BUG_CALLS_REDOS
   DUMP << "***CALLPROVE*** Before Making Choice Point " << s << NL;
   DUMP << "X(0) = ";
   pTSVC->Dump(X(0));
   pCNTL->Dump();
#endif

   // Save I/O state, in case an exec inside of
   // a read, which could happen in the listener
   // debugger IDE environment.  For now, just
   // flag an error instead.
   if (pREAD->IsReading())
      pXCPT->Error(nestedexecE);


   exittagP = (CODEptr)ExitCode;                       // use static storage 
   *(cdOPptr)exittagP = Oexit;
   *(TERMptr)(exittagP + cdOP_L) = NULL;  // set up space for the cut_tag term 

   // get the htop before the string term, so we
   // don't miss it when we restore the heap top
   TERM htop = pHXL->HTop();
   //TERM *tp;

   // By setting X[0] here, we have lost the old setting.  This is the correct
   // value for the choice point we're building.  This doesn't appear to be a
   // problem, except for the case of the async threads allowed for the ARulesXL
   // debugger.  The async call might be waiting, when other LSAPI calls are
   // made, and it's copy of X[0] gets trashed before returning.  So the hack
   // is to fix it in the async call predicate.
   //
   // The better long term solution would be to do a SaveX RestoreX as we have
   // the luxury of doing with the ExecProves.  But because we close shop later with
   // calls, we would have to store the oldXs on the heap and in the choice point for
   // restoration.  For now we live with the hack.
   pPSTR->strStringTerm(s, tp);
   pHXL->SetXRef(0, *tp);

   oldCP = CP;

   g = goal;
   * (cdOPptr) g = Oexec;                              // Call op
   g += cdOP_L;
   * (cdMODIXptr) g = USER_MODULE;  // ls calls by default from user
   g += cdMODIX_L;
   * (cdATOMptr) g = pATAB->callA;                            // 20
   g += cdATOM_L;
   g += cdATOM_L;
   * (cdSMINTptr) g = 1;                               // the arity 

   newcp = pCNTL->NewChoicePoint(0);

   newcp->CLc  = (CODEptr) exittagP; 
   newcp->Bc   = pCNTL->BTop();
   newcp->Tagc = NULL;
   //rrdc newcp->bTag = FALSE;
   newcp->Whoc = NULL;
   //newcp->HBc  = pHXL->HTop();
   newcp->HBc  = htop;   // picked it up earlier, before the string-term
   newcp->Lc   = pHXL->LTop();
   newcp->NTVc = 1;                                    // used to be 0 
   newcp->Ec   = pCNTL->ETop();   
   newcp->TRc  = pCNTL->TRTop();
   //rrdc newcp->BF   = FALSE;
   newcp->flags = 0;                                   //rrdc
   newcp->Discon = 0;                                  //rrdc
   // *(TERM)(newcp+1) = X[0];
   *(TERM)(newcp+1) = pHXL->XVal(0);

   *tp = (TERM)(newcp+1);

   pCNTL->SetBTop(newcp);


#ifdef BUG_CALLS_REDOS
   DUMP << "***CALLPROVE*** Before Prove " << NL;
   DUMP << "X(0) = ";
   pTSVC->Dump(X(0));
   pCNTL->Dump();
#endif
   rc = Prove(goal, NULL);

   if ( (rc == 0) && (*(TERMptr)(exittagP + cdOP_L)) )  
       pXCPT->Error(cuttagE);                         // we were cut_tagging 
// We're thinking of forbidding cuttags from crossing API call and
// NewProve boundaries.  A cuttag is now used to implement catch/throw
// so the better thing to do here is catch the term, which for now
// we can't do so lets just flag an error. 

   CP = oldCP;

// *** SEE COMMENT IN REDOPROVE ***
if (rc == FALSE)                     // back out the choice point as well 
   {
#ifdef BUG_CALLS_REDOS
   DUMP << "***CALLPROVE*** After Prove = false, before clearing choice point" << NL;
   DUMP << "X(0) = ";
   pTSVC->Dump(X(0));
   pCNTL->Dump();
#endif
      pCNTL->PopBTop();
      // These calls were in and seem to be working before.  Only worked
      // when top of old stack was a CP, it wipes out Es.
      //pCNTL->SetBTop( pCNTL->BTop()->Bc );
      //pHXL->SetLTop( pCNTL->BTop()->Lc );
      //pCNTL->SetETop( pCNTL->BTop()->Ec );
      //pCNTL->SetTRTop( pCNTL->BTop()->TRc );
      //pHXL->SetHTop( pCNTL->BTop()->HBc );

#ifdef BUG_CALLS_REDOS
   DUMP << "***CALLPROVE*** After Prove = false, after clearing choice point" << NL;
   DUMP << "X(0) = ";
   pTSVC->Dump(X(0));
   pCNTL->Dump();
#endif
   }


#ifdef BUG_COVERCALL
 DUMP << "***" << NL;
 DUMP << "*** Exit CallProve rc = " << rc << NL << FLUSH;
 DUMP << "*******************************" << NL;
#endif
   
   return(rc);
}


/* codeCallProve is used by the cover functions to allow a C program to
 * call a loaded Prolog program.  It is called with X[0] already set
 * to the term to be called.  It is the argument of callA.  
 */

int LExec::CallProve(TERMptr tp)
{
   int               rc;
   CHOICE_POINTptr   newcp;
   CODEptr           oldCP;
   CODE              goal[cdTOP_L];
   CODEptr           g;
   CODEptr           exittagP;

#ifdef BUG_COVERCALL
 DUMP << "************************" << NL;
 DUMP << "*** Call CallProve" << NL;
 DUMP << "***" << NL;
 pTSVC->Dump(*tp);
 DUMP << NL << FLUSH;
#endif

   // Save I/O state, in case an exec inside of
   // a read, which could happen in the listener
   // debugger IDE environment.  For now, just
   // flag an error instead.
   if (pREAD->IsReading())
      pXCPT->Error(nestedexecE);

   exittagP = (CODEptr)ExitCode;                       // use static storage 
   *(cdOPptr)exittagP = Oexit;
   *(TERMptr)(exittagP + cdOP_L) = NULL;  // set up space for the cut_tag term 

   oldCP = CP;

   g = goal;
   * (cdOPptr) g = Oexec;                              // Call op
   g += cdOP_L;
   * (cdMODIXptr) g = USER_MODULE;  // ls calls by default from user
   g += cdMODIX_L;
   * (cdATOMptr) g = pATAB->callA;                            // 20
   g += cdATOM_L;
   g += cdATOM_L;
   * (cdSMINTptr) g = 1;                               // the arity 

   newcp = pCNTL->NewChoicePoint(0);

   newcp->CLc  = (CODEptr) exittagP; 
   newcp->Bc   = pCNTL->BTop();
   newcp->Tagc = NULL;
   //rrdc newcp->bTag = FALSE;
   newcp->Whoc = NULL;
   newcp->HBc  = pHXL->HTop();
   newcp->Lc   = pHXL->LTop();
   newcp->NTVc = 1;                                    // used to be 0 
   newcp->Ec   = pCNTL->ETop();   
   newcp->TRc  = pCNTL->TRTop();
   //newcp->pdci = NULL;
   newcp->gc_pdci = NULL;
   newcp->clause_number = 0;
   newcp->imod = 0;
   //rrdc newcp->BF   = FALSE;
   newcp->flags = 0;                                   //rrdc
   newcp->Discon = 0;                                  //rrdc
   // *(TERM)(newcp+1) = X[0];
   *(TERM)(newcp+1) = pHXL->XVal(0);

   *tp = (TERM)(newcp+1);

   pCNTL->SetBTop(newcp);

   rc = Prove(goal, NULL);

   if ( (rc == 0) && (*(TERMptr)(exittagP + cdOP_L)) )  
       pXCPT->Error(cuttagE);                         // we were cut_tagging 
// We're thinking of forbidding cuttags from crossing API call and
// NewProve boundaries.  A cuttag is now used to implement catch/throw
// so the better thing to do here is catch the term, which for now
// we can't do so lets just flag an error. 

   CP = oldCP;
// See comment in REDOPROVE at same code
if (rc == FALSE)                     // back out the choice point as well 
   {
#ifdef BUG_CALLS_REDOS
   DUMP << "***CALLPROVE*** After Prove = false, before clearing choice point" << NL;
   DUMP << "X(0) = ";
   pTSVC->Dump(X(0));
   pCNTL->Dump();
#endif
      pCNTL->PopBTop();
      // These calls were in and seem to be working before.  Only worked
      // when top of old stack was a CP, it wipes out Es.
      //pCNTL->SetBTop( pCNTL->BTop()->Bc );
      //pHXL->SetLTop( pCNTL->BTop()->Lc );
      //pCNTL->SetETop( pCNTL->BTop()->Ec );
      //pCNTL->SetTRTop( pCNTL->BTop()->TRc );
      //pHXL->SetHTop( pCNTL->BTop()->HBc );

#ifdef BUG_CALLS_REDOS
   DUMP << "***CALLPROVE*** After Prove = false, after clearing choice point" << NL;
   DUMP << "X(0) = ";
   pTSVC->Dump(X(0));
   pCNTL->Dump();
#endif
   }

#ifdef BUG_COVERCALL
 DUMP << "***" << NL;
 DUMP << "*** Exit CallProve rc = " << rc << NL << FLUSH;
 DUMP << "*******************************" << NL;
#endif
   
   return(rc);
}

// codeExecProve is used by the cover functions to allow a C program to
// execute a query with no backtracking.  It is called with X[0] already set
// to the term to be called.  It is the argument of callA.
// For this string version, we recapture the heap used creating the stringterm
// as well, but we don't on CallStr, (CallProve) because we want to make
// sure the callers terms is preserved.

int LExec::ExecProve(STRptr s, TERMptr tp)
{
   int               rc;
   CODE              goal[cdTOP_L];
   CODEptr           g;

   CHOICE_POINTptr    oldB, newcp;              // declarations from NewProve 
   CODEptr            oldCP, oldP;
   CODE               exittag[cdEXIT_L];
   CODEptr            exittagP;

#ifdef BUG_COVERCALL
 DUMP << "************************" << NL;
 DUMP << "*** Call ExecProve" << NL;
 DUMP << "*** " << s << NL;
 //pTSVC->Dump(*tp);
 DUMP << NL << FLUSH;
#endif

   // Save the X registers
   Cell *XI = pHXL->SaveX();
   // Save I/O state, in case an exec inside of
   // a read, which could happen in the listener
   // debugger IDE environment.  For now, just
   // flag an error instead.
   if (pREAD->IsReading())
      pXCPT->Error(nestedexecE);

   // get the htop before the string term, so we
   // don't miss it when we restore the heap top
   TERM htop = pHXL->HTop();
   //TERM *tp;
   pPSTR->strStringTerm(s, tp);
   pHXL->SetXRef(0, *tp);

   g = goal;
   * (cdOPptr) g = Oexec;                       // Call op
   g += cdOP_L;
   * (cdMODIXptr) g = USER_MODULE;  // ls calls by default from user
   g += cdMODIX_L;
   * (cdATOMptr) g = pATAB->callA;                     // 20
   g += cdATOM_L;
   g += cdATOM_L;
   * (cdSMINTptr) g = 1;                        // the arity 

   exittagP = (CODEptr) exittag;
   oldCP = CP;
   oldB = pCNTL->BTop();
   oldP = P;

   *(cdOPptr)exittagP = Oexit;
   *(TERMptr)(exittagP + cdOP_L) = NULL;  // set up space for the cut_tag term

   newcp = pCNTL->NewChoicePoint(0);

   newcp->CLc  = (CODEptr) exittagP; 
   newcp->Bc   = pCNTL->BTop();
   newcp->Tagc = NULL;
   //rrdc newcp->bTag = FALSE;
   newcp->Whoc = NULL;
   //newcp->HBc  = pHXL->HTop();                         // Heap 
   // we were called from engine's ExecStr, and the term being
   // called was built at *tp, so really, when we recover, we
   // can go back to that.  maybe not, let heapgc take care
   // of it...
   //newcp->HBc  = *tp;                                  // Heap
   newcp->HBc = htop;    // saved from before the string-term
   newcp->Lc   = pHXL->LTop();
   newcp->NTVc = 1;
   newcp->Ec   = pCNTL->ETop();   
   newcp->TRc  = pCNTL->TRTop();
   //newcp->pdci = NULL;
   newcp->gc_pdci = NULL;
   //newcp->clause_number = 0;
   newcp->imod = 0;
   //rrdc newcp->BF = FALSE;
   newcp->flags = 0;                                   //rrdc
   newcp->Discon = 0;                                  //rrdc
   //*(TERM)(newcp+1) = X[0];
   *(TERM)(newcp+1) = pHXL->XVal(0);

   *tp = (TERM)(newcp+1);
   
   pCNTL->SetBTop(newcp);

   rc = Prove(goal, NULL);
   /*
   try
     {
       rc = Prove(goal, NULL);
     }
   catch(exception &E)
     {
       pHXL->RestoreX(XI);
       const char *msg;
       msg = E.what();
       pXCPT->Error(internalE, aS("dot dot dot"));
//       throw;
     }
     catch(LExcept &LE)
     {
        pHXL->RestoreX(XI);
        throw;
     }
   catch(...)
     {
       pHXL->RestoreX(XI);
//       pXCPT->Error(internalE, aS("dot dot dot"));
       throw;
     }
   */
   
   if ( (rc == 0) && (*(TERMptr)(exittagP + cdOP_L)) )  
     {                                              // we were cut_tagging 
       pHXL->RestoreX(XI);
       //pSIO->RestoreioNow(ioOld);
       pXCPT->Error(cuttagE);
     }

   else
     pCNTL->SetBTop(oldB);
   
   //L = newcp->Lc;
   pHXL->SetLTop( newcp->Lc );
   pCNTL->SetETop(newcp->Ec);
   pCNTL->SetTRTop(newcp->TRc);
   //H = newcp->HBc;
   pHXL->SetHTop( newcp->HBc );
   CP = oldCP;
   P = oldP;
   
   pHXL->RestoreX(XI);
   //pSIO->RestoreioNow(ioOld);

#ifdef BUG_COVERCALL
 DUMP << "***" << NL;
 DUMP << "*** Exit ExecProve rc = " << rc << NL << FLUSH;
 DUMP << "*******************************" << NL;
#endif

   return(rc);
}

/* codeExecProve is used by the cover functions to allow a C program to
 * execute a query with no backtracking.  It is called with X[0] already set
 * to the term to be called.  It is the argument of callA.  
 */
int LExec::ExecProve(TERMptr tp)
{
   int               rc;
   CODE              goal[cdTOP_L];
   CODEptr           g;

   CHOICE_POINTptr    oldB, newcp;              // declarations from NewProve 
   CODEptr            oldCP, oldP;
   CODE               exittag[cdEXIT_L];
   CODEptr            exittagP;

#ifdef BUG_COVERCALL
 DUMP << "************************" << NL;
 DUMP << "*** Call ExecProve" << NL;
 DUMP << "***" << NL;
 pTSVC->Dump(*tp);
 DUMP << NL << FLUSH;
#endif

   // Save the X registers
   Cell *XI = pHXL->SaveX();
   // Save I/O state, in case an exec inside of
   // a read, which could happen in the listener
   // debugger IDE environment.  For now, just
   // flag an error instead.
   if (pREAD->IsReading())
      pXCPT->Error(nestedexecE);

   g = goal;
   * (cdOPptr) g = Oexec;                       // Call op
   g += cdOP_L;
   * (cdMODIXptr) g = USER_MODULE;  // ls calls by default from user
   g += cdMODIX_L;
   * (cdATOMptr) g = pATAB->callA;                     // 20
   g += cdATOM_L;
   g += cdATOM_L;
   * (cdSMINTptr) g = 1;                        // the arity 

   exittagP = (CODEptr) exittag;
   oldCP = CP;
   oldB = pCNTL->BTop();
   oldP = P;

   *(cdOPptr)exittagP = Oexit;
   *(TERMptr)(exittagP + cdOP_L) = NULL;  // set up space for the cut_tag term

   newcp = pCNTL->NewChoicePoint(0);

   newcp->CLc  = (CODEptr) exittagP; 
   newcp->Bc   = pCNTL->BTop();
   newcp->Tagc = NULL;
   //rrdc newcp->bTag = FALSE;
   newcp->Whoc = NULL;
   //newcp->HBc  = pHXL->HTop();                         // Heap 
   // we were called from engine's ExecStr, and the term being
   // called was built at *tp, so really, when we recover, we
   // can go back to that.  maybe not, let heapgc take care
   // of it...
   newcp->HBc  = *tp;                                  // Heap
   newcp->Lc   = pHXL->LTop();
   newcp->NTVc = 1;
   newcp->Ec   = pCNTL->ETop();   
   newcp->TRc  = pCNTL->TRTop();
   //newcp->pdci = NULL;
   newcp->gc_pdci = NULL;
   newcp->clause_number = 0;
   newcp->imod = 0;
   //rrdc newcp->BF = FALSE;
   newcp->flags = 0;                                   //rrdc
   newcp->Discon = 0;                                  //rrdc
   //*(TERM)(newcp+1) = X[0];
   *(TERM)(newcp+1) = pHXL->XVal(0);

   *tp = (TERM)(newcp+1);
   
   pCNTL->SetBTop(newcp);
   try
     {
       rc = Prove(goal, NULL);
     }
   catch(...)
     {
       pHXL->RestoreX(XI);
       //pSIO->RestoreioNow(ioOld);
       throw;
     }
   
   
   if ( (rc == 0) && (*(TERMptr)(exittagP + cdOP_L)) )  
     {                                              // we were cut_tagging 
       pHXL->RestoreX(XI);
       //pSIO->RestoreioNow(ioOld);
       pXCPT->Error(cuttagE);
     }

   else
     pCNTL->SetBTop(oldB);
   
   //L = newcp->Lc;
   pHXL->SetLTop( newcp->Lc );
   pCNTL->SetETop(newcp->Ec);
   pCNTL->SetTRTop(newcp->TRc);
   //H = newcp->HBc;
   pHXL->SetHTop( newcp->HBc );
   CP = oldCP;
   P = oldP;
   
   pHXL->RestoreX(XI);
   //pSIO->RestoreioNow(ioOld);

#ifdef BUG_COVERCALL
 DUMP << "***" << NL;
 DUMP << "*** Exit ExecProve rc = " << rc << NL << FLUSH;
 DUMP << "*******************************" << NL;
#endif

   return(rc);
}

/* codeRedoProve is used by the cover functions to allow a C program to
 * call a predicate again.  The caller's term is pointing to the
 * choice point stack's first X variable for the callA call.  
 */
int LExec::RedoProve(void)
{
   int               rc;
   CODEptr           oldCP;
   CODE              goal[cdOP_L];
   CODEptr           g;

   // CHOICE_POINTptr  B;

   g = goal;
   * (cdOPptr) g = Ofail;                // Call op

   oldCP = CP;

   rc = Prove(goal, NULL);

   CP = oldCP;

   // The choice point is not cleared by failure, so at this point the choice
   // is the one created for this LSAPI call.  We need to pop just the choice
   // point, not all of the other pointers.  Popping the other pointers led
   // to the bug that the code only worked when the top of the stack was a CP,
   // not an E.  Just popping B seems to be OK.
   //
   // But maybe we should add a flag indicating that this is a created choice
   // point and only pop it if that's the case.
if (rc == FALSE)                     // back out the choice point as well 
   {
#ifdef BUG_CALLS_REDOS
   DUMP << "***REDOPROVE*** After Redo = false, before clearing choice point" << NL;
   DUMP << "X(0) = ";
   pTSVC->Dump(X(0));
   pCNTL->Dump();
#endif
      pCNTL->PopBTop();
      // These calls were in and seem to be working before.  Only worked
      // when top of old stack was a CP, it wipes out Es.
      //pCNTL->SetBTop( pCNTL->BTop()->Bc );
      //pHXL->SetLTop( pCNTL->BTop()->Lc );
      //pCNTL->SetETop( pCNTL->BTop()->Ec );
      //pCNTL->SetTRTop( pCNTL->BTop()->TRc );
      //pHXL->SetHTop( pCNTL->BTop()->HBc );

#ifdef BUG_CALLS_REDOS
   DUMP << "***REDOPROVE*** After Redo = false, after clearing choice point" << NL;
   DUMP << "X(0) = ";
   pTSVC->Dump(X(0));
   pCNTL->Dump();
#endif
   }
//*/
   return(rc);
}

int LExec::InsertProve(CODEptr pc)
{
   short              err;
   CHOICE_POINTptr    oldB, newcp;
   CODEptr            oldCP, oldP;
   CODE               exittag[cdEXIT_L];
   CODEptr            exittagP;

//   Cell *XI = pHXL->SaveX();
   exittagP = (CODEptr) exittag;
   oldCP = CP;
   oldB = pCNTL->BTop();
   oldP = P;

   *(cdOPptr)exittagP = Oexit;
   *(TERMptr)(exittagP + cdOP_L) = NULL;  // set up space for the cut_tag term

   newcp = pCNTL->NewChoicePoint(0);

   newcp->CLc    = (CODEptr) exittagP; 
   newcp->Tagc   = NULL;
   newcp->Whoc   = NULL;
   newcp->HBc    = pHXL->HTop();
   newcp->Lc     = pHXL->LTop();
   newcp->NTVc   = 0;
   newcp->Bc     = pCNTL->BTop();
   newcp->Ec     = pCNTL->ETop();   
   newcp->TRc    = pCNTL->TRTop();
   //newcp->pdci = NULL;
   newcp->gc_pdci = NULL;
   newcp->clause_number = 0;
   newcp->imod = 0;
   newcp->flags  = 0;  //rrdc
   newcp->Discon = 0; //rrdc
   //rrdc newcp->BF = FALSE;
   //rrdc newcp->bTag = FALSE;
   
   pCNTL->SetBTop(newcp);
   err = Prove(pc, NULL);

   if ( (err == 0) && (*(TERMptr)(exittagP + cdOP_L)) )  
     {                                            // we were cut_tagging 
       pXCPT->Error(cuttagE);    
     }
//   else
//     pCNTL->SetBTop(oldB);
   
//   pHXL->SetLTop( newcp->Lc );
//   pCNTL->SetETop(newcp->Ec);
//   pCNTL->SetTRTop(newcp->TRc);
//   pHXL->SetHTop( newcp->HBc );
   CP = oldCP;
//   P = oldP;
   
//   pHXL->RestoreX(XI);
   return(err);
}

int LExec::ClearCall(void)
{

   // unwind real choice points til we get to an initial one,
   // created by CallStr or the like -- note use of Whoc, which
   // is assumed to have been NULL for a choicepoint created
   // outside of the main prove loop.
   while (pCNTL->BTop()->Whoc)
      pCNTL->SetBTop( pCNTL->BTop()->Bc );

   pCNTL->SetBTop( pCNTL->BTop()->Bc );

   pHXL->SetLTop( pCNTL->BTop()->Lc );
   pCNTL->SetETop( pCNTL->BTop()->Ec );
   pCNTL->SetTRTop( pCNTL->BTop()->TRc );
   pHXL->SetHTop( pCNTL->BTop()->HBc );

//printf("B = %p\n", pCNTL->BTop());
   return OK;
}

// Called by the interpreter, in alib, to implement a cut
TF LExec::p_cut_env()
{
   uintC           i;
   CHOICE_POINTptr pe, B;
   TERM            t;
   //TERM            h;

   t = (pHXL->XVar(0))->dref();

   if (t->IsInt()) 
     i = (unsigned int) t->getInt();
   else 
     pXCPT->Error(sysargE);

   pe = (CHOICE_POINTptr)  ((aBYTEptr)(pCNTL->GetStack()) + i);
   //TERM oldheap = pHXL->HTop();

   for (B = pCNTL->BTop(); B > pe; B = pCNTL->PopBTop())
      ;  //rrdc

   //TERM newheap = B->HBc;

   // In this space there are mscws used for backtracking
   // into dynamic clauses.  Those backtrack points need
   // to be cut at well.  Well, they are, but the iterators
   // in them are still live.  Need to free them up for
   // garbage collection.
   //for (h = newheap; h < oldheap; h++)
   //{
   //   if (h->IsMSCW())
   //      h->setUnused();
   //}

   return(TRUE);
}

// Called by the interpreter, in alib, to implement a cut for
// debug64.  We need to keep the choicepoint on the stack for
// reporting, so we just flag the choicepoint as cut for now
// and then really cut it when someone backtracks to it.
TF LExec::p_cut_debug64_env()
{
   if (pSTATE->m_debug64_cut == LOFF)
      return p_cut_env();

   uintC           i;
   CHOICE_POINTptr pe, B;
   TERM            t;
   //TERM            h;

   t = (pHXL->XVar(0))->dref();

   if (t->IsInt()) 
     i = (unsigned int) t->getInt();
   else 
     pXCPT->Error(sysargE);

   pe = (CHOICE_POINTptr)  ((aBYTEptr)(pCNTL->GetStack()) + i);
   //TERM oldheap = pHXL->HTop();

   for (B = pCNTL->BTop(); B > pe; B = B->Bc)
      B->debug64_cut = true;

   return(TRUE);
}

TF LExec::p_get_env()
{
   uintC      b, e;
   
   b = (uintC)((aBYTEptr)(pCNTL->BTop()) - (aBYTEptr)(pCNTL->GetStack()));
   e = (uintC)((aBYTEptr)(pCNTL->ETop()) - (aBYTEptr)(pCNTL->GetStack()));
   LASSERT( b != 0, aS("Null b in p_get_env"));
//#ifdef _DEBUG
//   if (b == 0) DebugBreak();
//#endif

   return( pTSVC->UnifyInt(b, pHXL->XVar(0)) && 
      pTSVC->UnifyInt(e, pHXL->XVar(1)) );
}

TF LExec::p_get_env1()
{
   uintC      b;
   
   b = (uintC)((aBYTEptr)(pCNTL->BTop()) - (aBYTEptr)(pCNTL->GetStack()));
   LASSERT( b != 0, aS("Null b in p_get_env"));
//#ifdef _DEBUG
//   if (b == 0) DebugBreak();
//#endif

   return( pTSVC->UnifyInt(b, pHXL->XVar(0)) );
}


// Called from interpreter's main loop to save the list which
// is the interpreter's call stack.  This is then used to
// provide the user with a call stack dump when an error occurs.
TF LExec::p_saveZstack()
{
   Cur_cStack = *((pHXL->XVar(0)))->dref();

   return TRUE;
}

// Called from the debug64 code to see if the use hit pause
// or not.  Acts like a break point.
TF LExec::p_debugZpause()
{
   if (pause_flag)
   {
      pause_flag = false;
      return TRUE;
   }
   else
      return FALSE;
}











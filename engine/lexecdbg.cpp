/****************************************************************************

lexecdbg.cpp -- tracing and spy point support


Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.

* 12/03/98 Ray Added breaks to InitDebug

****************************************************************************/


/* ----- Includes -------------------------------------------------------- */

#include "inc.h"
#include "pch.h"

#if defined(DOS) && defined(P16)
#include <conio.h>
#endif


/* ----- Internal definitions -------------------------------------------- */

#ifdef LANDFILL
aCHAR * ops[] =
{
/*  0 */      aS("no_op"),
/*  1 */      aS("get_con"),
/*  2 */      aS("get_nil"),
/*  3 */      aS("get_struc"),
/*  4 */      aS("get_list"),
/*  5 */      aS("put_unsafe"),
/*  6 */      aS("put_con"),
/*  7 */      aS("put_nil"),
/*  8 */      aS("put_struct"),
/*  9 */      aS("put_list"),
/* 10 */      aS("call"),
/* 11 */      aS("proceed"),
/* 12 */      aS("exec"),
/* 13 */      aS("escape"),
/* 14 */      aS("alloc"),
/* 15 */      aS("dealloc"),
/* 16 */      aS("cut"),
/* 17 */      aS("cutd"),
/* 18 */      aS("try_me_else"),
/* 19 */      aS("try"),
/* 20 */      aS("retry_me_else"),
/* 21 */      aS("retry"),
/* 22 */      aS("trust_me_else"),
/* 23 */      aS("trust"),
/* 24 */      aS("unify_con"),
/* 25 */      aS("unify_void"),
/* 26 */      aS("unify_nil"),
/* 27 */      aS("switch_on_term"),
/* 28 */      aS("goto"),
/* 29 */      aS("switch_on_cons"),
/* 30 */      aS("switch_on_struc"),
/* 31 */      aS("get_x_var"),
/* 32 */      aS("fail"),
/* 33 */      aS("trust_me_2_else"),
/* 34 */      aS("exit"),
/* 35 */      aS("get_y_var"),
/* 36 */      aS("unify_x_var"),
/* 37 */      aS("label"),
/* 38 */      aS("unify_unsafe"),
/* 39 */      aS("u_var_getlist"),
/* 40 */      aS("call_direct"),
/* 41 */      aS("exec_direct"),
/* 42 */      aS("unify_x_val"),
/* 43 */      aS("unify_y_val"),
/* 44 */      aS("put_x_val"),
/* 45 */      aS("put_y_val"),
/* 46 */      aS("unify_y_var"),
/* 47 */      aS("get_x_val"),
/* 48 */      aS("get_y_val"),
/* 49 */      aS("put_x_var"),
/* 50 */      aS("put_y_var"),
/* 51 */      aS("who_am_i"),
/* 52 */      aS("no_switch"),
/* 53 */      aS("no_try"),
/* 54 */      aS("try_me_or_else"),
/* 55 */      aS("mod_call"),
/* 56 */      aS("mod_exec")
};
#endif


/*
TF LExec::spying(MODIX imod, PATOM name, ARITY arity)
{   // is there a spy point on Name/Ar 
   // Need to look at this more closely, the old version
   // was just for compiled clauses.  This new fix is
   // for all predicates.  Don't know if that hurts or not.

   if (pDDB->IsSpying(imod, name, arity))
      return TRUE;
   else
      return FALSE;
/_*
   CLAUSE_BLKptr    ci;
   PRED_BLKptr      pi;

   if ((pi = pATAB->PredHead(Name)) == NULL)
      return(FALSE);

   for(ci = pi -> pclause; ci;  ci = ci -> clink)
	  if (ci -> arity == arity)
		 return( 0 != (ci -> info & spyC));
        
   return(FALSE);
   *_/
}
*/

void LExec::setwho()
{
   if (Who == NULL) return;

   Cur_file_ix = Who->file_ix;
   Cur_loadfile = pLOAD->GetLMfname(Cur_file_ix);
   Cur_goala  = (PATOM)Who->pred_atom;

   Cur_goal = *(Cur_goala);

   Cur_arity = (ARITY) Who->pred_arity;
   Cur_clause = 1;
   return;
}


void LExec::plmtrace_msg(aCHAR *fmt, ...)
{
  va_list   args;
  
  va_start(args, fmt);
  
//   vprintf(fmt, args);*/
//  if (pLOG->Logging())
//	 {
      //vfprintf(logfile, fmt, args);
//      pLOG->Write(aS("\n"));
//      pLOG->Write(fmt, args);
//      pLOG->Write(aS("\n"));
      //fflush(logfile);
//	 }
  va_end(args);
  
  return;
}


void LExec::plm_debug_header()
{
   setwho();
   return;
}

void LExec::plm_trace()
{
/*
   if (*Port=='C' && 0 == strcmp(Cur_goal,"clause"))
   {
      plmtrace_msg("CLAUSE ");
      plm_xs(Cur_arity);
      return;
   }
   else return;
*/
#ifdef LANDFILL
   if (*Port==aS('C') || *Port==aS('R') || *Port==aS('X')) 
	  depth++;

DUMP << "TR " << depth << "[" << Cur_clause << "] " << Port << SP << MODPREDAR(Cur_modix, Cur_goala, Cur_arity) << NL;


//   plmtrace_msg(aS("pt %d/[%d]%ls: %ls/%d"), depth, Cur_clause, Port, Cur_goal, 
//					 (int)Cur_arity);

//   plm_xs(Cur_arity);
   pHXL->DumpX(Cur_arity);

   if (*Port == aS('F') || *Port == aS('E')) 
	  depth--;
   return;
#endif
}

void LExec::plm_xs(ARITY ar)
{
  int i;
  TERM   t;
  
   if (ar == 0)
	 {
      plmtrace_msg(NL);
      return;
   }

   plmtrace_msg(aS("( "));
   for (i = 0; i < (int) ar; i++)
   {
      t = (pHXL->XVar(i))->dref();
//      plmtrace_msg("\n  X[%d] ", i); 
      if (i) plmtrace_msg(aS(", "));
      //pIO->termWriteLog(t);
//      plmprint_cell(t); 
	 }
  plmtrace_msg(aS(" )\n"));
}

/*
TF check_this_out()
{                          // can be used to custom control LANDFILL output 
   if (Cur_file_ix <= 1) return(FALSE);       not cpi.xpl or cpdcg.xpl 

   return(TRUE);
}
*/

void LExec::plmprint_cell(TERM t)
{
   int            i;
   ARITY            ar;
//   DynamicClause*   pdb;
   STRptr         name;
   //PATOM          a;
   
   plmtrace_msg(aS("%0*lx: "), PP_SIZE, * (intCptr) t);
   switch(t->getType())
	  {
//      case consT :
//         switch(pTSVC->SubType(t))
//         {
         case atomS:
            //a = t->getAtom();
            //if (i >= 0 && i < pATAB->GetMaxAtoms())            
            //{
               ar = t->getArity();
               name = *(t->getAtom());
               plmtrace_msg(aS("ATOM %s/%d "), name, ar);
               i = 1;
               while (ar-- > 0)
               {
                  plmtrace_msg(aS("\n  %s[%d]: "), name, i++);
                  plmprint_cell(++t);
               }
            //}
            //else
            //   plmtrace_msg(aS("ATOM - bad atom %0*lx"), PP_SIZE, *(intCptr)t);
            break;

         case intS:   plmtrace_msg(aS("INTG %d "), t->getInt());
                  break;

         /*
         case dbrefS:
            plmtrace_msg(aS("DBREF "));
            pdb = t->getDBRef()->getClause();
            plmtrace_msg(aS("\n  DB: "));
            plmprint_cell(pdb->getCode());
                  break;
                  */

            case doubleS:   plmtrace_msg(aS("DOUBLE "));
                        break;
            case singleS:   plmtrace_msg(aS("SINGLE "));
                        break;

            //case longS:   plmtrace_msg(aS("LONG "));
            //            break;

            //case mscwS:  plmtrace_msg(aS("MSCW "));
            //            break;

            case strS:   plmtrace_msg(aS("STRING "));
                        break;

//         default:    plmtrace_msg(aS("GARBAGE CONSTANT TYPE "));
//         }
//         break;

      case strucT:
         plmtrace_msg(aS("STRUCT "));
         plmtrace_msg(pHXL->cellname(t));
         t = t->getTerm();
         plmtrace_msg(aS("\n   --> "));
         plmprint_cell(t);
         break;

      case refT:
         if (t != t->getTerm())

         {
			  plmtrace_msg(aS("REF "));
			  plmtrace_msg(pHXL->cellname(t));
			  t = t->getTerm();
			  plmtrace_msg(aS("\n   --> "));
			  plmprint_cell(t);
         }
		 else
            plmtrace_msg(aS("UNBOUND "));
		 break;
		 
	  case listT:
		 plmtrace_msg(aS("LIST "));
		 plmtrace_msg(pHXL->cellname(t));
		 t = t->getTerm();
		 plmtrace_msg(aS("\n   --> "));
		 plmprint_cell(t);
		 break;
		 
	  default:
		 plmtrace_msg(aS("GARBAGE CELL TYPE "));
	  }
}


/* ----- External Functions ---------------------------------------------- */

void LExec::InitDebug(TF brks)
{
  breaks = brks;
   depth = 0;
#ifdef LANDFILL
   //remove("dump");
#endif
   return;
}

void LExec::DumpStack()
{
  CHOICE_POINTptr Bx;
  ENVptr          Ex;
  FILE*           f;
  
  //f = pLOG->GetLogFile();
  //if (f == NULL)
  f = fopen("dump", "a");
  
  fprintf(f, "\n-----Dumping Choice Points-----\n\n");
  
  for(Bx=pCNTL->BTop(); Bx!=NULL; Bx=Bx->Bc)
	 {
      fprintf(f, "B= %p\n", Bx);
#ifdef xBUG_CODE
      fprintf(f, "  HBc= %p, H[%d] ", Bx->HBc, Bx->HBc -Heap);
      if (IsMscw(Bx->HBc -2))
		  {
			 print_cell(Bx->HBc -2, 0);
		  }
      else fprintf(f, "\n");
#endif
	 }
  
  fprintf(f, "\n-----Dumping Environments-----\n\n");
  for(Ex=pCNTL->ETop(); Ex!=NULL; Ex=Ex->Ee)
	 {
      fprintf(f, "E = %p\n", Ex);
	 }
  //   fclose(f);
  return;
}

/*
TF LExec::p_spy()
{ // spy$(Name/Arity, OnOrOff)
   STUB(aS("p_spy"));
   return FALSE;

/_*
   TERM          t, t1, t2;
   MODIX         imod;
   PATOM         name;
   ARITY         arity;
   PredicateHead *ph;

   t = X(0);

   // a meta-predicate, so we must assume a colon and get the
   // the context module and term.

   // **** Need to use Goal to extract components ****

   // make sure first argument is of form Name/Arity
   if (! t1->IsStruct())
      pXCPT->Error(sysargE);
   t1 = t1->getTerm();
   if (pATAB->divA != t1->getAtom() || 2 != t1->getArity())
      pXCPT->Error(sysargE);

   t1++;
   t2 = (t1)->dref();  // Name
   t1++;
   t1 = (t1)->dref();  // Arity

   if ((! t2->IsAtom()) || (! t1->IsInt()))
      pXCPT->Error(sysargE);

   name = t2->getAtom();
   arity = (ARITY) t1->getInt();

   ph = pDDB->getPredicateHead(imod, name, arity);
   if (ph == NULL)
      return FALSE;

   t1 = X(1);      // set, reset or report spy point
   if (t1->IsInt())   // setting or resetting
   {
      if (1 == t1->getInt())
         ph->setSpy();
      else
         ph->unsetSpy();
   }

   if (ph->IsSpying())
      return pTSVC->UnifyInt(1, pHXL->XVar(1));
   else
      return pTSVC->UnifyInt(0, pHXL->XVar(1));
*_/
/_* It seems this can all be made simpler, as above, which we'll do
   for now and see if there are any gotchas later.

   TERM          t1, t2;
   PATOM         name;
   ARITY         arity;
   PRED_BLKptr   pi;
   CLAUSE_BLKptr ci;
   int           set_or_reset;               // 1 = set, 0 = reset, 2 = report 
   Cell          x;

// basically we need to set the spyC bit on info of the clause block
// for Name/Arity (THere will only be one such clause block).
// The problem is if there is no Clause Block. There may still be 
// Asserted predicates hanging off the dbase chain on Name. 
// So we contsruct a clause block - setting its type to 0 (unknown)
// and mark it spied upon. If compiled code is later loaded then
// we simply relabel the block as compC

   t1 = (pHXL->XVar(0))->dref();
   if (! t1->IsStruct())
      pXCPT->Error(sysargE);
   t1 = t1->getTerm();
   if (divA != t1->getAtom() || 2 != t1->getArity())
      pXCPT->Error(sysargE);

   t1++;
   t2 = (t1)->dref();       // Name
   t1++;
   t1 = (t1)->dref();  // Arity

   if ((! t2->IsAtom()) || (! t1->IsInt()))
      pXCPT->Error(sysargE);

   name = t2->getAtom();
   arity = (ARITY) t1->getInt();

   t1 = (pHXL->XVar(1))->dref();      // set, reset or report spy point
   if (! t1->IsInt())
      set_or_reset = 2;
   else
      set_or_reset = t1->getInt();

   if ((pi = pATAB->PredHead(name)) == NULL)
      return(FALSE);
   ci = pi -> pclause;
   //pTSVC->ATag(&x, consT | intS);

try_clause:
   while(ci)
	  if (ci -> arity == arity)
		 {
         switch(set_or_reset)
			  {
			  case 1:                           // set spy 
				 ci -> info |= spyC;
				 break;
				 
			  case 0:
				 ci -> info &= ~ spyC;
				 break;
			  }
			
         //pTSVC->AiValue(&x, (0 != (ci -> info & spyC)));
         x.setInt(0 != (ci -> info & spyC));
         return(pTSVC->Unify(pHXL->XVar(1), &x));
		 }
	  else
		 ci = ci -> clink;

   if (! ci )                               // no clause was found 
	  {
      
      // not sure why we need this, keep as comment until
      // determined its safe to destroy. see comment above
		 //ci = pATAB->adbNewClause(pi, 0);     // add new clause block at end 
		 //ci -> arity = arity;
		 //ci -> info = 0;                      // set up so type is 0 
		 //ci -> code.pbuf = NULL;
      
	  }
   goto try_clause;
   *_/
}
*/

#ifdef LANDFILL

// Internal LANDFILL functions 

void LExec::cdDebugPause()
{
/*
   errDebugMsg("Pause");
#ifndef WIN3
   getchar();
#endif
*/
}

void LExec::cdDebugChoice()
{
/*
   CHOICE_POINTptr   Bt;

   TERM               Xt;
   int               i;


   for (Bt = B->Bc; Bt != NULL; Bt = Bt->Bc)
   {
      errDebugMsg("\n  Choice at B = %0*lx", PP_SIZE, (long) Bt);

      Xt = (TERM) (Bt + 1);
      for (i = 0; i < Bt->NTVc; i++)
      {
         errDebugMsg("\nBX[%d] ", i);
         print_cell(Xt+i);
      }

      if (Bt->Tagc)
      {
         errDebugMsg("\n  TAG %0*lx ", PP_SIZE, (long) Bt->Tagc);
         print_cell(Bt->Tagc, 0);
      }
   }
   return;
*/
}

void LExec::cdHexDump(aBYTEptr p, int nbytes)
{
/*
  int i;

  errDebugMsg("\nHex Dump:");
  for(i = 0; i < nbytes; i++)
    {
      if (i % 16 == 8) errDebugMsg("  ");
      if (i % 16 == 0) errDebugMsg2("\n%lp ", "\n%p: ", p);
      errDebugMsg("%02x ", *p++);
    }
  errDebugMsg("\n");
*/
}

void LExec::cdPrintChoice(CHOICE_POINTptr Bt)
{
/*
   TERM Xt;
   int i;

   errDebugMsg2("\nCHOICE POINT B = %lp","\nCHOICE POINT B = %p", Bt);
   cdHexDump((aBYTEptr)Bt, 64);
   errDebugMsg2("\n  HBc  = %lp","\n  HBc  = %p", Bt->HBc);
   errDebugMsg2("\n  Lc   = %lp","\n  Lc   = %p", Bt->Lc);
   errDebugMsg2("\n  Ec   = %lp","\n  Ec   = %p", Bt->Ec);
   errDebugMsg2("\n  CPc  = %lp","\n  CPc  = %p", Bt->CPc);
   errDebugMsg2("\n  Bc   = %lp","\n  Bc   = %p", Bt->Bc);
   errDebugMsg2("\n  TRc  = %lp","\n  TRc  = %p", Bt->TRc);
   errDebugMsg2("\n  CLc  = %lp","\n  CLc  = %p", Bt->CLc);
   errDebugMsg( "\n  BF   = %d", Bt->BF);
   errDebugMsg( "\n  NTVc = %d", Bt->NTVc);
   errDebugMsg2("\n  Tagc = %lp","\n  Tagc = %p", Bt->Tagc);
   errDebugMsg2("\n  Whoc = %lp","\n  Whoc = %p", Bt->Whoc);
   Xt = (TERM)(Bt+1);
   for (i=0; i<Bt->NTVc; i++)
	  {
		 errDebugMsg("\n  BX[%d] = ", i);
		 print_cell(Xt+i, 0);
	  }
   errDebugMsg("\n");
*/
}

void LExec::cdDebugTrail()
{
/*
   TERM            t;
   TRAIL_CELLptr   tc;
   intC            i;

   errDebugMsg("\n----- TRAIL -----\n");
   tc = &Trail[TR];
   for (i = TR-1; i >= 0; i--)
   {
      t = (--tc) -> reset_cell;
      errDebugMsg("\nT[%d] %0*lx ", i, PP_SIZE, (long) t);
      print_cell(t, 0);
   }
*/
}

void LExec::cdDebugCode(CODEptr p, int length)
{
   cdOP      op;
   CODEptr   stop, start, q;
   cdSINT    i, x;
   PATOM     a;

   stop = p + length;
   start = p;
   DUMP << aS("--- Code Listing ---") << NL << FLUSH;
   while(p < stop)
   {
      op = * (cdOPptr) p;
      DUMP  << p 
            << std::setw(5) << p-start
            << std::setw(4) << op
            << SP << ops[op] << SP;
      p += cdOP_L;
      switch(op)
      {    
         case Owho_am_i:
            p -= cdOP_L;
            DUMP << ((CODE_HEADERptr) p) -> mod_ix << aS(":");
            a = (PATOM) ((CODE_HEADERptr) p) -> pred_atom;
            DUMP << PREDAR( *(a), ((CODE_HEADERptr) p) -> pred_arity ); 
            p += sizeof(CODE_HEADER) / sizeof(CODE);
            break;

         case Ono_op:       // no args
         case Ofail:
         case Oproceed:
         case Odealloc:
         case Ocut:
         case Otrust_me_else:
         case Ou_var_getlist:
         case Ounify_nil:
            break;
         
         case Oget_nil:     // Xi
         case Oget_list:
         case Oput_nil:
         case Oput_list:
         case Ounify_x_var:
         case Ounify_x_val:
            DUMP << aS("Xi ") << * (TERMptr) p;
            p += PTR_L;
            break;

         case Oget_x_var:   // Xi, Xj
         case Oget_x_val:
         case Oput_x_var:
         case Oput_x_val:
            DUMP << aS("Xi ") << * (TERMptr) p << aS(", ");
            p += PTR_L;
            DUMP << aS("Xj ") << * (TERMptr) p;
            p += PTR_L;
            break;

         case Ounify_y_var:         // Yi
         case Ounify_y_val:
            DUMP << aS("Yi ") << * (cdSMINTptr) p;
            p += cdSMINT_L;
            break;         

         case Ounify_unsafe:
            DUMP << aS("Yi ") << * (cdSMINTptr) p;
            p += cdSMINT_L;
            break;

         case Oget_y_var:      // Yi, Xj
         case Oget_y_val:
         case Oput_y_var:
         case Oput_y_val:
         case Oput_unsafe:
            DUMP << aS("Yi ") << * (cdSMINTptr) p << aS(", ");
            p += cdSMINT_L;
            DUMP << aS("Xi ") << * (cdSMINTptr) p;
            p += PTR_L;
            break;

         case Ounify_void:          // short int
         case Oalloc:
         case Olabel:
            DUMP << * (cdSINTptr) p << SP;
            p += cdSINT_L;
            break;

         case Ocutd:
         case Oretry_me_else:
         case Oretry:
         case Otrust:
         case Ogoto:
         case Otrust_me_2_else:  
            DUMP << aS(" offset ") << * (cdSINTptr) p + (p - start);
            p += cdSINT_L;
            break;


         case Otry_me_or_else:
         case Otry_me_else:
         case Otry:
            DUMP << aS(" offset ") << * (cdSINTptr) p + (p - start) << aS(", ");
            p += cdSINT_L;
            DUMP << aS("NTV ") << * (cdSINTptr) p << aS(", ");
            p += cdSINT_L;
            break;

         case Oget_con:        // Constant, Xi
         case Oput_con:
            DUMP << * (TERM) p;
            DUMP << aS(", ");
            p += CELL_L;
            DUMP << aS("Xi ") << * (TERMptr) p;
            p += PTR_L;
            break;

         case Oescape:
            DUMP << *(cdESCAPEptr)p;
            p += cdESCAPE_L;
            break;

         case Oexec:
         case Ocall:   
         case Omod_exec:
         case Omod_call:   
            DUMP << " module:" << *(cdMODIXptr)p << SP;
            p += cdMODIX_L;
         case Oget_struc:  // functor, arity, (Xi or short or null)
         case Oput_struc:
            DUMP << * (cdATOMptr) p << SP;
            p += cdATOM_L;
            if (op == Ocall || op == Oexec || op == Omod_call || op == Omod_exec)
            {
               DUMP << * (cdATOMptr) p;
               p += cdATOM_L;
            }
            DUMP << aS(" / ") << * (cdSMINTptr) p;
            p += cdSMINT_L;
            // now figure third (optional arg
            if (op == Oget_struc || op == Oput_struc)  // Xi
            {
               DUMP << aS(", Xi ") << * (TERMptr) p;
               p += PTR_L;
            }
            else if (op == Ocall || op == Omod_call) // short 
            {
               DUMP << aS(", ") << * (cdSINTptr) p;
               p += cdSINT_L;
            }
            // else no 3rd arg
            break;

         case Oexec_direct:
         case Ocall_direct:
            DUMP << aS("code* ") << * (CODEhnd) p;
            p += cdMODIX_L + 2 * cdATOM_L + cdSMINT_L;               /* the sizes from Ocall & Oexec */
            if (op == Ocall_direct)
            {
               DUMP << aS(", ") << * (cdSINTptr) p;
               p += cdSINT_L;
            }
            break;

         case Ounify_con:
            DUMP << * (TERM) p;
            p += CELL_L;
            break;


         case Oswitch_on_term:
            // three short ints
            // we map these to match what the pcode expects
            q = p;
            for (i=0; i<3; i++)
            {
               if (* (cdSINTptr) p == 0)
                  DUMP << aS(" offset fail ");
               else
                  DUMP << aS(" offset ")
                       << i * (cdSINT_L) + * (cdSINTptr) p + (q - start);
               p += cdSINT_L;
            }
            break;

         case Oswitch_on_cons:
            // short size, (size x |CELL|LABEL|)
            x = * (cdSINTptr) p;
            i = 0;
            //errDebugMsg("%d: ", * (cdSINTptr) p);
            p += cdSINT_L;
            while(x--)
            {
               DUMP << aS("  ") << * (TERM) p;
               p += CELL_L;
               DUMP << aS("  offset[") << * (cdSINTptr) p + (p - start) << aS("]");
               p += cdSINT_L;
            }
            break;

         case Oswitch_on_struc:
            // short size, (size x |NAME|ARITY|LABEL|)
            x = * (cdSINTptr) p;
            i = 0;
            DUMP << * (cdSINTptr) p << aS(": ");
            p += cdSINT_L;
            while(x--)
            {       
               // functor
               DUMP << SP << * (cdATOMptr) p;
               p += cdATOM_L;
               // arity
               DUMP << aS("/") <<  * (cdSMINTptr) p;
               p += cdSMINT_L;
               // label
               DUMP << aS("  offset[") << * (cdSINTptr) p + (p - start) << aS("] ");
               p += cdSINT_L;
            }
            break;

         default:
            DUMP << aS("Unknown op");
      }
      DUMP << NL << FLUSH;
   }
   DUMP << NL << aS("--- end ---") << NL << FLUSH;
}

#endif

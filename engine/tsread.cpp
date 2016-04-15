/****************************************************************************
* 
* tsread.cpp -- Term reader
* 
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
* 
* $Log: tsread.cpp,v $
* Revision 1.5  2007/01/29 22:01:40  dennis
* better flatten
*
* Revision 1.4  2006/12/02 20:38:00  mary
* Recognize {} as an atom like [].
*
* Revision 1.3  2004/01/21 22:03:30  dennis
* most of dcg working in reader
*
* Revision 1.2  2003/09/22 13:23:01  dennis
* stuff
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.33  2002/12/02 18:25:12  dennis
* Converted to Visual Studio .NET.
*
* Revision 1.32  2002/05/15 16:59:09  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.31  2002/03/21 01:25:10  dennis
* allow separator in file names to be either tilt on slash, converts
* to correct one for Unix or Windows
*
* Revision 1.30  2002/02/13 19:56:57  dennis
* fixed short bug, floatToReal, and real ** 1
*
* Revision 1.29  2002/02/04 17:21:00  dennis
* New lreal wrapper class on reals, start of number options:
* decimals: real/float, floats: single/double.  Created lmath.h/cpp
* but nothing in them yet.  Work to be done on sorting out mixed mode
* arithmetic for different options.  Also - casts removed, as they
* were causing problems.
*
* Revision 1.28  2002/01/28 06:29:20  dennis
* changes for parsing numbers, handling different options
*
* Revision 1.27  2002/01/06 20:31:29  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.26  2001/11/13 01:03:13  dennis
* synced engine and compiler
*
* Revision 1.25  2001/11/13 00:41:18  dennis
* updated documentation and use of .cfg and mode parameters, and
* created an documented amzi.cfg file.
*
* Revision 1.24  2001/10/05 19:15:18  dennis
* string streams, function streams working, reals set up to not require
* m_peng, just passed in parameters as necessary
*
* Revision 1.23  2001/10/05 17:07:02  ray
* Added real things. Fixed some bugs. Stopped adding '.' in prep
*
* Revision 1.22  2001/10/02 16:05:22  dennis
* changed streams, cleaner interface to lex, reimplemented
* readers as buffer between the two
*
* Revision 1.21  2001/09/19 02:35:33  dennis
* fixed io stuff
*
* Revision 1.20  2001/09/11 04:34:56  dennis
* cleaned up some io stuff, got consult working, etc.
*
* Revision 1.19  2001/09/08 15:27:57  dennis
* compiler working without calling fopen or fleopen, all open calls
*
* Revision 1.18  2001/09/04 01:57:01  dennis
* changed stream vector to reuse empty slots
*
* Revision 1.17  2001/08/05 19:11:20  dennis
* made unload work for .plm files again
*
* Revision 1.16  2001/08/02 18:51:00  dennis
* merge of new streams complete
*
* Revision 1.15  2001/08/01 20:18:00  ray
* removed tswrite.cpp & sio.cpp, added streams.cpp sowrite.cpp
*
* Revision 1.14  2001/07/21 00:39:47  dennis
* added garbage collector for strings and things
*
* Revision 1.13  2001/07/10 16:51:32  dennis
* added more memory leak checking tools, all clean so far
*
* Revision 1.12  2001/06/27 15:15:11  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.11  2001/04/16 05:21:14  dennis
* hacked together some fixes for sio/lex to be better friends,
* merged other changes, added new samples
*
* Revision 1.10  2001/04/02 21:50:13  dennis
* got debugger working again
*
* 
*  1999/12/30 Ray removed all sub-atom processing to lex
* 
****************************************************************************/

#include "inc.h"
#include "pch.h"
#include "lex.h"

#ifdef LANDFILL
#define noBUG_READ
#endif

#ifdef BUG_READ
aCHAR * token_names[] =
{
   aS("T_ZERO"),
   aS("T_TERM"),  // 1
   aS("T_VAR"),
   aS("T_INT"),
   aS("T_FLOAT"),
   aS("T_DOUBLE"),
   aS("T_ATOM"),
   aS("T_PREOP"),
   aS("T_POSTOP"),
   aS("T_INFOP"),
   aS("T_MPREOP"),
   aS("T_MPOSTOP"),
   aS("T_FUNCTOR"),
   aS("T_STR"),
   aS("T_LONG"),
   aS("T_FIXED"),
   aS("T_REAL"),
   aS("T_RATOR"),
   aS("T_NEST"),
   aS("T_CHAR"),
   aS("T_EOF"),
   aS("T_DEFINED"),
   aS("T_UNDEFINED")   //22
};
#endif

TermReader::TermReader(LEngine *peng)
{
   m_peng = peng;
   p_stack     = NULL;
   con_stack   = NULL;
   vars        = NULL;
   //readBuffer  = NULL;

   prec_commaA = 1000;                         // should be same as alib says.
   b_reading = false;
};

TermReader::~TermReader()
{
   delete[] p_stack;
   delete[] con_stack;
   delete[] vars;
   //delete[] readBuffer;
};

void TermReader::Init(uintC pread_depth, LFLAG stresc, uintC maxvars, 
                      intC buflen)
{
   bStringEsc = stresc;        // initial setting, can be changed through mode
   ReadDepth  = pread_depth;
   m_buflen   = buflen;

   LNEW(p_stack, PARSE_STACK[ReadDepth], aS("miscellaneous"));
   //if (p_stack == NULL)
   //   pXCPT->Error(outofmemE, aS("Parse Stack"));

   LNEW(con_stack, pCONTEXT[ReadDepth], aS("miscellaneous"));
   //if (con_stack == NULL)
   //   pXCPT->Error(outofmemE, aS("Context Stack"));

   LNEW(vars, VARNAME[maxvars], aS("miscellaneous"));
   //if (vars == NULL)
   //   pXCPT->Error(outofmemE, aS("Reader Vars"));

   return;
}

int TermReader::READERROR(ErrNo x)
{
   // this is better handled by user using catch/throw,
   // so we no longer support it.
   //if (pSTATE->m_readerrors)
   pXCPT->Error(x);
   
   return FALSE;
}

TF TermReader::p_read_term()
// read_term$(FILEID, TERM, OPTIONS)
//    called from alib for various flavors of read,
//    HANDLE is set to current_input for read/1 in alib.
// Term reading starts here, goes to TermReader::Read next.
{
   int h;
   TERM      rterm;

   TERM fileid_T = X(0);
   //if(!pIO->streamIndex(fileid_T, h))
   //   return FALSE;
   h = pIO->streamIndex(fileid_T);

#ifdef BUG_READ
 DUMP << "p_read_term h = " << h << NL << FLUSH;
#endif
   //pIO->current_input = h;   // no, this might be from a read/2, so don't want to change current_input
   //pLEX->set_read_handle(h);  // just let LEX know where we're reading from
   bool status = Read(h, &rterm);

   return status ? pTSVC->Unify(pHXL->XVar(1), rterm) : FALSE;
}

bool TermReader::Read(int h, TERMptr pt)
// called from TermReader::p_read_term(), LEX knows what input stream to use
// calls TermReader::tmGetToken() and TermReader::Action().
{             //  read next term from file - then set up code and error catcher
   read_err_index = -1;                        // support p_read for read/2
   top_con_stack = 1;
   top_p_stack = 0;
   //   pLEX->resetReadBuf();
   next_var = 0;
   con_stack[0].r_context = bottomC;
   con_stack[0].r_prec    = con_stack[1].r_prec = MAXPREC;
   con_stack[0].r_p_stack = con_stack[1].r_p_stack = top_p_stack + 1;
   con_stack[1].r_context = cntxt = inargC;
   expect = Erand;
   maxprec = MAXPREC;
   stashed_token = FALSE;
#ifdef BUG_READ
 FILL("\n----- READER START -----\n");
 DUMP << "Read h = " << h << NL << FLUSH;
#endif
   pLEX->initReader(h);
   try 
     {
		 //TOKEN_ lexeme;
		 do
			{
			  //lexeme = tmGetToken();                     // goes to lex
			  tmGetToken();                     // goes to lex
#ifdef xBUG_READ
			  debug_parse();
#endif
			} while( ! action());
		 
#ifdef BUG_READ
 DUMP << "Read: " << NL << FLUSH;
 pTSVC->Dump(p_stack[1].tv.t_tvalue);
 FILL("\n----- READER END -----\n" << FLUSH);
#endif
		 pLEX->finishReader();
		 *pt = p_stack[1].tv.t_tvalue;
	  } 
	catch (LExcept &pE)
	  {
		 if (pE.GetType() == READ)
			pLEX->ReadErr(pE);
       pLEX->errorResetReader();
       pLEX->finishReader();
		 throw(pE);
	  }
	return true;
}

//-----------------------
// Private Functions
//

/*
int TermReader::read__()                     // calls Read()
{                                            // this the p_read for read/2
  int h;
  TERM      rterm;

  TERM T = (pHXL->XVar(1))->dref();
  if(!pIO->streamIndex(T, h))
	 return false;

  //pIO->current_input = h;
  pLEX->set_input_stream(h);
  //  pLEX->reader = pIO->stream[h];

#ifdef BUG_READ
// DUMP << "Reading from reader: " << pLEX->reader->name << NL;
// pLEX->dump_readers();
#endif

 bool status = Read(&rterm);

 return status ? pTSVC->Unify(pHXL->XVar(0), rterm) : false;
}
*/

  /*
   * really does the work - shift/reduce engine 
   * A token is pushed onto p_stack and possibly an action taken
   * involving a reduction stage. When the stack is at the point of
   * 
   * | atom '.' |
   * | term(t)  |
   * ------------
   * 
   * and we are in bottomC context then we are done. Term read is t.
   */
/*
TERM TermReader::read_term_()
{
  TOKEN_ lexeme;
#ifdef BUG_READ
 FILL("\n----- READER START -----\n");
#endif

   do
     {
       lexeme = tmGetToken();                     // goes to lex
#ifdef xBUG_READ
       debug_parse();
#endif
     } while( ! action());

#ifdef BUG_READ
 DUMP << "Read: " << NL << FLUSH;
 pTSVC->Dump(p_stack[1].tv.t_tvalue);
 FILL("\n----- READER END -----\n" << FLUSH);
#endif

   return( p_stack[1].tv.t_tvalue);
}
*/
TF TermReader::action()
{
   int thisContext, ps; 
   //TOKEN_ type, lexeme;
   TOKEN_ type;

   while( TRUE )
   {
      type = cur_p_stack.t_type;
      switch(type)
      {
      case T_DOT:
          if (expect == Eop)
            if (cntxt == bottomC)
              return(collapse());
            else if (cntxt == inargC)
              {
                collapse_con();
                CHECKDEPTH(top_p_stack);
                p_stack[++top_p_stack].t_type = T_DOT;
                break;                          // and try again 
              }
          cur_p_stack.t_type = T_ATOM;          // else set to atom & try again
          set_cur_a_value( pATAB->periodA );
          break;
          
      case T_ATOM:
          if (cur_a_value() == pATAB->eofA)
            if (top_p_stack == 1)              // first token read in 
              {                                // force EOF term and done 
                p_stack[1].t_type = T_TERM;
                p_stack[1].tv.t_tvalue = pHXL->HTop();
                pHXL->heapGET()->setAtom(pATAB->eofA);
                return(TRUE);
              }
            else
              if (expect != Erand)
              {
                pXCPT->Error( eofE, (STRptr)*(pLEX->stream_reader->getStream()->get_name()) );             // fall through
              }
      case T_STR:
      case T_TERM:
      case T_INT:
      case T_CHAR:
      case T_LONG:
      case T_FIXED:
      case T_VAR:
      case T_DOUBLE:
      case T_FLOAT:
      case T_REAL:
          if (expect == Erand)
            {
              expect = Eop;
              start_arg();
              return(FALSE);
            }
          else
            pXCPT->Error(randE);
          
      case T_COMMA:
          if (expect != Eop)
            {                                   // punt it to an atom 
              cur_p_stack.t_type = T_ATOM;
              set_cur_a_value( pATAB->commaA );
              break;
            }
          else if (cntxt == inlistC || cntxt == instrucC)
            {
              -- top_p_stack;                   // toss it away 
              expect = Erand;
              return(FALSE);
            }
          else                   // punt the comma to an infix op and retry it 
            {
              cur_p_stack.t_type = T_INFOP;
              set_cur_a_value( pATAB->commaA );
              break;
            }
          
      case T_PREOP:
          if (expect != Erand)
            pXCPT->Error(randE);
                        // now have to rule out that the op was really an atom 
          tmGetToken();
          type = cur_p_stack.t_type;
          if (type == T_RPAR  || type == T_RBRACK || type == T_RBRACE ||
              type == T_COMMA || type == T_INFOP  || type == T_VBAR ||
              type == T_DOT)
            {
              pop_token();
              cur_p_stack.t_type = T_ATOM;
              break;                           // and try again 
            }
          pop_token();
          if (cntxt <= inparsC)
            EnterContext(inargC, RightPrec( T_PREOP, cur_a_value() ));
          else
            EnterContext(cntxt, RightPrec( T_PREOP, cur_a_value() ));
          return(FALSE);
          
      case T_POSTOP:
          if (expect == Erand)                  // punt it to atom 
            {
              cur_p_stack.t_type = T_ATOM;
              break;
            }
          if ( ! ( pATAB->getPrecedence(cur_a_value(),1) > maxprec))
            {
              EnterContext(cntxt, RightPrec( T_POSTOP, cur_a_value() ));
              SetCurrentPStack(top_p_stack - 1);
              expect = Eop;
            }
          else
            {                            // reduce what is on the stack first 
              ps = top_p_stack;
              collapse_con();
              CHECKDEPTH(top_p_stack);   // move the op down over unused space 
              p_stack[++top_p_stack].t_type = T_POSTOP;
              set_cur_a_value( p_stack[ps].tv.t_avalue );
              break;                            // and try again 
            }
          return(FALSE);
          
      case T_INFOP:
          if (expect == Erand)                  // punt it to atom 
            {
              cur_p_stack.t_type = T_ATOM;
              break;
            }
          else if ( !( pATAB->getPrecedence(cur_a_value(),2) > maxprec ) )
            {
 // have .. op a1 inf_op on p_stack, so swap this to .. op (a1 inf_op ... 
              if ( cntxt <= inparsC )
                EnterContext(inargC, RightPrec( T_INFOP, cur_a_value() ));
              else
                EnterContext(cntxt, RightPrec( T_INFOP, cur_a_value() ));  
              SetCurrentPStack(top_p_stack - 1);
              expect = Erand;
            }
          else                            // reduce what is on the stack first 
            {
              ps = top_p_stack;
              collapse_con();
              CHECKDEPTH(top_p_stack);   // move the op down over unused space 
              p_stack[++top_p_stack].t_type = T_INFOP;
              set_cur_a_value( p_stack[ps].tv.t_avalue );
              if (cur_a_value() == pATAB->commaA)
                p_stack[top_p_stack].t_type = T_COMMA;
              break;                             // and try again 
            }
          return(FALSE);
          
      case T_MPREOP:                         // is it a pre-op or an atom ?? 
          cur_p_stack.t_type = expect == Erand ? T_PREOP : T_INFOP;
          break;          // assumed it was a preop and letting T_PREOP decide 
          
      case T_MPOSTOP:
          if (expect == Erand)
            {
              cur_p_stack.t_type = T_ATOM;
              break;
            }
          tmGetToken();
          type = cur_p_stack.t_type;
          pop_token();
          if (type == T_RPAR || type == T_RBRACK || type == T_VBAR ||
              type == T_RBRACE || type == T_INFOP || type == T_DOT
              )
            cur_p_stack.t_type = T_POSTOP;
          else
            cur_p_stack.t_type = T_INFOP;
          break;
          
      case T_RBRACK:
          if (expect != Eop)
            {
              if (top_p_stack == GetCurrentPStack() && cntxt == inlistC)
                {
                  collapse_con();                // will pick up [] 
                  expect = Eop;
                  return(FALSE);
                }
              pXCPT->Error(delimE);
            }
          if (cntxt == inlistC)
            {
              collapse_con();        // but this hasn't terminated the list so:
            // but this hasn't terminated the list so
            // bigdig pHXL->heapPUSH(atomS | consT | pTSVC->ILong(nilA));
            //pHXL->heapPUSH(cellATOM(nilA));
            pHXL->heapGET()->setAtom(pATAB->nilA);
              expect = Eop;
              return(FALSE);
            }
          else if (cntxt == inargC)
            {            // reduce the last context then go back and try again 
              collapse_con();
              CHECKDEPTH(top_p_stack);    // push ) down over squished p_stack 
              p_stack[++top_p_stack].t_type = T_RBRACK;
              break;
            }
          else if (cntxt == incdrC)
            {
              collapse_con();
              expect = Eop;
              return(FALSE);
            }
          else
            pXCPT->Error(delimE);
          
      case T_VBAR:
          if (expect != Eop)
            {                                 // punt it to an atom 
              cur_p_stack.t_type = T_ATOM;
              set_cur_a_value( pATAB->vbarA );
              break;
            }
          
          if (cntxt == inargC)
            {
              collapse_con();
              CHECKDEPTH(top_p_stack);
              p_stack[++top_p_stack].t_type = T_VBAR;
              break;
            }
          else if (cntxt == inlistC)
            {
              EnterContext(incdrC, prec_commaA - 1);
              -- top_p_stack;
              expect = Erand;
              return(FALSE);
            }
          else
            pXCPT->Error(delimE);
      case T_LPAR:
          thisContext = inparsC;
          goto l1;
      case T_LBRACE:
          thisContext = inbraceC;
          goto l1;
      case T_LBRACK:
          thisContext = inlistC;
        l1:
          if (expect != Erand)
            pXCPT->Error(randE);
          start_arg();                           // sets context to inargC
          EnterContext( thisContext, MAXPREC);
          -- top_p_stack;
          return(FALSE);
          
      case T_RBRACE:
//        if (expect != Eop)
//            pXCPT->Error(delimE);
         // Check for {}
         if (expect != Eop)
            {
              if (top_p_stack == GetCurrentPStack() && cntxt == inbraceC)
                {
                  collapse_con();                // will pick up [] 
                  expect = Eop;
                  return(FALSE);
                }
              pXCPT->Error(delimE);
            }
          if(cntxt == inbraceC)
            {
              collapse_con();
              expect = Eop;
              return(FALSE);
            }
          goto l2;
/*
// Check for {}
         if (expect != Eop)
            pXCPT->Error(delimE);
          if (cntxt == inbraceC)
            {
              collapse_con();
              expect = Eop;
              return(FALSE);
            }
        
          if (cntxt == inargC)
            {            // reduce the last context then go back and try again 
              collapse_con();
              cntxt = GetCurrentContext();
              CHECKDEPTH(top_p_stack);// push ) or } down over squished p_stack
              p_stack[++top_p_stack].t_type = T_RBRACE;
              break;
            }
          pXCPT->Error(delimE);
*/
      case T_RPAR:
          if (expect != Eop)
            pXCPT->Error(delimE);  
          if ((cntxt == inparsC) || (cntxt == instrucC))
            {
              collapse_con();
              expect = Eop;
              return(FALSE);
            }
        l2:
          if (cntxt == inargC)
            {            // reduce the last context then go back and try again 
              collapse_con();
              cntxt = GetCurrentContext();
              CHECKDEPTH(top_p_stack);// push ) or } down over squished p_stack
              p_stack[++top_p_stack].t_type = 
                (type == T_RPAR) ? T_RPAR : T_RBRACE;
              break;
            }
          pXCPT->Error(delimE);
          
      case T_FUNCTOR:
          if (expect != Erand)
            {        // If not expecting a rand then try punt to infix_op ( ..)
              switch(OpClass(cur_p_stack.tv.t_avalue))
                {
                case T_INFOP:
                case T_MPREOP:
                case T_MPOSTOP:                  // Split func( to op ( 
                  cur_p_stack.t_type = OpClass(cur_p_stack.tv.t_avalue);
                  stash_p.t_type = T_LPAR;
                  stashed_token = TRUE;
                  break;                 
                default:
                  pXCPT->Error(randE);
                } 
            } 
          else
            {
              start_arg();
              EnterContext(instrucC, prec_commaA - 1);
              return(FALSE);
            }
          break;
      default:
         pXCPT->Error(randE);
      }                                       // end switch(type)
   }                                           // end while(TRUE)
}

int TermReader::PushStack(TOKEN_ type, void *pvalue)
{                           // push token type with * pvalue onto parse stack
#ifdef BUG_READ
 DUMP << NL << "PushStack ";
 if (type <= T_UNDEFINED)
   DUMP << token_names[type];
 else
   DUMP << "'" << (char) type << "'";
#endif
   if (++top_p_stack >= ReadDepth)
      pXCPT->Error(pstackE);

   cur_p_stack.t_type = type;
   switch(type)
   {
   case T_TERM:
      cur_p_stack.tv.t_tvalue = * (TERM *) pvalue;
      break;

   case T_VAR:
   case T_INT:
   case T_CHAR:
      cur_p_stack.tv.t_ivalue = * (int *) pvalue;
      break;

   case T_LONG:
//   case T_CHAR:   // moved above, OK?
      cur_p_stack.tv.t_lvalue = * (long *) pvalue;
      break;

   case T_DOUBLE:
      //cur_p_stack.tv.t_dvalue = * (double *) pvalue;
      cur_p_stack.tv.t_dvalue = * (GCDouble **) pvalue;
      break;
   case T_FLOAT:
      cur_p_stack.tv.t_flvalue = * (float *) pvalue;
      break;

   case T_REAL:
      cur_p_stack.tv.t_rvalue = *(GCReal **) pvalue;
      break;

   case T_FIXED:
      cur_p_stack.tv.t_fvalue = *(fixedC *) pvalue;
      break;

   case T_ATOM:
   case T_FUNCTOR:
   case T_PREOP:
   case T_POSTOP:
   case T_INFOP:
   case T_MPREOP:
   case T_MPOSTOP:
      cur_p_stack.tv.t_avalue = * (PATOM *) pvalue;
#ifdef BUG_READ
 DUMP << SP2 << "'" << * (PATOM *) pvalue << "'";
#endif
      break;

   case T_STR:
      cur_p_stack.tv.t_svalue = * (GCString **) pvalue;
      break;
   default:
      break;
   }
#ifdef BUG_READ
 DUMP << NL << FLUSH;
#endif
   return(top_p_stack);
}

  /*
   * Enter a new context - context is built when either a new structure
   * is encountered (eg a functor, list , tail of a list or argument in
   * a functor or list) or when an operator precedence conflict arises.
   * In this case the new context is inherited from the previous context - 
   * we use the new context simply to mark the parse stack so the operator 
   * precedence collapser knows where to start.
   */
void  TermReader::EnterContext(int contxt, int prec)
{
   if (++top_con_stack >= ReadDepth)
      pXCPT->Error(pstackE);
   SetCurrentConStack(contxt, prec, top_p_stack);
   cntxt   = GetCurrentContext();
   maxprec = GetCurrentPrec();
}

int TermReader::varid(aCHAR *buf)
{ // Given var name in buf, find index in var table (or add it) and return it.
   register int i;
   VARNAMEptr pvn;
   intC currl = (intC) Lstrlen(buf);

   for(i = 0; i < next_var; ++i)
   {
      pvn = &vars[i];
      if( (intC) (pvn->v_length) == currl)
        if( 0 == Lstrncmp(buf, pvn->v_name, minmum(currl, VNAME_LENGTH)))
          {
            pvn->v_flag = 1;                 // more than one ref 
            return(i);
          }
   }
   if( next_var >= pHXL->GetMaxVars() - 1)
      pXCPT->Error(maxvarrE);
   pvn = &vars[next_var];
   pvn->v_length = (aBYTE) currl;
   Lstrncpy(pvn->v_name, buf, minmum(currl, VNAME_LENGTH));
   pvn->v_name[minmum(currl, VNAME_LENGTH)] = EOS;
   pvn->v_skel = NULL;
   pvn->v_flag = 0;                         // only one ref so far 
   return(next_var++);
}

//TOKEN_ TermReader::tmGetToken()
void TermReader::tmGetToken()
// Called from Read() at top level, and other places as well
{                                     // get next token - may have been popped
#ifdef BUG_READ
 DUMP << "tmGetToken" << NL;
#endif
   if (stashed_token)
     {
       PushStack(stash_p.t_type, (void *) &stash_p.tv.t_tvalue);
       stashed_token = FALSE;
       //return stash_p.t_type;
       return;
     }
   //return pLEX->getLexeme(expect);          // go to lex
   // expect set to Erand initially,
   // LEX will be pushing the next token onto the TermReader
   // parse stack
   pLEX->getLexeme(expect);          // go to lex
}

void  TermReader::pop_token()
{                                           // pop off top_p_stack to stash
   stash_p.t_type = cur_p_stack.t_type;
	//   stash_p.tv.t_tvalue = cur_p_stack.tv.t_tvalue;
   if( stash_p.t_type == T_FIXED)
	  stash_p.tv.t_fvalue =  cur_p_stack.tv.t_fvalue;
	else
	  stash_p.tv.t_tvalue = cur_p_stack.tv.t_tvalue;

   stashed_token = TRUE;
   -- top_p_stack;
}

/*
 * reduces term being built on parse stack which corresponds
 * to current context - leaves it at cur_con_stack().r_p_stack
 * Scans stack up to but NOT including top_p_stack - top_p_stack
 * contains the delimeter which fired the reduction ) ] or ,
 */
TF  TermReader::collapse()
{
   TERM     u, t;
   register int ps;

   ps = GetCurrentPStack();
   switch(cntxt)
   {
   case instrucC:
      t = pHXL->heapGET();
      t->setStruct(t+1);
      pHXL->heapGET()->setFA(p_stack[ps].tv.t_avalue, top_p_stack-ps-1);

      while (++ps < top_p_stack)
        ParseToHeap(ps);                         // copy arguments 
      break;

   case inlistC:
     if (ps == top_p_stack)
       {
         t = pHXL->HTop();
         pHXL->heapGET()->setAtom(pATAB->nilA);
      }
      else
      {
         t = u = pHXL->heapGET();
         u->setList(u+1);
         // build a list but leave unterminated 
         //   since we don't know here whether it is terminated
         //   with a nil or a cdr (| .. )

         while(ps < top_p_stack)
           {
             ParseToHeap(ps++);
             u = pHXL->heapGET();
             u->setList(u+1);
           }
         pHXL->heapPOP();                         // pop off extra list-cell 
       }
     break;
     
   case bottomC:
      t = ParseToHeap(1);
      break;

   case inbraceC:
      // Check for {}
     if (ps == top_p_stack)
       {
         t = pHXL->HTop();
         pHXL->heapGET()->setAtom(pATAB->curlysA);
      }
      else
      {
      t = pHXL->heapGET();
      t->setStruct(t+1);
      pHXL->heapGET()->setFA(pATAB->curlyA, 1L);
      ParseToHeap(ps);
     }
      break;

   case incdrC:
                // recall we have stripped the "|" so the tail is at r_p_stack 
     -- top_con_stack;                           // to get back to listC 
     -- top_p_stack;                             // so we don't redo the cdr 
     cntxt = GetCurrentContext();
     collapse();                       // build the body of the list sans cdr
     ParseToHeap(ps);                            // terminating cdr 
     return(TRUE);

   case inargC:   // this is the only context in which we may have operators 
      switch(top_p_stack - ps)
      {
      case 1:                                     // have simple terminal 
         t = ParseToHeap(ps);
         break;

      case 2:                                     // prefix or postfix 
         t = pHXL->heapGET();
         t->setStruct(t+1);
         if (p_stack[ps].t_type == T_PREOP)
           { 
             pHXL->heapGET()->setFA(p_stack[ps].tv.t_avalue, 1L);
             ParseToHeap(1 + ps);
           }
         else                                     // is a post-op 
           {
             pHXL->heapGET()->setFA(p_stack[1 + ps].tv.t_avalue, 1L);
             ParseToHeap(ps);
           }
         break;

      case 3:                                     // infix 
         t = pHXL->heapGET();
         t->setStruct(t + 1);
         pHXL->heapGET()->setFA(p_stack[1+ps].tv.t_avalue, 2L);
         ParseToHeap(ps);
         ParseToHeap(2 + ps);
         break;

      default:  
         pXCPT->Error(inargE, top_p_stack - ps);
      }
      break;

   default:
      return(TRUE);
   }
   ps = GetCurrentPStack();
   p_stack[ps].t_type = T_TERM;
   p_stack[ps].tv.t_tvalue = t;
   return(TRUE);
}

void  TermReader::collapse_con()
{                                         //   collapses term and pops context
   collapse();
   top_p_stack = GetCurrentPStack();
   -- top_con_stack;
   maxprec = GetCurrentPrec();
   cntxt   = GetCurrentContext();
}

// Only simple structures should be on the parse stack at this point
TERM  TermReader::ParseToHeap(int index)
{                  // build terms on heap by copying from parse stack at index
   TERM      h, t;
   //double    d;
   long      l;
   fixedC    f;
   //Real      r;
   PARSE_STACKptr ps;

   ps = &p_stack[index];
   switch( ps -> t_type )
   {
   case T_TERM:
      t = pHXL->heapGET();
      t->setTerm(ps -> tv.t_tvalue);
      break;                             // t is refT 

   case T_VAR:
      t = pHXL->heapGET();
      h = vars[ps -> tv.t_vvalue].v_skel;
      if (h)
         t->setTerm(h);                      // var copied already, so ref it
      else
        {
          t->setUnbound();                   // unbound
          vars[ps -> tv.t_vvalue].v_skel = t;
        }
      break;

   case T_INT:
   //case T_CHAR:
      //t = pHXL->HTop();
      //pHXL->heapGET()->setInt(ps->tv.t_ivalue);
      t = pHXL->heapGET();
      t->setInt(ps->tv.t_ivalue);
      break;

   //case T_INT:
   case T_CHAR:
      //t = pHXL->HTop();
      //pHXL->heapGET()->setInt(ps->tv.t_ivalue);
      t = pHXL->heapGET();
      t->setChar(ps->tv.t_ivalue);
      break;

   case T_DOUBLE:
      //d = ps->tv.t_dvalue;
      t = pHXL->heapGET();
      //t->setDouble(pGCTH->make_float(d));
      t->setDouble(ps->tv.t_dvalue);
      break;

   case T_FLOAT:
      //d = ps->tv.t_dvalue;
      t = pHXL->heapGET();
      //t->setDouble(pGCTH->make_float(d));
      t->setSingle(ps->tv.t_flvalue);
      break;

   case T_REAL:
      //r = ps->tv.t_rvalue;
      t = pHXL->heapGET();
      //t->setReal(pGCTH->make_real(r));
      t->setReal(ps->tv.t_rvalue);
      break;

   case T_FIXED:
      f = ps->tv.t_fvalue;
      t = pHXL->heapGET();
      t->setFixed(f);
      break;

   case T_LONG:
      l = ps->tv.t_lvalue;
      t = pHXL->heapGET();
      t->setInt(l);
      break;

   case T_ATOM:
      t = pHXL->HTop();
      pHXL->heapGET()->setAtom(ps->tv.t_avalue);
      break;

   case T_STR:
      t = pHXL->heapGET();
      //t->setStr( pGCTH->make_string((STRptr) ps->tv.t_svalue) );
      t->setStr( ps->tv.t_svalue );
      break;

   default:
      pXCPT->Error(parsetypeS);
   }
   return(t);
}

TOKEN_  TermReader::OpClass(PATOM a)
{          //returns T_PREOP, T_POSTOP, T_INFOP, T_MPREOP, T_MPOSTOP or T_ATOM 

   if (pATAB->IsPrefix(a,1))
      return (pATAB->IsOperator(a,2) ? T_MPREOP : T_PREOP);
   else if (pATAB->IsPostfix(a,1))
      return (pATAB->IsOperator(a,2) ? T_MPOSTOP : T_POSTOP);
   else
      return (pATAB->IsOperator(a,2) ? T_INFOP : T_ATOM);
   //PRED_BLKptr      pi;
   //int unary, binary;

   //if (a < 0)
   //   pXCPT->Error(rdnotatomE);

   //if ((pi = pATAB->PredHead(a)) == NULL)
   //   return(T_ATOM);

   //unary  = pi->groups[0][0];   //associativity
   //binary = pi->groups[1][0];   //associativity

   //if (unary & 8)  // fx
   //   return( binary ? T_MPREOP : T_PREOP);
   //else if (unary & 2) // xf
   //   return( binary ? T_MPOSTOP : T_POSTOP);
   //else 
   //   return( binary ? T_INFOP : T_ATOM);
}

int TermReader::RightPrec(TOKEN_ lass, PATOM a)
{
   // if its left associative, decrement precedence

   if (lass == T_INFOP)
      //return(pATAB->binopP(a) - (0 != (pATAB->binopT(a) & 127 & (xfxP | yfxP))));
      return pATAB->getPrecedence(a,2) - ( pATAB->IsLeftAssoc(a,2) ? 1 : 0 );

   else if (lass == T_PREOP)
      //return(pATAB->unopP(a) - (0 != (pATAB->unopT(a) & 127 & fxP)));
      return pATAB->getPrecedence(a,1) - ( pATAB->IsLeftAssoc(a,1) ? 1 : 0 );

   return(0);                                       // a formality 
}

void TermReader::start_arg()
{
   if (cntxt == inlistC || cntxt == instrucC)
      EnterContext(inargC, prec_commaA - 1);
   else if (cntxt == inparsC || cntxt == incdrC || cntxt == inbraceC)
      EnterContext(inargC, MAXPREC);
}

TF TermReader::p_varlist()
{
   TERM    t, X0;
   int    i;

   X0 = t = pHXL->heapGET();
   for(i = 0; i < pREAD->Get_next_var(); ++i)   // first build skeleton list 
   {
      t->setList(pHXL->heapGET());      
      t = pHXL->heapGET();                      // cons cell
   }
   *t = pATAB->NilCell;

   t = X0 + 1;                                  // first cons cell 
   for(i = 0; i < pREAD->Get_next_var(); ++i)
   {
      *t = *pHXL->StrToCharList(pREAD->Get_var_v_name(i));
      t = (t+1)->getTerm();
   }
   return(pTSVC->Unify(X0, pHXL->XVar(0)));
}

#ifdef BUG_READ
void TermReader::debug_parse()
{
/*
   int i;
   TOKEN_ tt;

   errDebugMsg("\n----- Parse Stack -----\n");
   for (i=top_p_stack; i > 0; i--)
   {
      tt = p_stack[i].t_type;
      errDebugMsg("\n[%d] %d: ", i, (int) tt);
      switch (tt)
      {
         case T_TERM:
            print_cell(p_stack[i].tv.t_tvalue, 0);
            break;
         case T_ATOM:
            errDebugMsg("ATOM %s", *(p_stack[i].tv.t_avalue));
            break;
         case T_VAR:
            errDebugMsg(" VAR  %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_INT:
            errDebugMsg(" INT  %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_CHAR:
            errDebugMsg(" CHAR %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_DOUBLE:
            errDebugMsg(" FLOAT %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_PREOP:
            errDebugMsg(" PREOP %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_POSTOP:
            errDebugMsg(" POSTOP %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_INFOP:
            errDebugMsg(" INFOP %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_MPREOP:
            errDebugMsg(" MPREOP %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_MPOSTOP:
            errDebugMsg(" MPOSTOP %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_LBRACK:
            errDebugMsg(" LBRACK [ %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_RBRACK:
            errDebugMsg(" RBRACK ] %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_LBRACE:
            errDebugMsg(" LBRACE { %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_RBRACE:
            errDebugMsg(" RBRACE } %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_VBAR:
            errDebugMsg(" VBAR | %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_LPAR:
            errDebugMsg(" LPAR ( %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_RPAR:
            errDebugMsg(" RPAR ) %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_COMMA:
            errDebugMsg(" COMMA , %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_DOT:
            errDebugMsg(" DOT . %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_FUNCTOR:
            errDebugMsg(" FUNC %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_STR:
            errDebugMsg(" STR %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         case T_LONG:
            errDebugMsg(" LONG %0*lx ", PP_SIZE, 
                        (long) p_stack[i].tv.t_lvalue); break;
         default:
            errDebugMsg(" Bad parse thing ");
      }
   }
   errDebugMsg("\n----- ----------- -----\n");
   return;
*/
}
#endif

/*
TF TermReader::p_read()
{                                           // read_(Term, Handle)
   b_reading = true;
   TF tf = read__();
   b_reading = false;
   return tf;
}
*/








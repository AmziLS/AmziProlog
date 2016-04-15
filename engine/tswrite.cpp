/***************************************************************************\
*
* sowrite.cpp -- write out a term
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
*
\***************************************************************************/

#include <math.h>

#include "inc.h"
#include "streams.h"
#include "pch.h"
#include "lex.h"

#ifdef LANDFILL
#define noBUG_WRITE
#define noBUG_PRINT_REAL
#define noBUG_WRITE_ERR
#endif

TermWriter::TermWriter(LEngine *peng)
{
   m_peng = peng;
   write_stream = NULL;
   quoting = false;
   writeBuffer = NULL;
   writeqBuffer = NULL;
   outputBuffer = NULL;
}

TermWriter::~TermWriter()
{
   delete writeBuffer;
   delete writeqBuffer;
   delete outputBuffer;
}

void TermWriter::Init(int bufsz)
{
   m_buflen = bufsz;
   LNEW(writeBuffer, aCHAR[m_buflen], aS("miscellaneous"));
   LNEW(writeqBuffer, aCHAR[m_buflen], aS("miscellaneous"));
   LNEW(outputBuffer, aCHAR[m_buflen], aS("miscellaneous"));
   output_len = 0;
   m_is_string = false;
   m_maxstring = 0;
}

int TermWriter::termWriteString(TERM t, STRptr s, intC iMax, bool qB)
{  // sets up stream to write to s, writes to it, then deletes stream
   int h;
//   char* cs;  //k
//   int old_buflen = m_buflen;
   //std::cout << "termWriteString + " << iMax << "\n";
   h = pIO->makeStringStream(s, write_, iMax, false);
//   m_buflen = iMax-5;
   m_maxstring = iMax;  //k
   m_is_string = true;   //k
   try {   //k
   write_term(h, t, qB);
   //std::cout << "noerror: " << s << "\n";
   } catch( char* ) {
   //std::cout << "beforedots: " << s << "\n";
    //  Lstrcat(s, aS("..."));
      ;
   //std::cout << "afterdots: " << s << "\n";
   }
//   m_buflen = old_buflen;
   m_is_string = false;
   m_maxstring = 0;
   pIO->remove_stream(h);
   return OK;
}

//int TermServices::writet(TERM S_or_a, TERM t, TERM opts)
TF TermWriter::p_write()
{                                             // write term to a Handle 
   int h;
   TERM  S_or_a, t, opts;

   S_or_a = X(0);
   t = X(1);
   opts = X(2);

   //S_or_a = S_or_a->dref();

   h = pIO->streamIndex(S_or_a);

#ifdef BUG_WRITE
 DUMP << "----- write -----" << NL;
 pIO->DumpStreams();
 DUMP << "Write to stream ";
 pTSVC->DumpWrite(S_or_a);
 DUMP << " id: " << h << NL << SP2;
 pTSVC->DumpWrite(t);
 DUMP << NL << FLUSH;
#endif

  // STREF strmp = pIO->stream[h];
   TERM q;

  //if(pIO->stream[h]->getMode() < 2)
  //    pXCPT->Error(wrongwriteS);

   //int oldco = pIO->current_output;
   //pIO->current_output = h;
   bool quote = false;
   if (opts->IsStruct())
   {
      q = opts->getTerm();
      quote = 0 !=(q + 1)->getInt();
   }
   //sowrite(t, MAXPREC);
   //pIO->current_output = oldco;   
   //quoting = false;
   write_term(h, t, quote);
   return(true);
}

// globals to catch recursions due to cyclic bindings
//TERM g_last_term = NULL;
//int g_cyclic_count = 0;
int g_sorecurse_count = 0;

// all writing must come through here first
void TermWriter::write_term(int h, TERM t, bool quote)
{
   quoting = quote;
   write_stream = pIO->stream[h];
   outputBuffer[0] = 0;
   output_len = 0;
//   g_sorecurse_count = 0;
//   g_last_term = NULL;
//   g_cyclic_count = 0;
   written.clear();
   sowrite(t, MAXPREC);

   write_stream->put_string(outputBuffer);
}

bool TermWriter::write_occurs_check(TERM t)
{
   std::map<TERM,int>::iterator i;
   i = written.find(t);
   if ( i == written.end() )
   {
      written.insert( std::pair<TERM,int>(t,1) );
      return false;
   }
   else if ( i->second > 3 )
   {
      return true;
   }
   else
   {
      (i->second)++;
      return false;
   }
}

void TermWriter::write_occurs_pop(TERM t)
{
   std::map<TERM,int>::iterator i;
   i = written.find(t);
   (i->second)--;
}


TERM TermWriter::sowrite(TERM tin, int inprec)
{
  int         i;
  int         T;   // len;
  //PATOM       op_assoc;
  ARITY       arity;
  //PRED_BLKptr pi;
  aCHAR       *s;
  aCHAR       *p;
  int         op_prec;
  int         decs   = pSTATE->m_decimal_places;
  TERM        t, t1, top;
  PATOM       a;
#ifdef REALS
  Real        r;
#endif
  fixedC f;

  top = t = tin->dref();

  if (write_occurs_check(top))
  {
     w_f_puts(aS("..."));
     return t;
  }

  //write_stream = h;
  T = t->getType();

  //g_sorecurse_count++;
  //if (pSTATE->m_occurs_check && write_occurs_check(t))
  //   w_f_puts(aS("..."));

  //if (g_sorecurse_count > pENG->m_ini.outputdepth)
  //{
  //   w_f_puts(aS("..."));
  //   g_sorecurse_count--;
  //   return t;
     // just throw the int, will get caught above
     //throw(outputdepthE);
//  }

  switch ( T )
    {
    case intS:
      Lsprintf(writeBuffer, m_buflen, aS("%d"), t->getInt());
      w_f_puts(writeBuffer);
      break;

    case charS:
      //Lsprintf(writeBuffer, m_buflen, aS("0w%04x"), t->getInt());
      Lsprintf(writeBuffer, m_buflen, aS("0'%lc"), (aCHAR)t->getInt());
      w_f_puts(writeBuffer);
      break;

    case atomS:
      //PrAtom(*(t->getAtom()), aS(""));
      PrAtom((t->getAtom()), aS(""));
      break;

    //case dbrefS:
    case ptrS:
    //case mscwS:
    //case mscwinuseS:
      Lsprintf(writeBuffer, m_buflen, aS("0x%0*lX"), PP_SIZE, t->getPtr());
      w_f_puts(writeBuffer);
      break;

    case stashS:
      Lsprintf(writeBuffer, m_buflen, aS("Stash%0*lX"), PP_SIZE,
					t->getStash());
      w_f_puts(writeBuffer);
      break;

    case strS:
      if (quoting)
        w_f_putsq(t->getStr(), pSTATE->m_properQuotes ? '"' : '`');
      else
        w_f_puts(t->getStr());
      break;

    case doubleS:
      //Lsprintf(writeBuffer, m_buflen, aS("%.15g"), (double)*t);
      // for quoting, we want the same as for non-quoting, as numbers
      // are read in in the correct syntax
#ifdef LANDFILL
      if (quoting)
         Lsprintf(writeBuffer, m_buflen, aS("%0.14e"), t->getDouble());
#else
      if (false)
         ;
#endif
      else
      {
         if (decs > 14 || decs < 0)
            decs = 14;
         //decs++;
         Lsprintf(writeBuffer, m_buflen, aS("%.*g"), decs, t->getDouble());
         //Lsprintf(writeBuffer, m_buflen, aS("%.15g"), t->getDouble());
      }
      w_f_puts(writeBuffer);
      break;

    case singleS:
      //Lsprintf(writeBuffer, m_buflen, aS("%.15g"), (double)*t);
#ifdef LANDFILL
      if (quoting)
         Lsprintf(writeBuffer, m_buflen, aS("%0.6e"), t->getSingle());
#else
      if (false)
         ;
#endif
      else
      {
         if (decs > 6 || decs < 0)
            decs = 6;
         //decs++;
         //Lsprintf(writeBuffer, m_buflen, aS("%.6g"), t->getSingle());
         Lsprintf(writeBuffer, m_buflen, aS("%.*g"), decs, t->getSingle());
      }
      w_f_puts(writeBuffer);
      break;

    case refT:
      w_f_puts(pHXL->cellname(t));
      break;

    case strucT:                            // terms and args are contiguous
      t1 = t->getTerm();
      a = t1->getAtom();
#ifdef LANDFILL
// if (a == (PATOM)0xdddddddd)
// {
//    w_f_puts(aS("***dddddddd***"));
//    break;
// }
#endif

      s = *a;
      arity = t1->getArity();

      if (arity == 2)
      {
         if (pATAB->IsOperator(a, arity))
         {
            op_prec = pATAB->getPrecedence(a, arity);
            if (inprec < op_prec)
               w_f_puts(aS("("));
            sowrite(++t1, op_prec - (pATAB->IsLrand(a, arity) ? 1 : 0) );
            // looks better without spaces, but take care not to
            // write terms that can't be read back in.
				if (Lstrlen(s) == 1 &&
                 (*s == '/' || *s == '*' || *s == ':') )
            {
               // if there is a prefix operator following, leave a
               // space so written terms can be read.
               //t2 = t1+1;
               //if (t2->IsAtom() && pATAB->IsPrefix(t2->getAtom(), 1))
               //   w_f_puts(aS(" "));
					//PrAtom(s, aS(""));
					PrAtom(a, aS(""));
            }
            else
            {
					 w_f_puts(aS(" "));
					 //PrAtom(s, aS(" "));
					 PrAtom(a, aS(" "));
				 }
            sowrite(++t1, op_prec - (pATAB->IsRrand(a, arity) ? 1 : 0) );
            if (inprec < op_prec)
               w_f_puts(aS(")"));
            break;
         }
      }

      else if (arity == 1)
      {
         if (a == pATAB->curlyA)
         {
            w_f_puts(aS("{"));
            sowrite(++t1, MAXPREC);
            w_f_puts(aS("}"));
            break;
         }
         else if (pATAB->IsOperator(a, arity))
         {
            op_prec = pATAB->getPrecedence(a,arity);
            if (inprec < op_prec)
               w_f_puts(aS("("));
            if (pATAB->IsPostfix(a,arity))
            {
               sowrite(++t1, op_prec - (pATAB->IsRightAssoc(a,arity) ? 1 : 0));
               w_f_puts(aS(" "));
               //PrAtom(s, aS(""));
               PrAtom(a, aS(""));
            }
            else
            {
               //w_f_puts(aS(" "));
               //PrAtom(s, aS(" "));
               PrAtom(a, aS(" "));
               sowrite(++t1, op_prec - (pATAB->IsLeftAssoc(a,arity) ? 1 : 0) );
            }
            if (inprec < op_prec)
               w_f_puts(aS(")"));
            break;
         }
         // the numbervars case, where '$VAR'(i) is output as a variable
         else if (a == pATAB->zvarA)
         {
            t1++;
            t1 = t1->dref();
            if (t1->IsInt())
            {
               w_f_puts(aS("_X"));
               i = t1->getInt();
               Lsprintf(writeBuffer, m_buflen, aS("%d"), i);
               w_f_puts(writeBuffer);
            }
            else if (t1->IsAtom())
            {
               w_f_puts(*t1->getAtom());
            }
            break;
         }
      }

      if(s)                                        // ray
        //PrAtom(s, aS("("));
        PrAtom(a, aS("("));
      for (i=1; i < (int) arity; ++i)
        {
          sowrite(++t1, pREAD->Get_prec_commaA()-1);
          w_f_puts(aS(", "));
        }
      if(arity > 0)
        {
          sowrite(++t1, pREAD->Get_prec_commaA()-1);
          w_f_puts(aS(")"));
        }
      break;

    case listT:
		printList(t, false);  // not the tail
      break;
#ifdef REALS
	 case realS:
		r = t->getReal();
		printReal(r, quoting);
		break;
#endif
	 case fixedS:
		f = t->getFixed();
#ifdef BUG_PRINT_REAL
      f.Dump();
#endif
		if(f.isNeg())
		  {
			 //f.hi &= 0x7fffffff;
			 w_f_puts(aS("-"));
		  }
		Lsprintf(writeBuffer, m_buflen, aS("%d"), f.getHI());
		w_f_puts(writeBuffer);
      if (decs < 0)  // just show what's naturally there
      {
		   w_f_puts(aS("."));
		   Lsprintf(writeBuffer, m_buflen, aS("%09d"), f.getLO());
         p = writeBuffer + 8;   // last digit
         while (*p == aS('0') && (p > writeBuffer))
            p--;
         *(p+1) = EOS;
         w_f_puts(writeBuffer);
      }
      else if (decs > 0)
      {
		   w_f_puts(aS("."));
		   Lsprintf(writeBuffer, m_buflen, aS("%09d"), f.getLO());
         if (decs < 9)
            writeBuffer[decs] = EOS;
         w_f_puts(writeBuffer);
      }
#ifdef LANDFILL
      if (quoting)
         w_f_puts(aS("f"));
#endif
      break;
    default:
#ifdef BUG_WRITE_ERR
 w_f_puts(aS("\n\n"));
 DUMP << "Bad term in write, type = " << T << NL << FLUSH;
#endif
      pXCPT->Error(wrongwriteS);
    }
  //g_sorecurse_count--;
  write_occurs_pop(top);
  return(t);
}

bool TermWriter::all_chars(TERM t)
{
   TERM h = t->getTerm();
   while( h->IsChar() )
   {
      h++;
      h = h->dref();
      if (h->IsList())
         h = h->getTerm();
      else if (h->IsAtom() && h->getAtom() == pATAB->nilA)
         return true;
   }
   return false;
}

void TermWriter::printList(TERM t, bool tail)
{
   t = t->dref();

   if ( pSTATE->m_properQuotes == LOFF && all_chars(t) )
   {
      w_f_putc(aC('\"'));
      TERM h = t->getTerm();
      while( h->IsChar() )
      {
         w_f_putc( h->getChar() );
         h++;
         h = h->dref();
         if (h->IsList())
            h = h->getTerm();
      }
      w_f_putc(aC('\"'));
   }
   else
   {
      if (! tail)
         w_f_puts(aS("["));
      t = t->getTerm();
      sowrite(t++, pREAD->Get_prec_commaA()-1);
      t = t->dref();
      if (t->IsList())
      {
         w_f_puts(aS(", "));
         printList(t, true);
      }
      else if (t->IsAtom())
      {
         if (t->getAtom() != pATAB->nilA)
         {
            w_f_puts(aS(" | "));
            sowrite(t, MAXPREC);
         }
      }
      else
      {
         w_f_puts(aS(" | "));
         sowrite(t, MAXPREC);
      }
      if (! tail)
         w_f_puts(aS("]"));
   }
}

/*
      t1 = t->getTerm();
      while(true)
        {
          sowrite(t1++, pREAD->Get_prec_commaA()-1);
          t1 = t1->dref();
          if (t1->IsList())
            {
              w_f_puts(aS(", "));
              t1 = t1->getTerm();
            }
          else if (t1->IsAtom())
            {
              if (t1->getAtom() == pATAB->nilA)
                break;
              else
                {
                  w_f_puts(aS(" | "));
                  sowrite(t1, MAXPREC);
                  break;
                }
            }
          else
            {
              w_f_puts(aS(" | "));
              sowrite(t1, MAXPREC);
              break;
            }
        }
*/


void TermWriter::printReal(Real r, TF quoting)
{
   // LReal sends back a string that is always of the same
   // format:  digits, dot, digits.  Fractions always have
   // a leading 0.  and integers a trailing .0.
   r->toString(writeBuffer, m_buflen);
   aCHAR *dot;
   // For not quoting, we truncate the decimal digits
   // based on the decimal_places flag, although a negative
   // means show all the digits.
   // For quoted, we show all the digits all the time, plus
   // a trailing 'r'.
   // Actually, decided to turn off quoting for numbers as
   // they read in correctly.  Leave like this in case we
   // change our mind.
#ifdef LANDFILL
   if (quoting)
   {
      w_f_puts(writeBuffer);
      w_f_puts(aS("g"));
   }
#else
   if (false)
      ;
#endif
   else
   {
      intC frac_digits;
      intC dec_places = pSTATE->m_decimal_places;
      if (dec_places >= 0)  // negative means show all
      {
         dot = Lstrchr(writeBuffer, aS('.'));
         LASSERT((dot), aS("missing decimal point from Real::toString()"));
         frac_digits = (intC)Lstrlen(dot)-1;
         if (dec_places == 0)
            *dot = EOS;
         else if (dec_places < frac_digits)  // trim excess
         {
            *(dot + dec_places + 1) = EOS;
         }
         else if (dec_places > frac_digits)  // pad to right place
         {
            while (frac_digits++ < dec_places)
               *(dot + frac_digits) = aS('0');
            *(dot + frac_digits) = EOS;
         }
      }
      w_f_puts(writeBuffer);
   }
}

//#ifdef REALS
/*
 * If exp == 0 then display mantissa alone.
 * Prefix zeros is -(length + exp). Positive for proper fractions.
 * If zeros is 0 then display mantissa with prefix '0.'.
 * If -length < zeros < 0 then display mantissa with interior point.
 * Else display mantissa and exponent. However, in this case, if 
 * decimal_places flag truncates the mantissa display then exp is false
 * for the display, so exp must be decremented by the excess to correct it. 
 */
/*
void TermWriter::printReal(Real r, TF quoting)
{       // printReal should be in class GD, but GDs don't know any environment 
#ifdef BUG_PRINT_REAL
   r->Dump();
#endif

  STRptr bufp = writeBuffer;
  int length = r->getLen();
  int exp    = r->lsExp();
  int msCol  = r->msCol();               // most sig math col
  int lsCol  = r->lsCol();               // least sig math col
  int zeros  = -(1 +  msCol);            // # of prefix zeros
  int decs   = pSTATE->m_decimal_places - 1;
  int nanos  = 1 + decs/9;               // # nanodigits needed
  int decs9  = 1 + decs%9;               // # dec digits in last nano
  int excess = 0; 
  int last = lsCol;                            // last math col to display
  if(msCol > -2 && msCol < length && lsCol < 0)  // gigapoint to be displayed
	 {
		excess = -(nanos + exp);
		last += excess;
	 }
  if(last < lsCol)
	 last = lsCol;

  int lzs = length + exp;                      // leading zeros
  int i, len, stopIndex = msCol + 1;           // out of range
  bool stopped = false;
  STRptr p;
  intC gd;

  if(r->isZero())
	 {                                          // special case zero
		if(quoting)
		  //w_f_puts(aS("0g"));
		  w_f_puts(aS("0r"));
		else
		  w_f_puts(aS("0"));
		return;
	 }

  if(r->isNeg())
	 w_f_puts(aS("-"));

  if(zeros == 0)
	 {
		stopped = true;
		w_f_puts(aS("0."));                      // stop is in very first place
		stopIndex = msCol;
	 }
  for(i = msCol; i >= last; i--)
	 {                                          // print digits
		if(msCol + zeros == i && i < msCol)
		  {                                      // gigapoint, & fraction
			 stopped = true;                      // gigapoint inserted flag
			 w_f_puts(aS("."));                   // stop not in first place
			 stopIndex = i;
		  }

		gd = r->getCol(i);
		if(i == msCol && !stopped)            // first gigit & ! stopped
			 len = Lsprintf(bufp, m_buflen, aS("%d"), gd); // elide ldeczeros
		else                                     // all but 1st gigit
			 len = Lsprintf(bufp, m_buflen, aS("%09d"), gd); // show ldeczeros

		if(i == last)
		  {                                      // last wanted gigit
			 if(decs9 < 9)
				{                                  // needs truncating
				  p = bufp + decs9;                // last char
				  *p-- = EOS;
				}
			 else
				  p = bufp + len - 1;              // last char
			 if(stopped)
				{                                  // passed dp index
				  while(*p == aS('0'))             // elide trailing zeros
					 {
						if((p - bufp) > 0)
						  *p = EOS;
						p--;
					 }
				}
			 //	 break;
		  }                                      // end if lsCol
		if(i != stopIndex)
		  w_f_puts(aS(" "));                     // for readability
		w_f_puts(writeBuffer);
	 }                                          // end for length
  if(exp)
		if(!stopped)
		  {                                      // exponent
			 w_f_puts(aS("g"));
			 Lsprintf(writeBuffer, m_buflen, aS("%d"), exp + excess );
			 w_f_puts(writeBuffer);
		  }
		//else
		  //if(quoting)
			 // w_f_puts(aS("g"));
		  //else;
  //else
  //	 if(quoting)
  //      w_f_puts(aS("g"));
}
//#endif
*/
/* quoteq is true if s is non-standard atom 
 * but we don't want to quote atoms used as variables, _02
 * nor do we want to quote operators 
 */
TF TermWriter::quoteq(aCHAR *s)
{
   aCHAR   c;
   aCHAR   *start;
   aCHAR   *op_chars = aS("@#$%^&></+*-=\\~`:?;,!.[]") ;

   start = s;

   if (*s++ == '_')                          // test for variable of form _02 or _X1
      while(Lisdigit(*s++)) ;
   if (s > start+1 && *--s == EOS) return(FALSE);

   s = start;
   if (*s++ == '_' && *s++ == 'X')                          // test for variable of form _02 or _X1
      while(Lisdigit(*s++)) ;
   if (s > start+1 && *--s == EOS) return(FALSE);

//   s = start;
   if (*--s == EOS)    // we want to quote a null atom, so it can be reread
//   if (*--s == EOS)    // we want to quote a null atom, so it can be reread
     return(TRUE);

   for(s = start; *s != EOS && pLEX->is_noquotes(*s); s++) ; // test for rator 
   if (*s == EOS) 
     return(FALSE);

   s = start;                                 // test for normal atom 
   c = *s++;
   if (! (pLEX->is_atominitial(c)))
     return(TRUE);
   while((c = *s++) != EOS) 
      if ( ! pLEX->is_lud(c) )
         return(TRUE);
   return(FALSE);
}

void TermWriter::PrAtom(aCHAR * s, aCHAR * cont)
{      // print atom s, quote if needed and required follows with cont printed
   if (quoting && quoteq(s))
      w_f_putsq(s, '\'');
   else
      w_f_puts(s);
	if(*cont)
	  w_f_puts(cont);
}

void TermWriter::PrAtom(PATOM a, aCHAR * cont)
{      // print atom s, quote if needed and required follows with cont printed
   aCHAR *s;
   s = a->get_display_name(m_peng);
 //std::cout << "PrAtom(PATOM)" << s << "\n";
   if (quoting && quoteq(s))
      w_f_putsq(s, '\'');
   else
      w_f_puts(s);
	if(*cont)
	  w_f_puts(cont);
}


void TermWriter::w_f_putc(aCHAR c)
{
   output_len++;
   if (output_len >= m_buflen)
   {
      write_stream->put_string(outputBuffer);
      outputBuffer[0] = c;
      outputBuffer[1] = EOS;
      output_len = 1;
   }
   else
   {
      outputBuffer[output_len-1] = c;
      outputBuffer[output_len] = EOS;
   }
}

void TermWriter::w_f_puts(aCHAR * pstring)
{                                         // write string to window or file 
   intC   len;

//   if (pLOG->Logging()) pLOG->Write( aS("%s"), pstring);
   len = (intC)Lstrlen(pstring);
   if (len > m_buflen)
      pXCPT->Error(wr_buf_overE, len - m_buflen);

   output_len += len;

   if ( m_is_string && output_len >= m_maxstring ) //k
   {
      write_stream->put_string(outputBuffer);
      write_stream->put_string(pstring);
//mm      throw("stringdone");
   }

   if (output_len >= m_buflen)
   {
      write_stream->put_string(outputBuffer);
//      if (write_stream->isEndOfStream())   // might happen writing to a string
//         pXCPT->Error(wr_buf_overE, len - m_buflen);
      Lstrcpy(outputBuffer, pstring);
      output_len = len;
   }
   else
   {
      Lstrcat(outputBuffer, pstring);
   }
   //write_stream->put_string(pstring);
   //WRITE_STREAM->putString(pstring);
}

void TermWriter::w_f_putsq(aCHAR * ps, aCHAR qc)
{
   aCHAR   *pbuf;
   intC  len;

   pbuf = writeqBuffer;

   if ((len = (intC)Lstrlen(ps)) > m_buflen)
      pXCPT->Error(wr_buf_overE, len - m_buflen);

   *pbuf++ = qc;
   while( *ps )
   {
      if (*ps == qc) *pbuf++ = qc;
      *pbuf++ = *ps++;
   }
   *pbuf++ = qc;
   *pbuf = EOS;

   w_f_puts(writeqBuffer);
}


/*
TF TermServices::p_write()                     // old style
{                                              // write_(handle, term, quoted)
    return writet(pHXL->XVar(2), pHXL->XVar(0), 
                  (TF)((pHXL->XVar(1))->getInt()));
}
*/
//TF TermServices::p_write1()                    // new style
//{                                              // write_(handle, term, quoted)
//   return writet( pHXL->XVar(0), pHXL->XVar(1), pHXL->XVar(2));
//}

#ifdef LANDFILL
TF TermWriter::p_logZterm()
{
   //errDebugMsg("\n\n**** log$term *****\n");
   DUMP << aS("\n\n**** log$term *****\n");
   // definition in HeapDebug, get if if we need it.
   // print_cell(pHXL->XVar(0),-1);
   DUMP << aS("**** ******* *****\n") << std::flush;

   return TRUE;
}
#endif


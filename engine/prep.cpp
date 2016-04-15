/***************************************************************************\
*
* prep.cpp -- pre-processor
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: prep.cpp,v $
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.27  2002/12/06 17:37:36  dennis
* a6-3-2, finally got rid of mscw tagging of heap cells to maintain
* state of dynamic clause backtracking, put backtracking info in
* the choice point structure instead.  much cleaner.  retract can now
* just analyze the control stack to see if a retract is safe.  heapgc
* no longer has to worry about those wierd cells.
*
* Revision 1.26  2002/12/02 18:25:12  dennis
* Converted to Visual Studio .NET.
*
* Revision 1.25  2002/05/15 16:59:09  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.24  2002/04/19 19:41:43  dennis
* fixed retract bug with sorted/indexed clauses, implemented abolish for
* those types as well
*
* Revision 1.23  2002/03/21 01:25:09  dennis
* allow separator in file names to be either tilt on slash, converts
* to correct one for Unix or Windows
*
* Revision 1.22  2002/01/28 06:29:19  dennis
* changes for parsing numbers, handling different options
*
* Revision 1.21  2002/01/06 20:31:28  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.20  2001/11/15 13:54:07  dennis
* Fixed logging and apitrace, made logfile into its own entity,
* removed from the Prolog stream IO.
*
* Revision 1.19  2001/10/06 03:08:00  dennis
* running on Linux mostly, compiling compiler at least
*
* Revision 1.18  2001/10/05 19:15:17  dennis
* string streams, function streams working, reals set up to not require
* m_peng, just passed in parameters as necessary
*
* Revision 1.17  2001/10/05 17:07:01  ray
* Added real things. Fixed some bugs. Stopped adding '.' in prep
*
* Revision 1.16  2001/10/02 16:05:21  dennis
* changed streams, cleaner interface to lex, reimplemented
* readers as buffer between the two
*
* Revision 1.15  2001/09/19 02:35:33  dennis
* fixed io stuff
*
* Revision 1.14  2001/09/11 04:34:56  dennis
* cleaned up some io stuff, got consult working, etc.
*
* Revision 1.13  2001/08/16 17:07:25  ray
* merged everything, repaired errors, added BINSTREAM
*
* Revision 1.12  2001/08/08 00:21:17  dennis
* unworking commit - stream bugs need fixing
*
* Revision 1.11  2001/08/01 20:18:00  ray
* removed tswrite.cpp & sio.cpp, added streams.cpp sowrite.cpp
*
* Revision 1.10  2001/07/10 16:51:32  dennis
* added more memory leak checking tools, all clean so far
*
* Revision 1.9  2001/06/27 15:15:10  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.8  2001/04/18 15:30:50  ray
* removed enum as builtin
* allowed it as prep directive and .h file statement
*
* Revision 1.7  2001/04/16 05:21:14  dennis
* hacked together some fixes for sio/lex to be better friends,
* merged other changes, added new samples
*
* Revision 1.6  2001/03/28 15:07:21  ray
* char_code/2, number_chars/2 added
* 1 char macros proscribed
*
* Revision 1.5  2001/03/25 15:29:51  ray
* Implemented fixed
* repaired #include
*
* Revision 1.4  2001/02/27 21:09:13  ray
* removed ? from properNames
* implemented integer/1 for reals
* repaired -ve reals error
*
* Revision 1.3  2001/02/22 15:52:00  ray
* repaired rpoper names and proper quotes
*
* Revision 1.2  2001/02/05 18:52:50  ray
* modified prep to correct include directive
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.16  2000/10/01 17:02:12  dennis
* cleaned up bigdig comments
*
* Revision 1.15  2000/08/26 00:32:07  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.14  2000/08/14 02:05:38  ray
* Added Real class.
* No division yet.
* No error reporting yet.
*
* Revision 1.12  2000/06/08 20:30:55  ray
* Changed readers to be line oriented
*
* Revision 1.11  2000/04/18 01:32:01  ray
*
* Added builtin stream_attrs/1 for users and error reporting.
*
* Revision 1.10  2000/04/10 00:57:27  ray
*
* #include
*
* Revision 1.9  2000/04/02 23:54:45  ray
*
* Fixed + and - bug
*
* Revision 1.8  2000/04/01 00:36:11  ray
*
* fixed -ve number error
*
* Revision 1.7  2000/03/30 07:35:42  dennis
* some minor changes to make gcc happy on Linux
*
*
*
* 1999/12/30 Ray
*
\****************************************************************************/

#include <math.h>
#include "inc.h"
#include "pch.h"

#ifdef LANDFILL
#define noBUG_PREP
#endif

#define PREPTRACE(DIRECTIVE) if(pSTATE->m_macrotrace && !skipLevel)\
     Lprintf(aS("line: %d file: %s #%s: branch level: %d\n"), \
     stream_reader->getStream()->getLineNo(), (STRptr)*(stream_reader->getStream()->get_name()), \
     DIRECTIVE, branchLevel)

extern int branchLevel;
extern int skipLevel;
extern int gotLevel;

class PROSTREAM;
class ISTREAM;

/**
flow control is accomplished with three variables:

    branchLevel       is the current nesting level,
    
    skipLevel       is the level being skipped. 0 is no skip.

    gotLevel        denotes true branch already found at this level.

The usage is as follows.  All three initialised to 0.

#if: #ifdef: #ifndef:
    branchLevel++
    if(skipLevel == 0)                     ; not skipping at any level
        if(predicate) 
            gotLevel = branchLevel         ; found at this level
        else
            skipLevel = branchLevel        ; skipping at this level

#else
    if(skipLevel == branchLevel)           ; flip the skip switch
        skipLevel = 0
        gotLevel = branchLevel             ; found at this level
    else
        if(skipLevel == 0)
           skipLevel = branchLevel
                                           ; else skipping at lower level
#elif
    if(skipLevel == branchLevel &&
       gotLevel < branchLevel)             ; not yet found
        if(predicate)
            gotLevel = branchLevel         ; found now: stop more elifs
            skipLevel = 0
#endif
    branchLevel--
    if(gotLevel > branchLevel ||           ; if activity above, it must 
       skipLevel > branchLevel )           ; be found & !skipped here
        gotLevel = branchLevel
        skipLevel = 0
                                           ; else skip & found unchanged
*/

//         getPrepControl
//
//   Following a '#' in column 1 a lexeme is scanned 
// and a directive token returned if recognized, else 0.
//
// To identify one of a small and constant set of names
// we walk a tree of char decisions.

control_ LEX::getPrepControl(aCHAR &c)
{
  int discrim;                          // discriminator returned by strcmp 
  aCHAR *s;
  uintCH size;

  s = nameBuffer+1;  // not necessarily initialized yet
  c = blackchar(' ');
  getName(s, c, size);    // c is delim, may be LF, size a ref, valued filled in

#ifdef BUG_PREP
 DUMP << "getPrepControl: " << s << NL << FLUSH;
#endif
  discrim = Lstrcmp(s, aS("ifdef"));      
  if(discrim < 0)
    {
      discrim = Lstrcmp(s, aS("else"));      
      if(discrim < 0)
        {
          discrim = Lstrcmp(s, aS("define"));      
			 if(discrim < 0)
				return HUHTK;
          else
            if(discrim > 0)
              {
                discrim = Lstrcmp(s, aS("elif"));
                return discrim == 0 ? ELIFTK : HUHTK;
              }
            else
              return(DEFINETK);
        }
      else
        if(discrim > 0)
          {
            discrim = Lstrcmp(s, aS("endif"));      
            if(discrim < 0)
              return HUHTK;
            else
              if(discrim > 0)
                {
						discrim = Lstrcmp(s, aS("enum"));      
						if(discrim < 0)
                    return HUHTK;
						else
						  if(discrim > 0)
							 {
								discrim = Lstrcmp(s, aS("error"));
								if(discrim < 0)
								  return HUHTK;
								else
								  if(discrim > 0)
									 {
										discrim = Lstrcmp(s, aS("if"));
										return discrim == 0 ? IFTK : HUHTK;
									 }
								  else
									 return discrim < 0 ? HUHTK : ERRORTK;
							 }
						  else
							 return discrim < 0 ? HUHTK : ENUMTK;
					 }
              else
                return(ENDIFTK);
          }
        else
          return(ELSETK);
    }
  else
    if(discrim > 0)
      {
        discrim = Lstrcmp(s, aS("include"));      
        if(discrim < 0)
          {
            discrim = Lstrcmp(s, aS("ifndef"));      
            return discrim == 0 ? IFNDEFTK : HUHTK;
          }
        else
          if(discrim > 0)
            {
              discrim = Lstrcmp(s, aS("line"));      
              if(discrim < 0)
                return HUHTK;
              else
                if(discrim > 0)
                  {
                    discrim = Lstrcmp(s, aS("undef"));      
                    if(discrim < 0)
                      {
                        discrim = Lstrcmp(s, aS("pragma"));
                        return discrim == 0 ? PRAGMATK : HUHTK;
                      }
                    else
                      return discrim > 0 ? HUHTK : UNDEFTK;
                  }
                else
                  return(LINETK);
            }
          else
            return(INCLUDETK);
      }
    else
      return(IFDEFTK);
}

void LEX::doPrepControl(control_ controlToken, aCHAR &c)
{
  RAND_ rand;

  preprocessing = true;
  switch(controlToken)
    {
    case ENDIFTK:
      --branchLevel;
      //      if(branchLevel < 0)
      //        {pXCPT->Error(badprepE, FILO, LINO, aS("Unmatched #endif"));}
      PREPTRACE(aS("endif"));
      if((skipLevel > branchLevel) ||(gotLevel > branchLevel))
        {
          skipLevel  = 0;
          gotLevel = branchLevel;
        }
      break;
    case ELSETK:
      if(branchLevel < 1)
        {pXCPT->Error(badprepE, FILO, LINO, aS("Unmatched #else"));}
      
      if((skipLevel == branchLevel) && (gotLevel < branchLevel))
        {
          skipLevel = 0;                       // found else 
          gotLevel = branchLevel;
        }
      else
        if(skipLevel == 0)
          skipLevel = branchLevel;             // skip this 
      
      break;
    case ELIFTK:
      if(branchLevel < 1)
        {pXCPT->Error(badprepE, FILO, LINO, aS("Unmatched #else"));}

      if((skipLevel == branchLevel) &&
         (gotLevel != branchLevel))
        {
          if(!CExpr(c, NOCPREC, 0, 0, rand))
            {pXCPT->Error(badprepE, FILO, LINO, 
                          aS("Invalid constant expression"));}
          if(rand.l)
            {
              gotLevel = branchLevel;
              skipLevel = 0;
            }
        }
      else
        {
          if(gotLevel == branchLevel)
            skipLevel = branchLevel;
          //            SKIPLINE();
        }
      break;
    case IFTK:
      PREPTRACE(aS("if"));
      ++branchLevel;
      if(skipLevel)
        break;
      if(!CExpr(c, NOCPREC, 0, 0, rand))
        {pXCPT->Error(badprepE, FILO, LINO, 
                      aS("Variable in constant expression"));}
      if(rand.l)
        gotLevel = branchLevel;
      else
        skipLevel = branchLevel;     
      break;
    case IFDEFTK:
      PREPTRACE(aS("ifdef"));
      ++branchLevel;
      if(skipLevel)
        break;
      expanding = false;                       // don't let lex expand this 
      if(is_defined(c))
        gotLevel = branchLevel;
      else
        skipLevel = branchLevel;     
      expanding = true;
      break;
    case IFNDEFTK:
      PREPTRACE(aS("ifndef"));
      ++branchLevel;
      if(skipLevel)
        break;
      expanding = false;                       // don't let lex expand this 
      if(!is_defined(c))
        gotLevel = branchLevel;
      else
        skipLevel = branchLevel;
      expanding = true;
      break;
    case DEFINETK:
      if(!skipLevel)
        scanDefinition(scanDefiniend(c), c);
      else
        pLEX->lineComment(c);
      break;
    case LINETK:
      if(skipLevel)
      {
        pLEX->lineComment(c);
        break;
      }
      acc = 0;
      c = blackchar(c);
      while( pLEX->is_digit(c))
        {
          acc *= 10;                           // accumulate
          acc += c - '0';
          c = rawchar();
        }
      if(acc)
        stream_reader->getStream()->setLineNo( (unsigned short)acc );
      break;
    case INCLUDETK:
      if(skipLevel)
      {
        pLEX->lineComment(c);
        break;
      }
      doIncl(c);                                // find the file 
      break;
    case PRAGMATK:
      if(skipLevel)
      {
        pLEX->lineComment(c);
        break;
      }
      doPragma(c);
      break;
    case UNDEFTK:
      if(skipLevel)
      {
        pLEX->lineComment(c);
        break;
      }
      doUndef(c);
      break;
	 case ENUMTK:
      if(skipLevel)
      {
        pLEX->lineComment(c);
        break;
      }
		scanEnum();
		break;
    case ERRORTK:
      if(skipLevel)
      {
        pLEX->lineComment(c);
        break;
      }
      pXCPT->Error(badprepE, FILO, LINO, aS("#error directive encountered"));
      break;
    default:
      pXCPT->Error(badprepE, FILO, LINO, aS("Control not implemented "));
    }                                          // end switch 
  if(c == LEOF)
    return;
//  if(stream_reader->col > 2)
//    stream_reader->getLine();             // get next line if not already
  preprocessing = false;
}

//
// is_defined is solely for #IFDEF and #IFNDEF controls where the
// argument is simply a (possibly parenthesised) name.
//
bool LEX::is_defined(aCHAR &c)
{                                              // does not expect 'defined'
  bool lpred;
  aCHAR *s, namebuf[64];
  uintCH size;
  MACRO *macRef, *previous = NULL;
  CRator_ opToken = NOCOP;

  c = blackchar(c);                            // get to black
  if(c == aS('('))
    {
      c = blackchar(' ');
      lpred = is_defined(c);
      if(c != aS(')'))
        pXCPT->Error(badprepE, FILO, LINO, 
                     aS("Unbalanced parens in predicate"));
      c = rawchar();
      return lpred;
    }
  if(!is_letter(c))
    return false;

  s = &namebuf[1];
  getName(s, c, size);
  *(s-1) = size;
  lpred = dfined(s, 0, previous, macRef) ? true : false;

  c = blackchar(c);
  return lpred;
}  
/*
 * In spite of abstracting away support functions as far as possible,
 * CExpr is hideously complicated. It employs the precedence of
 * operators from the CRator_ type, and operands of class RAND_,
 * which are labelled unions of all possible types.
 * It is called with a ceiling precedence, and will only process 
 * expressions tighter than that.
 * If the expression is parenthesised then it recurses immediately,
 * otherwise, it attempts to find a prefix operator denotation and
 * then identify it. Some operators are ambiguous between prefix and 
 * infix, so the identifier always returns the infix form which is then
 * promptly corrected.  If there was a prefix operator then we recurse
 * to find the operand, otherwise we resolve some form of atom.
 * If the atom is a name then we check to see if it denotes "defined"
 * which is another prefix operator. The argument to this is a
 * (possibly parenthesised) name, so we apply it and then apply the 
 * original prefix operator, if there was one. A name which is not a 
 * prefix operator is invalid, since we only expect constant expressions.
 * Then we look for another operator and check if it is postfix or infix.
 * A postfix operator can be applied at once, but an infix operator
 * must be compared with the ceiling. If it is higher, then we push 
 * it back on the stream and return. Otherwise, we recurse to find 
 * the right operand, and then apply the infix operation. If the 
 * left and right operands are of different arithmetic type they are
 * promoted to float. There are separate routines for integer and float 
 * operands.
 * Notice that this depth first approach will not apply any infix 
 * operator until the rightmost operand is evaluated.
 */
// All of this, and what it calls is just for #IF expressions
bool LEX::CExpr(aCHAR &c, CRator_ Prec, bool is_LRand, aCHAR delim, RAND_ &rand)
{                            // We are only interested in constant expressions
  RAND_ lrand, rrand;
  CRator_ opToken;
   bool is_prefix, is_postfix, is_infix;
  uintCH ratorSize;                             // doubles as truth value
  aCHAR *pc;

  c = blackchar(c);
  if(c == aS('('))
    {                                           // nested expression
      c = blackchar(' ');
      CExpr(c, NOCPREC, 0, aS(')'), rand);      // pass it straight back up
      if(c == aS(')'))
        {
          c = rawchar();
          return true;
        }
      else
        pXCPT->Error(badprepE, FILO, LINO, 
                     aS("Unbalanced parens in predicate"));
    }
  ratorSize = scanCRator(c, nameBuffer);        // get rator if there is one 

  is_prefix =                                   // get prefix token
    ratorSize ? CPrefixToken(nameBuffer, ratorSize, opToken) : 0;
  
  if(is_prefix)
    ratorSize = 0;                              // have used up rator

  if(is_LRand &&                                // resolve ambiguities 
     ((opToken == PLUSCOP) || (opToken == MINUSCOP) || (opToken == MULTCOP)))
    is_prefix = false;                          // ambig. rator is infix
  else
    opToken =                                   // ambig. rator is prefix
      opToken == PLUSCOP  ? opToken = PREPLUSCOP :
     (opToken == MINUSCOP ? opToken = PREMINUSCOP :
     (opToken == MULTCOP  ? opToken = STARCOP : opToken));

  if(is_prefix)
    CExpr(c, opToken, 0, 0, lrand);             // get prefix's rand
  else
    {                                           // not a prefix, so far
      pc = nameBuffer+1;
      getCAtom(c, pc, lrand);                   // try to get lrand
      c = blackchar(c);
      if(c == aS(')'))
        {
          rand = lrand;
          return true;
        }
      if(Lstrcmp(pc, aS("defined")) == 0)
        {
          lrand.kind = T_INT;
          lrand.l = is_defined(c);
        }
    }

  if(is_prefix)
    lrand.evalPreop(opToken);                   // perform pre-op on lrand

  if(!c)
    {
      rand = lrand;
      return true;
    }
  
  ratorSize = scanCRator(c, nameBuffer);        // get rator if there is one 

  is_postfix =                                  // get postfix tok
    ratorSize ? CPostfixToken(nameBuffer, ratorSize, opToken) : 0;
  if(is_postfix)
    {
      ratorSize = 0;                            // have used up rator
      lrand.evalPostop(opToken);                // perform post-op on lrand
    }
l2:
  if(!ratorSize)
    ratorSize = scanCRator(c, nameBuffer);      // get another rator
  if(c == delim)
    {
      rand = lrand;
      return true;
    }

  is_infix =                                    // get rator token
    ratorSize ? CInfixToken(nameBuffer, ratorSize, opToken) : 0;

  if((opToken >> 4) > (Prec >> 4) || !is_infix) // compare precedence
    {                                           // this sub-expr ends here
      rand = lrand;                             // result
      stream_reader->change_next_char(aS(' '));

      while(ratorSize-- > 0)                    // let caller read it again
        backupScan();
          
      return true;
    }

  if((opToken ==  ORCOP &&  lrand.l) ||         // look for short cut
     (opToken == ANDCOP && !lrand.l))
    {                                
      skip(c, delim);                           // ignore the rest
      rand = lrand;
      return true;
    }

  CExpr(c, opToken, 1, delim, rrand);           // get rrand
  
  if(opToken & ASSCOP)
    lrand.evalAssop(opToken, rrand);            // eval assign operation
  else                                          // eval other infix operation
    {
      /*
      if(lrand.kind == T_VAR)
        {
          Lsprintf(nameBuffer, 128, 
                   aS("Variable <%s> in constant expression"), lrand.s+1);
          pXCPT->Error(badprepE, FILO, LINO, nameBuffer);
        }
        */
      if((opToken & 0x180) && lrand.promote(rrand))
        rand.evalFInfix(opToken, lrand, rrand); // arith with both float
      else
        rand.evalIInfix(opToken, lrand, rrand); // both ints
    }
  if(c == delim)
    return true;

  ratorSize = 0;
  goto l2;
}

bool RAND_::promote(RAND_ &rrand)                 // this is lrand
{                                               // true when rands are float
  if(kind == T_DOUBLE)
    if(rrand.kind == T_DOUBLE)
      return true;
    else
      {
        rrand.kind = T_DOUBLE;
        rrand.d = (double)(rrand.l);            // promotion
        return true;
      }
  else
    if(rrand.kind == T_DOUBLE)
      {
        kind = T_DOUBLE;
        d = (double)(l);                        // promotion
        return true;
      }
    else
        return false;
}

bool LEX::getCAtom(aCHAR &c, aCHAR *pc, RAND_ &rand)
{                                               // must return LFs
  bool result;
  uintCH size;
  aCHAR d;
  MACRO *macRef, *previous = NULL;

  float_flag = hex_flag = char_flag = 0;

  c = blackchar(c);
  if(c == aS('('))
    {
      c = blackchar(' ');
      result = getCAtom(c, pc, rand);           // try to get rand
      if( c == aS(')'))
        {
          c = rawchar();                        // consume )
          return result;
        }
      else
        pXCPT->Error(badprepE, FILO, LINO, 
                     aS("Unbalanced parens in expression"));
    }
  if(is_letter(c))
    {
      getName(pc, c, size);      
      *(pc-1) = size;
      if(dfined(pc, 0, previous, macRef))         // a macro name
        if(macRef->vacant())
          if(macRef->dynamic)
            macRef->callDynamic(m_peng, NULL);
          else;                                   // vacant & not dynamic
        else
          {                                       // not vacant   
            backupScan();                         // clear decks for macro body
            c = aS(' ');
            macRef->applyMacro(c);                // make new reader, get args
            return getCAtom(c, pc, rand);         // try to get rand
          }
      else
        {                                         // name not a macro
          rand.l = 0;                             // undef name has value 0
          rand.kind = T_INT;
        }
    }
  else
    if(c == aS('-'))
      {
        d = rawchar();
        if(is_digit(d))
          scanNumber(d, pc, 1, rand);
      }
    else
      if (is_digit(c))                            // number 
        scanNumber(c, pc, 0, rand);
  
  if (float_flag)
    {
      rand.d = (double)Latof(nameBuffer+1);
    }
  return true;
}

MACRO * LEX::scanDefiniend(aCHAR &c)
{                                                // parses name & formal pars
  aCHAR *free, *macName = nameBuffer+1;
  uintCH size;
  intC  hix;
  MACRO * macRef, *previous = NULL;
  bool    noParNames = 0;
  bool    niladic = 0;
  intCH arity;
  aCHAR *defalt[16];                           // temp for arg defaults

  c = blackchar(c);                            // definiend initial
  getName(macName, c, size);                   // macro name
  nameBuffer[0] = size;
  if(size == 1)
  {
     pXCPT->Error(badprepE, FILO, LINO, 
						aS("A char is a char. Do not redefine"));
  }
  if(Lstrcmp(nameBuffer+1, aS("define")) == 0) // nono 
    {pXCPT->Error(badprepE, FILO, LINO, aS("Attempt to redefine define"));}
  free = formalSpace;                          // temp for formal names

  if(c == aS('/'))
    {                                          // no par names
      c = binaryacc(4, 1, free);
      arity = (intCH)acc;
      for(int i = 0; i <= arity; i++)
        defalt[i] = NULL;
      noParNames = 1;
      backupScan();
    }
  else
    {    
      arity = 0;
      if(c == aS('('))
        {                                      // get par names
          for(arity = 1; c != aS(')'); arity++)
            {                                  // process named pars
              do
                {
                  c = blackchar(' ');          // lose the comma or '('
                } while(!c);                   // and the LF 
                
              if(arity == 1 && c == aS(')'))
                {
                  niladic = 1;
                  break;
                }
              size = 0;
              getName(free + 1, c, size);
              *free = size;                    // size goes before param
              free+= size + 2;
              c = blackchar(c);
              if(c == aS('='))
                {                              // default, use macro heap
                  defalt[arity] = macroFree;
                  getBalancedText(macroFree, c, aCHAR(','),
                                  macroHeap + macroHeapsz, 0);
                  *macroFree++ = 0;            // may be empty string
                }
              else
                defalt[arity] = NULL;          // no default
            }                                  // end for arity
          c = blackchar(' ');                  // consume ')'
          arity--;
        }                                      // end if('(')
    }                                          // end par names

  hix = macroHash(macName);                    // now we have name/arity
  if(!dfined(macName, hix, previous, macRef))  // look it up
      macRef = addMacro(nameBuffer + 1, arity, hix); // new macro object 
  macRef->noParNames = noParNames;
  macRef->niladic = niladic;
  for(int i = 1; i <= arity; i++)
    macRef->defalt[i] = defalt[i];             // copy defaults into macro

  return macRef;
}

void LEX::scanDefinition(MACRO *macRef, aCHAR &c)
{                 // if user used formal par names replace them with par index
  aCHAR  quote, d;
  aCHAR *bodyStart;

  uintCH size, ix;
  aCHAR *catpoint;                             // where to start concatenation

  c = blackchar(c);
//  if(macRef && !c)
  if(macRef && c == '\n')
    {
      macRef->body = NULL;                     // macro is vacant
      return;
    }
  expanding = false;
  bodyStart = macroFree;
  //while(c) 
  while(c != '\n') 
    {                                          // scan the body until LF
      //      skipComments(c);

      if(c == aS('\\') && stream_reader->lookahead() == '\n')
        {                                      // line continuation
          c = rawchar();                       // ignore backslash, get LF
          c = aS(' ');                         // substitute space for LF
        }
      //if(!c)
      if(c == '\n')
        break;
      if(skipWhite(c))
        continue;                              // char read, better restart


      if(c == LEOF)                            // should have seen LF first
        pXCPT->Error(badprepE, FILO, LINO, aS("Unexpected EOF"));

      if(c == aS('#'))
        {          // take everything literally except references to par names
          d = rawchar();
          if(d == aS('#'))
            {                                  // a cat op
              c = blackchar(' ');
              macroFree = catpoint;            // back up
            }
          else
            {                                  // not a cat op
              *macroFree++ = c;
              while(is_digit(d))
                {
                  *macroFree++ = d;
                  d = rawchar();   
                }
              if(d != aS('#'))
                pXCPT->Error(badprepE, FILO, LINO, 
                             aS("bad macro arg index\n"));
              *macroFree++ = d;
              c = rawchar();
            }
          continue;
        }

      if((char_class(c) == QUOTE)  || (c == aS('$')))
        {
          quote = c;
          *macroFree++ = c;                    // put quote mark
          grab_str(macroFree, c, c, macroSpaceLeft());
          *macroFree++ = quote;                // put quote mark
          //          c = rawchar();
          continue;
        }
      if (is_letter(c)) 
        {                                      // name
          getName(macroFree, c, size);         // macroFree is temp for name
          if(is_formalName(macRef, ix))
            {                                  // replace name  
              *macroFree++ = aS('#');
              size = Lsprintf(macroFree, 8, aS("%d"), ix);
              macroFree+= size;
              *macroFree++ = aS('#');
            }
          else
            macroFree+= size ;                 // keep name
          catpoint = macroFree;
          skipWhite(c);
          continue;
        }  
      //      reader->getLine();
      *macroFree++ = c;                        // vanilla
      catpoint = macroFree;
      if(!c)
        break;
      c = rawchar();   
    }                                          // end main for loop
  expanding = true;                            // end while. we must be at LF
  *macroFree++ = 0;
}
/*
void LEX::skipComments(aCHAR &c)
{
      if(fileLang == pro && c == aS('%'))
        skip(c, 0);
      else
        if(fileLang == cpp && c == aS('/') && reader->lookahead() == aS('/'))
          skip(c, 0);
        else
          if(c == aS('/') && reader->lookahead() == aS('*'))
            blockComment(c);
}
*/
bool LEX::skipWhite(aCHAR &c)                    // only for macro bodies
{                                              // return false if c was black
  bool wasWhite;                                 // compresses white to 1 char
                                               // set c to black
  for(wasWhite = 0; char_class(c) == WHITE; wasWhite|= 1, c = rawchar())
    *macroFree = c;                            // don't bump yet
  return wasWhite ? macroFree++, true : false; // bump maybe once now
}

bool LEX::is_formalName(MACRO * macRef, uintCH &ix)
  {                         // sets ix to index of matching par name, or fails
    aCHAR *s = formalSpace;
    uintCH size;

    for(ix = 1; ix <= (uintCH)macRef->arity; ix++, s+= (size+1))
      {                                        // walk s along formal names
        size = *s++;
        if(Lstrcmp(macroFree, s) == 0)         // macroFree is the name
          return true;
      }
    ix = 0;
    return false;
  }

void LEX::scanEnum(void)
{                                              // interprets C enum declaration
  aCHAR  nameBuffer[34], intList[512];
  aCHAR *s = intList;
  aCHAR *enumName = nameBuffer+1;
  aCHAR  c;
  MACRO *enumMacRef, *previous = NULL;
  int    hix;
  bool     hasName = false;
  int    size;
  uintCH esize;

  c = blackchar(' ');
  if(is_letter(c))
    {
      getName(enumName, c, esize);              // enum has a name
      nameBuffer[0] = esize;
      hasName = true;
    }
  
  c = blackchar(c);
  if(c != aS('{'))
    pXCPT->Error(badprepE, FILO, LINO, aS("no body in enum"));
  
  c = aS(',');
  for(intC i = 0; c == aS(','); i++)
    {                                         // walk enum list
      getEnumItem(c, i);
      size = Lsprintf(s, 16, aS("%d"), i);      
      s+= size;
      if(c == aS(','))
        {
          *s++ = c;
          *s++ = aS(' ');
        }
    }
  *s = EOS;                                    // c is }

  if(hasName)
    {                                          // can now use macroFree
      hix = macroHash(enumName);                
      if(!dfined(enumName, hix, previous, enumMacRef))   // look it up
        enumMacRef = addMacro(enumName, 0, hix); // new macro object 
      else
        pXCPT->Error(badprepE, FILO, LINO, 
                     aS("Attempt to redefine enum name"));
      size = macroSpaceLeft();
      if(size < 512)
        pXCPT->Error(badprepE, FILO, LINO, aS("macro heap overflow"));
      else
        size = 1 + Lsprintf(macroFree, size, aS("[%s]"), intList);
      macroFree+= size;
    }
  stream_reader->change_next_char(EOS); 
}

void LEX::getEnumItem(aCHAR &c, intC &i)
{
  aCHAR  *macName = nameBuffer+1;
  MACRO  *macRef, *previous = NULL;
  intC   hix;
  uintCH size;

  c = blackchar(' ');
  if(is_letter(c))
    {                 
      getName(macName, c, size);
      nameBuffer[0] = size;
      hix = macroHash(macName);   
      if(!dfined(macName, hix, previous, macRef)) // look it up
        macRef = addMacro(macName, 0, hix);    // new macro object 
      else
        pXCPT->Error(badprepE, FILO, LINO, aS("duplicate enum item"));
      c = blackchar(c);
      if(c == aS('='))
        {
          decimalacc(c, macroFree, false);
          macroFree++;
          i = (intC)acc;                             // set i to prescribed value
        }
      else
        {                                      // use current i
          size = Lsprintf(macroFree, 20, aS("%d "), i);
          macroFree+= size + 1;
        }
    }
  else
    pXCPT->Error(badprepE, FILO, LINO, aS("enum item not recognised"));
}

void LEX::doUndef(aCHAR &c)
{
  aCHAR *p;
  uintCH size;
  MACRO *macRef, *previous = NULL;
  intC   hix;

  p = &nameBuffer[1];
  c = blackchar(c);
  getName(p, c, size);  
  nameBuffer[0] = size;
  hix = macroHash(p);   
  if(dfined(p, hix, previous, macRef))          // look it up
    {
      if(previous)
        previous->chain = macRef->chain;
      else
//        bucket[hix] = NULL;
        bucket[hix] = macRef->chain;      // a fix?
      delete macRef;
    }
}

void LEX::doPragma(aCHAR &c)
{
  aCHAR  name[34], nameBuffer[128];
  aCHAR *p;
  uintCH size;

  p = name;
  c = blackchar(c);
  getName(p, c, size);      
  if(Lstrcmp(name, aS("message")) == 0)
    {
      c = blackchar(c);
      if(c != aS('('))
        pXCPT->Error(badprepE, FILO, LINO,
                     aS("pragma message left paren not found"));
      p = nameBuffer;
      c = blackchar(' ');                     // should be "
      grab_str(p, c, c, 128);
      c = blackchar(' ');                     // consume initial "      
      Lprintf(nameBuffer);
      Lprintf(aS("\n"));
      c = blackchar(c);
      if(c != aS(')'))
        pXCPT->Error(badprepE, FILO, LINO,
                     aS("pragma message right paren not found"));
    }
  stream_reader->getLine();
}

void LEX::doIncl(aCHAR &c)
{
#ifdef BUG_PREP
 DUMP << "including..." << NL;
#endif
   STUB(aS("LEX::doIncl"));
/*
  aCHAR   path[128], name[128];
  aCHAR  *p = &path[0], *q;
  aCHAR   system[_MAX_PATH];
  aCHAR  *system_path = &system[0];
  MACRO  *macRef, *previous = NULL;
  control_ controlToken;
  bool    localFlag;
  char    fname_SA[_MAX_PATH];
  uintCH  size;

  c = blackchar(c);
  if(c == aS('"'))
    {
      grab_str(p, c, c, 128);
      localFlag = true;
    }
  else
    if(c == aS('<'))
      {
        grab_str(p, c, aS('>'), 128);           // given path
        localFlag = false;
      }
  //  c = blackchar(' ');                       
  
  for(q = p; q > path; q--)                    // get qualifier
	 {
		if(*q == aS('.'))
		  break;
	 }
  //  pIO->reader()->backupScan(false);            // read again later
  fileLang = classify(q);
  
  if(fileLang == plm)
    pXCPT->Error(badprepE, FILO, LINO, aS(".plm files may not be included"));
  if(fileLang == xpl)
    pXCPT->Error(badprepE, FILO, LINO, aS(".xpl files may not be included"));
  
  PATOM pathA = pATAB->EnterAtom(path);
  int len = Lstrlen(*pathA) + 1;
  wcstombs(fname_SA, *pathA, len);
  
  for(int h = 4; h < pIO->stream.size(); h++)
	 if((pIO->stream[h])->get_name() == pathA)  // file already in use, so abort
		pXCPT->Error(streamsurfeitE, (STRptr)*pathA, 
		(pIO->stream[h])->getMode());
  
  //ISTREAM *rdr= new ISTREAM(m_peng, NULL, NULL, read_, text_, pLEX->bufSize);
  PrologStream *rdr;
  LNEW(rdr, FilePStream(m_peng, pathA), aS("reader"));

  //pIO->pushNewIstream(rdr);                     // new ISTREAM, not yet open
  pIO->push_stream(read_handle, rdr);
  
  if(rdr->sio.is_open())                       
	 goto opened;

  delete rdr;
  system_path = pLENV->amzi_directory();
  pathA = pATAB->EnterAtom(path);
  Lstrcat(system_path, path);
  len = Lstrlen(*pathA) + 1;
  wcstombs(fname_SA, *pathA, len);
  rdr =                                       // else try to open here
	 new ISTREAM(m_peng, pathA, NULL, read_, text_, pLEX->bufSize);
  if(rdr->sio.is_open())
	 goto opened;

  Lsprintf(name, 128, aS("include file %s  not found"), path);
  pXCPT->Error(badprepE, FILO, LINO, name);

 opened:
  rdr->kind = INCLUDE_;
  pIO->push_stream((PROSTREAM *)rdr);                  // uses read_handle
#ifdef BUG_PREP
  DUMP << "opened included file: " << path << NL;
#endif

  if(fileLang == pro)
		return;

  p = &name[1];                                // must be an h file	 
  while(c != LEOF)
    {                                          // scan the whole file
      while(!c)
        c = rawchar();
      c = blackchar(c);

      if(c == aS('#'))                         // prep directive
        {
          controlToken = getPrepControl(c);    // get control token
          if(controlToken == ENDIFTK || controlToken == ELSETK)
            {                                  // on new line already
              doPrepControl(controlToken, c);  // do it
              c = blackchar(c);
            }
          else
            {                                  // still on same line
              c = blackchar(c);
              doPrepControl(controlToken, c);  // do it
            }
          continue;
        }
      else
        if(skipLevel)
          stream_reader->getLine();
        else
          if (c == aS('e'))
            {                                  // may be enum
              getName(p, c, size);      
              *(p-1) = size;
				  if(Lstrcmp(p, aS("enum")) == 0)
					 scanEnum();
				  else
					 if(dfined(p, 0, previous, macRef))
						if(macRef->vacant())
						  if(macRef->dynamic)
							 macRef->callDynamic(m_peng, NULL);
            }
      stream_reader->getLine();
    }                                          //  end while
  stream_reader->close();
  //pIO->popStream(pIO->current_input);
  pIO->pop_stream();
  //  c = rawchar();
*/
}

lang_ LEX::classify(aCHAR *q)
{
  if(Lstrcmp(q, aS(".h")) == 0)
    return cpp;                                // wild assumption
  if(Lstrcmp(q, aS(".pro")) == 0)
    return pro;
  if(Lstrcmp(q, aS(".plm")) == 0)
    return plm;
  if(Lstrcmp(q, aS(".xpl")) == 0)
    return xpl;
  return unknown;
}  
/*
void LEX::macHisto(histoArg_ histo)
{                                           // supports macnames and mac$histo
  int i, buckets = 32, slot[32], liveBuckets = 0, total = 0;
  float devsq = (float)0.0, average;
  MACRO *macRef;
  aCHAR nameBuffer[256];
  IODEV ioOld;

  ioOld = pSIO->SaveioNow();                // like termWriteString
  //  quoting = FALSE;
  nameBuffer[0] = EOS;
  
  pPSTR->SetPutSMax(256);
  pSIO->SetioNow(STRINGIO, &nameBuffer[0]);
  
  Cell x;
  x.setInt(-1);
  pSIO->ioGetIOCB(&x);

  for(i = 0; i < buckets; i++)
    {
      slot[i] = 0;
      if(!bucket[i])
        {
          if(histo == histogram)
            pSIO->ioPutString(aS("\n"));
          continue;
        }

      liveBuckets++;
      for (macRef = bucket[i]; 
           macRef;
           macRef = macRef->chain)
        {
          slot[i]++;
          switch(histo)
            {
            case macNames:
              macRef->display();
              break;
            case histogram:
              pSIO->ioPutString(aS("*"));
              if(!(macRef->chain))
                pSIO->ioPutString(aS("\n"));
              break;
            }
        }                                            // end chain
      total+= slot[i];
    }                                                // end buckets

  Lsprintf(nameBuffer, 256,aS( "\nsymbols interned = %d\n"), total);
  pSIO->ioPutString(nameBuffer);

  if(histo == histogram)
    {
      average = (float)total / liveBuckets;
      for(i = 0; i < buckets; i++)
        devsq+= (float)pow(fabs((double)((float)slot[i] - average)), 2.0);
      Lsprintf(nameBuffer, 256, aS("\nmacro space free = %d\n"), 
               macroSpaceLeft());
      pSIO->ioPutString(nameBuffer);
      Lsprintf(nameBuffer, 256, aS("average chain length = %6.2f \n"), average);
      pSIO->ioPutString(nameBuffer);
    }
  pSIO->RestoreioNow(ioOld);
}
*/
void MACRO::display()
{  // displays an individual macro
  aCHAR nameBuffer[256], *np = 1+&name[0], *bp = &nameBuffer[0];
  int size;

  //std::ostream *log = pIO->getLogCppStream();

  pLEX->expanding = false;                  // do not expand

  size = Lsprintf(bp, 256, aS("%s/%d  "), np, arity); 
  if(body && !dynamic)
    Lsprintf(bp + size, 256 - size, aS("%s\n"), body); 
  else
    Lsprintf(bp + size, 256 - size, aS("\n")); 
  //pIO->stream[pIO->current_output]->put_string(nameBuffer);
  LOG( nameBuffer );

  for(int i = 0; i < arity; i++)
    if(defalt[i])
      {                                     // display default args
         Lsprintf(bp, 256, aS("default arg %d: %s\n"), i, defalt[i]); 
         //pIO->stream[pIO->current_output]->put_string(nameBuffer);
         LOG( nameBuffer );
      }
  pLEX->expanding = true;
}

void  LEX::setIncDirs(void)
{
  intC len;

  incfree = &incdir[0];
  int ninc = 0;
//#ifdef UNIX
  //*incfree++ = aS("/usr/include/");
//#endif
  
//#ifdef OPENVMS
//  extern char   *getenv();
  
//  if (getenv("C$LIBRARY")  != NULL)
//    *incfree++ = "C$LIBRARY:";
//  *incfree++ = "SYS$LIBRARY:";
//#endif  
  
//#ifdef WINDOWS
// This shouldn't be necessary here, should move path walk
// to lenv.cpp, let lfopen handle it.
#ifdef WINDOWS
  char pathsep = ';';
  char dirsep = '\\';
#else
  char pathsep = ':';
  char dirsep = '/';
#endif

  aCHAR *envptr ;
  aCHAR *envsave;
  aCHAR *incptr ;
  
  envptr = envsave = Lgetenv_dup(aS("INCLUDE")) ;
//  if (! envptr)
//    {                                               // try something. 
//      *incfree++ =aS("D:\\Program Files\\DevStudio\\VC\\INCLUDE\\");
//    }
//  else
  if (envptr)
  {
      len = (intC)Lstrlen(envptr) + 17;
      //*incfree++ = incptr = incsave = new aCHAR[len] ;
      LNEW(incsave, aCHAR[len], aS("reader"));
      *incfree++ = incptr = incsave;
      while (*incptr = *envptr++)               // walk along %PATH%
        if (*incptr == pathsep)
          {                                     // end of one path
            *incptr++ = dirsep ;                  // terminate it
            *incptr++ = '\0' ;
            *incfree++ = incptr ;               // bump free entry
            ninc++;
            if (ninc > 15)
               pXCPT->Error(internalE, aS("macro include dirs"));
          }
        else incptr++ ;

      while(--incptr == '\0');                  // back up to last char
      if(*incptr != dirsep)                       // test it
        {                                       // needs backslash
          *++incptr = dirsep;
          *++incptr = '\0';
        }            
    }
  delete envsave;
}

uintCH LEX::scanCRator(aCHAR &c, aCHAR *rator)
{                                           // get C rator string & return size
  uintCH size;
  aCHAR *s = rator;

  c = blackchar(c);
  for(size = 0; is_CRator(c); size++)
    {                                           // it consists of rator chars
      *s++ = c;
      c = rawchar();  
    }
  *s = EOS;
  return size;
}

bool LEX::CPrefixToken(aCHAR *rator, uintC size, CRator_ &opToken)
{  // given rator string, returns prefix opToken (which has implied precedence)
  opToken = NOCOP;                                  // failure
  switch(*rator)
    {
    case aS('!'): opToken = NOTCOP; break;
    case aS('~'): opToken = BITNOTCOP; break; 
    case aS('+'): opToken = size == 1 ? PLUSCOP  : NOCOP; break;
    case aS('-'): opToken = size == 1 ? MINUSCOP : NOCOP; break;
    case aS('*'): opToken = size == 1 ? MULTCOP  : NOCOP; break;
    case aS('&'): opToken = size == 1 ? ADDRCOP  : NOCOP; break;
    default:
      return false;
    }
  return opToken == NOCOP ? false : true;    
}
bool LEX::CPostfixToken(aCHAR *rator, uintC size, CRator_ &opToken)
{ // given rator string, returns postfix opToken (which has implied precedence)
  opToken = NOCOP;                                  // failure
  switch(*rator)
    {
    case aS('+'): 
      opToken = *(rator+1) == aS('+') ? POSTINCCOP : NOCOP; break;
    case aS('-'): 
      opToken = *(rator+1) == aS('-') ? POSTDECCOP : NOCOP; break;
    default:
      return false;
    }
  return opToken == NOCOP ? false : true;
}

bool LEX::CInfixToken(aCHAR *rator, uintC size, CRator_ &opToken)
{   // given rator string, returns infix opToken (which has implied precedence)
  opToken = NOCOP;                                  // failure
  switch(*rator)
    {
    case aS('+'): 
      if(size == 1)
        opToken = PLUSCOP;
      else
        if(size == 2)
          if(*(rator+1) == aS('='))
            opToken = PLUSASSCOP;
      break;
    case aS('-'): 
      if(size == 1)
        opToken = MINUSCOP;
      else
        if(size == 2)
          if(*(rator+1) == aS('='))
            opToken = MINUSASSCOP;
      break;
    case aS('s'): opToken = size == 6 ? SIZEOFCOP : NOCOP; break;
    case aS('*'): 
      if(size == 1)
        opToken = STARCOP;
      else
        if(size == 2 && *(rator+1) == aS('='))
          opToken = MULTASSCOP;
      break;
    case aS('/'):
      if(size == 1)
         opToken = DIVCOP; 
      else
        if(size == 2 && *(rator+1) == aS('='))
          opToken = DIVASSCOP;
      break;
    case aS('%'):
      if(size == 1)
         opToken = MODCOP; 
      else
        if(size == 2 && *(rator+1) == aS('='))
          opToken = MODASSCOP;
      break;
    case aS('<'):
      if(size == 1)
         opToken = LESSCOP; 
      else
        if(*(rator+1) == aS('<'))
          if(size == 2)
            opToken = LSHIFTCOP;
          else
            if(*(rator+2) == aS('='))
              opToken = LSHIFTASSCOP;
            else;
        else
          if(*(rator+1) == aS('='))
            opToken = LESSEQCOP;
      break;
    case aS('>'):
      if(size == 1)
         opToken = GRCOP; 
      else
        if(*(rator+1) == aS('>'))
          if(size == 2)
            opToken = RSHIFTCOP;
          else
            if(*(rator+2) == aS('='))
              opToken = RSHIFTASSCOP;
            else;
        else
          if(*(rator+1) == aS('='))
            opToken = GREQCOP;
      break;
    case aS('='):
      if(size == 1)
         opToken = ASSCOP; 
      else
        if(size == 2 && *(rator+1) == aS('='))
          opToken = EQCOP;
      break;
    case aS('&'): 
      if(size == 2)
        if(*(rator+1) == aS('&'))
          opToken = ANDCOP;
        else
          if(*(rator+1) == aS('='))
            opToken = ANDASSCOP;
      break;
    case aS('|'): 
      if(size == 1)
        opToken = BITORCOP; 
      else
        if(size == 2)
          if(*(rator+1) == aS('|'))
            opToken = ORCOP; 
          else
            if(*(rator+1) == aS('='))
              opToken = ORASSCOP;
      break;
    case aS('^'): 
      if(size == 1)
        opToken = XORCOP; 
      else
        if(size == 2 && *(rator+1) == aS('='))
          opToken = XORASSCOP;
      break;
    case aS('?'): opToken = QUERYCOP; break;    // things are a bit funny here
    case aS(':'): opToken = COMMACOP; break;
    }
  return opToken == NOCOP ? false : true;
}

bool LEX::is_CRator(const aCHAR c)
{                            // verifies that c is a C operator char
  switch(c)
    {
    case aS('<'): case aS('>'): case aS('-'): case aS('+'): case aS('*'):
    case aS('/'): case aS('%'): case aS('~'): case aS('='): case aS('!'):
    case aS('^'): case aS('&'): case aS('|'): case aS('?'): case aS(':'):
    case aS('.'):
      return true;
    default:
      return false;
    }
}

bool RAND_::evalPreop(CRator_ opToken)
{
  switch(opToken)
    {                                           // perform pre-op, if any
    case PREPLUSCOP:
      return true;
    case PREMINUSCOP:
      l = -l;
    case STARCOP:
      return true;
    case PREINCCOP:
      l++;
      return true;
    case PREDECCOP:
      l--;
      return true;
    case NOTCOP:
      l = l ? false : true;
      return true;      
    case BITNOTCOP:
      l = ~l;
      return true;
    case DEFINEDCOP:
      l = kind == T_DEFINED ? true : false;
      kind = T_INT;
      return true;
    case ADDRCOP:
    case NOCOP:
    default:
      return false;
    }
}

bool RAND_::evalPostop(CRator_ opToken)
{
  switch(opToken)
    {                                           // perform pre-op, if any
    case POSTINCCOP:
      l++;
      return true;
    case POSTDECCOP:
      l--;
      return true;
    default:
      return false;
    }
}

bool RAND_::evalFInfix(CRator_ opToken, RAND_ lrand, RAND_ rrand)
{                                                // float rands
  kind = T_DOUBLE;
  switch(opToken)
    {       
   case MULTCOP: 
     d = lrand.d * rrand.d;
     break;
   case DIVCOP: 
     d = lrand.d / rrand.d;
     break;
   case PLUSCOP: 
     d = lrand.d + rrand.d;
     break;
   case MINUSCOP: 
     d = lrand.d - rrand.d;
     break;
    default:
      return false;
    }
  return true;
}

bool RAND_::evalIInfix(CRator_ opToken, RAND_ lrand, RAND_ rrand)
{
  kind = T_INT;
  switch(opToken)
    {       
   case MULTCOP: 
     l = lrand.l * rrand.l;
     break;
   case DIVCOP: 
     l = lrand.l / rrand.l;
     break;
   case PLUSCOP: 
     l = lrand.l + rrand.l;
     break;
   case MINUSCOP: 
     l = lrand.l - rrand.l;
     break;
   case MODCOP: 
     l = lrand.l % rrand.l;
     break;
   case LSHIFTCOP: 
     l = lrand.l << rrand.l;
     break;
    case RSHIFTCOP: 
     l = lrand.l >> rrand.l;
     break;
   case LESSCOP: 
     l = lrand.l < rrand.l;
     break;
   case LESSEQCOP: 
     l = lrand.l <= rrand.l;
     break;
   case GRCOP: 
     l = lrand.l > rrand.l;
     break;
   case GREQCOP:
     l = lrand.l >= rrand.l;
     break;
   case EQCOP: 
     l = lrand.l = rrand.l;
     break;
   case NOTEQCOP:
     l = lrand.l != rrand.l;
     break;
   case BITANDCOP:
     l = lrand.l & rrand.l;
     break;
   case XORCOP:
     l = lrand.l ^ rrand.l;
     break;
   case BITORCOP:
     l = lrand.l | rrand.l;
     break;
   case ANDCOP:
     l = lrand.l && rrand.l;
     break;
   case ORCOP:
     l = lrand.l || rrand.l;
     break;
   case QUERYCOP: 
   case COLONCOP:
   case PARENCOP: 
   case BRACKETCOP: 
   case DOTCOP: 
   case ARROWCOP: 
   case NOTCOP: 
   case BITNOTCOP: 
   case SIZEOFCOP: 
   case ADDRCOP: 
   case CASTCOP:
   case COMMACOP:
   case NOCPREC:
    default:
      return false;
    }
  return true;
}

bool RAND_::evalAssop(CRator_ opToken, RAND_ rrand)
{
  switch(opToken)
    {       
   case ASSCOP: 
     l = rrand.l;
     break;
   case MULTASSCOP: 
     l *= rrand.l;
     break;
   case DIVASSCOP: 
     l /= rrand.l;
     break;
   case MODASSCOP: 
     l %= rrand.l;
     break;
   case PLUSASSCOP: 
     l += rrand.l;
     break;
   case MINUSASSCOP: 
     l -= rrand.l;
     break;
   case XORASSCOP: 
     l ^= rrand.l;   
     break;
    case ORASSCOP: 
     l |= rrand.l;
     break;
   case ANDASSCOP: 
     l &= rrand.l;
     break;
   case LSHIFTASSCOP: 
     l <<= rrand.l;
     break;
   case RSHIFTASSCOP:
     l >>= rrand.l;
     break;
    default:
      return false;
    }
  return true;
}















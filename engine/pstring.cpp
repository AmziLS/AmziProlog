/***************************************************************************\
*
* pstring.cpp -- Functions for manipulating Prolog constants that are 
*            one cell or more long, called things.  
*             Also functions for string handling.
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
*
\****************************************************************************/

#include "inc.h"
#include "pch.h"

PString::~PString()
{
   delete strBuffer;
}

void PString::Init(intC strbufsz)
{
   m_buflen = strbufsz;
   LNEW(strBuffer, aCHAR[m_buflen], aS("miscellaneous"));          // used for various string ops.
   //if (strBuffer == NULL)
   //   pXCPT->Error(outofmemE, aS("String Buffer"));

   return;
}   


/*---------------------------------------------------------
** String handling functions
*/
int PString::string_compare(TERM t1, TERM t2)
{
   STRptr ps1, ps2;

   ps1 = t1->getStr();
   ps2 = t2->getStr();

   return(0 == Lstrcmp(ps1, ps2)); 
}   

STRptr PString::_get_term_chars(TERM t)
{
   t = (t)->dref();
   if (t->IsAtom())
      return *(t->getAtom());
   else if (t->IsStr())
      return(t->getStr());
   else
      return(NULL);
}      


/* ----- External Functions ---------------------------------------------- */

int PString::strStringTerm(STRptr s, TERMptr tP)
{
   int   err;

#ifdef BUG_STR
   errDebugMsg("\nstrStringTerm: string = %s\n", s);
#endif

   int h = pIO->makeStringStream(s, read_, LINE_SIZE, true);
   try
   {
      err = pREAD->Read(h, tP);
   }
   catch(LExcept &pE)
   {
      pIO->remove_stream(h);
      throw(pE);
   }
   pIO->remove_stream(h);
   return err;
}

//---------------------------------
// String and Thing Predicates
//


// The original support for substring.
TF PString::p_substring()
{                              // $substring(String, Index, Length, Substring)
   TERM      t, t2, t3;
   STRptr    pstring, endc, psrch;
   intC       index, length, start_index, err;
   aCHAR     tempc;
   //STRptr    pstrb;
   Cell      s, i, l;
   
   pstring = _get_term_chars(pHXL->XVar(0));
   if (!pstring)
     {
       pCNTL->FCut();
       pXCPT->Error(sysargE);
     }

   t2 = (pHXL->XVar(1))->dref();
   t3 = (pHXL->XVar(2))->dref();
      
   if (t2->IsInt() && t3->IsInt()) 
     {                                           // Find substring at index 
       pCNTL->FCut();                            // deterministic 
       index = t2->getInt();
       length = t3->getInt();
       if (index + length > (int) Lstrlen(pstring))
         return(FALSE);
       
       endc = pstring + index + length;        // create substring temporarily 
       tempc = *endc;
       *endc = EOS;
       //if (TRUE != (err = strEnterString( (STRptr)(pstring + index), &pstrb)))
       //  {
       //    *endc = tempc;
       //    return(err);
       //  }
       
       s.setStr(pGCTH->make_string((STRptr)(pstring + index)));
       *endc = tempc;
       return(pTSVC->Unify(&s, pHXL->XVar(3)));
     }
   else if (psrch = _get_term_chars(pHXL->XVar(3)))
     {
       //rrdcif (! pCNTL->BTop() -> BF)   /* first time through */
       if (! (pCNTL->BTop()->flags & BF))   
         {                                       // first time through 
           t = pHXL->heapGET();                  // get cell for index counter 
           pCNTL->BTop() -> HBc = pHXL->HTop();  // reserve it 
           //pTSVC->ATag(t, consT | intS);
           //pTSVC->AiValue(t, 0);
		    t->setInt(0);                         // always start at index 0 
           start_index = 0;
         } else
           {
             t = (pCNTL->BTop() -> HBc - 1);     // get last index 
             start_index = 1 + t->getInt();      // start where we left off 
           }      
       endc = Lstrstr(pstring+start_index, psrch);
       if (!endc)
         {
           pCNTL->FCut();
           return(FALSE);
         }
       
       index = (intC)(Lstrlen(pstring) - Lstrlen(endc));
       //pTSVC->AiValue(t, index);
     t->setInt(index);                         // save index for next time 
       //pTSVC->ATag(&i, consT | intS);
       //pTSVC->AiValue(&i, index);
                 i.setInt(index);
       if (TRUE != (err = pTSVC->Unify(pHXL->XVar(1), &i)))
         return(err);
       //pTSVC->ATag(&l, consT | intS);
       //pTSVC->AiValue(&l, Lstrlen(psrch));
       l.setInt((intC)Lstrlen(psrch));
  return (TF)(pTSVC->Unify(pHXL->XVar(2), &l));
     } else
       {
         pCNTL->FCut();
         pXCPT->Error(sysargE);
       }
   return TRUE;                                  // a formality 
}

  /* 
     Note: 
    sub$string is designed to be used in a 'repeat, sub$string' loop.  
    On final failure, the pCNTL->FCut() jumps backwards over the repeat.  
    If index & length are bound, then it is a deterministic call, so 
    remove the repeat as well.
     
     This is a helper for two CPLIB predicates.  They are
     
     sub_string(S,I,L,SS) :-
     string(S), !, repeat, sub$string(S,I,L,SS).
     sub_string(S,I,L,SS) :-
     err$exec(typeE, $arg1 must be string$, sub_string(A,I,L,SA)).
     
     sub_atom(A,I,L,SA) :-
     atom(A), !, repeat, sub$string(A,I,L,SA).
     sub_atom(A,I,L,SA) :-
      err$exec(typeE, $arg1 must be atom$, sub_atom(A,I,L,SA)).
*/
TF PString::p_subZstring()
{                       // sub$string(+StringAtom, ?Index, ?Length, ?Substring)
   TERM      tArg1, tArg2, tArg3, tArg4;
   TERM      t;
   STRptr   endc;
   STRptr   str1, str2;
   intC       index, length, start_index, fullSize, err;
   //char      tempc;
   //STRptr   substr;
   PATOM     subatom;
   Cell      cell;
   
   // Determine if sub_atom or sub_string and set escape predicate,
   //   used in error handling, to the name of the correct ALIB
   //   functor.

   tArg1 = (pHXL->XVar(0))->dref();
   tArg4 = (pHXL->XVar(3))->dref();

   if (tArg1->IsAtom())
     {
       pEXEC->SetEsc_predA(pATAB->FindAtom(aS("sub_atom")));
       str1 = *(tArg1->getAtom());
       if (tArg4->IsAtom())
         str2 = *(tArg4->getAtom());
       else if (!tArg4->IsVar())
         {
           pCNTL->FCut();
           pXCPT->Error(typeE, aS("arg4 must be atom or var"));
         }
     }
   else if (tArg1->IsStr())
     {
       pEXEC->SetEsc_predA( pATAB->FindAtom(aS("sub_string")) );
       //str1 = pTSVC->getSTR(tArg1);
                 str1 = tArg1->getStr();
       if (tArg4->IsStr())
         //str2 = pTSVC->getSTR(tArg4);
         str2 = tArg4->getStr();
       else if (!tArg4->IsVar())
         {
           pCNTL->FCut();
           pXCPT->Error(typeE, aS("arg4 must be string or var"));
         }
     }
   else
     {
       pCNTL->FCut();
       pXCPT->Error(typeE,aS("arg1 must be atom or string"));
     }
   
   tArg2 = (pHXL->XVar(1))->dref();
   tArg3 = (pHXL->XVar(2))->dref();
   

   fullSize =  (int) Lstrlen(str1);

   if (tArg2->IsInt())          
     {                      // No bactracking, so just find substring at index 
       pCNTL->FCut();
       index = tArg2->getInt()-1;          // convert from 1-based to 0-based 
       
       if (tArg3->IsInt())
         {
           length = tArg3->getInt();
           if (index + length > fullSize)
             return(FALSE);
         }
       else if (tArg3->IsVar())
         {
		   if (tArg4->IsVar())
		      length = fullSize-index;
		   else
			   length = (int)Lstrlen(str2) - index;

           if (length < 0)
             return(FALSE);
           
           //pTSVC->putINT(&cell, length);
		    cell.setInt(length);                  // substring length 
           if (TRUE != (err = pTSVC->Unify(tArg3, &cell)))
             return(err);
         }
       else
         pXCPT->Error(typeE, aS("arg3 must be int or var"));
       //endc = str1 + index + length;
       //tempc = *endc;
       //*endc = EOS;
       aCHAR* sub;
       LNEW(sub, aCHAR[length+1], aS("miscellaneous"));    // create substring temporarily 
       for (int i=0; i<length; i++)
         sub[i] = str1[i+index];
       sub[length] = EOS;
       
       if (tArg1->IsAtom())
         {
           //pATAB->EnterAtom( (STRptr)(str1+index), &subatom);
           subatom = pATAB->EnterAtom( (STRptr)(sub));
           cell.setAtom( subatom);
         }
       else 
         {
           //strEnterString( (STRptr)(str1 + index), &substr);
           //strEnterString( (STRptr)(sub), &substr);
           cell.setStr( pGCTH->make_string((STRptr)(sub)));
         }
       //*endc = tempc;
       delete sub;
       return(pTSVC->Unify(&cell, tArg4));
     }
   
   if (!(tArg2->IsInt() || tArg2->IsVar()))
     {
       pCNTL->FCut();
       pXCPT->Error(typeE, aS("arg2 must be int or var"));
     }
   if (!(tArg3->IsInt() || tArg3->IsVar()))
     {
       pCNTL->FCut();
       pXCPT->Error(typeE, aS("arg3 must be int or var"));
     }
   if (tArg4->IsVar())
     {
       pCNTL->FCut();
       pXCPT->Error(instanceE,
                    "arg4 must be instantiated when arg2 or arg3 is var");
     }
   
   else 
     if (str2)
       {
         if (! (pCNTL->BTop()->flags & BF))   
           {                                      // first time through 
             t = pHXL->heapGET();                 // get cell for index cntr 
             pCNTL->BTop()->HBc = pHXL->HTop();   // reserve it 
             //pTSVC->putRHEAPI(t, 0);
                                 t->setRHeapI(0);
             start_index = 0;
           }
     else
       {
         t = (pCNTL->BTop()->HBc - 1);            // get last index
         //start_index = 1 + pTSVC->getRHEAPI(t); // start where we left off
         start_index = 1 + t->getRHeapI();
       }      
     endc = Lstrstr(str1+start_index, str2);
     if (endc == NULL)
       {
         pCNTL->FCut();
         return(FALSE);
       }
     
     index = (intC)(fullSize - Lstrlen(endc));
     //pTSVC->putRHEAPI(t, index);
          t->setRHeapI(index);
     //pTSVC->putINT(&cell, index+1);     // indices start at 1 for interface
          cell.setInt(index+1);
     if (TRUE != (err = pTSVC->Unify(tArg2, &cell)))
       return(err);
     //pTSVC->putINT(&cell, Lstrlen(str2));
          cell.setInt((intC)Lstrlen(str2));
     return(pTSVC->Unify(tArg3, &cell));
     }
   else
     {
       pCNTL->FCut();
       pXCPT->Error(sysargE);
     }
   return TRUE;                                  // a formality 
}

TF PString::p_string_length()
{
   TERM a1;
   Cell a2;
   
   a1 = (pHXL->XVar(0))->dref();
   
   if (! a1->IsStr())
      pXCPT->Error(sysargE);
      
   //pTSVC->ATag(&a2, consT | intS);
   //pTSVC->AiValue(&a2, Lstrlen(a1->getStr()));
   a2.setInt((intC)Lstrlen(a1->getStr()));
   return(pTSVC->Unify(pHXL->XVar(1), &a2));
}

void PString::token_out(aCHAR *token, TERM *head, TERM *tail)
{
   PATOM a;

   *head = pHXL->heapGETN(2);
   //(*tail)->setList( *head);
   (*tail)->setList(*head);
   *tail = *head + 1;

   a = pATAB->EnterAtom(token);
   (*head)->setAtom( a);
}

/*   
   Tokenizes an input string into alphanumberic words (atoms)
   and punctuation (atoms of single character, printing,
   non-alphanumeric characters).  Uses whitespace to separate.

   A special case is a period followed by whitespace, which
   is tokenized as '. '.

   instantiation error
      string a var

   type error
      string not a string
   */
TF PString::p_string_tokens()
{                                       // string_tokens(+string, -tokenlist)
   TERM t1, t2;
   TERM head, tail;
   aCHAR *p, *t;
   aCHAR token[256];

   t1 = (pHXL->XVar(0))->dref();

   if (t1->IsVar())
      pXCPT->Error(instanceE, aS("arg1 must be a string"));
   else if (! t1->IsStr())
      pXCPT->Error(typeE, aS("arg1 must be a string"));

   p = t1->getStr();
   t = token;

   t2 = pHXL->heapGET();
   tail = t2;

   while (*p != EOS)
   {
      if (Lisalnum(*p) || *p==aS('_'))
         *t++ = *p++;

      else if(Lispunct(*p))
      {
         if (t > token)
         {
            *t = EOS;
            token_out(token, &head, &tail);
            t = token;
         }
         *t++ = *p++;
         if (*(p-1) == aS('.') &&
            (Lisspace(*p) || *p == EOS))
         {
            *t++ = aS(' ');
         }
         *t = EOS;
         token_out(token, &head, &tail);
         t = token;
      }

      else if(Lisspace(*p))
      {
         if (t > token)
         {
            *t = EOS;
            token_out(token, &head, &tail);
            t = token;
         }
         p++;
         while(Lisspace(*p))
            p++;
      }
      else                             // its some strange char, treat as alpha
         *t++ = *p++;
   }
   if (t > token)
   {
      *t = EOS;
      token_out(token, &head, &tail);
   }
   tail->setAtom(pATAB->nilA);
   return(pTSVC->Unify(t2, pHXL->XVar(1)));
}

/*   
   Like string_tokens, but punctuation chars is used to
   define what the punctuation marks are.

   instantiation error
      string a var

   type error
      string not a string
   */
TF PString::p_string_tokens3()
{                     // string_tokens3(+string, -tokenlist, +punctuationchars)
   TERM t1, t2, t3;
   TERM head, tail;
   aCHAR *p, *t, *punc;
   aCHAR token[256];

   t1 = (pHXL->XVar(0))->dref();
   t3 = (pHXL->XVar(2))->dref();

   if (t1->IsVar())
      pXCPT->Error(instanceE, aS("arg1 must be a string"));
   else if (! t1->IsStr())
      pXCPT->Error(typeE, aS("arg1 must be a string"));

   if (! t3->IsStr())
      pXCPT->Error(instanceE, aS("arg3 must be a string"));

   p = t1->getStr();
   punc = t3->getStr();
   t = token;

   t2 = pHXL->heapGET();
   tail = t2;

   while (*p != EOS)
   {
      if (Lstrchr(punc, *p))
      {
         if (t > token)
         {
            *t = EOS;
            token_out(token, &head, &tail);
            t = token;
         }
         *t++ = *p++;
         if (*(p-1) == aS('.') &&
            (Lisspace(*p) || *p == EOS))
         {
            *t++ = aS(' ');
         }
         *t = EOS;
         token_out(token, &head, &tail);
         t = token;
      }

      else if(Lisspace(*p))
      {
         if (t > token)
         {
            *t = EOS;
            token_out(token, &head, &tail);
            t = token;
         }
         p++;
         while(Lisspace(*p))
            p++;
      }
      else                            // its some strange char, treat as alpha
         *t++ = *p++;
   }
   if (t > token)
   {
      *t = EOS;
      token_out(token, &head, &tail);
   }
   tail->setAtom(pATAB->nilA);
   return(pTSVC->Unify(t2, pHXL->XVar(1)));
}

TF PString::p_str_list()
{                                      // string_list(String, List) predicate  
   TERM     s, t;
   Cell     l;
   int      err;
   //STRptr   psb;
   
   s = (pHXL->XVar(0))->dref();

   if (s->IsStr())
      return(pTSVC->Unify(pHXL->StrToCharList(s->getStr()), 
                          pHXL->XVar(1)));

   t = (pHXL->XVar(1))->dref();

   if (t->IsList())
   {
      if (TRUE != (err = pHXL->CharListToStr(t, strBuf()))) 
        return(err);
   }
   else if (t->IsAtom() && (t->getAtom() == pATAB->nilA))
      strBuf()[0] = EOS;
   else
      pXCPT->Error(sysargE);

   //if (TRUE != (err = strEnterString((STRptr) strBuf(), &psb)))
   //   return(err);
   l.setStr( pGCTH->make_string((STRptr) strBuf()));
   return(pTSVC->Unify(&l, pHXL->XVar(0)));
}

TF PString::p_string_trim()
{              // string_trim(S1, S2) - trims leading and trailing white space
   TERM t1, t2;
   //STRptr   psb;
   LString s;
   Cell c;

   t1 = (pHXL->XVar(0))->dref();
   if (! t1->IsStr()) 
     pXCPT->Error(sysargE);
   t2 = (pHXL->XVar(1))->dref();
   if (! t2->IsVar()) 
     pXCPT->Error(sysargE);

   if ( (size_t)strBufLen() <= Lstrlen(t1->getStr()) )
      pXCPT->Error(stringoverE);
   s = (STRptr)t1->getStr();
   s.TrimWhiteSpace();
   //strEnterString(s, &psb);
   c.setStr( pGCTH->make_string(s));

   return pTSVC->Unify(&c, pHXL->XVar(1));
}

TF PString::p_strcat()
{              // strcat(S1, S2, S3) - concatenate two strings to form a third 
   TERM     t1, t2;
   Cell     c3;
   //STRptr   psb;
   //int      err;

   aCHAR   *buf;

   buf = strBuf();

   t1 = (pHXL->XVar(0))->dref();
   if (! t1->IsStr()) 
     pXCPT->Error(typeE, aS("Arg 1 must be a string"));
   t2 = (pHXL->XVar(1))->dref();
   if (! t2->IsStr()) 
     pXCPT->Error(typeE, aS("Arg 2 must be a string"));

   if ( (size_t)strBufLen() <= 
        Lstrlen(t1->getStr()) + Lstrlen(t2->getStr()) )
      pXCPT->Error(stringoverE);
   Lstrcpy(buf, t1->getStr());
   Lstrcat(buf, t2->getStr());
   //if ( TRUE != (err = strEnterString((STRptr) buf, &psb)) )
   //   return(err);
   c3.setStr( pGCTH->make_string(buf));
   return(pTSVC->Unify(&c3, pHXL->XVar(2)));
}

/*   
   Splits a string into a list of substrings
   that were separated by the characters in delims.
   */
TF PString::p_string_split()
{                             //   string_split(+string, +delims, -stringlist)
   TERM t1, t2;
   STRptr delims, buffer;

   t1 = (pHXL->XVar(0))->dref();
   if (! t1->IsStr())
      pXCPT->Error(typeE, aS("arg1 must be a string"));
   t2 = (pHXL->XVar(1))->dref();
   if (! t2->IsStr())
      pXCPT->Error(typeE, aS("arg2 must be a string"));

   delims = t2->getStr();
   buffer = strBuf();
   Lstrcpy(buffer, t1->getStr());

   //STRptr psb;
   TERM list = pHXL->heapGET();
   TERM tail = list;
   TERM head;

   aCHAR* token = Lstrtok(buffer, delims);
   while(token != NULL)
   {
      head = pHXL->heapGETN(2);
      //(tail)->setList( head);
      tail->setList(head);
      tail = head + 1;
      //strEnterString((STRptr)token, &psb);
      head->setStr( pGCTH->make_string(token));
      token = Lstrtok(NULL, delims);
   }
   tail->setAtom(pATAB->nilA);

   return (pTSVC->Unify(list, pHXL->XVar(2)));
}


//   Case insensitive compare of two strings or atoms.
TF PString::p_string_icomp()
{                                          // string_icomp(+string1, +string2)
   TERM t1, t2;
   STRptr s1, s2;

   t1 = (pHXL->XVar(0))->dref();
   if (!t1->IsStr() && !t1->IsAtom())
      pXCPT->Error(typeE, aS("arg1 must be a string or atom"));
   t2 = (pHXL->XVar(1))->dref();
   if (!t2->IsStr() && !t2->IsAtom())
      pXCPT->Error(typeE, aS("arg2 must be a string or atom"));

   if (t1->IsStr())
      s1 = t1->getStr();
   else
      s1 = *(t1->getAtom());

   if (t2->IsStr())
      s2 = t2->getStr();
   else
      s2 = *(t2->getAtom());

   if (Lstrlen(s1) != Lstrlen(s2))
      return FALSE;

   while( *s1 != EOS )
   {
      if (Ltolower(*s1++) != (Ltolower(*s2++)))
         return FALSE;
   }

   return TRUE;
}

/* 
   instantiation error:
      stringlist a variable

   type error:
      string not a string or variable
      stringlist not a list of strings or atoms 
*/
TF PString::p_stringlist_concat()
{                                  // stringlist_concat(+stringlist, -string)
   TERM    tArg1, tArg2, t, x;
   STRptr  buffer;
   //STRptr  psb;
   Cell    cell;
   //int   err;

   tArg1 = (pHXL->XVar(0))->dref();
   if (tArg1->IsVar())
      pXCPT->Error(instanceE, aS("arg1 must be a list of strings/atoms"));
   if (!tArg1->IsList())
      if (!tArg1->IsAtom() ||
            (tArg1->IsAtom() && pATAB->nilA != tArg1->getAtom()) )
         pXCPT->Error(typeE, aS("arg1 must be a list of strings/atoms"));

   tArg2 = (pHXL->XVar(1))->dref();
   if (!(tArg2->IsVar() || tArg2->IsStr()))
      pXCPT->Error(typeE, aS("arg2 must be a string or variable"));

   buffer = strBuf();
   buffer[0] = EOS;
   t = tArg1;
   while(t->IsList())
   {
    t = t->getListHead();                       // head of list 
    x = (t)->dref();                            // term at head of list 
      if (x->IsStr())
      {
         if ( (size_t)strBufLen() <= Lstrlen(buffer) + Lstrlen(x->getStr()) )
            pXCPT->Error(stringoverE);
         Lstrcat(buffer, x->getStr());
      }
      else if (x->IsAtom())
      {
         if ( (size_t)strBufLen() <= Lstrlen(buffer) + Lstrlen(*(x->getAtom())) )
            pXCPT->Error(stringoverE);
         if (pSTATE->m_vba)
            Lstrcat(buffer, (x->getAtom())->get_display_name(m_peng));
         else
            Lstrcat(buffer, *(x->getAtom()));
      }
      else
         pXCPT->Error(typeE, aS("arg1 must be a list of strings/atoms"));
      t++;
      t = (t)->dref();
   }

   //if ( TRUE != (err = strEnterString((STRptr)buffer, &psb)) )
   //   return(err);
   //cell.setStr( psb);
   cell.setStr(pGCTH->make_string(buffer));
   return(pTSVC->Unify(&cell, tArg2));
}

/* implemented in alib.pro now
TF PString::p_string_integer()
{                                  // string_integer(String, Integer) predicate
   TERM       s, t;
   Cell       l;
   STRptr     ps;
   //int        err;
   intC       val;
   STRptr     psb;
   aCHAR     *string_buf, c;

   string_buf = strBuf();

   s = (pHXL->XVar(0))->dref();
   if (s->IsStr())
    {                                         // arg 1 instantiated to string
      psb = ps = s->getStr();
//      pTSVC->ATag(&l, consT | intS);
//      pTSVC->AiValue(&l, atoi(ps));
		if(*psb == '-')
	    psb++;
		while(c = *psb++)
	    if( c < 0x30 || c > 0x39)
		   return FALSE;
      val = Latol(ps);
      return(pTSVC->UnifyInt(val, pHXL->XVar(1)));
   }

   t = (pHXL->XVar(1))->dref();
   if (t->IsInt())       
    {                                         // arg 2 instantiated to int
	   val = (long)t->getInt();
	   Lsprintf(string_buf, m_buflen, aS("%ld"), val);
    }
   //else if (pTSVC->TisXint(i)) val = pTSVC->getXINT(i);
	else 
    pXCPT->Error(sysargE);

   // itoa(val, buf, 10); // replaced by above
   //if (TRUE != (err = strEnterString((STRptr) string_buf, &psb)))
   //   return(err);
   //l.setStr( psb);
   l.setStr(pGCTH->make_string(string_buf));
   return(pTSVC->Unify(&l, pHXL->XVar(0)));
   return FALSE;                                // a formality 
}

TF PString::p_string_float()
{                                  // string_integer(String, Integer) predicate
   TERM       s, t;
   Cell       l;
   STRptr     ps;
   //int        err,
   int        length;
   double     val;
   //STRptr     psb;
   aCHAR     *p, *string_buf;

   string_buf = strBuf();

   s = (pHXL->XVar(0))->dref();
   if (s->IsStr())
    {                                         // arg 1 instantiated to string
      ps = s->getStr();
      val = Latof(ps);
      t = pHXL->heapGET();
      t->setDouble(pGCTH->make_float(val));
      return(pTSVC->Unify(t, pHXL->XVar(1)));
   }

   t = (pHXL->XVar(1))->dref();
   if (t->IsScientific())       
    {                                         // arg 2 instantiated to float
	   if (t->IsSingle())
         val = (double)t->getSingle();
      else
         val = (double)t->getDouble();
	   length = Lsprintf(string_buf, m_buflen, aS("%lf"), val);
	   p = string_buf + length - 1;
	   while(*p == aS('0'))
			*p-- = EOS;                             // truncate trailing zeros
    }
	else 
    pXCPT->Error(sysargE);

//	err = strEnterString((STRptr) string_buf, &psb);
//   if (!err)
//      return(err);
   l.setStr(pGCTH->make_string(string_buf));
   return(pTSVC->Unify(&l, pHXL->XVar(0)));
}
*/

TF PString::p_string_atom()
{                                      // string_atom(String, Atom) predicate
   TERM     s, i;
   Cell     l;
   STRptr   ps;
   //int      err;
   PATOM    a;
   //STRptr   psb;

   s = X(0);
   if (s->IsStr())
   {
      ps = s->getStr();
      //pTSVC->ATag(&l, consT | atomS);
      a = pATAB->EnterAtom(ps);
      //pTSVC->AaValue(&l, a);
      l.setAtom(a);
      return(pTSVC->Unify(pHXL->XVar(1), &l));
   }

   i = (pHXL->XVar(1))->dref();
   if (i->IsAtom())
   {
      a = i->getAtom();
      //if (TRUE != (err = strEnterString((STRptr) *(a), &psb)))
      //   return(err);
      //l.setStr( psb);
      l.setStr(pGCTH->make_string((STRptr) *(a)));
      return(pTSVC->Unify(&l, pHXL->XVar(0)));
   }
   pXCPT->Error(sysargE);
   return FALSE;                                 // a formality 
}


TF PString::p_string()
{                                                // string() predicate
   return((pHXL->XVar(0))->dref()->IsStr());
}


TF PString::p_read_string()
{ // read_string_(Term, Handle)
   Cell s;
   aCHAR *p;

   int h = pIO->streamIndex(X(1));

   if (pIO->stream[h]->isEndOfStream())
        s.setAtom(pATAB->eofA);
   else
   {
      pIO->stream[h]->get_line(strBuffer, m_buflen);
      // replace newline with eos
      p = strBuffer;
      while (*p && *p != aS('\n'))
         p++;
      *p = EOS;

      s.setStr(pGCTH->make_string(strBuffer));
   }
   return(pTSVC->Unify(pHXL->XVar(0), &s));
}


TF PString::p_string_term()
{
   TERM   t0, t1;
   TERM   x = NULL;
   Cell   c;
   int    err;
   //STRptr sb;
   aCHAR *s;
   
   s = strBuf();

   t0 = X(0);
   if (t0->IsStr())
    {
	   err = strStringTerm(t0->getStr(), &x);
	   return err == TRUE ? 
			pTSVC->Unify(pHXL->XVar(1), x) :
			pREAD->READERROR((ErrNo)err) ;
    }
   else if (t0->IsRef())
    {
	   t1 = X(1);
	   pWRIT->termWriteString(t1, s, strBufLen(), FALSE);
	   c.setStr(pGCTH->make_string(s));
	   return( pTSVC->Unify(pHXL->XVar(0), &c) );
    }
   else pXCPT->Error(sysargE);
	
   return FALSE;
}

TF PString::p_is_string_term()
{
   TERM   t0, t1;
   TERM   x = NULL;
   Cell   c;
   //int    err;
   //STRptr sb;
   aCHAR *s;
   
   s = strBuf();

   t0 = X(0);
   if (t0->IsStr())
   {
	   try
      {
         strStringTerm(t0->getStr(), &x);
         return pTSVC->Unify(X(1), x);
      }
      //catch(LExcept &pE)
      catch(LExcept)
      {
         return FALSE;
      }
   }
   else if (t0->IsRef())
   {
	   t1 = X(1);
	   pWRIT->termWriteString(t1, s, strBufLen(), FALSE);
	   c.setStr(pGCTH->make_string(s));
	   return( pTSVC->Unify(X(0), &c) );
   }
   else pXCPT->Error(sysargE);
	
   return FALSE;
}

TF PString::p_string_termq()
{
   TERM   t0, t1;
   TERM   x = NULL;
   Cell   c;
   int    err;
   //STRptr sb;
   aCHAR *s;
   
   s = strBuf();

   t0 = X(0);
   if (t0->IsStr())
   {
     err = strStringTerm(t0->getStr(), &x);
     return err == TRUE ? 
	   pTSVC->Unify(pHXL->XVar(1), x) :
	   pREAD->READERROR((ErrNo)err) ;
   }
   else if (t0->IsRef())
   {
      t1 = X(1);
      pWRIT->termWriteString(t1, s, strBufLen(), TRUE);
      //strEnterString(s, &sb);
      //c.setStr(sb);
      c.setStr(pGCTH->make_string(s));
      return( pTSVC->Unify(pHXL->XVar(0), &c) );
   }
   else pXCPT->Error(sysargE);

   return FALSE;
}

TF PString::p_string_termq3()
{
   TERM   t0, t1;
   TERM   x = NULL;
   Cell   c;
   int    err;
   //STRptr sb;
   aCHAR *s;
   int len;
   
   s = strBuf();
   t0 = X(0);
   if (t0->IsStr())
   {
     err = strStringTerm(t0->getStr(), &x);
     return err == TRUE ? 
	   pTSVC->Unify(pHXL->XVar(1), x) :
	   pREAD->READERROR((ErrNo)err) ;
   }
   else if (t0->IsRef())
   {
      t1 = X(1);
      len = X(2)->getInt() + 1;  // plus one for EOS
      //std::cout << "stringtermq3 calling termWriteString\n";
      pWRIT->termWriteString(t1, s, len, TRUE);
      //strEnterString(s, &sb);
      //c.setStr(sb);
      c.setStr(pGCTH->make_string(s));
      return( pTSVC->Unify(pHXL->XVar(0), &c) );
   }
   else pXCPT->Error(sysargE);

   return FALSE;
}

TF PString::p_nonblank_string()
{
   TERM   t;
   STRptr s;

   t = X(0);
   if (!t->IsStr())
      pXCPT->Error(sysargE);

   s = t->getStr();
   while (*s)
      if (*s++ > 32) 
        return TRUE;

   return FALSE;
}

TF PString::p_tilt_slashes()
{
   LPathString p1;
   PATOM a1, a2;
   Cell c;

   TERM t1 = X(0);
   if (t1->IsAtom())
   {
      a1 = t1->getAtom();
      p1 = *a1;
      p1.TiltSlashes();
      a2 = pATAB->EnterAtom(p1);
      c.setAtom(a2);
   }
   else if (t1->IsStr())
   {
      p1 = *(t1->getLString());
      p1.TiltSlashes();
      c.setStr(pGCTH->make_string(p1));
   }
   else
   {
      pXCPT->Error(sysargE);
   }

   return pTSVC->UnifyConst(X(1), &c);
}


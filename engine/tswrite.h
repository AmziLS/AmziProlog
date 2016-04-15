/*****************************************************************\
*
* tswrite.h - term writer
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
\*****************************************************************/

#ifndef TSWRITE_H
#define TSWRITE_H

class PrologStream;

class TermWriter
{
private:
   LEngine *m_peng;
   PrologStream *write_stream;
   int output_len;
   bool quoting;
   int m_buflen;
   int m_maxstring;
   bool m_is_string;
   std::map<TERM,int> written;

   STRptr  writeBuffer;
   STRptr  writeqBuffer;
   STRptr  outputBuffer;
public:
   TermWriter(LEngine *peng);
   ~TermWriter();
   void Init(int buflen);
   void write_term(int h, TERM t, bool quote);
   int   termWriteString(TERM, STRptr, intC, bool);

public:
   // builtin predicates
   TF p_write(void);
#ifdef LANDFILL
   TF p_logZterm(void);
#endif

private:
   //void  setWriteStream(PrologStream *s)
   //{ write_stream = s; }
   TERM  sowrite(TERM, int);
   bool all_chars(TERM t);
   void  printList(TERM, bool);
   void  printReal(Real, TF);
   TF    quoteq(aCHAR *);
   void  PrAtom(aCHAR *, aCHAR *);
   void  PrAtom(PATOM, aCHAR *);
   void  w_f_putsq(aCHAR *, aCHAR);
   void  w_f_puts(aCHAR *);
   void  w_f_putc(aCHAR);
   bool  write_occurs_check(TERM);
   void  write_occurs_pop(TERM);
};

#endif // TSWRITE_H



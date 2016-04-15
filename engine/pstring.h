/**************************************************************************\
*
* pstring.h -- predicates that manipulate strings
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: pstring.h,v $
* Revision 1.3  2005/12/23 03:59:07  dennis
* string_termq3
*
* Revision 1.2  2004/02/16 22:38:59  dennis
* fixed leaky broken streams
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.9  2002/05/15 16:59:09  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.8  2002/03/21 01:25:09  dennis
* allow separator in file names to be either tilt on slash, converts
* to correct one for Unix or Windows
*
* Revision 1.7  2001/11/15 13:54:07  dennis
* Fixed logging and apitrace, made logfile into its own entity,
* removed from the Prolog stream IO.
*
* Revision 1.6  2001/10/02 16:05:21  dennis
* changed streams, cleaner interface to lex, reimplemented
* readers as buffer between the two
*
* Revision 1.5  2001/08/24 16:06:44  dennis
* fixed header file comments at end of files, removed ostream include
* which isn't apparently necessary
*
* Revision 1.4  2001/07/21 00:39:47  dennis
* added garbage collector for strings and things
*
* Revision 1.3  2001/07/10 16:51:32  dennis
* added more memory leak checking tools, all clean so far
*
* Revision 1.2  2001/06/27 15:15:10  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.5  2000/08/26 00:32:07  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.4  2000/08/14 02:05:38  ray
* Added Real class.
* No division yet.
* No error reporting yet.
*
* Revision 1.2  2000/03/28 01:05:17  dennis
* merged Ray's changes with bigdig.  bigdig is at point where
* new Cell class is used, but there are no modules in the system.
*
* Revision 1.1.2.1  2000/03/08 04:12:02  dennis
* builtin.cpp compiles
*
*
\***************************************************************************/

#ifndef PSTRING_H
#define PSTRING_H

class PString
{
private:
   LEngine   *m_peng;
   // For string manipulation functions
   intC       m_buflen;
   STRptr     strBuffer;
   int        get_buff_index;

   //intC       TSMax;        // length of output buffer for strPutS

public:
   PString(LEngine *peng) { m_peng = peng; strBuffer = NULL; }
   ~PString();
   //int      strEnterString(STRptr, STRhnd);
   //STRptr   strMemDup(STRptr);
   int      strStringTerm(STRptr s, TERMptr tP);
   //STRptr   initStringIO(STRptr s);
   //int      strGetC(STRptr);
   //int      strUngetC(int, STRptr);
   //int      strPutS(STRptr, STRptr);
   intC     strBufLen()
   {   return m_buflen; }
   STRptr   strBuf()
   {   return strBuffer; }
   //void     SetPutSMax(intC max)
   //{   TSMax = max; }

   void Init(intC);
   STRptr   _get_term_chars(TERM);

   // Predicates
   void token_out(aCHAR*, TERM*, TERM*);
   TF p_string_tokens(void);
   TF p_string_tokens3(void);
   TF p_str_list(void);
   TF p_string(void);
   TF p_string_length(void);
   //TF p_string_integer(void);
   //TF p_string_float(void);
   TF p_string_atom(void);
   TF p_substring(void);
   TF p_subZstring(void);
   TF p_strcat(void);
   TF p_stringlist_concat(void);
   TF p_string_split(void);
   TF p_string_icomp(void);
   TF p_read_string();
   TF p_string_term(void);
   TF p_is_string_term(void);
   TF p_string_termq(void);
   TF p_string_termq3(void);
   TF p_string_trim(void);
   TF p_nonblank_string(void);
   TF p_tilt_slashes(void);

private:
   int      string_compare(TERM, TERM);
};

#endif //PSTRING_H

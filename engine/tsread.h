//-*-C++-*-
/****************************************************************************
* 
* tsread.h -- Term reader
* 
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
* 
* 2000/01/01 Ray edited for new tsread
*
* $Log: tsread.h,v $
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.13  2002/05/15 16:59:09  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.12  2002/02/04 17:21:00  dennis
* New lreal wrapper class on reals, start of number options:
* decimals: real/float, floats: single/double.  Created lmath.h/cpp
* but nothing in them yet.  Work to be done on sorting out mixed mode
* arithmetic for different options.  Also - casts removed, as they
* were causing problems.
*
* Revision 1.11  2002/01/28 06:29:20  dennis
* changes for parsing numbers, handling different options
*
* Revision 1.10  2001/10/02 16:05:22  dennis
* changed streams, cleaner interface to lex, reimplemented
* readers as buffer between the two
*
* Revision 1.9  2001/09/11 04:34:56  dennis
* cleaned up some io stuff, got consult working, etc.
*
* Revision 1.8  2001/08/02 18:51:00  dennis
* merge of new streams complete
*
* Revision 1.7  2001/08/01 20:18:00  ray
* removed tswrite.cpp & sio.cpp, added streams.cpp sowrite.cpp
*
* Revision 1.6  2001/07/21 00:39:47  dennis
* added garbage collector for strings and things
*
* Revision 1.5  2001/07/10 16:51:32  dennis
* added more memory leak checking tools, all clean so far
*
* Revision 1.4  2001/04/02 21:50:13  dennis
* got debugger working again
*
* Revision 1.3  2001/03/28 15:07:21  ray
* char_code/2, number_chars/2 added
* 1 char macros proscribed
*
* Revision 1.2  2001/03/25 15:29:51  ray
* Implemented fixed
* repaired #include
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.10  2000/08/26 00:32:08  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.9  2000/08/14 02:05:38  ray
* Added Real class.
* No division yet.
* No error reporting yet.
*
* Revision 1.8  2000/05/14 03:52:35  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
* Revision 1.7  2000/04/02 23:54:45  ray
*
* Fixed + and - bug
*
* Revision 1.6  2000/03/30 05:25:52  ray
*
* #INCLUDE and other controls
*
* Revision 1.5  2000/03/28 23:47:52  dennis
* Changed all tabs to three spaces, and also changed Logic Server
* to use void* for TERM externally and cast to Cell* in LEngine
* implementation.
*
* Revision 1.4  2000/03/06 05:34:30  ray
* *** empty log message ***
*
* Revision 4.1  1998/01/13 07:38:16  pete
* Initial changes for solaris
*
* 
****************************************************************************/

#ifndef TSREAD_H
#define TSREAD_H

/*  Reader Parameters  */
/*  ~~~~~~~~~~~~~~~~~  */

#define   PARSE_DEPTH       256             // max depth of temp parse stack 
#define   MAXPREC          1200             // max operator precedence 
#define   VNAME_LENGTH       64             // max length of a variable name 

enum
{
  inlistC = 1,                              // just entered [ 
  instrucC,                                 // just entered f( 
  inparsC,                                  // just entered ( 
  incdrC,    
  inbraceC,                                 // just entered { 
  bottomC,                                  // marker for exit 
  inargC                                    // started arg in functor or list 
};

/* operator precedence and type extractors */

enum EXPECT {Erand, Eop};                   // type of term next expected 

struct pCONTEXT
{
  int       r_context;                      // context of parse in progess 
  int       r_prec;                         // precedence of parse in progress 
  int       r_p_stack;                      // p_stack index for this context 
};
typedef pCONTEXT * pCONTEXTptr;

enum TOKEN_ {T_TERM = 1, T_VAR, T_INT, T_FLOAT, T_DOUBLE, T_ATOM, T_PREOP, T_POSTOP, 
             T_INFOP, T_MPREOP, T_MPOSTOP, T_FUNCTOR, T_STR, T_LONG, T_FIXED, 
				 T_REAL, T_RATOR, T_NEST, T_CHAR, T_EOF, T_DEFINED, T_UNDEFINED,
             T_CUT    = '!',                // the rest are their own tokens
             T_COMMA  = ',',
             T_DOT    = '.',
             T_LPAR   = '(',
             T_RPAR   = ')',
             T_LBRACK = '[',
             T_RBRACK = ']',
             T_LBRACE = '{',
             T_RBRACE = '}',
             T_VBAR   = '|'};

struct PARSE_STACK 
{                                            // the parse stack 
  TOKEN_    t_type;
  union
  {
    PATOM     t_avalue;
    int       t_vvalue;
    TERM      t_tvalue;
    int       t_ivalue;
    float     t_flvalue;
    long      t_lvalue;
    fixedC    t_fvalue;
    GCString *t_svalue;
    GCDouble *t_dvalue;
    GCReal   *t_rvalue;
  }  tv;
};
typedef PARSE_STACK * PARSE_STACKptr;

struct VARNAME
{
   aBYTE      v_length;
   aBYTE      v_flag;                  // set to 1 if more than one ref else 0 
   aCHAR      v_name[VNAME_LENGTH+1];
   TERM       v_skel;                       // cell containing first occurence 
};
typedef VARNAME * VARNAMEptr;

//const aCHAR SpecialChars[] = aS("#$%&*-+/:;<=>?@\\~^");

class TermReader
{
  friend class LEX;

protected:
  LFLAG  bStringEsc;

private:
   LEngine *m_peng;
   intC  m_buflen;

   int   read_err_index;
   int   prec_commaA;                        // comma's precedence for I/O  
   intC  ReadDepth;                          // parse and read depth for reader

   //STRptr readBuffer;
   STRptr readTokenBuf;

   pCONTEXT    *con_stack;
   PARSE_STACK *p_stack;

   PARSE_STACK  stash_p;
   TF          stashed_token;

   bool b_reading;

   VARNAME  *vars;

   int    top_con_stack;                         // current context stack ptr 
   int    top_p_stack;                           // next parse stack ptr 
   int    next_var;                              // next free var in var table 

   int      cntxt;
   EXPECT   expect;
   int      maxprec;

   LEX *lex;

   // the string esc function - the mode commands should change this
   // through a member function for the reader object, when implemented

public:
   TermReader(LEngine *peng);
   ~TermReader();
   void Init(uintC pread_depth, LFLAG stresc, uintC maxvars, intC buflen);
   bool Read(int h, TERMptr);

   void SetStrEsc(LFLAG stresc)
   {   bStringEsc = stresc; }
   LFLAG GetStrEsc()
   {   return bStringEsc; }
   int Get_read_err_index()
   {   return read_err_index; }
   //STRptr GetReadBuffer()
   //{   return readBuffer; }
   int Get_next_var()
   {   return next_var; }
   // for capturing read errors
   int READERROR(ErrNo x);
   //{   return (int)((m_readerrors) ? errExec(x) : FALSE); }
   aCHAR* Get_var_v_name(int i)
   {   return vars[i].v_name; }
   int Get_prec_commaA()
   {   return prec_commaA; }
   bool IsReading()
   { return b_reading; }
   void setReading()                     // called from read_string in pstring
   { b_reading = true; }
   void unsetReading()
   { b_reading = false; }

   //int   read__();                       // the work of read (TF is encryption 

   int   p_read_term(void);
   //TF p_read(void);
   TF p_varlist(void);

private:
   void CHECKDEPTH(int x)
   {   if (x >= ReadDepth) pXCPT->Error(pstackE); }
   #define    cur_p_stack     p_stack[top_p_stack]
   //pCONTEXT cur_con_stack()
   //{   return con_stack[top_con_stack]; }
   PATOM cur_a_value()
   {   return cur_p_stack.tv.t_avalue; }
   void set_cur_a_value(PATOM a)
   {   cur_p_stack.tv.t_avalue = a; }
   //int current_context()
   //{   return con_stack[top_con_stack].r_context; }
   //int current_prec()
   //{   return con_stack[top_con_stack].r_prec; }
   void SetCurrentConStack(int context, int prec, int pstack)
   {
      con_stack[top_con_stack].r_context = context;
      con_stack[top_con_stack].r_prec = prec;
      con_stack[top_con_stack].r_p_stack = pstack;
   }
   int GetCurrentContext()
   {   return con_stack[top_con_stack].r_context; }
   int GetCurrentPrec()
   {   return con_stack[top_con_stack].r_prec; }
   int GetCurrentPStack()
   {   return con_stack[top_con_stack].r_p_stack; }
   void SetCurrentPStack(int pstack)
   {   con_stack[top_con_stack].r_p_stack = pstack; }

   int minmum(int x, int y)
   {   return ( (x <= y) ? x : y); }

   TF    action();
   TF    collapse(void);
   void  collapse_con(void);
   void  EnterContext(int, int);
   //TOKEN_ tmGetToken(void);
   void tmGetToken();
   //TOKEN_ tmGetToken_(void);
   TOKEN_ OpClass(PATOM);
   CharInt   nextchar(void);
   TERM  ParseToHeap(int);
   void  pop_token(void);
   int   PushStack(TOKEN_, void *);
   void  ReadError(int);
   int   RightPrec(TOKEN_, PATOM);
   void  start_arg(void);
   int   varid(aCHAR *);
   TF    is_skip(aCHAR);
#ifdef LANDFILL
   void  debug_parse(void);
#endif
};

#endif // TSREAD_H








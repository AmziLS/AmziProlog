//-*-C++-*-
/**************************************************************************\
*
* termsvc.h -- Term services
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
*
\***************************************************************************/
#include <math.h>

#ifndef LTERM_H
#define LTERM_H

// Macros for some of the more common services

#define tXVAR(n)     (pHXL->XVar(n))->dref()
#define tISVAR(t)    pTSVC->IsVar(t)
#define tISSTRING(t) pTSVC->IsStr(t)
#define tISATOM(t)   pTSVC->IsAtom(t)
#define tISINTS(t)   pTSVC->IsIntS(t)

enum ORDER_ {OREFS, ONUMBERS, OATOMS, OSTRINGS, OSTRUCTS, OLISTS};

// used in copyterm, to indicate action to be taken when
// copying cells containing gc objects.  the counter in the
// object reflects the number of copies in non-heap space.
enum MARKGC {GC_NO, GC_INC, GC_DEC};

//#define WRITE_STREAM pIO->stream[write_stream]


//#ifndef REAL_H
//enum errors_ {numberCast, numberDomain};    // aberration of ray
//#endif


#define MAX_COUNTERS 100

// Used by term write functions
#define PARENS     1
#define NO_PARENS  0

//struct VLIST
//{          // VLIST is a linked list structure of old/new variable pairs.
           // used by routines which copy terms to cells not on either stack
   //VLIST *next_vlist;
//   TERM   src_var;
//   TERM   dest_var;
//};

//typedef VLIST * VLISTptr;

class VarPair
{
public:
   TERM   src_var;
   TERM   dest_var;

   VarPair(TERM s, TERM t)
   { src_var = s; dest_var = t; }
};

class PString;


// TermServices provides services for terms that
// exist anywhere in the system.
class TermServices
{
private:
   LEngine  *m_peng;
   bool b_vars_first;
   std::map<PATOM, int> gensyms;
   std::vector<TERM> occurrences;
public:
   TermServices(LEngine *peng);
   ~TermServices();
   void Init(intC);

public:
  // Term unification functions.
   int Unify(TERM, TERM);
   int UnifyInt(intC, TERM);
   int UnifyAtom(PATOM, TERM);
   // Unify term representing constant (2nd arg) with term
   int UnifyConst(TERM term, TERM constant);
   int StrongUnify(TERM, TERM);
   bool IsCyclic(TERM);

   TF UnifyCopy(TERM query, TERM ddb, DynamicClause*);
   //TF unify_copy2(TERM query, TERM heap, TERM space, TERMptr pnewspace, TERM ddb, MARKGC);
   TF unify_copy2(TERM query, TERM ddb);

  // Arithmetic functions
private:
   intC    counters[MAX_COUNTERS];
   int unify_loop(TERM, TERM);
   int occurs_check(TERM, TERM);
   void occurs_pop();
   bool is_cyclic(TERM);
   bool cyclic_check(TERM);
   void cyclic_pop();
   int strong_unify(TERM t1, TERM t2);

public:                         
   aCHAR *TermVal(TERM x, aCHAR* termS, int tslen);

   void SetCounter(int i, intC val)
   {   counters[i] = val; }
   intC GetCounter(int i)
   {   return counters[i]; }

   void setVarsFirst(bool tf)
   { b_vars_first = tf; }

  TF p_fixed_list(void);
  TF p_cntr(void);
  TF p_for(void);
  
  TF p_number_codes(void);
  TF p_term_codes(void);


  // Term Copying Functions
private:
   //VLISTptr vl;
   //VLISTptr pvl;
   //VLISTptr ptopvl;
   int varcount;
   std::vector<VarPair> var_pairs;

public:
   int Copy(TERM, TERM, TERMptr, TERM, MARKGC);
   void UpdateGCThings(TERM src, MARKGC markuse);
   // next two functions used by SortedDynamicPredicate in
   // creating iterators.  they produce query patterns that
   // will work with STL map to bracket keys.
   TERM query_copy(TERM);
   bool hasVar(TERM);
   bool is_rule(TERM tt);

private:
   void  copyterm2_(TERM, TERM, TERMptr, TERM, MARKGC);
   TERM  copy_to_heap(TERM);

  // Term Classification Functions
public:
   TERM MakeAbstractList(void);
   TERM MakeStruc(PATOM, ARITY);
   ORDER_  ordtype_(TERM);
   int  varsof_(TERM, TERM);
   //bool Ground(TERM);

   //int lexord(TERM, TERM);
   //bool less(TERM, TERM, bool);
   int compare(TERM, TERM, bool);

   TF p_copy_term(void);
   TF p_getZmodix(void);
   TF p_arg(void);
   TF p_atom(void);
   //TF p_dbref(void);
   TF p_functor(void);
   TF p_char(void);
   TF p_memberv(void);
   TF p_not_strong_unify(void);
   TF p_number(void);
   TF p_strong_unify(void);
   TF p_univ(void);
   TF p_varsof(void);
   TF p_gensym(void);
   TF p_is_cyclic(void);
   TF p_ground(void);

  // Stashing Functions
   int   stash_get_term(TF);
   void  stash_kill_term(TERM);

   TF p_get_term(void);
   TF p_peek_term(void);
   TF p_reserve_heap(void);
   TF p_stash_term(void);
   TF p_stash_free(void);

  // Writing Terms
private:
   //intC    m_buflen;
   //STRptr  writeqBuffer;
   //int     write_stream;

public:
  
  //bool    quoting;
  //int   termWriteLog(TERM);
  //int   writet(TERM, TERM, TERM);              // new style
  //  int   writet(TERM, TERM, TF);                // old style
  //TERM  writet_(TERM, int);                    // made public for histo - ray
  
  //TF p_write(void);
#ifdef LANDFILL
  TF p_landZfill(void);   // writes a term to the landfill
  void DumpWrite(TERM t);
  void DumpCell(TERM t);
  void Dump(TERM t);
  void Dump(TERM t, int indent);

#endif

private:
  //int   wn_writet(WINDOWPTR, TERM, TF);         // not used??
};

#endif  // LTERM_H



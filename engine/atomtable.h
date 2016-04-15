//-*-C++-*-
/***************************************************************************\
*
* atomtable.h -- Atom Table Definitions
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: atomtable.h,v $
* Revision 1.11  2007/05/16 02:15:38  dennis
* a7-6-8
*
* Revision 1.10  2006/12/19 04:58:46  dennis
* module names are just atoms
*
* Revision 1.9  2006/12/15 04:05:55  dennis
* undecapitalization of module names
*
* Revision 1.8  2006/12/02 20:38:33  mary
* Add curlysA {} atom. Changed writes to sync file to be under BUG_SYNC ifdef.
*
* Revision 1.7  2006/08/10 13:31:49  dennis
* fix to async
*
* Revision 1.6  2005/12/06 23:24:47  dennis
* writeing of '_' fixed
*
* Revision 1.5  2005/02/21 17:40:07  dennis
* vba addition
*
* Revision 1.4  2005/02/09 17:35:39  dennis
* ?var added
*
* Revision 1.3  2004/03/26 21:37:10  dennis
* implemented occurs check
*
* Revision 1.2  2004/02/16 22:38:58  dennis
* fixed leaky broken streams
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.37  2003/08/27 03:04:43  dennis
* added initialization support for remote debugging LSX
*
* Revision 1.36  2003/06/13 18:37:01  dennis
* date_time and other fixes
*
* Revision 1.35  2003/03/24 15:49:01  dennis
* getting read for 6.4.0 alpha
*
* Revision 1.34  2003/03/15 14:34:04  dennis
* Many changes in debug64 and proto_debug, source code
* debugging is closer to a reality
*
* Revision 1.33  2003/02/13 23:44:36  dennis
* latest debug64 stuff
*
* Revision 1.32  2002/11/26 16:50:40  dennis
* thanksgiving updates
*
* Revision 1.31  2002/11/20 14:54:44  dennis
* added support for user defined streams, fixed java jni bug with
* extended predicates
*
* Revision 1.30  2002/09/14 20:47:54  dennis
* two new .cfg parameters, maxmemory for unix only, outputdepth to
* catch cyclic terms
*
* Revision 1.29  2002/05/15 16:59:07  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.28  2002/05/02 17:39:30  dennis
* various minor bug fixes, added locale as a settable prolog flag
*
* Revision 1.27  2002/04/25 03:42:22  dennis
* more documentation, logicbase.htm, and some fiddling with sources
*
* Revision 1.26  2002/04/19 19:41:43  dennis
* fixed retract bug with sorted/indexed clauses, implemented abolish for
* those types as well
*
* Revision 1.25  2002/03/25 22:24:06  dennis
* Added read/write_binary predicates to replace fwrite, also repositioning
* for binary files. Note that even though the standard says there's a read
* and write position, they are one and the same.
*
* Revision 1.24  2002/02/19 04:11:39  dennis
* changed reals to use pass by reference, eliminating almost all needs
* for new and delete, seems to have eliminated most all leaks due to math,
* put in memcpy, memmove etc. for copying gigit arrays around.
*
* Revision 1.23  2002/02/13 03:19:59  dennis
* reals changed to have a LReal class, moved to file of same name,
* math functions moved out of termsvc and into lmath.cpp/h, eval rewritten
* to reflect various options for numbers, lexcept modified so anyone, even
* non-engine objects, can throw LExcept objects.
*
* Revision 1.22  2002/02/04 17:20:58  dennis
* New lreal wrapper class on reals, start of number options:
* decimals: real/float, floats: single/double.  Created lmath.h/cpp
* but nothing in them yet.  Work to be done on sorting out mixed mode
* arithmetic for different options.  Also - casts removed, as they
* were causing problems.
*
* Revision 1.21  2002/01/28 06:29:18  dennis
* changes for parsing numbers, handling different options
*
* Revision 1.20  2002/01/06 20:31:27  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.19  2001/12/03 20:20:52  dennis
* fixed copyterm to allow infinite variables, not limited to maxvars.
*
* Revision 1.18  2001/11/21 00:31:50  dennis
* removed mode stuff, filled out prolog flags
*
* Revision 1.17  2001/11/15 13:54:07  dennis
* Fixed logging and apitrace, made logfile into its own entity,
* removed from the Prolog stream IO.
*
* Revision 1.16  2001/10/05 20:44:11  dennis
* fixed p_prolog_flag so gcc would be happier
*
* Revision 1.15  2001/10/05 19:15:17  dennis
* string streams, function streams working, reals set up to not require
* m_peng, just passed in parameters as necessary
*
* Revision 1.14  2001/10/02 16:05:20  dennis
* changed streams, cleaner interface to lex, reimplemented
* readers as buffer between the two
*
* Revision 1.13  2001/08/29 19:34:33  ray
* Corrections to 'seen' in alib etc.
*
* Revision 1.12  2001/08/16 17:07:24  ray
* merged everything, repaired errors, added BINSTREAM
*
* Revision 1.11  2001/08/01 20:17:59  ray
* removed tswrite.cpp & sio.cpp, added streams.cpp sowrite.cpp
*
* Revision 1.10  2001/06/27 15:15:09  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.9  2001/03/28 15:07:21  ray
* char_code/2, number_chars/2 added
* 1 char macros proscribed
*
* Revision 1.8  2001/03/13 20:05:17  dennis
* added meta$call to try and isolate meta calls.
*
* Revision 1.7  2001/03/09 20:13:59  dennis
* consolidating for KW delivery, fixed jni memory leak
*
* Revision 1.6  2001/02/21 04:46:42  dennis
* debugger working, updated documentation for 6.1
*
* Revision 1.5  2001/02/05 20:17:09  dennis
* fixed some bugs
*
* Revision 1.4  2001/01/30 16:47:28  dennis
* Made, after many trials, alib into amzi_system module.
*
* Revision 1.3  2001/01/11 01:49:23  dennis
* Implemented rest of import/export and metapredicates, working
* for test cases.
*
* Revision 1.2  2001/01/05 06:05:54  dennis
* fixed minor discrepancies
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.9  2000/10/24 14:00:44  ray
* Added atom_chars.
* Performed cosmetic surgery on termsvc.
*
* Revision 1.8  2000/10/12 19:05:14  ray
* Corrected mult for real.
* Added clock for integer calculation of time in milliseconds.
* Added run time printout to alis
*
* Revision 1.7  2000/10/03 01:37:17  dennis
* Got it running under Linux, had to ifdef out reals for now
*
* Revision 1.6  2000/10/01 16:20:03  dennis
* cleaned up modules, ddb, got defined, abolish and friends working
*
* Revision 1.5  2000/09/27 01:42:03  dennis
* implemented listing, needed current_predicate, predicate_property,
* and current_module
*
* Revision 1.4  2000/09/20 17:09:12  ray
* added casting operators to cell and rand to clean up code.
* added prolog flag 'modulo' and adapted arithmetic to use it.
* added fraction, realSpan and nth.
* added fast ** for reals.
*
* Revision 1.3  2000/09/15 21:42:24  dennis
* 12->13
*
* Revision 1.2  2000/08/26 00:32:04  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.1  2000/05/14 03:52:32  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
*
\***************************************************************************/
#ifndef ATOMTABLE_H
#define ATOMTABLE_H

// An atom is a string.  A separate object in case
// we want to add any special properties.  
// Atoms are stored in an STL map.
class LAtom : public LString
{
public:
   LString display_name;

public:
   LAtom(STRptr s) : LString(s) { display_name = LString(); }
   LAtom(LString s) : LString(s) { display_name = LString(); }
   LAtom(LEngine* m_peng, STRptr s);
   LAtom(LEngine* m_peng, LString s);

   LString get_display_name(LEngine* m_peng);
   void set_display_name(LString s) { display_name = s; }

   //LAtom(STRptr s) : LString(s) { display_name = LString(s); toLower(); }
   //LAtom(LString s) : LString(s) { display_name = LString(s); toLower(); }
/*
public:
   LAtom(STRptr s) : LString(s) { ; }
   LAtom(LString s) : LString(s) { ; }
*/
// just an experiment - trying to stop decapitalization of module names...
//  operator aCHAR*() const { return (aCHAR*)display_name; }

  int operator>( const LAtom &ls )
  {   return Lstrcmp(m_rep->m_chars, ls.m_rep->m_chars) > 0 ? 1 : 0; }

  int operator<( const LAtom &ls )
  {   return Lstrcmp(m_rep->m_chars, ls.m_rep->m_chars) < 0 ? 1 : 0; }
};

Lostream & operator<<( Lostream &, PATOM );

// A predicate has a functor and arity and is used as an index
// into various maps that use predicates, such as operators and
// the dynamic databases in modules.
class LPredicate
{
public:
   PATOM   functor;
   ARITY   arity;

   LPredicate()
      { functor = NULL; arity = 0; }
   LPredicate(PATOM a, ARITY ar)
      { functor = a; arity = ar; }
   LPredicate(const LPredicate &p)
      { functor = p.functor; arity = p.arity; }
   ~LPredicate()
      {};

   //friend Lostream & operator<<( Lostream &, const LPredicate & );
   friend Lostream & operator<<( Lostream &, const LPredicate & );

   LPredicate &operator=(const LPredicate &p)
      { functor = p.functor; arity = p.arity; return *this; }

   bool IsNull()
   { return (functor == NULL); }
};

bool operator<(LPredicate a, LPredicate b);   // define < for keys of STL maps

bool operator==(LPredicate a, LPredicate b);

class LOperator
{
public:
   int     prec;                              // from 0-1200
   PATOM   assoc;                             // one of the xfx family of atoms

   LOperator()
   { prec = 0; assoc = NULL; }
   LOperator(int pr, PATOM as)
   { prec = pr; assoc = as; }
   LOperator(const LOperator &o)
   { prec = o.prec; assoc = o.assoc; }
   ~LOperator()
   {};

   LOperator &operator=(const LOperator &o)
   { prec = o.prec; assoc = o.assoc;
     return *this; }
   bool operator==(const LOperator &o)
   { return (prec == o.prec && assoc == o.assoc); }

   //friend Lostream & operator<<( Lostream &, const LOperator & );
   friend Lostream & operator<<( Lostream &, const LOperator & );
};

// The atom table map contains the string key, and the 
// PATOM (pointer to string) value.  
// Thus, PATOM is unique and can be used for comparison.
typedef std::map<LString, LAtom*> AtomMap;
typedef AtomMap::iterator AtomIter;
typedef std::pair<LString, LAtom*> AtomPair;

// Operators are stored in a map with the predicate as a key.
typedef std::map<LPredicate, LOperator> OpMap;
typedef OpMap::iterator OpIter;
typedef std::pair<LPredicate, LOperator> OpPair;

class AtomTable
{
   friend class LOperator;        // so LOperator can see the xfxAs and stuff
   int atom_count;

public:
   //-----------------------
   // Predefined Atoms
   //

   // the arithmetic operator atoms
   // unary operators
   PATOM  plusA;
   PATOM  minusA;                              // can be both unary and binary
   PATOM  negA;
   PATOM  notA;
   PATOM  integerA;
   PATOM  floatA;
   PATOM  realA;
   PATOM  lnA;
   PATOM  logA;
   PATOM  log10A;                              // unary structures 
   PATOM  expA;
   PATOM  sinA;
   PATOM  cosA;
   PATOM  tanA;
   PATOM  asinA;
   PATOM  acosA;
   PATOM  atanA;
   PATOM  sqrtA;
   PATOM  absA; 
   PATOM  signA;
   PATOM  truncA;
   PATOM  roundA;
   PATOM  froundA;
   PATOM  ftruncA;
   PATOM  intpartA;
   PATOM  fracpartA; 
   PATOM  floorA;
   PATOM  ceilA;
   PATOM  eA;
   PATOM  piA;
   PATOM  degtoradA;
   PATOM  radtodegA;
   PATOM  cpuclockA;
   PATOM  cputimeA;
   PATOM  randomA; 
   PATOM  infA;
   PATOM  callA;
   PATOM  oddA;
   PATOM  nilA;
   PATOM  curlysA;
   // binary operators
   // these can take both floats and ints as args
   PATOM  timesA;
   PATOM  divA;
   PATOM  powerA;
   PATOM  maxA;
   PATOM  minA; 
   // these just take ints 
   PATOM  modA;
   PATOM  modsA;
   PATOM  moduA;
   PATOM  divdivA;
   PATOM  divsA;
   PATOM  divuA; 
   PATOM  bitandA;
   PATOM  bitorA;
   PATOM  xorA;
   PATOM  lshiftA;
   PATOM  rshiftA;
   PATOM  remA; 

   // atoms that are flag values (see BuiltIns::p_prologZflag)

   PATOM  heapA;
   PATOM  localA;
   PATOM  controlA;
   PATOM  trailA;
   PATOM  heapbumperA;
   PATOM  readbufferA;
   PATOM  readdepthA;
   PATOM  maxclausesA;
   PATOM  outputdepthA;
#ifdef UNIX
   PATOM  maxmemoryA;
#endif
   PATOM  logfileA;
   PATOM  apitraceA;
   PATOM  string_escA;
   PATOM  lsxloadA;
   PATOM  macroheapszA;
   PATOM  gcthingfreqA;
   PATOM  maxfilesA;
   PATOM  localeA;

   PATOM  occurs_checkA;
   PATOM  double_quote_stringsA;
   PATOM  macrotraceA;
   PATOM  preprocessorA;
   PATOM  vars_sort_equalA;
   PATOM  debug64_cutA;
   PATOM  traceA;
   PATOM  upper_case_atomsA;
   PATOM  vbaA;
   PATOM  utf8ioA;
   PATOM  debug_portA;
   PATOM  debug_hostA;

   PATOM  decimal_placesA;
   PATOM  epsilonA;
   PATOM  deltaA;
   PATOM  moduloA;
      
   PATOM  boundedA;
   PATOM  char_conversionA;
   PATOM  debugA;
   PATOM  exceptionZdebugZstackA;
   PATOM  integer_rounding_functionA;
   PATOM  max_arityA;
   PATOM  max_integerA;
   PATOM  min_integerA;
   PATOM  prolog_copyrightA;
   PATOM  prolog_dateA;
   PATOM  prolog_nameA;
   PATOM  prolog_versionA;
   PATOM  amziA;
   PATOM  undefined_predicateA;
   PATOM  unicodeA;
   PATOM  amzi_listenerA;
   PATOM  amzi_compilerA;
   PATOM  amzi_debuggerA;

   PATOM  onA;
   PATOM  offA;
   PATOM  unknown_flagA;
   PATOM  toward_zeroA;
   PATOM  not_applicableA;

   PATOM  decimalsA;
   PATOM  floatsA;
   PATOM  singleA;
   PATOM  doubleA;

   // used as returns for predicate_property/2
   PATOM  staticA;
   PATOM  dynamicA;
   PATOM  built_inA;
   PATOM  extendedA;
   PATOM  publicA;
   PATOM  privateA;
   PATOM  multifileA;
   PATOM  exportedA;
   PATOM  metapredicateA;
   PATOM  imported_fromA;
   PATOM  defined_inA;

   // error types for err$exec/3
   PATOM  typeEA;
   PATOM  instanceEA;
   PATOM  execEA;
   PATOM  syntaxEA;

   // types used by IO::read/write_binary
   PATOM  charA;
   PATOM  wide_charA;
   PATOM  short_integerA;
   PATOM  single_floatA;
   PATOM  double_floatA;

  // other common atoms
   PATOM  userA;
   PATOM  stdinA;
   PATOM  stdoutA;
   PATOM  stderrA;
   PATOM  functionA;
   PATOM  stringA;
   PATOM  stdlogA;

   PATOM  file_nameA;
   PATOM  modeA;
	PATOM  aliasA;
	PATOM  inputA;
	PATOM  outputA;
	PATOM  typeA;
	PATOM  end_of_streamA;
	PATOM  eofActionA;
	PATOM  positionA;
	PATOM  repositionA;
	PATOM  error_A;
	PATOM  eof_code_A;
	PATOM  reset_A;
	PATOM  at_A;
	PATOM  past_A;
	PATOM  no_A;
	PATOM  readA;
	PATOM  writeA;
	PATOM  readwriteA;
	PATOM  appendA;
	PATOM  readappendA;
	PATOM  macroA;
	PATOM  defaultA;
   PATOM  nlA;
   PATOM  beginningA;
   PATOM  currentA;
   PATOM  endA;
   PATOM  line_numberA;
	
   PATOM  ifA;   
   PATOM  commaA;       
   PATOM  vbarA;
   PATOM  queryA;       
   PATOM  isA;   
   PATOM  semiA;   
   PATOM  colonA;
   PATOM  cutA;
   PATOM  textA;
   PATOM  binaryA;
   PATOM  wide_textA;
   PATOM  userInA;
   PATOM  userOutA;
   PATOM  userErrA;
   PATOM  userFuncA;
   PATOM  userLogA;
   PATOM  emptyA;

   PATOM  yfyA;
   PATOM  xfxA;
   PATOM  xfyA;
   PATOM  yfxA;
   PATOM  xfA;
   PATOM  yfA;
   PATOM  fxA;
   PATOM  fyA;

   PATOM  eofA;
   PATOM  trueA;
   PATOM  falseA;
   PATOM  failA;
   PATOM  curlyA;
   PATOM  periodA;
   PATOM  errorA;
   PATOM  errorflagA;
   PATOM  topA;           // top predicate in lib.pro 
   //PATOM  arithEA;        // special atom returned by 'is' for bad arithmetic
   PATOM  latentA;
   PATOM  latentodA;
   PATOM  call_dollarA;
   PATOM  call_d_dollarA;
   PATOM  metaZconvertA;
   PATOM  rbZA;
   PATOM  runZcodeA;
   PATOM  call_nometaA;
   PATOM  metaZcallA;
   PATOM  nthA;
   PATOM  zvarA;
   PATOM  debugZcallA;
   PATOM  debugZinfoA;
   PATOM  goalA;
   PATOM  clauseA;
   PATOM  user_breakA;
   PATOM  user_pauseA;
   //modules
   PATOM amzi_systemA;

   //-------------

   // global nil cell (empty list []) used as
   // a convenience by many functions
   Cell   NilCell;
   TERM   NilTerm;
   Cell   TrueCell;
   TERM   TrueTerm;
   LOperator  NilOp;

private:
   LEngine   *m_peng;                     // the owning engine
   AtomMap   *m_atomtable;                // the global atom table
   OpMap     *m_operators;                // the global operator definitions

public:
   AtomTable(LEngine *peng);
   ~AtomTable();
   void   Init();
   PATOM  EnterAtom(STRptr s);            // finds or creates atom for a string
   PATOM  EnterCIAtom(STRptr s);            // finds or creates case insensitive atom for a string
#ifdef _UNICODE
   PATOM  EnterAtom(char* s);
   PATOM  EnterCIAtom(char* s);
#endif
   PATOM  FindAtom(STRptr);               // returns an existing atom, or null
   PATOM modeName(int);
   PATOM typeName(int);
	PATOM eoStreamName(int);
	PATOM eofActionName(int);

   void ValidGoal(PATOM a)
   { // used by assert and similar functions to ensure user isn't being dumb.
      if ( a == failA || a == commaA || a == vbarA || a == periodA ||
           a == cutA || a == semiA)
         pXCPT->Error(sysassertE, (STRptr)(*a));
      return;
   }

   //-----------------------------------
   // operator functions
   //

   bool IsOperator(PATOM a, ARITY ar);
   LOperator &FindOperator(PATOM a, ARITY ar);

   int getPrecedence(PATOM a, ARITY ar);
   PATOM getAssociativity(PATOM a, ARITY ar);
   bool IsLeftAssoc(PATOM a, ARITY ar);
   bool IsRightAssoc(PATOM a, ARITY ar);
   bool IsLrand(PATOM a, ARITY ar);
   bool IsRrand(PATOM a, ARITY ar);
   bool IsPostfix(PATOM a, ARITY ar);
   bool IsPrefix(PATOM a, ARITY ar);

private:
   void MakeAtoms();                  // create the predefined atoms
   void MakeOp(PATOM, int, PATOM );   // create an op defn, called from p_op

public:
   //-----------------------
   // built-in predicates
   //
   TF p_atom_length(void);
   TF p_atomlist_concat(void);
   TF p_name(void);
   TF p_number_chars(void);
   TF p_atom_chars(void);
   TF p_atom_codes(void);
   TF p_atom_uplow(void);
   TF p_op(void);

   TF p_getZop(void);

   //-----------------

#ifdef LANDFILL
   void Dump();
#endif

};

#endif // ATOMTABLE_H

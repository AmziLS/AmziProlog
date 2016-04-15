/***************************************************************************\
*
* atomtable.cpp -- Atom Table Implementation
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: atomtable.cpp,v $
* Revision 1.11  2007/05/16 02:15:38  dennis
* a7-6-8
*
* Revision 1.10  2006/12/19 04:58:46  dennis
* module names are just atoms
*
* Revision 1.9  2006/12/02 20:38:33  mary
* Add curlysA {} atom. Changed writes to sync file to be under BUG_SYNC ifdef.
*
* Revision 1.8  2006/08/10 13:31:49  dennis
* fix to async
*
* Revision 1.7  2005/12/06 23:24:47  dennis
* writeing of '_' fixed
*
* Revision 1.6  2005/12/03 02:03:35  dennis
* atom_codes fix for vba mode
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
* Revision 1.43  2003/08/27 03:04:43  dennis
* added initialization support for remote debugging LSX
*
* Revision 1.42  2003/07/24 17:30:40  dennis
* support for debugging compiled code, reorg of alib etc to support that
*
* Revision 1.41  2003/06/13 18:37:01  dennis
* date_time and other fixes
*
* Revision 1.40  2003/03/24 15:49:01  dennis
* getting read for 6.4.0 alpha
*
* Revision 1.39  2003/03/15 14:34:04  dennis
* Many changes in debug64 and proto_debug, source code
* debugging is closer to a reality
*
* Revision 1.38  2003/02/13 23:44:36  dennis
* latest debug64 stuff
*
* Revision 1.37  2002/11/26 16:50:39  dennis
* thanksgiving updates
*
* Revision 1.36  2002/11/20 14:54:44  dennis
* added support for user defined streams, fixed java jni bug with
* extended predicates
*
* Revision 1.35  2002/09/14 20:47:54  dennis
* two new .cfg parameters, maxmemory for unix only, outputdepth to
* catch cyclic terms
*
* Revision 1.34  2002/05/15 16:59:07  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.33  2002/05/02 17:39:30  dennis
* various minor bug fixes, added locale as a settable prolog flag
*
* Revision 1.32  2002/04/25 03:42:22  dennis
* more documentation, logicbase.htm, and some fiddling with sources
*
* Revision 1.31  2002/04/19 19:41:43  dennis
* fixed retract bug with sorted/indexed clauses, implemented abolish for
* those types as well
*
* Revision 1.30  2002/03/25 22:24:06  dennis
* Added read/write_binary predicates to replace fwrite, also repositioning
* for binary files. Note that even though the standard says there's a read
* and write position, they are one and the same.
*
* Revision 1.29  2002/02/19 04:11:39  dennis
* changed reals to use pass by reference, eliminating almost all needs
* for new and delete, seems to have eliminated most all leaks due to math,
* put in memcpy, memmove etc. for copying gigit arrays around.
*
* Revision 1.28  2002/02/13 03:19:59  dennis
* reals changed to have a LReal class, moved to file of same name,
* math functions moved out of termsvc and into lmath.cpp/h, eval rewritten
* to reflect various options for numbers, lexcept modified so anyone, even
* non-engine objects, can throw LExcept objects.
*
* Revision 1.27  2002/02/04 17:20:58  dennis
* New lreal wrapper class on reals, start of number options:
* decimals: real/float, floats: single/double.  Created lmath.h/cpp
* but nothing in them yet.  Work to be done on sorting out mixed mode
* arithmetic for different options.  Also - casts removed, as they
* were causing problems.
*
* Revision 1.26  2002/01/28 06:29:18  dennis
* changes for parsing numbers, handling different options
*
* Revision 1.25  2002/01/20 20:48:05  ray
* revised real divide, printReal
*
* Revision 1.24  2002/01/06 20:31:27  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.23  2001/12/03 20:20:52  dennis
* fixed copyterm to allow infinite variables, not limited to maxvars.
*
* Revision 1.22  2001/11/21 00:31:50  dennis
* removed mode stuff, filled out prolog flags
*
* Revision 1.21  2001/11/15 13:54:07  dennis
* Fixed logging and apitrace, made logfile into its own entity,
* removed from the Prolog stream IO.
*
* Revision 1.20  2001/10/05 20:44:10  dennis
* fixed p_prolog_flag so gcc would be happier
*
* Revision 1.19  2001/10/05 19:15:17  dennis
* string streams, function streams working, reals set up to not require
* m_peng, just passed in parameters as necessary
*
* Revision 1.18  2001/10/02 16:05:20  dennis
* changed streams, cleaner interface to lex, reimplemented
* readers as buffer between the two
*
* Revision 1.17  2001/08/29 19:34:33  ray
* Corrections to 'seen' in alib etc.
*
* Revision 1.16  2001/08/16 17:07:24  ray
* merged everything, repaired errors, added BINSTREAM
*
* Revision 1.15  2001/08/01 20:17:59  ray
* removed tswrite.cpp & sio.cpp, added streams.cpp sowrite.cpp
*
* Revision 1.14  2001/07/21 00:39:46  dennis
* added garbage collector for strings and things
*
* Revision 1.13  2001/07/10 16:51:31  dennis
* added more memory leak checking tools, all clean so far
*
* Revision 1.12  2001/06/27 15:15:09  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.11  2001/03/28 15:07:21  ray
* char_code/2, number_chars/2 added
* 1 char macros proscribed
*
* Revision 1.10  2001/03/13 20:05:17  dennis
* added meta$call to try and isolate meta calls.
*
* Revision 1.9  2001/03/09 20:13:59  dennis
* consolidating for KW delivery, fixed jni memory leak
*
* Revision 1.8  2001/02/21 04:46:42  dennis
* debugger working, updated documentation for 6.1
*
* Revision 1.7  2001/02/18 18:47:14  ray
* Substituted end_of_file for !EOS
* Replaced ` with 0' for char code denotation
* Introduced ` quote mark as default for strings
*
* Revision 1.6  2001/02/05 20:17:09  dennis
* fixed some bugs
*
* Revision 1.5  2001/01/30 16:47:28  dennis
* Made, after many trials, alib into amzi_system module.
*
* Revision 1.4  2001/01/11 01:49:23  dennis
* Implemented rest of import/export and metapredicates, working
* for test cases.
*
* Revision 1.3  2001/01/05 06:05:54  dennis
* fixed minor discrepancies
*
* Revision 1.2  2001/01/01 15:46:28  ray
* Added float to real promotion in expressions
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.13  2000/10/24 14:00:44  ray
* Added atom_chars.
* Performed cosmetic surgery on termsvc.
*
* Revision 1.12  2000/10/15 18:04:23  ray
* Eliminated requirement to annotate integers > 1 billion with 'L'
* Now, only numbers greater than MAXintC get promoted to Real
*
* Revision 1.11  2000/10/12 19:05:14  ray
* Corrected mult for real.
* Added clock for integer calculation of time in milliseconds.
* Added run time printout to alis
*
* Revision 1.10  2000/10/01 16:20:03  dennis
* cleaned up modules, ddb, got defined, abolish and friends working
*
* Revision 1.9  2000/09/27 01:42:03  dennis
* implemented listing, needed current_predicate, predicate_property,
* and current_module
*
* Revision 1.8  2000/09/25 02:11:19  dennis
* first version of modules working, runs the modular version of
* duck world.  still needs import and export.  release 6.1.1
*
* Revision 1.7  2000/09/20 17:09:11  ray
* added casting operators to cell and rand to clean up code.
* added prolog flag 'modulo' and adapted arithmetic to use it.
* added fraction, realSpan and nth.
* added fast ** for reals.
*
* Revision 1.6  2000/09/05 20:24:28  ray
* Added real divide
*
* Revision 1.5  2000/09/02 02:10:19  dennis
* new version of get$db replacing dbrefgen for getting clauses
* from dynamic database
*
* Revision 1.4  2000/08/26 00:32:04  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.3  2000/08/14 02:05:37  ray
* Added Real class.
* No division yet.
* No error reporting yet.
*
* Revision 1.1  2000/05/14 03:52:32  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
*
\***************************************************************************/
#include "inc.h"
#include "pch.h"

// defined for STL map templates that need < for keys
bool operator<(LPredicate a, LPredicate b)
{
   // first sort by functor
   if ( *(a.functor) < *(b.functor) )
      return true;
   if ( *(a.functor) > *(b.functor) )
      return false;
   // use arity if a tie
   if (a.arity < b.arity)
      return true;
   return false;
}

bool operator==(LPredicate a, LPredicate b)
{
   if (a.functor == b.functor && a.arity == b.arity)
      return true;
   else
      return false;
}

/*
Lostream & operator<<( Lostream &os, PATOM a)
{
   os << *a;
   return os;
}

Lostream& operator<<( Lostream &os, const LPredicate &p )
{
   os << *(p.functor) << "/" << p.arity;
   return os;
}

Lostream& operator<<( Lostream &os, const LOperator &o )
{
   os << o.prec << " " << *(o.assoc);
   return os;
}
*/

Lostream& operator<<( Lostream &os, PATOM a)
{
   os << *a;
   return os;
}

Lostream& operator<<( Lostream &os, const LPredicate &p )
{
   os << *(p.functor) << "/" << p.arity;
   return os;
}

Lostream& operator<<( Lostream &os, const LOperator &o )
{
   os << o.prec << " " << *(o.assoc);
   return os;
}

LAtom::LAtom(LEngine *m_peng, STRptr s) : LString(s)
{
   //if (pSTATE->m_vba)
   //{
      display_name = LString(s);
      toLower();
      //std::cout << "LAtom(vba)" << display_name << *this << "\n";
   //}
}

LAtom::LAtom(LEngine *m_peng, LString s) : LString(s)
{
   //if (pSTATE->m_vba)
   //{
      display_name = LString(s);
      toLower();
      //std::cout << "LAtom(vba)" << display_name << *this << "\n";
   //}
}

LString LAtom::get_display_name(LEngine *m_peng)
{
   if (pSTATE->m_vba && display_name != LString())
   {
      //std::cout << "get_display_name(vba)" << display_name << "\n";
      return display_name;
   }
   else
   {
      //std::cout << "get_display_name(notvba)" << *this << "\n";
      return *this;
   }
}

AtomTable::AtomTable(LEngine *peng)
{
   m_peng = peng;
   m_atomtable = NULL;
   m_operators = NULL;

   atom_count = 0;
}

AtomTable::~AtomTable()
{
   if (m_atomtable)  // might not have gotten through initialization
   {
      AtomIter ai = m_atomtable->begin();
      while (ai != m_atomtable->end())
      {
         delete ai->second;
         atom_count--;
         //delete ai->second;
         ai++;
      }
   }

   delete m_atomtable;
   delete m_operators;
};

void AtomTable::Init()
{
   LNEW(m_atomtable, AtomMap(), aS("initialization"));
   LNEW(m_operators, OpMap(), aS("initialization"));

   MakeAtoms();
}

struct ATOM_INIT
{
   aCHAR *Aname;
   PATOM *AAtom;
};

void AtomTable::MakeAtoms()
{
   // atoms used internally
   ATOM_INIT  AtomTab[] =
   {
      // hardwired arithmetic operator atoms
      {aS("-"),       &minusA},
      {aS("+"),       &plusA},
      {aS("/"),       &divA},
      {aS("divs"),    &divsA},
      {aS("divu"),    &divuA},
      {aS("//"),      &divdivA},
      {aS("*"),       &timesA},
      {aS("**"),      &powerA},
      {aS("max"),     &maxA},
      {aS("min"),     &minA},
      {aS("mod"),     &modA},
      {aS("mods"),    &modsA},
      {aS("modu"),    &moduA},
      {aS("rem"),     &remA},
      {aS("/\\"),     &bitandA},
      {aS("\\/"),     &bitorA},
      {aS("xor"),     &xorA},
      {aS(">>"),      &rshiftA},
      {aS("<<"),      &lshiftA},
      {aS("\\"),      &notA},
      {aS("call"),    &callA},
      {aS("[]"),      &nilA},
      {aS("{}"),      &curlysA},
      {aS("integer"), &integerA},
      {aS("float"),   &floatA},
      {aS("real"),    &realA},
      {aS("abs"),     &absA},
      {aS("ln"),      &lnA},
      {aS("log"),     &logA},
      {aS("log10"),   &log10A},
      {aS("exp"),     &expA},
      {aS("sin"),     &sinA},
      {aS("cos"),     &cosA},
      {aS("tan"),     &tanA},
      {aS("asin"),    &asinA},
      {aS("acos"),    &acosA},
      {aS("atan"),    &atanA},
      {aS("sqrt"),    &sqrtA},
      {aS("e"),       &eA},
      {aS("pi"),      &piA},
      {aS("degtorad"),&degtoradA},
      {aS("radtodeg"),&radtodegA},
      {aS("cpuclock"),&cpuclockA},
      {aS("cputime"), &cputimeA},
      {aS("random"),  &randomA},
      {aS("inf"),     &infA},
      {aS("is_odd"),  &oddA},
      {aS("nth"),     &nthA},
      {aS("sign"),    &signA}, 
      {aS("floor"),   &floorA}, 
      {aS("ceiling"), &ceilA},
      {aS("round"),   &roundA}, 
      {aS("truncate"), &truncA}, 
      {aS("float_round"), &froundA}, 
      {aS("float_truncate"), &ftruncA}, 
      {aS("float_integer_part"), &intpartA}, 
      {aS("float_fractional_part"), &fracpartA},

      // atoms that are flag values (see BuiltIns::p_prologZflag)

      {aS("heap"), &heapA},
      {aS("local"), &localA},
      {aS("control"), &controlA},
      {aS("trail"), &trailA},
      {aS("heapbumper"), &heapbumperA},
      {aS("readbuffer"), &readbufferA},
      {aS("readdepth"), &readdepthA},
      {aS("maxclauses"), &maxclausesA},
      {aS("outputdepth"), &outputdepthA},
#ifdef UNIX
      {aS("maxmemory"), &maxmemoryA},
#endif
      {aS("logfile"), &logfileA},
      {aS("apitrace"), &apitraceA},
      {aS("string_esc"), &string_escA},
      {aS("lsxload"), &lsxloadA},
      {aS("macroheapsz"), &macroheapszA},
      {aS("gcthingfreq"), &gcthingfreqA},
      {aS("maxfiles"), &maxfilesA},
      {aS("locale"), &localeA},

      {aS("occurs_check"), &occurs_checkA},
      {aS("double_quote_strings"), &double_quote_stringsA},
      {aS("macrotrace"), &macrotraceA},
      {aS("preprocessor"), &preprocessorA},
      {aS("vars_sort_equal"), &vars_sort_equalA},
      {aS("debug64_cut"), &debug64_cutA},
      //{aS("string_esc"), &string_escA},
      {aS("trace"), &traceA},
      {aS("upper_case_atoms"), &upper_case_atomsA},
      {aS("vba"), &vbaA},
      {aS("utf8io"), &utf8ioA},
      {aS("debug_port"), &debug_portA},
      {aS("debug_host"), &debug_hostA},

      {aS("decimal_places"), &decimal_placesA},
      {aS("epsilon"), &epsilonA},
      {aS("delta"), &deltaA},
      {aS("modulo"), &moduloA},
      
      {aS("bounded"), &boundedA},
      {aS("char_conversion"), &char_conversionA},
      {aS("debug"), &debugA},
      {aS("exception$debug$stack"), &exceptionZdebugZstackA},
      {aS("integer_rounding_function"), &integer_rounding_functionA},
      {aS("max_arity"), &max_arityA},
      {aS("max_integer"), &max_integerA},
      {aS("min_integer"), &min_integerA},
      {aS("prolog_copyright"), &prolog_copyrightA},
      {aS("prolog_date"), &prolog_dateA},
      {aS("prolog_name"), &prolog_nameA},
      {aS("prolog_version"), &prolog_versionA},
      {aS("amzi"), &amziA},
      {aS("undefined_predicate"), &undefined_predicateA},
      {aS("unicode"), &unicodeA},
      {aS("amzi_listener"), &amzi_listenerA},
      {aS("amzi_compiler"), &amzi_compilerA},
	  {aS("amzi_debugger"), &amzi_debuggerA},

      {aS("on"), &onA},
      {aS("off"), &offA},
      {aS("unknown_flag"), &unknown_flagA},
      {aS("toward_zero"), &toward_zeroA},
      {aS("not_applicable"), &not_applicableA},

      {aS("decimals"), &decimalsA},
      {aS("floats"), &floatsA},
      {aS("single"), &singleA},
      {aS("double"), &doubleA},


      // return values for predicateZproperty/2
      {aS("static"),       &(staticA)},
      {aS("dynamic"),      &(dynamicA)},
      {aS("built_in"),     &(built_inA)},
      {aS("extended"),     &(extendedA)},
      {aS("public"),       &(publicA)},
      {aS("private"),      &(privateA)},
      {aS("multifile"),    &(multifileA)},
      {aS("exported"),     &(exportedA)},
      {aS("metapredicate"),&(metapredicateA)},
      {aS("imported_from"),&(imported_fromA)},
      {aS("defined_in"),   &(defined_inA)},

      // error types, used for err$exec/3
      {aS("typeE"),        &(typeEA)},
      {aS("instanceE"),    &(instanceEA)},
      {aS("execE"),        &(execEA)},
      {aS("syntaxE"),      &(syntaxEA)},

   // types used by IO::read/write_binary
      {aS("char"),          &(charA)},
      {aS("wide_char"),     &(wide_charA)},
      {aS("short_integer"), &(short_integerA)},
      {aS("single_float"),  &(single_floatA)},
      {aS("double_float"),  &(double_floatA)},

      // other common atoms
      
      {aS(":-"),           &(ifA)},
      {aS("?-"),           &(queryA)},
      {aS(","),            &(commaA)},
      {aS("|"),            &(vbarA)},
      {aS("is"),           &(isA)},
      {aS("latent_exp"),   &(latentA)},
      {aS("latent_opdef"), &(latentodA)},
      {aS("stdin"),        &(stdinA)},
      {aS("stdout"),       &(stdoutA)},
      {aS("stderr"),       &(stderrA)},
      {aS("function"),     &(functionA)},
      {aS("end_of_file"),  &(eofA)},
//      {aS("!EOF"),  &(eofA)},
		{aS("file_name"),	   &(file_nameA)},
		{aS("mode"),      	&(modeA)},
		{aS("alias"),      	&(aliasA)},
		{aS("input"),	      &(inputA)},
		{aS("output"),	      &(outputA)},
		{aS("type"),	      &(typeA)},
		{aS("end_of_stream"), &(end_of_streamA)},
		{aS("eof_action"),   &(eofActionA)},
		{aS("position"),     &(positionA)},
		{aS("reposition"),   &(repositionA)},
		{aS("error"),   	   &(error_A)},
		{aS("eof_code"),     &(eof_code_A)},
		{aS("reset"),	      &(reset_A)},
		{aS("at"),	         &(at_A)},
		{aS("past"),	      &(past_A)},
		{aS("no"),	         &(no_A)},
		{aS("read"),         &(readA)},
		{aS("write"),        &(writeA)},
		{aS("readwrite"),    &(readwriteA)},
		{aS("append"),       &(appendA)},
		{aS("readappend"),   &(readappendA)},
		{aS("macro"),        &(macroA)},
		{aS("default"),      &(defaultA)},
		{aS("\n"),           &(nlA)},
      {aS("beginning"),    &(beginningA)},
      {aS("current"),      &(currentA)},
      {aS("end"),          &(endA)},
      {aS("line_number"),  &(line_numberA)},

      {aS("yfx"),          &(yfxA)},
      {aS("yfy"),          &(yfyA)},
      {aS("xfx"),          &(xfxA)},
      {aS("xf"),           &(xfA)},
      {aS("yf"),           &(yfA)},
      {aS("fx"),           &(fxA)},
      {aS("fy"),           &(fyA)},
      {aS("xfy"),          &(xfyA)},
      {aS("true"),         &(trueA)},
      {aS("false"),        &(falseA)},
      {aS("fail"),         &(failA)},
      {aS("{}"),           &(curlyA)},
      {aS("."),            &(periodA)},
      {aS("err$handle"),   &(errorA)},
      {aS("error"),        &(errorflagA)},
      {aS("cp$top"),       &(topA)},
      {aS(";"),            &(semiA)},
      {aS(":"),            &(colonA)},
      //{aS("!IS"),          &(arithEA)},
      {aS("call$i"),       &(call_dollarA)},
      {aS("call$d"),       &(call_d_dollarA)},
      {aS("meta$convert"), &(metaZconvertA)},
      {aS("rb$"),          &(rbZA)},
      {aS("run$code"),     &(runZcodeA)},
      {aS("call_nometa"),  &(call_nometaA)},
      {aS("meta$call"),    &(metaZcallA)},
      {aS("!"),            &(cutA)},
      {aS("text"),         &(textA)},
      {aS("binary"),       &(binaryA)},
      {aS("wide_text"),    &(wide_textA)},
      {aS("user_input"),   &(userInA)},
      {aS("user_output"),  &(userOutA)},
      {aS("user_error"),   &(userErrA)},
      {aS("user_function"), &(userFuncA)},
      {aS("user_log"),     &(userLogA)},
      {aS("string"),       &(stringA)},
      {aS(""),             &(emptyA)},
      {aS("$VAR"),         &(zvarA)},   // used by numbervars and write
      {aS("debug64$call"),   &(debugZcallA)},
      {aS("debug64$info"),   &(debugZinfoA)},
      {aS("goal"),         &(goalA)},
      {aS("clause"),       &(clauseA)},
      {aS("user_break"),   &(user_breakA)},
      {aS("user_pause"),   &(user_pauseA)},

	  //modules
	  {aS("amzi_system"),  &(amzi_systemA)},
	  {aS("user"),         &(userA)},

      {NULL, 0},
   };

   ATOM_INIT *pai;

   pai = &AtomTab[0];
   while( pai-> Aname)
   {
      *(pai->AAtom) = EnterAtom(pai->Aname);
      ++pai;
   }
   NilCell.setAtom(nilA);
   NilTerm = &(NilCell);
   NilOp = LOperator(0,NULL);
   TrueCell.setAtom(trueA);
   TrueTerm = &(TrueCell);
}

//  Using the STL map for an atom table.  The key is the LAtom object,
//  that is, a string, and the value is the pointer to the object.
//  Thus the map lets you find the pointer to the LAtom based on its
//  String value.

//  STL note, insert returns a pair(iterator, bool) where the bool
//  is false if nothing was inserted.

PATOM AtomTable::EnterAtom(STRptr s)
{

   LAtom *pla;
   if (pSTATE->m_vba)
   {
      LNEW(pla, LAtom(m_peng, s), aS("atom"));
   }
   else
   {
      LNEW(pla, LAtom(s), aS("atom"));
   }
   atom_count++;

   std::pair<AtomIter, bool> result = m_atomtable->insert(AtomPair(*pla, pla));
   if (! result.second)    // if result.second is false no insertion occurred, 
   {
      delete pla;          // so no need for the atom we just made
      atom_count--;
   }  // need brackets for LDEL expansion
   if (pSTATE->m_vba) { result.first->second->set_display_name(s); }
   return result.first->second;        // return the atom from the atomtable
}

PATOM AtomTable::EnterAtom(char *s)
{
   aCHAR *ws = g_lenv.new_wcs_dup(s);
   PATOM a = EnterAtom(ws);
   delete ws;
   return a;
}

// When we know the atom is case insensitive
PATOM AtomTable::EnterCIAtom(STRptr s)
{

   LAtom *pla;
   LNEW(pla, LAtom(m_peng, s), aS("atom"));
   atom_count++;

   std::pair<AtomIter, bool> result = m_atomtable->insert(AtomPair(*pla, pla));
   if (! result.second)    // if result.second is false no insertion occurred, 
   {
      delete pla;          // so no need for the atom we just made
      atom_count--;
   }  // need brackets for LDEL expansion
   return result.first->second;        // return the atom from the atomtable
}

PATOM AtomTable::EnterCIAtom(char *s)
{
   aCHAR *ws = g_lenv.new_wcs_dup(s);
   PATOM a = EnterCIAtom(ws);
   delete ws;
   return a;
}

// STL notes, ATOMITER is an iterator for atoms, see
// definition in atomtable.h.  The iterator acts as
// a pointer.  If its equal to end(), then there the atom
// wasn't found.  Otherwise the iterator 'points' to the
// pair and we can return the 'second' value of the pair.

PATOM AtomTable::FindAtom(STRptr s)
{                                    // Find an atom, return NULL if not found.
   AtomIter ai;
   if (pSTATE->m_vba)
      ai = m_atomtable->find(LAtom(m_peng, s));
   else
      ai = m_atomtable->find(LAtom(s));

   return ai == m_atomtable->end() ? NULL : ai->second;
}
//----------------------------------------
// Operator functions
//

void AtomTable::MakeOp(PATOM a, int prec, PATOM assoc)
{
   ARITY ar;
   if (assoc == xfxA || assoc == xfyA || assoc == yfxA || assoc == yfyA)
      ar = 2;
   else if (assoc == xfA || assoc == yfA || assoc == fxA || assoc == fyA)
      ar = 1;
   else
      pXCPT->Error(opopE, aS("unknown associativity"));

#ifdef BUG_OPS
   FILL( "MakeOp: " << a << " " << prec << " " << assoc );
#endif

   if (prec > 1200 || prec < 0)
      pXCPT->Error(opopE, aS("precedence out of range"));

   std::pair<OpIter, bool> result =
         m_operators->insert( OpPair(LPredicate(a, ar),
												 LOperator(prec, assoc)) );
   if ( ! result.second )              // no insertion implies existing value.
   {                                 // we should replace it, so do that
      m_operators->erase(result.first);
      result = m_operators->insert( OpPair(LPredicate(a, ar),
														 LOperator(prec, assoc)) );
      if ( ! result.second )
         pXCPT->Error(opopE, aS("unable to replace existing definition"));
   }
}

bool AtomTable::IsOperator(PATOM a, ARITY ar)
{   // the iterator points to a std::pair, of which the 2nd value is our value.
   OpIter oi = m_operators->find(LPredicate(a, ar));

   return oi == m_operators->end() ? false : true;
}

LOperator &AtomTable::FindOperator(PATOM a, ARITY ar)
{
   OpIter oi = m_operators->find(LPredicate(a,ar));
   return oi == m_operators->end() ? NilOp : oi->second;
}

int AtomTable::getPrecedence(PATOM a, ARITY ar)
{
   LOperator o = FindOperator(a, ar);
   return o == NilOp ? 0 : o.prec;
}

PATOM AtomTable::getAssociativity(PATOM a, ARITY ar)
{
   LOperator o = FindOperator(a, ar);
   return o == NilOp ? NULL : o.assoc;
}

bool AtomTable::IsLeftAssoc(PATOM a, ARITY ar)
{
   PATOM assoc = getAssociativity(a, ar);
   return assoc == NULL ? false : 
	  (assoc == fxA || assoc == xfxA || assoc == yfxA); 
}

bool AtomTable::IsRightAssoc(PATOM a, ARITY ar)
{
   PATOM assoc = getAssociativity(a, ar);
   return assoc == NULL ? false : 
	  (assoc == xfA || assoc == xfxA || assoc == xfyA); 
}

bool AtomTable::IsLrand(PATOM a, ARITY ar)
{
   PATOM assoc = getAssociativity(a, ar);
   return assoc == NULL ? false : (assoc == xfxA || assoc == xfyA); 
}

bool AtomTable::IsRrand(PATOM a, ARITY ar)
{
   PATOM assoc = getAssociativity(a, ar);
   return assoc == NULL ? false : (assoc == xfxA || assoc == yfxA); 
}

bool AtomTable::IsPostfix(PATOM a, ARITY ar)
{
   PATOM assoc = getAssociativity(a, ar);
   return assoc == NULL ? false : (assoc == xfA || assoc == yfA); 
}

bool AtomTable::IsPrefix(PATOM a, ARITY ar)
{
   PATOM assoc = getAssociativity(a, ar);
   return assoc == NULL ? false : (assoc == fxA || assoc == fyA); 
}

//---------------------
// Atom predicates
//

//   instantiation errors:
//   atom is variable
//   
//   type errors:
//   atom not atom or variable
//   length is not integer or variable 
TF AtomTable::p_atom_length()
{                                              // atom_length(+atom, ?integer)
   TERM   tArg1, tArg2;
   int    length;

   tArg1 = X(0);
   if (tArg1->IsVar())
      pXCPT->Error(instanceE, aS("arg1 must be instantiated"));
   if (!tArg1->IsAtom())
      pXCPT->Error(typeE, aS("arg1 must be an atom"));

   tArg2 = X(1);
   if (!(tArg2->IsVar() || tArg2->IsInt()))
      pXCPT->Error(typeE, aS("arg2 must be integer or variable"));

   length = tArg1->getAtom()->Length();
   return(pTSVC->UnifyInt(length, tArg2));
}

//  instantiation error:
//  atomlist a variable
//  
//  type error:
//  atom not an atom or variable
//  atomlist not a list of atoms 
TF AtomTable::p_atomlist_concat()
{                                         // atomlist_concat(+atomlist, -atom)
  TERM    tArg1, tArg2, t, x;
  STRptr  buffer;
  PATOM   patom;
  Cell    cell;
  
  tArg1 = X(0);
  if (tArg1->IsVar())
	 pXCPT->Error(instanceE, aS("arg1 must be a list of atoms"));
  if (!tArg1->IsList())
	 pXCPT->Error(typeE, aS("arg1 must be a list of atoms"));
  
  tArg2 = X(1);
  if (!(tArg2->IsVar() || tArg2->IsAtom()))
	 pXCPT->Error(typeE, aS("arg2 must be an atom or variable"));
  if (!(tArg1->IsList()))
	 pXCPT->Error(typeE, aS("arg1 must be a list"));
  
  buffer = pPSTR->strBuf();
  buffer[0] = EOS;
  t = tArg1;
  for(bool first = true; t->IsList(); first = false)
	 {
      t = t->getListHead();                  // head of list
      x = (t)->dref();                       // term at head of list
		if(x->IsInt())
		{                                      // allow numbers to be concated
		  if(first)
			 pXCPT->Error(typeE, aS("head of list must be atom, not integer"));
		  aCHAR numbuf[8];
		  pWRIT->termWriteString(x, numbuf, 8, FALSE);
		  if((size_t)pSTATE->m_buflen <= Lstrlen(buffer) + Lstrlen(numbuf))
			 pXCPT->Error(stringoverE);
		  
		  Lstrcat(buffer, numbuf);
		}
			else 
		{ 
		  if (!x->IsAtom())
			 pXCPT->Error(typeE, aS("arg1 must be a list of atoms"));
		  
		  if((size_t)pSTATE->m_buflen <= 
			  Lstrlen(buffer) + Lstrlen(*(x->getAtom())))
			 pXCPT->Error(stringoverE);
		  
		  Lstrcat(buffer, *(x->getAtom()));
		}
      t++;
      t = (t)->dref();
	 }
  
  patom = EnterAtom(buffer);
  cell.setAtom( patom);
  return(pTSVC->Unify(&cell, tArg2));
}

//    name(+atom, +list)
//    name(+atom, -list)
//    name(-atom, +list)
//    
//    errors:
//    arg1 & arg2 are variables - instantiation_error
//    arg1 is neither variable nor atom - type_error
//    arg2 is neither variable nor list - type_error
//    
TF AtomTable::p_name()
{         //    name/2 is the classic, atom_codes/2 uses new Unicode char type 
   TERM    tArg1, tArg2;
   int   err;
   Cell    x;
   PATOM   a;
   STRptr  namebuf;
   
   namebuf = pPSTR->strBuf();

   tArg1 = X(0);
   if (!(tArg1->IsAtom() || tArg1->IsVar()))
      pXCPT->Error(typeE, aS("arg1 must be an atom or variable"));
   tArg2 = X(1);
   if (!(tArg2->IsList() || tArg2->IsVar()))
      if (!(tArg2->IsAtom() && tArg2->getAtom()==nilA))
         pXCPT->Error(typeE, aS("arg2 must be a char list or variable"));
   if (tArg1->IsVar() && tArg2->IsVar())
      pXCPT->Error(instanceE, aS("at least one arg must be instantiated"));

   if (tArg1->IsAtom()) 
      return (pTSVC->Unify(pHXL->XVar(1),
            pHXL->StrToCharList(*(tArg1->getAtom())) ));

   if (tArg2->IsAtom())                      // The empty list
      namebuf[0] = EOS;
   else if (TRUE != (err = pHXL->CharListToStr(tArg2, namebuf)))
      return(err);

   a = EnterAtom((STRptr)namebuf);
   x.setAtom( a);
   return(pTSVC->Unify(pHXL->XVar(0), &x));
}

// atom_codes(+atom, +list)
// atom_codes(+atom, -list)
// atom_codes(-atom, +list)
//
// errors:
//  arg1 & arg2 are variables - instantiation_error
//  arg1 is neither variable nor atom - type_error
//  arg2 is neither variable nor list - type_error
TF AtomTable::p_atom_codes()
{
   TERM    tArg1, tArg2;
   int   err;
   Cell    x;
   PATOM   a;
   STRptr  namebuf;
   
   namebuf = pPSTR->strBuf();

   tArg1 = X(0);
   if (!(tArg1->IsAtom() || tArg1->IsVar()))
      pXCPT->Error(typeE, aS("arg1 must be an atom or variable"));
   tArg2 = X(1);
   if (!(tArg2->IsList() || tArg2->IsVar()))
      if (!(tArg2->IsAtom() && tArg2->getAtom()==nilA))
         pXCPT->Error(typeE, aS("arg2 must be a char list or variable"));
   if (tArg1->IsVar() && tArg2->IsVar())
      pXCPT->Error(instanceE, aS("at least one arg must be instantiated"));

   if (tArg1->IsAtom())
   {
      if (pSTATE->m_vba)
     return (pTSVC->Unify(pHXL->XVar(1),
                          pHXL->StrToCharList((tArg1->getAtom())->get_display_name(m_peng)) ));
      else
     return (pTSVC->Unify(pHXL->XVar(1),
                          pHXL->StrToCharList(*(tArg1->getAtom())) ));
   }

   if (tArg2->IsAtom())                        // The empty list
      namebuf[0] = EOS;
   else if (TRUE != (err = pHXL->CharListToStr(tArg2, namebuf)))
      return(err);

   a = EnterAtom((STRptr)namebuf);
   x.setAtom( a);
   return(pTSVC->Unify(pHXL->XVar(0), &x));
}

TF AtomTable::p_atom_chars()
{                                        // standard requires a list of atoms
  TERM    tArg1, tArg2;
  int     err;
  Cell    x;
  PATOM   a;
  STRptr  namebuf;
  
  namebuf = pPSTR->strBuf();
  
  tArg1 = X(0);
  if (!(tArg1->IsAtom() || tArg1->IsVar()))
	 pXCPT->Error(typeE, aS("arg1 must be an atom or variable"));
  tArg2 = X(1);
  if (!(tArg2->IsList() || tArg2->IsVar()))
	 if (!(tArg2->IsAtom() && tArg2->getAtom()==nilA))
		pXCPT->Error(typeE, aS("arg2 must be a char list or variable"));
  if (tArg1->IsVar() && tArg2->IsVar())
	 pXCPT->Error(instanceE, aS("at least one arg must be instantiated"));
  
  if (tArg1->IsAtom())                         // atom points to name string
   {
      if (pSTATE->m_vba)
     return (pTSVC->Unify(pHXL->XVar(1),
                          pHXL->StrToAtomList((tArg1->getAtom())->get_display_name(m_peng)) ));
      else
     return (pTSVC->Unify(pHXL->XVar(1),
                          pHXL->StrToAtomList(*(tArg1->getAtom())) ));
   }
  /*-------------------------------------------------------------------*/
  if (tArg2->IsAtom())                        // The empty list
	 namebuf[0] = EOS;
  else if (!(err = pHXL->AtomListToStr(tArg2, namebuf)))
	 return(err);                              // err is always 1 or 0
  
  a = EnterAtom((STRptr)namebuf);
  x.setAtom(a);
  return(pTSVC->Unify(pHXL->XVar(0), &x));
}

TF AtomTable::p_number_chars()
{                                        // standard requires a list of atoms
  TERM    tArg1, tArg2;
  int     err;
  STRptr  namebuf = pPSTR->strBuf();
  
  tArg1 = X(0);
  if (!(tArg1->IsNumber() || tArg1->IsVar()))
	 pXCPT->Error(typeE, aS("arg1 must be an number or variable"));

  tArg2 = X(1);
  if (!(tArg2->IsList() || tArg2->IsVar()))
	 if (!(tArg2->IsAtom() && tArg2->getAtom()==nilA))
		pXCPT->Error(typeE, aS("arg2 must be a char list or variable"));

  if (tArg1->IsVar() && tArg2->IsVar())
	 pXCPT->Error(instanceE, aS("at least one arg must be instantiated"));
  
  if (tArg1->IsNumber())                         // atom points to name string
	 {
		pWRIT->termWriteString(tArg1, namebuf, pPSTR->strBufLen(), FALSE);
		//pPSTR->strEnterString(namebuf, &sb);
		return pTSVC->Unify(pHXL->XVar(1), pHXL->StrToAtomList(namebuf));
	 }
  /*-------------------------------------------------------------*/
  if (tArg2->IsAtom())                        // The empty list
	 namebuf[0] = EOS;
  else 
	 {
		err = pHXL->AtomListToStr(tArg2, namebuf); // err is 1 or 0
		if (!err)
		  return(err);     
	 }
  err = pPSTR->strStringTerm(namebuf, &tArg1);
  return err ? pTSVC->Unify(pHXL->XVar(0), tArg1): err;
}

// atom_uplow(+atomup, -atomlow)
// atom_uplow(-atomup, +atomlow)
//
// instantiation error:
//    both are variables
//
// type error:
//    atomup not atom or variable
//    atomlow not atom or variable 
TF AtomTable::p_atom_uplow()
{       // Creates an upper or lower case atom from a lower or upper case one.
   TERM   tArg1, tArg2;
   PATOM  a;
   Cell   x;
   aCHAR   *namebuf, *p;

   p = namebuf = pPSTR->strBuf();

   tArg1 = X(0);
   tArg2 = X(1);

   if (tArg1->IsAtom() && (tArg2->IsVar() || tArg2->IsAtom()))
     {
       Lstrcpy(namebuf, *(tArg1->getAtom()));
       while (*p)
         {
           *p = (aCHAR)tolower(*p);
           p++;
         }
       a = EnterAtom((STRptr)namebuf);
       x.setAtom( a);
       return(pTSVC->Unify(&x, tArg2));   
     }
   
   if (tArg2->IsAtom() && tArg1->IsVar())
     {
       Lstrcpy(namebuf, *(tArg2->getAtom()));
       while (*p)
         {
           *p = (aCHAR)toupper(*p);
           p++;
         }
       a = EnterAtom((STRptr)namebuf);
       x.setAtom( a);
       return(pTSVC->Unify(tArg1, &x));
     }
   
   if (tArg1->IsVar() && tArg2->IsVar())
     pXCPT->Error(instanceE, aS("either arg1 or arg2 must be instantiated"));
   
   if (!tArg1->IsAtom() && !tArg1->IsVar())
     pXCPT->Error(typeE, aS("arg1 must be atom or variable"));
   
   if (!tArg2->IsAtom() && !tArg2->IsVar())
     pXCPT->Error(typeE, aS("arg2 must be atom or variable"));
   
   return FALSE;
}

TF AtomTable::p_op()
{                              // op$(Prec, Class, Symbol) - define an operator
   TERM arg1 = X(0);
   TERM arg2 = X(1);
   TERM arg3 = X(2);

   if (arg1->IsInt() && arg2->IsAtom() && arg3->IsAtom())
         {                    // call with atom, precedence and associativity
             MakeOp(arg3->getAtom(), arg1->getInt(), arg2->getAtom());
             return(TRUE);
         }
   pXCPT->Error(opopE, aS("argument of wrong type, must be op(integer, atom, atom)"));
   
   return FALSE;
}

// get$op(PTR, PREC, ASSOC, OP)
// PTR - var on first call, returned value of OpIter for next
// PREC - precedence of op
// ASSOC - its associativity
// OP - and name
//
// Note this is called from alib for current_op/3
// is a recursive repeat like loop.
TF AtomTable::p_getZop(void)
{
   TERM poiT;
   OpIter *poi;
   LPredicate lp;
   Cell c;
   TF tf;

   poiT = X(0);
   if (poiT->IsVar())  // first call
   {
      LNEW(poi, OpIter, aS("temporary"));   // deleted below
      c.setPtr(poi);
      pTSVC->UnifyConst(X(0), &c);

      *poi = m_operators->begin();

      if (*poi == m_operators->end())
         return FALSE;
   }
   else
      poi = (OpIter*) (poiT->getPtr());

   while (! (*poi == m_operators->end()) )
   {
      c.setInt((*poi)->second.prec);
      tf = pTSVC->UnifyConst( X(1), &c );
      if (tf)
      {
         c.setAtom((*poi)->second.assoc);
         tf = pTSVC->UnifyConst( X(2), &c );
      }
      if (tf)
      {
         c.setAtom((*poi)->first.functor);
         tf = pTSVC->UnifyConst( X(3), &c );
      }

      (*poi)++;

      if (tf)
         return TRUE;
   }

   delete poi;
   return FALSE;
}

//--------------------------------------------------
// LANDFILL functions
//

#ifdef LANDFILL

void AtomTable::Dump()
{
   DUMP << NL << "---------------------------------------" << NL;
   DUMP << "Dumping: AtomTable" << NL;
   DUMP << "size: " << m_atomtable->size() << NL;

   AtomIter ai = m_atomtable->begin();
   while (ai != m_atomtable->end())
   {
      DUMP << ai->first;
      DUMP << "  " << *(ai->second);
      DUMP << "  PATOM: " << ai->second << NL;
      ai++;
   }

   DUMP << NL << "---------------------------------------" << NL;
   DUMP << "Dumping: Operators" << NL;
   DUMP << "size: " << m_operators->size() << NL;

   OpIter oi = m_operators->begin();
   while (oi != m_operators->end())
   {
      //DUMP << oi->first;
      DUMP << "key: " << oi->first;
      DUMP << "  value: " << oi->second << NL;
      oi++;
   }

   DUMP << "---------------------------------------" << NL;
   DUMP << FLUSH;
}
#endif // LANDFILL

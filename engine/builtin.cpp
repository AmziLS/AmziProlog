/****************************************************************************
*
* builtin1.cpp -- Built-in predicates
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
****************************************************************************/
#ifdef WINDOWS
// disable warning C4786: symbol greater than 255 character,
// okay to ignore
#pragma warning(disable: 4786)
#endif

#include "inc.h"
#include "pch.h"

void BuiltIns::Init()
{
   // Some predicates are built-in predicates that the user will
   // use, others are for internal use only, that is, just called
   // from alib, which provides a cover function.  The last argument
   // determines if the predicate is exported from amzi_system or not.

   MakeBIPred(aS("gc"), 1,                &BuiltIns::p_gc, true, aS("gc"), aS("Forces garbage collection of unused dynamic clauses, heap cells, and strings"));
   MakeBIPred(aS("seed_random"), 1,       &BuiltIns::p_srand, true, aS("seed_random(SeedN)"), aS("Seeds the random number generator"));
   MakeBIPred(aS("abort"), 1,             &BuiltIns::p_abort, true, aS("abort(SeverityN)"), aS("Depending on Severity, 0 aborts and restarts Prolog, 1 aborts back to OS normally, 2 aborts back to OS with abnormal return"));
   MakeBIPred(aS("command_line"), 1,      &BuiltIns::p_commandl, true, aS("command_line(ArgsListofListsV)"), aS("Unifies the list ArgsListofLists with the command line arguments used to start the program"));
   MakeBIPred(aS("timer"), 1,             &BuiltIns::p_timer, true, aS("timer(TicksV)"), aS("Unifies Ticks with floating point seconds since whenever"));
   MakeBIPred(aS("time"), 3,              &BuiltIns::p_time, true, aS("time(HourNV, MinNV, SecNV)"), aS("Unifies arguments with current time, if any arguments are bound, then succeeds of fails if unification succeeds or fails"));
   MakeBIPred(aS("time"), 4,              &BuiltIns::p_time4, true, aS(""), aS(""));
   MakeBIPred(aS("date"), 3,              &BuiltIns::p_date, true, aS("date(MonthNV, DayNV, YearNV)"), aS("Returns the current date if unbound, or succeeds if it is today"));
   MakeBIPred(aS("openlog"), 1,           &BuiltIns::p_openlog, true, aS("openlog(FileA)"), aS("Open File as a log file and turn logging on"));
   MakeBIPred(aS("closelog"), 0,          &BuiltIns::p_closelog, true, aS("closelog"), aS("Close the logging file"));
   MakeBIPred(aS("writelog"), 1,          &BuiltIns::p_writelog, true, aS("writelog(Term)"), aS("Write Term to the log file, if logging"));
   MakeBIPred(aS("nllog"), 0,             &BuiltIns::p_nllog, true, aS("nllog"), aS("Write a newline to the log file"));
   MakeBIPred(aS("plmtrace_on"), 0,       &BuiltIns::p_plmtraceon, true, aS(""), aS(""));
   MakeBIPred(aS("plmtrace_off"), 0,      &BuiltIns::p_plmtraceoff, true, aS(""), aS(""));
   MakeBIPred(aS("pro_heap"), 2,          &BuiltIns::p_pro_heap, true, aS("pro_heap(TopV, PosV)"), aS("Returns current size, Top, and position, Pos, of the heap"));
   MakeBIPred(aS("pro_control"), 2,       &BuiltIns::p_pro_stack, true, aS("pro_control(TopV, PosV)"), aS("Returns current size, Top, and position, Pos, of the control stack"));
   MakeBIPred(aS("pro_local"), 2,         &BuiltIns::p_pro_local, true, aS("pro_local(TopV, PosV)"), aS("Returns current size, Top, and position, Pos, of the local stack"));
   MakeBIPred(aS("pro_trail"), 2,         &BuiltIns::p_pro_trail, true, aS("pro_trail(TopV, PosV)"), aS("Returns current size, Top, and position, Pos, of the trail stack"));
   MakeBIPred(aS("highwater"), 4,         &BuiltIns::p_highwater, true, aS("highwater(HeapV, LocalV, ControlV, TrailV)"), aS("Returns the highwater marks for these heaps and stacks"));
   MakeBIPred(aS("comm"), 1,              &BuiltIns::p_syscom, true, aS(""), aS(""));
   MakeBIPred(aS("command"), 1,           &BuiltIns::p_syscom, true, aS(""), aS(""));
   MakeBIPred(aS("system"), 1,            &BuiltIns::p_syscom, true, aS("system(CmdAS)"), aS("Executes an operating system program or command"));
   MakeBIPred(aS("opsys"), 1,             &BuiltIns::p_opsys, true, aS(""), aS(""));
   MakeBIPred(aS("build$env"), 1,         &BuiltIns::p_buildenv, false, NULL, NULL);
   MakeBIPred(aS("debug$pause"), 0,       &BuiltIns::p_debugZpause, false, NULL, NULL);
   MakeBIPred(aS("user$name"), 1,         &BuiltIns::p_userZname, false, NULL, NULL);

   MakeBIPred(aS("address$"), 2,          &BuiltIns::p_address, false, NULL, NULL);  // ray
   //MakeBIPred(aS("mode$"), 3,             &BuiltIns::p_mode, true, aS(""), aS(""));
   MakeBIPred(aS("version"), 1,           &BuiltIns::p_version, true, aS("version(VersionS)"), aS("Return current Amzi! version"));
   //MakeBIPred(aS("version$build"), 2,     &BuiltIns::p_version_build, true, aS(""), aS(""));
   //MakeBIPred(aS("version_build"), 2,     &BuiltIns::p_version_build, true, aS(""), aS(""));
   MakeBIPred(aS("version_build"), 3,     &BuiltIns::p_version_build, true, aS(""), aS(""));
   //MakeBIPred(aS("flag$value"), 2,        &BuiltIns::p_flagZvalue, true, aS(""), aS(""));
   //MakeBIPred(aS("set$flag$value"), 2,    &BuiltIns::p_set_flag_value, true, aS(""), aS(""));
   MakeBIPred(aS("prolog$flag"), 2,       &BuiltIns::p_prologZflag, false, NULL, NULL);
   MakeBIPred(aS("is_unicode"), 0,        &BuiltIns::p_is_unicode, true, aS(""), aS(""));
   MakeBIPred(aS("faulty$tower"), 0,      &BuiltIns::p_faultyZtower, true, aS(""), aS(""));
   MakeBIPred(aS("amzi_directory"), 1,    &BuiltIns::p_amzi_directory, true, aS(""), aS(""));

   // Module Predicates

   MakeBIPred(aS("module$"), 1,           &BuiltIns::p_moduleZ, true, aS(""), aS(""));
   MakeBIPred(aS("end_module$"), 1,       &BuiltIns::p_end_moduleZ, true, aS(""), aS(""));
   MakeBIPred(aS("current$module"), 1,    &BuiltIns::p_currentZmodule, false, NULL, NULL);
   //MakeBIPred(aS("loading$module"), 1,    &BuiltIns::p_loading_module, true, aS(""), aS(""));
   MakeBIPred(aS("loading_module"), 1,    &BuiltIns::p_loading_module, true, aS(""), aS(""));  // used by compiler
   MakeBIPred(aS("module$index"), 2,      &BuiltIns::p_moduleZindex, false, NULL, NULL);
   MakeBIPred(aS("get$modix"), 2,         &BuiltIns::p_getZmodix, false, NULL, NULL);
   MakeBIPred(aS("remove_module"), 1,     &BuiltIns::p_remove_module, true, aS(""), aS(""));

   MakeBIPred(aS("import"), 1,            &BuiltIns::p_import_1, true, aS("import(PredicatesTL)"), aS("Imports a specific Module:Functor/Arity or a list of module names"));
   MakeBIPred(aS("import$2"), 3,          &BuiltIns::p_importZ2, false, NULL, NULL);
   MakeBIPred(aS("import$mod"), 2,        &BuiltIns::p_importZmod, false, NULL, NULL);
   MakeBIPred(aS("export$"), 2,           &BuiltIns::p_exportZ, false, NULL, NULL);
   MakeBIPred(aS("set$meta"), 2,          &BuiltIns::p_setZmeta, false, NULL, NULL);
   MakeBIPred(aS("is$meta"), 3,           &BuiltIns::p_isZmeta, false, NULL, NULL);
   MakeBIPred(aS("set$discontiguous"), 2,        &BuiltIns::p_setZdiscontiguous, false, NULL, NULL);
   MakeBIPred(aS("set$sorted"), 2,        &BuiltIns::p_setZsorted, false, NULL, NULL);
   MakeBIPred(aS("set$indexed"), 1,     &BuiltIns::p_setZindexed, false, NULL, NULL);

   // Atom Table & Atom Predicates

   MakeBIPred(aS("atom_length"), 2,       &BuiltIns::p_atom_length, true, aS("atom_length(AtomA, LengthV)"), aS("Unifies the Length of Atom"));
   MakeBIPred(aS("atomlist_concat"), 2,   &BuiltIns::p_atomlist_concat, true, aS("atomlist_concat(AtomListL, AtomV)"), aS("Concatenates all of the atoms in AtomList to create a single atom, Atom"));
   MakeBIPred(aS("number_chars"), 2,      &BuiltIns::p_number_chars, true, aS("number_chars(Number, CharList)"), aS("Convert back and forth between a number and a list of characters"));
   MakeBIPred(aS("atom_chars"), 2,        &BuiltIns::p_atom_chars, true, aS("atom_chars(Atom, CharList)"), aS("Convert back and forth between an atom and a list of characters"));
   MakeBIPred(aS("atom_codes"), 2,        &BuiltIns::p_atom_codes, true, aS("atom_codes(AtomAV, CharListCV)"), aS("Convert back and forth between an atom and a list of characters"));
   MakeBIPred(aS("name"), 2,              &BuiltIns::p_atom_codes, true, aS("name(Atom, CharList)"), aS("Convert back and forth between an atom and a list of characters"));
   MakeBIPred(aS("atom_uplow"), 2,        &BuiltIns::p_atom_uplow, true, aS("atom_uplow(AtomUpperAV, AtomLowerAV)"), aS("Creates a new upper case atom from a lower and vice versa"));
   MakeBIPred(aS("op_"), 3,               &BuiltIns::p_op, false, NULL, NULL);

   // Dynamic Database Predicates

   MakeBIPred(aS("assert$"), 3,           &BuiltIns::p_assertZ, false, NULL, NULL);
   MakeBIPred(aS("retract$db"), 3,        &BuiltIns::p_retractZdb, false, NULL, NULL);
   MakeBIPred(aS("clause$db"), 5,         &BuiltIns::p_clauseZdb, false, NULL, NULL);
   MakeBIPred(aS("stack_sizes"), 4,       &BuiltIns::p_debug_data, true, aS(""), aS(""));

   MakeBIPred(aS("get$pred"), 4,           &BuiltIns::p_getZpred, false, NULL, NULL);
   MakeBIPred(aS("predicate$property"), 2, &BuiltIns::p_predicateZproperty, false, NULL, NULL);
   MakeBIPred(aS("predicate$enginfo"), 3, &BuiltIns::p_predicateZenginfo, false, NULL, NULL);
   MakeBIPred(aS("get$op"), 4,             &BuiltIns::p_getZop, false, NULL, NULL);

   MakeBIPred(aS("abolish$"), 1,          &BuiltIns::p_abolish, false, NULL, NULL);
   MakeBIPred(aS("is$code"), 2,           &BuiltIns::p_iscode, false, NULL, NULL);
   MakeBIPred(aS("is$code"), 4,           &BuiltIns::p_isZcode, false, NULL, NULL);
   MakeBIPred(aS("defined$"), 1,           &BuiltIns::p_definedZ, false, NULL, NULL);

   //MakeBIPred(aS("pro_db"), 2,            &BuiltIns::p_pro_db, true, aS(""), aS(""));

   // String and Thing Predicates

   MakeBIPred(aS("string_tokens"), 2,     &BuiltIns::p_string_tokens, true, aS("string_tokens(String, ListV)"), aS("Parses a string into a list of tokens"));
   MakeBIPred(aS("string_tokens"), 3,     &BuiltIns::p_string_tokens3, true, aS("string_tokens(String, ListV, DelimetersS)"), aS("Parses a string into a list of tokens delimited by the specified characters"));
   MakeBIPred(aS("string_list"), 2,       &BuiltIns::p_str_list, true, aS("string_list(String, CharList)"), aS("Converts between string and list of characters"));
   MakeBIPred(aS("string"), 1,            &BuiltIns::p_string, true, aS("string(X)"), aS("Succeeds if X is a string"));
   MakeBIPred(aS("string_length"), 2,     &BuiltIns::p_string_length, true, aS("string_length(String, Length)"), aS("Returns the length of a string"));
   MakeBIPred(aS("$substring"), 4,        &BuiltIns::p_substring, false, NULL, NULL);
   MakeBIPred(aS("sub$string"), 4,        &BuiltIns::p_subZstring, false, NULL, NULL);
   MakeBIPred(aS("strcat"), 3,            &BuiltIns::p_strcat, true, aS("strcat(S1, S2, S3)"), aS("Concatenates strings S1 and S2 to create S3; the first two arguments must be instantiated"));
   MakeBIPred(aS("stringlist_concat"), 2, &BuiltIns::p_stringlist_concat, true, aS("stringlist_concat(StringList, String)"), aS("Concatenates the strings and atoms in StringList to create the single String"));
   MakeBIPred(aS("string_split"), 3,      &BuiltIns::p_string_split, true, aS("string_split(String, DelmitersS, ListV)"), aS("Splits a string by the delimiters"));
   MakeBIPred(aS("string_icomp"), 2,      &BuiltIns::p_string_icomp, true, aS("string_icomp(String1, String2)"), aS("Case insensitive compare of two strings"));
   MakeBIPred(aS("read_term$"), 3,        &BuiltIns::p_read_term, false, NULL, NULL);
   MakeBIPred(aS("read_string_"), 2,      &BuiltIns::p_read_string, false, NULL, NULL);
   MakeBIPred(aS("string_term"), 2,       &BuiltIns::p_string_term, true, aS("string_term(String, Term)"), aS("Converts between strings and terms, throws error on invalid string"));
   MakeBIPred(aS("is_string_term"), 2,    &BuiltIns::p_is_string_term, true, aS("is_string_term(String, Term)"), aS("Converts between strings and terms, fails on invalid string"));
   MakeBIPred(aS("string_termq"), 2,      &BuiltIns::p_string_termq, true, aS("string_termq(String, Term)"), aS("Converts between strings and terms, but when going from term to string, it quotes atoms if necessary and puts strings in ` ` delimiters for rereading"));
   MakeBIPred(aS("string_termq"), 3,     &BuiltIns::p_string_termq3, true, aS("string_termq(String, Term, Len)"), aS("Converts between strings and terms, but when going from term to string, it quotes when necessary and truncates at length"));
   MakeBIPred(aS("string_trim"), 2,       &BuiltIns::p_string_trim, true, aS("string_trim(String, StringV)"), aS("Trims leading and trailing whitespace"));
   MakeBIPred(aS("nonblank_string"), 1,   &BuiltIns::p_nonblank_string, true, aS("nonblank_string(String)"), aS("Succeeds if String is not empty"));
   MakeBIPred(aS("string_atom"), 2,       &BuiltIns::p_string_atom, true, aS("string_atom(String, Atom)"), aS("Convert between String and Atom name"));
   MakeBIPred(aS("tilt_slashes"), 2,      &BuiltIns::p_tilt_slashes, true, aS("tilt_slashes(InAS, OutAS)"), aS("Convert back slashes to forward slashes for either an atom or string"));

	// Arith predicates
	
   MakeBIPred(aS("fixed_list"), 2,        &BuiltIns::p_fixed_list, true, aS(""), aS(""));
   MakeBIPred(aS("arith_plus"), 3,        &BuiltIns::p_arith_plus, true, aS(""), aS(""));
//   MakeBIPred(aS("arith$plus"), 3,        &BuiltIns::p_arith_plus, true, aS(""), aS(""));
   MakeBIPred(aS("cntr$"), 3,             &BuiltIns::p_cntr, false, NULL, NULL);
   MakeBIPred(aS("for$"), 4,              &BuiltIns::p_for, false, NULL, NULL);
   MakeBIPred(aS("=:="), 2,               &BuiltIns::p_numeq, true, aS("ArithExp1 =:= ArithExp2"), aS("Succeeds if evaluation of ArithExp1 and ArithExp2 are the same"));
   MakeBIPred(aS("~="), 2,                &BuiltIns::p_almost_equal, true, aS("ArithExp1 ~= ArithExp2"), aS("Succeeds if evaluation of ArithExp1 is almost equal to ArithExp2; useful for non-integer values"));
   MakeBIPred(aS(">"), 2,                 &BuiltIns::p_gt, true, aS("ArithExp1 > ArithExp2"), aS("Succeeds if evaluation of ArithExp1 greater than evaluation of ArithExp2"));
   MakeBIPred(aS("<"), 2,                 &BuiltIns::p_lt, true, aS("ArithExp1 < ArithExp2"), aS("Evaluates both arithmetic expressions, succeeds if second greater than first"));
   MakeBIPred(aS("is"), 2,                &BuiltIns::p_is, true, aS("Number is ArithExp"), aS("Evaluate ArithExp and unify with Number"));
   MakeBIPred(aS("nth$"), 3,              &BuiltIns::p_nth, false, NULL, NULL);
   MakeBIPred(aS("newReal"), 3,           &BuiltIns::p_newReal, true, aS(""), aS(""));
   //MakeBIPred(aS("realDescr"), 5,         &BuiltIns::p_realDescr, true, aS(""), aS(""));
   MakeBIPred(aS("divrem"), 4,            &BuiltIns::p_divrem, true, aS(""), aS(""));
   MakeBIPred(aS("divmodu"), 4,           &BuiltIns::p_divmodu, true, aS(""), aS(""));
   MakeBIPred(aS("divmods"), 4,           &BuiltIns::p_divmods, true, aS(""), aS(""));

	// Primes predicates
   MakeBIPred(aS("makePrimes"), 4,        &BuiltIns::p_makePrimes, true, aS(""), aS(""));
   MakeBIPred(aS("primes"), 1,            &BuiltIns::p_Primes, true, aS(""), aS(""));
   MakeBIPred(aS("nthPrime"), 2,          &BuiltIns::p_nthPrime, true, aS(""), aS(""));

   // Number Types
   MakeBIPred(aS("integer"), 1,           &BuiltIns::p_integer, true, aS("integer(X)"), aS("Succeeds if X is an integer"));
   //MakeBIPred(aS("long"), 1,              &BuiltIns::p_integer, true, aS(""), aS(""));
   //MakeBIPred(aS("short"), 1,             &BuiltIns::p_short, true, aS(""), aS(""));
   //MakeBIPred(aS("fixed"), 1,             &BuiltIns::p_fixed, true, aS(""), aS(""));
   MakeBIPred(aS("real"), 1,              &BuiltIns::p_real, true, aS("real(X)"), aS("Succeeds if X is of numeric type 'real', either fixed or long"));
   //MakeBIPred(aS("decimal"), 1,           &BuiltIns::p_decimal, true, aS(""), aS(""));
   MakeBIPred(aS("fixed_real"), 1,        &BuiltIns::p_fixed_real, true, aS("fixed_real(X)"), aS("Succeeds if X is of numeric type 'fixed_real'"));
   MakeBIPred(aS("long_real"), 1,         &BuiltIns::p_long_real, true, aS("long_real(X)"), aS("Succeeds if X is of numeric type 'long_real"));
   //MakeBIPred(aS("real_real"), 1,         &BuiltIns::p_decimal, true, aS(""), aS(""));
   MakeBIPred(aS("single_float"), 1,      &BuiltIns::p_single, true, aS("single_float(X)"), aS("Succeeds if X is of numeric type 'single_float'"));
   MakeBIPred(aS("double_float"), 1,      &BuiltIns::p_double, true, aS("double_float(X)"), aS("Succeeds if X is of numeric type 'double_float'"));
   MakeBIPred(aS("float"), 1,             &BuiltIns::p_float, true, aS("float(X)"), aS("Succeeds if X is a float"));
   MakeBIPred(aS("real_components"), 5,   &BuiltIns::p_real_components, true, aS(""), aS(""));

   // Term Classification Predicates
   MakeBIPred(aS("arg$"), 3,              &BuiltIns::p_arg, false, NULL, NULL);
   MakeBIPred(aS("atom"), 1,              &BuiltIns::p_atom, true, aS("atom(X)"), aS("Succeeds if X is an atom"));
   // bigdig MakeBIPred(aS("atomic$"), 1,            &BuiltIns::p_atomic, true, aS(""), aS(""));
   //MakeBIPred(aS("dbref"), 1,             &BuiltIns::p_dbref, true, aS(""), aS(""));
   MakeBIPred(aS("double"), 1,            &BuiltIns::p_float, true, aS(""), aS(""));
   MakeBIPred(aS("functor"), 3,           &BuiltIns::p_functor, true, aS("functor(TermV, FunctorAV, ArityNV)"), aS("Split a term into its functor and arity, or build a term from a functor and arity"));
   MakeBIPred(aS("is_fraction"), 1,       &BuiltIns::p_fraction, true, aS("is_fraction(X)"), aS("Succeeds if X is mathematically a fraction"));
   MakeBIPred(aS("is_integer"), 1 ,       &BuiltIns::p_is_integer, true, aS("integer(X)"), aS("Succeeds if X is mathematically an integer, so both 3 and 3.0 succeed as arguments"));
   MakeBIPred(aS("is_odd"), 1 ,           &BuiltIns::p_is_odd, true, aS("is_odd(X)"), aS("Succeeds if X is mathematically an odd number"));
   MakeBIPred(aS("epsilon"), 1 ,          &BuiltIns::p_epsilon, true, aS(""), aS(""));

   MakeBIPred(aS("char"), 1,              &BuiltIns::p_char, true, aS("char(X)"), aS("Succeeds if X is a char"));
   //MakeBIPred(aS("cmpt$"), 3,             &BuiltIns::p_lexord, true, aS(""), aS(""));
   MakeBIPred(aS("less$"), 2,             &BuiltIns::p_lessZ, false, NULL, NULL);
   MakeBIPred(aS("copy_term"), 2,         &BuiltIns::p_copy_term, true, aS("copy_term(OldTerm, NewTerm)"), aS("Makes a copy of OldTerm with new variables in NewTerm"));

   //MakeBIPred(aS("member$v"), 2,    &BuiltIns::p_memberv, true, aS(""), aS("")); // internal compiler

   MakeBIPred(aS("is_member"), 2,         &BuiltIns::p_memberv, true, aS("is_member(Term, List)"), aS("Tests if Term is a member of List"));  // external
   MakeBIPred(aS("\\=="), 2,              &BuiltIns::p_not_strong_unify, true, aS("Term1 \\== Term2"), aS("Succeeds if Term1 not identical with Term2, meaning the terms cannot be unified without requiring variable bindings"));
   MakeBIPred(aS("number"), 1,            &BuiltIns::p_number, true, aS("number(X)"), aS("Succeeds if X is an integer or floating point number"));
   MakeBIPred(aS("=="), 2,                &BuiltIns::p_strong_unify, true, aS("Term1 == Term2"), aS("Succeeds if Term1 and Term2 are identical, meaning they unify without requiring variable bindings"));
   MakeBIPred(aS("=.."), 2,               &BuiltIns::p_univ, true, aS("Term =.. List"), aS("Convert a term into a list whose head is the functor and tail is a list of the arguments, or take a list and reverse the process to create a term (univ)"));
   MakeBIPred(aS("varsof"), 2,            &BuiltIns::p_varsof, true, aS("varsof(Term, List)"), aS("Unify List with a list of all uninstantiated variables in Term"));
   MakeBIPred(aS("gen$sym"), 2,           &BuiltIns::p_gensym, false, NULL, NULL);
   MakeBIPred(aS("is_cyclic"), 1,         &BuiltIns::p_is_cyclic, true, aS("is_cyclic(Term)"), aS("Succeeds if Term is a cyclic term"));
   MakeBIPred(aS("ground"), 1,            &BuiltIns::p_ground, true, aS("ground(Term)"), aS("Succeeds if Term nas no unbound variables"));
//#ifdef REALS
   MakeBIPred(aS("truncate"), 2,          &BuiltIns::p_truncate, true, aS(""), aS(""));
   MakeBIPred(aS("integer_real"), 2,      &BuiltIns::p_int_real, true, aS(""), aS(""));
   MakeBIPred(aS("float_real"), 2,        &BuiltIns::p_float_real, true, aS(""), aS(""));
   MakeBIPred(aS("real_list"), 2,         &BuiltIns::p_real_list, true, aS(""), aS(""));
//#endif	
	
   // Stashing Predicates

   MakeBIPred(aS("get$"), 2,              &BuiltIns::p_get_term, true, aS(""), aS(""));
   MakeBIPred(aS("peek$"), 2,             &BuiltIns::p_peek_term, true, aS(""), aS(""));
   // bigdig MakeBIPred(aS("poke$"), 2,             &BuiltIns::p_poke_term, true, aS(""), aS(""));
   MakeBIPred(aS("reserve$"), 1,          &BuiltIns::p_reserve_heap, true, aS(""), aS(""));
   MakeBIPred(aS("stash$"), 2,            &BuiltIns::p_stash_term, true, aS(""), aS(""));
   MakeBIPred(aS("stash$free"), 1,        &BuiltIns::p_stash_free, true, aS(""), aS(""));

   // Term Reading and Writing Predicates

   MakeBIPred(aS("varlist"), 1,           &BuiltIns::p_varlist, true, aS("varlist(List)"), aS("A list of the variable names, each as a character list, when the predicate was called"));
   //MakeBIPred(aS("read_"), 2,             &BuiltIns::p_read, true, aS(""), aS(""));
   //MakeBIPred(aS("write_"), 3,            &BuiltIns::p_write1, true, aS(""), aS(""));
   MakeBIPred(aS("write1_"), 3,           &BuiltIns::p_write, false, NULL, NULL);
   MakeBIPred(aS("write_"), 3,           &BuiltIns::p_write, false, NULL, NULL);
#ifdef LANDFILL
   MakeBIPred(aS("land$fill"), 1,         &BuiltIns::p_landZfill, true, aS(""), aS(""));
   MakeBIPred(aS("log$term"), 1,          &BuiltIns::p_logZterm, true, aS(""), aS(""));
#endif

   // Stream I/O Predicates
#ifdef STREAMS_H
   MakeBIPred(aS("current_input"), 1,     &BuiltIns::p_cur_input, true, aS("current_input(IdV)"), aS("Unifies Id with the current input stream"));
   MakeBIPred(aS("current_output"), 1,    &BuiltIns::p_cur_output, true, aS("current_output(IdV"), aS("Unifies Id with the current output stream"));
   MakeBIPred(aS("set_input"), 1,         &BuiltIns::p_set_input, true, aS("set_input(IdAN)"), aS("Sets the current input stream to ID"));
   MakeBIPred(aS("set_output"), 1,        &BuiltIns::p_set_output, true, aS("set_output(IdAN)"), aS("Sets the current output stream to ID"));
   MakeBIPred(aS("set_stream_position"), 2, &BuiltIns::p_set_streampos, true, aS("set_stream_position(IdAN, Position)"), aS("First call to get a Position; second call to reset the stream to that position"));
   MakeBIPred(aS("at_end_of_stream"), 1,  &BuiltIns::p_at_eos, true, aS("at_end_of_stream(ID)"), aS("Succeeds if the end_of_file has been reached for stream ID"));
#endif
	/*
   //MakeBIPred(aS("sopen_"), 3,            &BuiltIns::p_sopen, true, aS(""), aS(""));
	*/
	// funny stuff
   //MakeBIPred(aS("sclose_"), 1,           &BuiltIns::p_sclose, true, aS(""), aS(""));
   MakeBIPred(aS("fread"), 3,             &BuiltIns::p_fread, true, aS("fread(ID, ValueV, TypeN {0=one byte, 1=two-byte integer, 2=four-byte float, 3=four-byte integer})"), aS("Read direct from file identified by ID a Value of length specified by Type"));
   MakeBIPred(aS("fseek"), 4,             &BuiltIns::p_fseek, true, aS("fseek(ID, OffsetN, MethodN{0=start,1=current,2-end NewOff)"), aS("Reposition file identified by ID according to Offset and Method. Unify NewOff with new position"));

   MakeBIPred(aS("stream$list"), 1,       &BuiltIns::p_streamList, false, NULL, NULL);
   MakeBIPred(aS("stream$property"), 2,   &BuiltIns::p_stream_props, false, NULL, NULL);
   MakeBIPred(aS("current_streams"), 3,   &BuiltIns::p_cur_streams, true, aS("current_streams(InputIdV, OutputIdV, ErrorIdV)"), aS("For each bound argument, sets the appropriate current stream, for unbound arguments, unifies the argument with the appropriate current stream"));
   MakeBIPred(aS("current_user"), 3,      &BuiltIns::p_cur_user, true, aS("current_user(InputIdV, OutputIdV, ErrorIdV)"), aS("For each bound argument, sets the appropriate user (default) stream, for unbound arguments, unifies the argument with the appropriate user (default) stream"));
   //MakeBIPred(aS("my_current_user"), 3,      &BuiltIns::p_cur_user, true, aS(""), aS(""));
   //MakeBIPred(aS("fclose_"), 1,           &BuiltIns::p_close, true, aS(""), aS(""));
   //MakeBIPred(aS("fopen_"), 3,            &BuiltIns::p_fopen, true, aS(""), aS(""));
   //MakeBIPred(aS("fleopen"), 3,           &BuiltIns::p_fleopen, true, aS(""), aS(""));

   MakeBIPred(aS("close$"), 1,            &BuiltIns::p_close, false, NULL, NULL);
   MakeBIPred(aS("open$"), 4,             &BuiltIns::p_open, false, NULL, NULL);
   //MakeBIPred(aS("see$"), 1,              &BuiltIns::p_see, true, aS(""), aS(""));
   //MakeBIPred(aS("tell$"), 1,              &BuiltIns::p_tell, true, aS(""), aS(""));

   MakeBIPred(aS("fflush"), 1,            &BuiltIns::p_fflush, true, aS("fflush(ID)"), aS("Flush the I/O to file identified with ID "));
	/*
   MakeBIPred(aS("fwrite"), 3,            &BuiltIns::p_fwrite, true, aS(""), aS(""));
	*/
   MakeBIPred(aS("flewrite"), 3,          &BuiltIns::p_flewrite, true, aS(""), aS(""));
   MakeBIPred(aS("get0_"), 2,             &BuiltIns::p_geto, false, NULL, NULL);
   //MakeBIPred(aS("getcode_"), 2,          &BuiltIns::p_getcodeo, true, aS(""), aS(""));

	//   MakeBIPred(aS("unget0_"), 2,           &BuiltIns::p_ungeto, true, aS(""), aS(""));
   MakeBIPred(aS("handle_name"), 2,       &BuiltIns::p_handle_name, true, aS("handle_name(HandleNV, NameAV)"), aS("Find either a streams Name or Handle from the other"));
   //MakeBIPred(aS("stream_type"), 2,       &BuiltIns::p_stream_type, true, aS(""), aS(""));
   MakeBIPred(aS("put"), 3,               &BuiltIns::p_putc, true, aS(""), aS(""));
   MakeBIPred(aS("putc"), 3,              &BuiltIns::p_putc, true, aS(""), aS(""));

   MakeBIPred(aS("key$b"), 1,             &BuiltIns::p_keyZb, true, aS(""), aS(""));
   MakeBIPred(aS("read_binary"), 3,       &BuiltIns::p_read_binary, true, aS("read_binary(ID, TypeA {char, wide_char, short_integer, integer, single_float, double_float}, DataV)"), aS("Reads a binary quantity, of type Type, from stream ID and unifies the read quantity with Data"));
   MakeBIPred(aS("write_binary"), 3,      &BuiltIns::p_write_binary, true, aS("write_binary(ID, TypeA {char, wide_char, short_integer, integer, single_float, double_float}, DataN)"), aS("Writes a binary quantity, Data, of type Type, to stream ID"));

	//   MakeBIPred(aS("stream_attrs"), 1,      &BuiltIns::p_stream_attrs, true, aS(""), aS(""));

   // Load & Code Predicates

   MakeBIPred(aS("load$ops"), 1,          &BuiltIns::p_loadZops, false, NULL, NULL);
   MakeBIPred(aS("load_code$"), 1,        &BuiltIns::p_loadZfile, false, NULL, NULL);
   MakeBIPred(aS("load$file"), 1,         &BuiltIns::p_loadZfile, false, NULL, NULL);
   MakeBIPred(aS("load$memory"), 3,       &BuiltIns::p_loadZmemory, false, NULL, NULL);
   MakeBIPred(aS("unload_code$"), 1,      &BuiltIns::p_unload, false, NULL, NULL);
   MakeBIPred(aS("is$loaded"), 1,         &BuiltIns::p_is_loaded, false, NULL, NULL);
   MakeBIPred(aS("loadlsx"), 1,           &BuiltIns::p_loadlsx, true, aS("loadlsx(FilenameA)"), aS("Dynamically loads a logic server extension from Filename at runtime from a Prolog component"));
   MakeBIPred(aS("ensure_loaded"), 1,     &BuiltIns::p_ensure_loaded, true, aS("ensure_loaded(FilenameA"), aS("Ensure that a plm file has been loaded; if not, loads it"));

   MakeBIPred(aS("cut$env"), 1,           &BuiltIns::p_cut_env, false, NULL, NULL);
   MakeBIPred(aS("cut$debug64$env"), 1,   &BuiltIns::p_cut_debug64_env, false, NULL, NULL);
   MakeBIPred(aS("get$cpenv"), 2,         &BuiltIns::p_get_env, false, NULL, NULL);
   MakeBIPred(aS("get$env"), 1,           &BuiltIns::p_get_env1, false, NULL, NULL);
   MakeBIPred(aS("save$stack"), 1,        &BuiltIns::p_saveZstack, true, aS(""), aS(""));

   MakeBIPred(aS("cut_tag"), 1,           &BuiltIns::p_cuttag, true, aS(""), aS(""));
   MakeBIPred(aS("cut$tag"), 1,           &BuiltIns::p_cutZtag, false, NULL, NULL);
   MakeBIPred(aS("de$tag"), 1,            &BuiltIns::p_deZtag, false, NULL, NULL);
   MakeBIPred(aS("re$tag"), 1,            &BuiltIns::p_reZtag, false, NULL, NULL);
   MakeBIPred(aS("tag"), 1,               &BuiltIns::p_tag, true, aS(""), aS(""));

//   MakeBIPred(aS("spy$"), 4,              &BuiltIns::p_spy, true, aS(""), aS(""));

   // Error Handling Predicates

   MakeBIPred(aS("err_read"), 2,          &BuiltIns::p_err_read, true, aS(""), aS(""));
   MakeBIPred(aS("err$exec"), 4,          &BuiltIns::p_errZexec, false, NULL, NULL);
   MakeBIPred(aS("err$fatal"), 0,         &BuiltIns::p_err_fatal, false, NULL, NULL);
   MakeBIPred(aS("err$abort"), 0,         &BuiltIns::p_err_abort, false, NULL, NULL);

   MakeBIPred(aS("debug$stack"), 1,       &BuiltIns::p_debugZstack, true, aS(""), aS(""));

#ifdef THREADED_EXEC_STR
   MakeBIPred(aS("get_async_action"), 1,  &BuiltIns::p_get_async_action, true, aS("get_async_action(ActionStr)"), aS("Get an asynchronous action from user, use with lsExecStrTh()"));
   MakeBIPred(aS("set_event"), 1,         &BuiltIns::p_set_event, true, aS("set_event(EventNameStr)"), aS("Set an event"));
#endif

// LANDFILL Predicates
#ifdef LANDFILL
   MakeBIPred(aS("dump$db"), 0,           &BuiltIns::p_dumpdb, true, aS(""), aS(""));
   MakeBIPred(aS("dump$control"), 0,      &BuiltIns::p_dumpZcontrol, true, aS(""), aS(""));
   MakeBIPred(aS("dump$hxl"), 0,          &BuiltIns::p_dumpZhxl, true, aS(""), aS(""));
   MakeBIPred(aS("debug$break"), 0,       &BuiltIns::p_debugZbreak, true, aS(""), aS(""));
	//   MakeBIPred(aS("macnames"), 0,          &BuiltIns::p_macNames, true, aS(""), aS(""));
	//   MakeBIPred(aS("mac$histo"), 0,         &BuiltIns::p_macHisto, true, aS(""), aS(""));
#ifdef BUG_LEAK
   MakeBIPred(aS("leak$report"), 1,       &BuiltIns::p_leakZreport, true, aS(""), aS(""));
#endif
#endif

   // Other Initializations

	
   time(&StartTime);   // Starting time for timer predicates
   //pDDB->Dump();
}

void BuiltIns::MakeBIPred(STRptr name, ARITY ar, pBIP bip, bool b_export, STRptr args, STRptr desc)
{
   PATOM          a;
   //CLAUSE_BLKptr  ci;

   a = pATAB->EnterAtom(name);
   pDDB->EnterBIP(a, ar, bip, b_export, args, desc);
}


TF BuiltIns::p_gc()
{
   pHXL->GC(0);
   pDDB->GC();
   pGCTH->gc_things();
   return TRUE;
}

TF BuiltIns::p_abort()
/*
   abort(0)  --> call error and reset
   abort(1)  --> clean exit
   abort(2)  --> dirty exit
   3 - break
*/
{
   TERM t;

   t = (pHXL->XVar(0))->dref();

   if ( ! t->IsInt() )
      pXCPT->Error(sysargE);

   switch (t->getInt())
   {
   case 0:
      //pEXEC->Reset();
      //pHXL->Reset();
      //longjmp(jmpstack[jmpi], USER_RESET);
      pXCPT->Error(userfatalE);

   case 1:
      // Is this necessary?
      //pSIO->ioCloseStreams();
      //globResetBrk();
      //longjmp(jmpstack[jmpi], USER_EXIT);
      pXCPT->Error(userexitE);    

   case 2:
      // Is this necessary?
      //pSIO->ioCloseStreams();
      //globResetBrk();
      //longjmp(jmpstack[jmpi], USER_ABORT);
      pXCPT->Error(userabortE);
   case 3:
      pXCPT->Error(breakE);
   }

   return(TRUE);                                 // stop compiler warnings 
}


TF BuiltIns::p_commandl()
{                                                // get command line arguments 
#ifndef WIN3
   TERM    t, X0;
   int    i, start;
   //CELL  NilCell = {0L};

   start = 1;

   X0 = t = pHXL->heapGET();
   for(i = start; i < pSTATE->m_argc; ++i)       // first build skeleton list 
   {
      //pTSVC->LValue(t) = (CELL) pHXL->heapGET() | (CELL) listT;      
      // bigdig pTSVC->putVAL( t, (CELL) pHXL->heapGET() | (CELL) listT );      
      t->setList(pHXL->heapGET());      
      t = pHXL->heapGET();                       // cons cell 
   }
   //pTSVC->LValue(t) = NilCell;
   // bigdig pTSVC->putVAL(t, pATAB->NilCell);
   //t->setNil();
   *t = pATAB->NilCell;

   t = X0 + 1;                                   // first cons cell 
   for(i = start; i < pSTATE->m_argc; ++i)
   {
      *t = *pHXL->StrToCharList(pSTATE->m_argv[i]);
      // bigdig t = t+1->getTerm();
      t = (t+1)->getTerm();
   }
   return(pTSVC->Unify(X0, pHXL->XVar(0)));
#else
   return(FALSE);
#endif
}

TF BuiltIns::p_srand()
{
   TERM t;

   t = (pHXL->XVar(0))->dref();
   if (! t->IsInt())
      pXCPT->Error(sysargE);

   srand( (unsigned int) t->getInt() );
   return TRUE;
}

TF BuiltIns::p_timer()
{                                   // system time equivalent to X is cputime. 
   TERM    t;
   double  lapsetime;
   Cell    c;

// MS doesn't support clock() for DLLs in the 16-bit world.
#if defined(P16) && defined(LIB_DLL)
   time_t now;
   time(&now);
   lapsetime = (short) (difftime(now,StartTime) / 10);
#else
   lapsetime = (double) (clock() / (double)CLOCKS_PER_SEC);
#endif

   t = (pHXL->XVar(0))->dref();

   // bigdig pTSVC->putFLOAT(&c, lapsetime);
   c.setDouble(pGCTH->make_float(lapsetime));
   if (t->IsRef())
      pTSVC->Unify(&c, pHXL->XVar(0));
   else
      pXCPT->Error(sysargE);

   return(TRUE);
}

TF BuiltIns::p_time()
{                                   // time(Hr, Min, Secs) built-in predicate 
   time_t     t;
   struct tm *st;
   short      i, xargs[3];
   TERM       targ;

   time(&t);
   st = localtime(&t);

   xargs[0] = (short) st->tm_hour;
   xargs[1] = (short) st->tm_min;
   xargs[2] = (short) st->tm_sec;
   for (i = 0; i <= 2; i++)
   {
      targ = (pHXL->XVar(i))->dref();
      if (targ->IsInt())
      {
         if (targ->getInt() != xargs[i])
            return(FALSE);
      }
      else if (targ->IsRef())
         pTSVC->UnifyInt(xargs[i], pHXL->XVar(i));
      else
         pXCPT->Error(sysargE);
   }

   return(TRUE);
}

TF BuiltIns::p_time4()
/*   time(Hr, Min, Secs, Hundredths) kept for compatibility
   with earlier release.  time/3 should be used instead.
   Hundredths are simply set to zero, timing of partial
   seconds should be done with timer or cputim. */
{
   time_t      t;
   struct tm *   st;
   short         i, xargs[3];
   TERM         targ;

   time(&t);
   st = localtime(&t);

   xargs[0] = (short) st->tm_hour;
   xargs[1] = (short) st->tm_min;
   xargs[2] = (short) st->tm_sec;
   for (i = 0; i <= 2; i++)
   {
      targ = (pHXL->XVar(i))->dref();
      if (targ->IsInt())
      {
         if (targ->getInt() != xargs[i])
            return(FALSE);
      }
      else if (targ->IsRef())
         pTSVC->UnifyInt(xargs[i], pHXL->XVar(i));
      else
         pXCPT->Error(sysargE);
   }

   pTSVC->UnifyInt(0, pHXL->XVar(3));
   return(TRUE);
}

TF BuiltIns::p_date()
{                             // date(Month, Day, Year) built-in predicate 
   time_t      t;
   struct tm *   st;
   short         i, xargs[3];
   TERM         targ;

   time(&t);
   st = localtime(&t);

   xargs[0] = (short) st->tm_mon + 1;
   xargs[1] = (short) st->tm_mday;
   xargs[2] = (short) st->tm_year + 1900;

   for (i = 0; i <= 2; i++)
   {
      targ = (pHXL->XVar(i))->dref();
      if (targ->IsInt())
      {
         if (targ->getInt() != xargs[i])
            return(FALSE);
      }
      else if (targ->IsRef())
         pTSVC->UnifyInt(xargs[i], pHXL->XVar(i));
      else
         pXCPT->Error(sysargE);
   }

   return(TRUE);
}

TF BuiltIns::p_openlog()
{                                         // open a file for logging a session 
   if (pENG->m_logging)
      return TRUE;

   TERM fname = X(0);
   if (! fname->IsAtom()) 
      pXCPT->Error(sysargE);

   char *aname = g_lenv.new_mbs_dup(*(fname->getAtom()));
   LNEW(pENG->m_log, std::ofstream, aS("logfile"));
   pENG->m_log->open(aname);
   delete aname;
   *(pENG->m_log) <<
      LString(aS("Amzi! Log File  ")) + LDateString() + 
      aS("  ") + LTimeString() + aS("\n\n");
   pENG->m_log->setf(std::ios::showbase);
   pENG->m_logging = true;

   return TRUE;
}

TF BuiltIns::p_closelog()
{
   if (pENG->m_logging && g_sync_file == NULL)
   {
      pENG->m_log->close();
      pENG->m_logging = false;
      delete pENG->m_log;
   }
   return TRUE;
}

TF BuiltIns::p_writelog()
{                                             // write a term just to the log 
   if (!pENG->m_logging)
      return TRUE;

   aCHAR buf[2001];
   pWRIT->termWriteString(X(0), buf, 2000, true);
   *(pENG->m_log) << buf << FLUSH;
   return TRUE;
}

TF BuiltIns::p_nllog()
{                                             // put a nl to the log 
   if (pENG->m_logging)
      *(pENG->m_log) << NL << FLUSH;
   return TRUE;
}

TF BuiltIns::p_plmtraceon()
{                                             // turn on trace of compiled code
   //if (pLOG->Logging()) pEXEC->SetPLMTraceB(TRUE);
   return(TRUE);
}

TF BuiltIns::p_plmtraceoff()
{                                            // turn off trace of compiled code
   pEXEC->SetPLMTraceB(FALSE);           
   return(TRUE);
}

/* The stack size predicates. 
 * Each returns the max, the current used and the percentage 
 */
TF BuiltIns::p_pro_heap()
{
   if (! (pHXL->XVar(0))->dref()->IsRef() ) pXCPT->Error(sysargE);
   if (! (pHXL->XVar(1))->dref()->IsRef() ) pXCPT->Error(sysargE);

   //s = pHXL->heapSIZE();
   //i = pHXL->heapUSE();

   pTSVC->UnifyInt(pHXL->heapSIZE(), pHXL->XVar(0));
   pTSVC->UnifyInt(pHXL->heapUSE(), pHXL->XVar(1));

   return(TRUE);
}

TF BuiltIns::p_pro_stack()
{
   intC s, i;

   if (! (pHXL->XVar(0))->dref()->IsRef() ) pXCPT->Error(sysargE);
   if (! (pHXL->XVar(1))->dref()->IsRef() ) pXCPT->Error(sysargE);

   //s = StackTop - Stack;
   //i = (STACK_ELMptr) E - Stack;

   s = pCNTL->controlSIZE();
   i = pCNTL->controlUSE();

   pTSVC->UnifyInt(s, pHXL->XVar(0));
   pTSVC->UnifyInt(i, pHXL->XVar(1));

   return(TRUE);
}

TF BuiltIns::p_pro_local()
{
   intC s, i;

   if (! (pHXL->XVar(0))->dref()->IsRef() ) pXCPT->Error(sysargE);
   if (! (pHXL->XVar(1))->dref()->IsRef() ) pXCPT->Error(sysargE);

   s = pHXL->localSIZE();
   i = pHXL->localUSE();

   pTSVC->UnifyInt(s, pHXL->XVar(0));
   pTSVC->UnifyInt(i, pHXL->XVar(1));

   return(TRUE);
}

TF BuiltIns::p_pro_trail()
{
   intC      s, i;

   if (! (pHXL->XVar(0))->dref()->IsRef()) pXCPT->Error(sysargE);
   if (! (pHXL->XVar(1))->dref()->IsRef()) pXCPT->Error(sysargE);

   s = pCNTL->trailSIZE();
   i = pCNTL->trailUSE();

   pTSVC->UnifyInt(s, pHXL->XVar(0));
   pTSVC->UnifyInt(i, pHXL->XVar(1));

   return(TRUE);
}

TF BuiltIns::p_highwater()
{
   if (! (pHXL->XVar(0))->dref()->IsRef())
      pXCPT->Error(instanceE, aS("arg1 must be uninstantiated"));
   if (! (pHXL->XVar(1))->dref()->IsRef())
      pXCPT->Error(instanceE, aS("arg2 must be uninstantiated"));
   if (! (pHXL->XVar(2))->dref()->IsRef())
      pXCPT->Error(instanceE, aS("arg3 must be uninstantiated"));
   if (! (pHXL->XVar(3))->dref()->IsRef())
      pXCPT->Error(instanceE, aS("arg4 must be uninstantiated"));

   pTSVC->UnifyInt(pHXL->GetHeapHW(), pHXL->XVar(0));
   //pTSVC->UnifyInt(8, pHXL->XVar(0));
   pTSVC->UnifyInt(pHXL->GetLocalHW(), pHXL->XVar(1));
   pTSVC->UnifyInt(pCNTL->GetStackHW(), pHXL->XVar(2));
   pTSVC->UnifyInt(pCNTL->GetTrailHW(), pHXL->XVar(3));

   return TRUE;
}

TF BuiltIns::p_syscom()
{                                                  /* system/0  */

   aCHAR    command[_MAX_PATH];
   TERM     t;
   int      rc;
   PATOM      a;
   STRptr     s;

   t = X(0);
   if (t->IsAtom())
     {
       a = t->getAtom();
       s = *(a);
       if (Lstrlen(s) >= _MAX_PATH)
         pXCPT->Error(pathlenE);
       Lstrcpy(command, s);
     }
   else if (t->IsStr())
     {
       s = t->getStr();
       if (Lstrlen(s) >= _MAX_PATH)
         pXCPT->Error(pathlenE);
       Lstrcpy(command, s);
     }
   else if (t->IsList())
     pHXL->CharListToStr(t, command);
   else
     pXCPT->Error(argE, aS("file name must be an atom, string or char list"));
   //pIO->ioGetName(t, command);

#ifdef _UNICODE
   if (g_lenv.getOSVer() == W95)
   {
      int len = (int)Lstrlen(command)+1;
      char* acommand;
      LNEW(acommand, char[len*2], aS("temporary"));  // delete below
      wcstombs(acommand, command, len*2);
      rc = system(acommand);
      delete acommand;
   }
   else
      rc = Lsystem(command);
#else
   rc = Lsystem(command);
#endif

   if (0 == rc)
      return TRUE;
   else
      return FALSE;
}

TF BuiltIns::p_amzi_directory()
{
   Cell  c;
   aCHAR *ad = g_lenv.amzi_directory();
   c.setStr( pGCTH->make_string(ad) );
   delete ad;
   return(pTSVC->Unify(&c,pHXL->XVar(0)));
}

TF BuiltIns::p_opsys()
{
   PATOM   a;
   Cell    c;

   LString os(OPSYS);
   a = pATAB->EnterAtom(os);
   c.setAtom(a);
//   c.setModule(SYSTEM_MODULE);
   return(pTSVC->Unify(&c, pHXL->XVar(0)));
}

TF BuiltIns::p_buildenv()
{
   PATOM   a;
   Cell    c;

#if defined(ENVmw3)
   aCHAR  env[] = aS("mw3");
#elif defined(ENVgl3)
   aCHAR  env[] = aS("gl3");
#elif defined(ENVgb3)
   aCHAR  env[] = aS("gb3");
#elif defined(ENVgh3)
   aCHAR  env[] = aS("gh3");
#elif defined(ENVss3)
   aCHAR  env[] = aS("ss3");
#else
   aCHAR  env[] = aS("none");
#endif

   a = pATAB->EnterAtom(env);
   c.setAtom(a);
//   c.setModule(SYSTEM_MODULE);
   return(pTSVC->Unify(&c, pHXL->XVar(0)));
}

TF BuiltIns::p_userZname()
{
   Cell  c;

   c.setStr( pGCTH->make_string(g_lenv.get_user_name()) );
   return(pTSVC->Unify(&c,pHXL->XVar(0)));

/*   // this experiment failed because of incompatible security
   // dlls, wonder why?  Add Secur32.lib to project properties
   // and uncomment inc.h security includes to try again.

   aCHAR cid[512];
   ULONG len=511;

   bool rc = GetComputerObjectName(NameDisplay, cid, &len);
   if (rc)
   {
      c.setStr( pGCTH->make_string(cid) );
      return(pTSVC->Unify(&c,pHXL->XVar(0)));
   }
   else
   {
      int err = GetLastError();
      return pTSVC->UnifyInt(err, X(0));
   } */
}

TF BuiltIns::p_version()
{                       // return the version and environment as a string 
   Cell  c;
   c.setStr( pGCTH->make_string(pSTATE->m_version) );

   return(pTSVC->Unify(&c,pHXL->XVar(0)));
}

/*
TF BuiltIns::p_version_build()
{
   // used by compiler to set version and build in .plm file
   pTSVC->UnifyInt(VERSION_NUMBER, X(0));
   pTSVC->UnifyInt(BUILD_NUMBER, X(1));
   return TRUE;
}
*/

TF BuiltIns::p_version_build()
{  // version_build(MAJOR/MINOR, BUILD, BUILDDATE)
   // used by compiler to set version and build in .plm file
   pTSVC->UnifyInt(VERSION_NUMBER, X(0));
   pTSVC->UnifyInt(BUILD_NUMBER, X(1));
   Cell c;
   c.setStr(pGCTH->make_string(pSTATE->datestamp));
   pTSVC->Unify(&c, X(2));
   return TRUE;
}

TF BuiltIns::p_address()
{                                          // to study garbling from C debug 
   TERM t = (pHXL->XVar(0))->dref();
   return (pTSVC->UnifyInt((long)t, pHXL->XVar(1)) );
}

TF BuiltIns::p_prologZflag()
{
   TERM flagT = X(0);
   if (! flagT->IsAtom())
      pXCPT->Error(instanceE, aS("Flag must be an atom"));

   PATOM flagA = flagT->getAtom();

   TERM valT = X(1);
   Cell retval;
   PATOM valA = pATAB->emptyA;
   STRptr valS;
   int   valI = 0;

   if (valT->IsVar())
   {
      // return the current value

      // config parameters
      if (flagA == pATAB->heapA)
         retval.setInt(pENG->m_ini.heap);
      else if (flagA == pATAB->localA)
         retval.setInt(pENG->m_ini.local);
      else if (flagA == pATAB->controlA)
         retval.setInt(pENG->m_ini.control);
      else if (flagA == pATAB->trailA)
         retval.setInt(pENG->m_ini.trail);
      else if (flagA == pATAB->heapbumperA)
         retval.setInt(pENG->m_ini.heapbumper);
      else if (flagA == pATAB->readbufferA)
         retval.setInt(pENG->m_ini.readbuffer);
      else if (flagA == pATAB->readdepthA)
         retval.setInt(pENG->m_ini.readdepth);
      else if (flagA == pATAB->maxclausesA)
         retval.setInt(pENG->m_ini.maxclauses);
      else if (flagA == pATAB->outputdepthA)
         retval.setInt(pENG->m_ini.outputdepth);
#ifdef UNIX
      else if (flagA == pATAB->maxmemoryA)
         retval.setInt(pENG->m_ini.maxmemory);
#endif
      else if (flagA == pATAB->logfileA)
         retval.setStr(pGCTH->make_string(pENG->m_ini.logfile));
      else if (flagA == pATAB->localeA)
         retval.setStr(pGCTH->make_string(pSTATE->m_locale));
      else if (flagA == pATAB->apitraceA)
      {
         if (pENG->m_ini.apitrace == LON)
            retval.setAtom(pATAB->onA);
         else
            retval.setAtom(pATAB->offA);
      }
      else if (flagA == pATAB->string_escA)  // settable as well
      {
         if ((LFLAG)pREAD->GetStrEsc() == LON)
            retval.setAtom(pATAB->onA);
         else
            retval.setAtom(pATAB->offA);
      }
      else if (flagA == pATAB->lsxloadA)
         retval.setStr(pGCTH->make_string(pENG->m_ini.lsxload));
      else if (flagA == pATAB->macroheapszA)
         retval.setInt(pENG->m_ini.macroheapsz);
      else if (flagA == pATAB->gcthingfreqA)
         retval.setInt(pENG->m_ini.gcthingfreq);
      else if (flagA == pATAB->maxfilesA)
         retval.setInt(pENG->m_ini.maxfiles);

      // settable parameters
      else if (flagA == pATAB->occurs_checkA)
      {
         if (pSTATE->m_occurs_check == LON)
            retval.setAtom(pATAB->onA);
         else
            retval.setAtom(pATAB->offA);
      }
      else if (flagA == pATAB->double_quote_stringsA)
      {
         if (pSTATE->m_properQuotes == LON)
            retval.setAtom(pATAB->onA);
         else
            retval.setAtom(pATAB->offA);
      }
      else if (flagA == pATAB->macrotraceA)
      {
         if (pSTATE->m_macrotrace == LON)
            retval.setAtom(pATAB->onA);
         else
            retval.setAtom(pATAB->offA);
      }
      else if (flagA == pATAB->preprocessorA)
      {
         if (pSTATE->m_prep == LON)
            retval.setAtom(pATAB->onA);
         else
            retval.setAtom(pATAB->offA);
      }
      else if (flagA == pATAB->vars_sort_equalA)
      {
         if (pSTATE->m_vars_sort_equal == LON)
            retval.setAtom(pATAB->onA);
         else
            retval.setAtom(pATAB->offA);
      }
      else if (flagA == pATAB->debug64_cutA)
      {
         if (pSTATE->m_debug64_cut == LON)
            retval.setAtom(pATAB->onA);
         else
            retval.setAtom(pATAB->offA);
      }
      else if (flagA == pATAB->traceA)
      {
         if (pSTATE->m_trace == LON)
            retval.setAtom(pATAB->onA);
         else
            retval.setAtom(pATAB->offA);
      }
      else if (flagA == pATAB->upper_case_atomsA)
      {
         if (pSTATE->m_properNames == LON)
            retval.setAtom(pATAB->onA);
         else
            retval.setAtom(pATAB->offA);
      }
      else if (flagA == pATAB->vbaA)
      {
         if (pSTATE->m_vba == LON)
            retval.setAtom(pATAB->onA);
         else
            retval.setAtom(pATAB->offA);
      }
      else if (flagA == pATAB->utf8ioA)
      {
         if (pSTATE->m_utf8io == LON)
            retval.setAtom(pATAB->onA);
         else
            retval.setAtom(pATAB->offA);
      }
      else if (flagA == pATAB->undefined_predicateA)
	  {
         if (pSTATE->m_undefined_predicate == LFAIL)
            retval.setAtom(pATAB->failA);
         else
            retval.setAtom(pATAB->errorflagA);
      }
      else if (flagA == pATAB->decimalsA)
      {
         if (pSTATE->m_decimals == real_)
            retval.setAtom(pATAB->realA);
         else if (pSTATE->m_decimals == float_)
            retval.setAtom(pATAB->floatA);
         else;
      }
      
      else if (flagA == pATAB->floatsA)
      {
         if (pSTATE->m_floats == single_)
            retval.setAtom(pATAB->singleA);
         else if (pSTATE->m_floats == double_)
            retval.setAtom(pATAB->doubleA);
         else;
      }

      else if (flagA == pATAB->decimal_placesA)
         retval.setInt(pSTATE->m_decimal_places);
      else if (flagA == pATAB->epsilonA)
         retval.setInt(9 * pSTATE->m_epsilon);
      else if (flagA == pATAB->deltaA)
         retval.setInt(9 * pSTATE->m_delta);
      else if (flagA == pATAB->moduloA)
         retval.setInt(pSTATE->m_modulo);

      else if (flagA == pATAB->boundedA)
      {
         if (pSTATE->m_decimals == real_)
            retval.setAtom(pATAB->falseA);
         else
            retval.setAtom(pATAB->trueA);
      }
      else if (flagA == pATAB->char_conversionA)
         retval.setAtom(pATAB->offA);
      else if (flagA == pATAB->debugA)
      {
         if (pSTATE->m_trace == LON)
            retval.setAtom(pATAB->onA);
         else
            retval.setAtom(pATAB->offA);
      }
      else if (flagA == pATAB->integer_rounding_functionA)
         retval.setAtom(pATAB->toward_zeroA);
      else if (flagA == pATAB->max_arityA)
         retval.setInt(MAXarity);
      else if (flagA == pATAB->max_integerA)
         retval.setAtom(pATAB->not_applicableA);
      else if (flagA == pATAB->min_integerA)
         retval.setAtom(pATAB->not_applicableA);
      else if (flagA == pATAB->prolog_copyrightA)
         retval.setStr(pGCTH->make_string((STRptr)copyright));
      else if (flagA == pATAB->prolog_dateA)
         retval.setStr(pGCTH->make_string(pSTATE->datestamp));
      else if (flagA == pATAB->localeA)
         retval.setStr(pGCTH->make_string(pSTATE->m_locale));
      else if (flagA == pATAB->prolog_nameA)
         retval.setStr(pGCTH->make_string(PROLOG_NAME));
      else if (flagA == pATAB->prolog_versionA)
         retval.setStr(pGCTH->make_string(AMZI_VERSION));
      else if (flagA == pATAB->debug_portA)
         retval.setInt(pSTATE->m_debug_port);
      else if (flagA == pATAB->debug_hostA)
         retval.setString(pGCTH->make_string(pSTATE->m_debug_host));
      else if (flagA == pATAB->unicodeA)
#ifdef _UNICODE
         retval.setAtom(pATAB->trueA);
#else
         retval.setAtom(pATAB->falseA);
#endif
      else
         //pXCPT->Error(unknown_flagE, (STRptr)(*flagA));
         retval.setAtom(pATAB->unknown_flagA);

      pTSVC->UnifyConst(X(1), &retval);
   }
   else // nonvar value
   {
      if (valT->IsAtom())
         valA = valT->getAtom();
      else if (valT->IsInt())
         valI = valT->getInt();
      else if (valT->IsStr())
         valS = valT->getStr();

      // set the value of the flag
      if (flagA == pATAB->occurs_checkA)
         pSTATE->m_occurs_check = (valA == pATAB->onA ? LON : LOFF);
      else if (flagA == pATAB->double_quote_stringsA)
         pSTATE->m_properQuotes = (valA == pATAB->onA ? LON : LOFF);
      else if (flagA == pATAB->macrotraceA)  // not published, for diagnostics
         pSTATE->m_macrotrace = (valA == pATAB->onA ? LON : LOFF);
      else if (flagA == pATAB->preprocessorA)
         pSTATE->m_prep = (valA == pATAB->onA ? LON : LOFF);
      else if (flagA == pATAB->vars_sort_equalA)
         pSTATE->m_vars_sort_equal = (valA == pATAB->onA ? LON : LOFF);
      else if (flagA == pATAB->debug64_cutA)
         pSTATE->m_debug64_cut = (valA == pATAB->onA ? LON : LOFF);
      else if (flagA == pATAB->string_escA)
      {
         if (valA == pATAB->onA)
            pREAD->SetStrEsc(LON);
         else
            pREAD->SetStrEsc(LOFF);
      }
      else if (flagA == pATAB->traceA)  // not published, used by debugger
         pSTATE->m_trace = (valA == pATAB->onA ? LON : LOFF);
      else if (flagA == pATAB->upper_case_atomsA)
         pSTATE->m_properNames = (valA == pATAB->onA ? LON : LOFF);
      else if (flagA == pATAB->vbaA)
      {
         pSTATE->m_properQuotes = (valA == pATAB->onA ? LON : LOFF);
         pSTATE->m_properNames = (valA == pATAB->onA ? LON : LOFF);
         pSTATE->m_vba = (valA == pATAB->onA ? LON : LOFF);
      }

      else if (flagA == pATAB->utf8ioA)
         pSTATE->m_utf8io = (valA == pATAB->onA ? LON : LOFF);
      else if (flagA == pATAB->undefined_predicateA)
         pSTATE->m_undefined_predicate = (valA == pATAB->failA ? LFAIL : LERROR);
      else if (flagA == pATAB->decimalsA)
      {
         if (valA == pATAB->realA)
            pSTATE->m_decimals = real_;
         else if (valA == pATAB->floatA)
            pSTATE->m_decimals = float_;
      }
      
      else if (flagA == pATAB->floatsA)
      {
         if (valA == pATAB->singleA)
            pSTATE->m_floats = single_;
         else if (valA == pATAB->doubleA)
            pSTATE->m_floats = double_;
      }

      else if (flagA == pATAB->decimal_placesA)
         pSTATE->m_decimal_places = valI;
      else if (flagA == pATAB->epsilonA)
         pSTATE->m_epsilon = (valI-8) / 9 ;
      else if (flagA == pATAB->deltaA)
         pSTATE->m_delta = (valI+8) / 9;
      else if (flagA == pATAB->moduloA)
         pSTATE->m_modulo = valI;
      else if (flagA == pATAB->localeA)
      {
         int vlen = (int)Lstrlen(valS)+1;
         char *valAS = new char[vlen*2];
         wcstombs(valAS, valS, vlen*2);
         pSTATE->m_locale = setlocale(LC_CTYPE, valAS);
         delete valAS;
      }

      else
         pXCPT->Error(unknown_flagE, (STRptr)(*flagA));
   }

   return TRUE;
}

TF BuiltIns::p_epsilon(void)
{                                     // epsilon/1 returns 1g'epsilon flag'
  TERM t = pHXL->heapGETN(1);  
  Real r;
    long long llone = 1;
  LNEWX(r, LReal(llone));
  r->setExp(pSTATE->m_epsilon);
  t->setReal(pGCTH->make_real(r));
  return pTSVC->Unify(t, pHXL->XVar(0));    
}

TF BuiltIns::p_is_unicode()
{  // used by compiler in determining how to write characters
#ifdef _UNICODE
   return TRUE;
#else
   return FALSE;
#endif
}

TF BuiltIns::p_err_read()
{   // kept so programs linked with old amzilib still work 
   return FALSE;
}

TF BuiltIns::p_faultyZtower()
{   // kept so programs linked with old amzilib still work 
   pXCPT->Error(faultytowerE);
   return FALSE;
}


//------------------------------------------
// Predicates defined in other classes
//

// Module Predicates

TF BuiltIns::p_moduleZ(void)
{   return pDDB->p_moduleZ(); }
TF BuiltIns::p_end_moduleZ(void)
{   return pDDB->p_end_moduleZ(); }
TF BuiltIns::p_currentZmodule(void)
{   return pDDB->p_currentZmodule(); }
TF BuiltIns::p_loading_module(void)
{   return pDDB->p_loading_module(); }
TF BuiltIns::p_moduleZindex(void)
{   return pDDB->p_moduleZindex(); }
TF BuiltIns::p_getZmodix(void)
{   return pTSVC->p_getZmodix(); }
TF BuiltIns::p_remove_module(void)
{   return pDDB->p_remove_module(); }


// Atom Table & Atom Predicates

TF BuiltIns::p_atom_length(void)
{   return pATAB->p_atom_length(); }
TF BuiltIns::p_atomlist_concat(void)
{   return pATAB->p_atomlist_concat(); }
TF BuiltIns::p_number_chars(void)
{   return pATAB->p_number_chars(); }
TF BuiltIns::p_atom_chars(void)
{   return pATAB->p_atom_chars(); }
TF BuiltIns::p_atom_codes(void)
{   return pATAB->p_atom_codes(); }
TF BuiltIns::p_atom_uplow(void)
{   return pATAB->p_atom_uplow(); }
TF BuiltIns::p_op(void)
{   return pATAB->p_op(); }

// Dynamic Database Predicates

TF BuiltIns::p_assertZ(void)
{   return pDDB->p_assertZ(); }
TF BuiltIns::p_clauseZdb(void)
{   return pDDB->p_clauseZdb(); }
TF BuiltIns::p_retractZdb(void)
{   return pDDB->p_retractZdb(); }
TF BuiltIns::p_debug_data(void)
{   return pDDB->p_debug_data(); }
TF BuiltIns::p_abolish(void)
{   return pDDB->p_abolish(); }
TF BuiltIns::p_iscode(void)
{   return pDDB->p_iscode(); }
TF BuiltIns::p_isZcode(void)
{   return pDDB->p_isZcode(); }
TF BuiltIns::p_definedZ(void)
{   return pDDB->p_definedZ(); }
TF BuiltIns::p_setZdiscontiguous(void)
{   return pDDB->p_setZdiscontiguous(); }

TF BuiltIns::p_getZpred(void)
{   return pDDB->p_getZpred(); }
TF BuiltIns::p_predicateZproperty(void)
{   return pDDB->p_predicateZproperty(); }
TF BuiltIns::p_predicateZenginfo(void)
{   return pDDB->p_predicateZenginfo(); }
TF BuiltIns::p_getZop(void)
{   return pATAB->p_getZop(); }

//TF BuiltIns::p_pro_db(void)
//{   return pDMEM->p_pro_db(); }

TF BuiltIns::p_import_1(void)
{   return pDDB->p_import_1(); }
TF BuiltIns::p_importZ2(void)
{   return pDDB->p_importZ2(); }
TF BuiltIns::p_importZmod(void)
{   return pDDB->p_importZmod(); }
TF BuiltIns::p_exportZ(void)
{   return pDDB->p_exportZ(); }
TF BuiltIns::p_setZmeta(void)
{   return pDDB->p_setZmeta(); }
TF BuiltIns::p_isZmeta(void)
{   return pDDB->p_isZmeta(); }
TF BuiltIns::p_setZsorted(void)
{   return pDDB->p_setZsorted(); }
TF BuiltIns::p_setZindexed(void)
{   return pDDB->p_setZindexed(); }

// String and Thing Predicates

TF BuiltIns::p_string_tokens(void)
{   return pPSTR->p_string_tokens(); }
TF BuiltIns::p_string_tokens3(void)
{   return pPSTR->p_string_tokens3(); }
TF BuiltIns::p_str_list(void)
{   return pPSTR->p_str_list(); }
TF BuiltIns::p_string(void)
{   return pPSTR->p_string(); }
TF BuiltIns::p_string_length(void)
{   return pPSTR->p_string_length(); }
TF BuiltIns::p_substring(void)
{   return pPSTR->p_substring(); }
TF BuiltIns::p_subZstring(void)
{   return pPSTR->p_subZstring(); }
TF BuiltIns::p_strcat(void)
{   return pPSTR->p_strcat(); }
TF BuiltIns::p_stringlist_concat(void)
{   return pPSTR->p_stringlist_concat(); }
TF BuiltIns::p_string_split(void)
{   return pPSTR->p_string_split(); }
TF BuiltIns::p_string_icomp(void)
{   return pPSTR->p_string_icomp(); }
TF BuiltIns::p_read_string(void)
{   return pPSTR->p_read_string(); }
TF BuiltIns::p_string_term(void)
{   return pPSTR->p_string_term(); }
TF BuiltIns::p_is_string_term(void)
{   return pPSTR->p_is_string_term(); }
TF BuiltIns::p_string_termq(void)
{   return pPSTR->p_string_termq(); }
TF BuiltIns::p_string_termq3(void)
{   return pPSTR->p_string_termq3(); }
TF BuiltIns::p_string_trim(void)
{   return pPSTR->p_string_trim(); }
TF BuiltIns::p_nonblank_string(void)
{   return pPSTR->p_nonblank_string(); }
TF BuiltIns::p_string_atom(void)
{   return pPSTR->p_string_atom(); }
TF BuiltIns::p_tilt_slashes(void)
{   return pPSTR->p_tilt_slashes(); }

TF BuiltIns::p_cntr(void)
{   return pTSVC->p_cntr(); }
TF BuiltIns::p_for(void)
{   return pTSVC->p_for(); }

// Arithmetic Predicates

TF BuiltIns::p_fixed_list(void)
{   return pTSVC->p_fixed_list(); }
TF BuiltIns::p_arith_plus(void)
{   return pMATH->p_arith_plus(); }
TF BuiltIns::p_numeq(void)
{   return pMATH->p_numeq(); }
TF BuiltIns::p_almost_equal(void)
{   return pMATH->p_almost_equal(); }
TF BuiltIns::p_gt(void)
{   return pMATH->p_gt(); }
TF BuiltIns::p_is(void)
{   return pMATH->p_is(); }
TF BuiltIns::p_lt(void)
{   return pMATH->p_lt(); }
TF BuiltIns::p_is_integer(void)
{   return pMATH->p_is_integer(); }
TF BuiltIns::p_is_odd(void)
{   return pMATH->p_is_odd(); }
TF BuiltIns::p_fraction(void)
{   return pMATH->p_fraction(); }

// Number Types

TF BuiltIns::p_integer(void)
{   return pMATH->p_integer(); }
//TF BuiltIns::p_long(void)
//{   return pTSVC->p_long(); }
//TF BuiltIns::p_short(void)
//{   return pTSVC->p_short(); }
TF BuiltIns::p_fixed_real(void)
{   return pMATH->p_fixed_real(); }
TF BuiltIns::p_long_real(void)
{   return pMATH->p_long_real(); }
TF BuiltIns::p_real(void)
{   return pMATH->p_real(); }
TF BuiltIns::p_single(void)
{   return pMATH->p_single(); }
TF BuiltIns::p_double(void)
{   return pMATH->p_double(); }
TF BuiltIns::p_float(void)
{   return pMATH->p_float(); }
TF BuiltIns::p_real_components(void)
{   return pMATH->p_real_components(); }

// Term Classification

TF BuiltIns::p_arg(void)
{   return pTSVC->p_arg(); }
TF BuiltIns::p_atom(void)
{   return pTSVC->p_atom(); }
//TF BuiltIns::p_dbref(void)
//{   return pTSVC->p_dbref(); }
TF BuiltIns::p_functor(void)
{   return pTSVC->p_functor(); }
TF BuiltIns::p_nth(void)
{   return pMATH->p_nth(); }

TF BuiltIns::p_newReal(void)
{   return pMATH->p_newReal(); }
TF BuiltIns::p_divrem(void)
{   return pMATH->p_divrem('r'); }
TF BuiltIns::p_divmodu(void)
{   return pMATH->p_divrem('u'); }
TF BuiltIns::p_divmods(void)
{   return pMATH->p_divrem('s'); }

TF BuiltIns::p_makePrimes(void)
{   return pMATH->p_makePrimes(); }
TF BuiltIns::p_Primes(void)
{   return pMATH->p_Primes(); }
TF BuiltIns::p_nthPrime(void)
{   return pMATH->p_nthPrime(); }

//#ifdef REALS
//#endif

//
TF BuiltIns::p_char(void)
{   return pTSVC->p_char(); }
//TF BuiltIns::p_lexord()
//{                                              // value is 1, 0, -1 or strcmp
//  int value = pTSVC->lexord(X(0), X(1));
//  Cell x;
//  x.setInt(value);
//  return pTSVC->Unify(&x, pHXL->XVar(2));
//}
TF BuiltIns::p_lessZ()
{
   if (pSTATE->m_vars_sort_equal == LOFF)
      return pTSVC->compare(X(0), X(1), false) < 0 ? TRUE : FALSE;
   else
      return pTSVC->compare(X(0), X(1), true) < 0 ? TRUE : FALSE;
}
TF BuiltIns::p_copy_term()
{   return pTSVC->p_copy_term(); }
TF BuiltIns::p_memberv(void)
{   return pTSVC->p_memberv(); }
TF BuiltIns::p_not_strong_unify(void)
{   return pTSVC->p_not_strong_unify(); }
TF BuiltIns::p_number(void)
{   return pTSVC->p_number(); }
TF BuiltIns::p_strong_unify(void)
{   return pTSVC->p_strong_unify(); }
TF BuiltIns::p_univ(void)
{   return pTSVC->p_univ(); }
TF BuiltIns::p_varsof(void)
{   return pTSVC->p_varsof(); }
TF BuiltIns::p_gensym(void)
{   return pTSVC->p_gensym(); }
TF BuiltIns::p_is_cyclic(void)
{   return pTSVC->p_is_cyclic(); }
TF BuiltIns::p_ground(void)
{   return pTSVC->p_ground(); }
//#ifdef REALS
TF BuiltIns::p_truncate(void)
{   return pMATH->p_truncate(); }
TF BuiltIns::p_int_real(void)
{   return pMATH->p_int_real(); }
TF BuiltIns::p_float_real(void)
{   return pMATH->p_float_real(); }
TF BuiltIns::p_real_list(void)
{   return pMATH->p_real_list(); }
//#endif

// Stashing Predicates

TF BuiltIns::p_get_term(void)
{   return pTSVC->p_get_term(); }
TF BuiltIns::p_peek_term(void)
{   return pTSVC->p_peek_term(); }
//TF BuiltIns::p_poke_term(void)
//{   return pTSVC->p_poke_term(); }
TF BuiltIns::p_reserve_heap(void)
{   return pTSVC->p_reserve_heap(); }
TF BuiltIns::p_stash_term(void)
{   return pTSVC->p_stash_term(); }
TF BuiltIns::p_stash_free(void)
{   return pTSVC->p_stash_free(); }

// Term Writing Predicates

TF BuiltIns::p_varlist(void)
{   return pREAD->p_varlist(); }
//TF BuiltIns::p_write(void)
//{   return pTSVC->p_write(); }
TF BuiltIns::p_write(void)
{   return pWRIT->p_write(); }
#ifdef LANDFILL
TF BuiltIns::p_landZfill(void)
{   return pTSVC->p_landZfill(); }
TF BuiltIns::p_logZterm(void)
{   return pWRIT->p_logZterm(); }
#endif

// Term Reading Predicates

TF BuiltIns::p_read_term(void)
{   return pREAD->p_read_term(); }
//TF BuiltIns::p_read(void)
//{   return pREAD->p_read(); }

// Stream I/O Predicates
TF BuiltIns::p_streamList(void)
{   return pIO->p_streamList(); }
TF BuiltIns::p_stream_props(void)
{   return pIO->p_stream_props(); }
TF BuiltIns::p_cur_streams(void)
{   return pIO->p_cur_streams(); }
TF BuiltIns::p_cur_user(void)
{   return pIO->p_cur_user(); }

TF BuiltIns::p_cur_input(void)
{   return pIO->p_cur_input(); }
TF BuiltIns::p_cur_output(void)
{   return pIO->p_cur_output(); }
TF BuiltIns::p_set_input(void)
{   return pIO->p_set_input(); }
TF BuiltIns::p_set_output(void)
{   return pIO->p_set_output(); }
TF BuiltIns::p_set_streampos(void)
{   return pIO->p_set_streampos(); }
TF BuiltIns::p_at_eos(void)
{   return pIO->p_at_eos(); }

TF BuiltIns::p_open(void)
{   return pIO->p_open(); }
TF BuiltIns::p_close(void)
{   return pIO->p_close(); }
//TF BuiltIns::p_see(void)
//{   return pIO->p_see(); }
//TF BuiltIns::p_tell(void)
//{   return pIO->p_tell(); }
TF BuiltIns::p_fseek(void)
{   return pIO->p_fseek(); }

//TF BuiltIns::p_sopen(void)
//{   return pSIO->p_sopen(); }
TF BuiltIns::p_flewrite(void)
{   return pIO->p_flewrite(); }
//TF BuiltIns::p_sclose(void)
//{   
//return 1;
//}
/*
TF BuiltIns::p_fclose(void)
{   return pSIO->p_fclose(); }
TF BuiltIns::p_fwrite(void)
{   return pSIO->p_fwrite(); }
*/
TF BuiltIns::p_fread(void)
{   return pIO->p_fread(); }
//TF BuiltIns::p_fopen(void)
//{   return pIO->p_fopen(); }
//TF BuiltIns::p_fleopen(void)
//{   return pIO->p_fleopen(); }
TF BuiltIns::p_fflush(void)
{   return pIO->p_fflush(); }
TF BuiltIns::p_geto(void)
{   return pIO->p_geto(); }
//TF BuiltIns::p_getcodeo(void)
//{   return pIO->p_getcodeo(); }
//TF BuiltIns::p_ungeto(void)
//{   return pIO->p_ungeto(); }
TF BuiltIns::p_handle_name(void)
{   return pIO->p_handle_name(); }
//TF BuiltIns::p_stream_type(void)
//{   return pIO->p_stream_type(); }

TF BuiltIns::p_putc(void)
{   return pIO->p_putc(); }

TF BuiltIns::p_keyZb(void)
{
   int a = g_lenv.e_keyZb();
   if (a == -1)
      return FALSE;
   else
      return ( pTSVC->UnifyInt(a, pHXL->XVar(0)) );
}

TF BuiltIns::p_read_binary(void)
{   return pIO->p_read_binary(); }
TF BuiltIns::p_write_binary(void)
{   return pIO->p_write_binary(); }

//TF BuiltIns::p_stream_attrs(void)
//{ return pLEX->p_stream_attrs(); }


// Load & Code Predicates

TF BuiltIns::p_loadZfile(void)
{   return pLOAD->p_loadZfile(); }
TF BuiltIns::p_loadZmemory(void)
{   return pLOAD->p_loadZmemory(); }
TF BuiltIns::p_loadZops(void)
{   return pLOAD->p_loadZops(); }
TF BuiltIns::p_unload(void)
{   return pLOAD->p_unload(); }
TF BuiltIns::p_is_loaded(void)
{   return pLOAD->p_is_loaded(); }
TF BuiltIns::p_loadlsx(void)
{   return pENG->p_loadlsx(); }
TF BuiltIns::p_ensure_loaded(void)
{   return pLOAD->p_ensure_loaded(); }


TF BuiltIns::p_cut_env(void)
{   return pEXEC->p_cut_env(); }
TF BuiltIns::p_cut_debug64_env(void)
{   return pEXEC->p_cut_debug64_env(); }
TF BuiltIns::p_get_env(void)
{   return pEXEC->p_get_env(); }
TF BuiltIns::p_get_env1(void)
{   return pEXEC->p_get_env1(); }
TF BuiltIns::p_saveZstack(void)
{   return pEXEC->p_saveZstack(); }

TF BuiltIns::p_cuttag(void)
{   return pEXEC->p_cuttag(); }
TF BuiltIns::p_cutZtag(void)
{   return pEXEC->p_cutZtag(); }
TF BuiltIns::p_deZtag(void)
{   return pEXEC->p_deZtag(); }
TF BuiltIns::p_reZtag(void)
{   return pEXEC->p_reZtag(); }
TF BuiltIns::p_tag(void)
{   return pEXEC->p_tag(); }
TF BuiltIns::p_debugZpause(void)
{   return pEXEC->p_debugZpause(); }


//TF BuiltIns::p_spy(void)
//{   return pEXEC->p_spy(); }

   // Error Handling Predicates

TF BuiltIns::p_err_abort(void)
{   return pXCPT->p_err_abort(); }
TF BuiltIns::p_err_fatal(void)
{   return pXCPT->p_err_fatal(); }
TF BuiltIns::p_errZexec(void)
{   return pXCPT->p_errZexec(); }

TF BuiltIns::p_debugZstack(void)
{   return pCNTL->p_debugZstack(); }


#ifdef THREADED_EXEC_STR
TF BuiltIns::p_get_async_action(void)
{   return m_peng->m_action_buffer->p_get_async_action(); }
TF BuiltIns::p_set_event(void)
{   return m_peng->m_action_buffer->p_set_event(); }
#endif

   // LANDFILL Predicates

#ifdef LANDFILL
TF BuiltIns::p_dumpdb(void)
{   return pDDB->p_dumpdb(); }
TF BuiltIns::p_dumpZcontrol(void)
{   return pCNTL->p_dumpZcontrol(); }
TF BuiltIns::p_dumpZhxl(void)
{   return pHXL->p_dumpZhxl(); }

#if defined(UNIX)
TF BuiltIns::p_debugZbreak(void)
{   return TRUE; }
#else
TF BuiltIns::p_debugZbreak(void)
{   DebugBreak(); return TRUE; }
#endif

#ifdef BUG_LEAK
TF BuiltIns::p_leakZreport(void)
{
   aCHAR *wleakfile = X(0)->getStr();
   char *aleakfile = g_lenv.new_mbs_dup(wleakfile);

   leak_report("p_leakZreport", aleakfile);
   delete aleakfile;
   
   return TRUE;
}
#endif
/*
TF BuiltIns::p_macNames()
{ pLEX->macHisto(macNames); return TRUE;}

TF BuiltIns::p_macHisto()
{ pLEX->macHisto(histogram); return TRUE;}
*/
#endif  // LANDFILL

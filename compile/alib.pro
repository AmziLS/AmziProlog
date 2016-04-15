%-*-Prolog-*-  
% alib indented on 12/23/2001 by 'JOLI' 1.0.

%----------------------------------------------------------------------
% amzilib.pro
%
% Copyright (c) 1992-2002 Amzi! inc. All Rights Reserved.
%
% $Log: alib.pro,v $
% Revision 1.29  2007/01/29 22:01:40  dennis
% better flatten
%
% Revision 1.28  2006/07/25 15:12:05  dennis
% I don't know, maybe some thread stuff
%
% Revision 1.27  2006/05/31 22:15:56  mary
% Default to user: in alib. Prepare for 7.4.11 (10 is reserved for eoTek)
%
% Revision 1.26  2006/03/08 23:38:59  dennis
% fixed LNEW bug, MS uses bad_alloc now...
%
% Revision 1.25  2006/01/16 02:40:57  dennis
% minor fixes
%
% Revision 1.24  2006/01/04 04:31:30  dennis
% atom_concat bug fix
%
% Revision 1.23  2005/12/23 03:59:07  dennis
% string_termq3
%
% Revision 1.22  2005/12/21 21:45:37  dennis
% fixfix
%
% Revision 1.21  2005/07/28 13:33:19  dennis
% minor bug fixes
%
% Revision 1.20  2005/02/09 17:35:39  dennis
% ?var added
%
% Revision 1.19  2004/05/23 21:56:06  dennis
% fixed dcg debug bug
%
% Revision 1.18  2004/05/05 16:31:44  dennis
% added tcltk stuff
%
% Revision 1.17  2004/03/29 21:37:28  mary
% Always flush bug file on write.
%
% Revision 1.16  2004/03/26 21:37:10  dennis
% implemented occurs check
%
% Revision 1.15  2004/02/18 22:17:13  mary
% Lots of cpp and lsapi cleanups.
%
% Revision 1.14  2004/02/16 22:38:58  dennis
% fixed leaky broken streams
%
% Revision 1.13  2004/02/16 20:59:21  mary
% Added all the math functions and operators for predicate$info hover help.
%
% Revision 1.12  2004/02/01 13:25:56  mary
% Fix predicate_info to not return blank results, and some predicates were
% documented incorrectly.
%
% Revision 1.11  2004/01/31 04:01:39  mary
% More arguments and descriptions.
%
% Revision 1.10  2004/01/30 23:52:41  mary
% More hover text args and descriptions.
%
% Revision 1.9  2004/01/30 16:33:44  mary
% Added argument and description strings to PredicateHead and alib.
% Added predicate_info and predicate$enginfo to return this information.
%
% Revision 1.8  2004/01/21 22:03:29  dennis
% most of dcg working in reader
%
% Revision 1.7  2004/01/15 20:29:47  dennis
% fixed nasty unifycopy gc bug
%
% Revision 1.6  2003/12/26 18:26:14  dennis
% some reader fixes
%
% Revision 1.5  2003/12/23 21:46:18  dennis
% moved unify_vars to alib
%
% Revision 1.4  2003/10/14 01:45:08  dennis
% keys/license stuff in engine
%
% Revision 1.3  2003/10/03 03:57:35  dennis
% fixed, broke, fixed, broke, fixed I hope the source debugger
%
% Revision 1.2  2003/09/22 13:23:01  dennis
% stuff
%
% Revision 1.1.1.1  2003/09/11 02:15:10  dennis
% Starting release 7.0
%
% Revision 1.117  2003/09/11 02:07:44  dennis
% fixed memory leak problem with dynamic iterators
%
% Revision 1.116  2003/09/01 01:24:58  dennis
% bigdig and minor fixes
%
% Revision 1.115  2003/08/30 16:31:08  dennis
% loads all go to ensure_loaded now
%
% Revision 1.114  2003/08/30 12:40:47  dennis
% *** empty log message ***
%
% Revision 1.113  2003/08/27 03:04:43  dennis
% added initialization support for remote debugging LSX
%
% Revision 1.112  2003/08/21 18:24:01  dennis
% latest fixes to debug64
%
% Revision 1.111  2003/07/24 17:30:40  dennis
% support for debugging compiled code, reorg of alib etc to support that
%
% Revision 1.110  2003/06/16 17:02:46  dennis
% debug64 fixes, working pretty good now
%
% Revision 1.109  2003/06/13 18:37:00  dennis
% date_time and other fixes
%
% Revision 1.108  2003/04/14 18:15:21  dennis
% fixed setof bug, new date_time functions
%
% Revision 1.107  2003/04/10 17:35:36  dennis
% more 6.4 stuff, new date_time features added for vaccine project
%
% Revision 1.106  2003/03/24 15:49:01  dennis
% getting read for 6.4.0 alpha
%
% Revision 1.105  2003/03/15 14:34:04  dennis
% Many changes in debug64 and proto_debug, source code
% debugging is closer to a reality
%
% Revision 1.104  2003/02/13 23:44:35  dennis
% latest debug64 stuff
%
% Revision 1.103  2003/01/25 18:49:23  dennis
% debug64 preliminary stuff added in various places
%
% Revision 1.102  2002/10/20 14:55:23  dennis
% bug fixes
%
% Revision 1.101  2002/08/12 16:36:15  dennis
% a6-2-11 changes
%
% Revision 1.100  2002/07/16 00:41:09  dennis
% minor changes
%
% Revision 1.99  2002/06/19 04:04:39  dennis
% alib missing exports added, fixed gc/0
%
% Revision 1.98  2002/06/09 03:07:56  dennis
% added locking
%
% Revision 1.97  2002/06/02 03:50:55  dennis
% all the XStr forms of logic server calls call strterm and grow the
% heap, so made new ExecProve and CallProve that to the strterm inside
% so that the heap can rolled back to before the Exec/Call.  Important
% that this be done in the Prove, so that if heapgc is encountered,
% the new heap is used for the rollback.
%
% Revision 1.96  2002/05/20 04:34:11  dennis
% final changes
%
% Revision 1.95  2002/05/18 14:16:03  dennis
% First 6.2.1 - TA DA!
%
% Revision 1.94  2002/05/15 16:59:06  dennis
% Final fixes for last 6.1 build, 80
%
% Revision 1.93  2002/05/08 16:14:53  dennis
% final a6-1-78 build, OK for chinese lsapi calls
%
% Revision 1.92  2002/05/05 17:29:14  dennis
% more fixes to wcstombs, need to added ending null if string
% didn't fit
%
% Revision 1.91  2002/05/04 15:21:43  dennis
% fixed chinese character bug, wcstombs must have len*2, not len!
%
% Revision 1.90  2002/05/02 19:50:37  dennis
% fixed small recent bug
%
% Revision 1.89  2002/05/02 17:39:29  dennis
% various minor bug fixes, added locale as a settable prolog flag
%
% Revision 1.88  2002/04/25 03:42:22  dennis
% more documentation, logicbase.htm, and some fiddling with sources
%
% Revision 1.87  2002/04/20 17:25:10  dennis
% minor changes
%
% Revision 1.86  2002/04/19 19:41:42  dennis
% fixed retract bug with sorted/indexed clauses, implemented abolish for
% those types as well
%
% Revision 1.85  2002/04/17 19:31:15  ray
% Extended q_cf to convert periodic cfs to rational
%
% Revision 1.84  2002/04/09 17:31:18  ray
% corrected error in divide and mod arith
%
% Revision 1.83  2002/04/02 22:52:42  dennis
% Moved the hotel two feet to the right, changing arity and xi
% in .plm files to be 2 bytes rather than 1.
%
% Revision 1.82  2002/03/21 01:25:08  dennis
% allow separator in file names to be either tilt on slash, converts
% to correct one for Unix or Windows
%
% Revision 1.81  2002/03/11 15:21:13  dennis
% ok
%
% Revision 1.80  2002/03/10 22:18:57  dennis
% fixed some small bugs, updated documentation
%
% Revision 1.79  2002/03/07 04:37:42  dennis
% updated tutorial and duckworld samples
%
% Revision 1.78  2002/03/01 22:06:08  dennis
% more documentation for 6.2, also timings added to apitrace functions
% execstr and friends.
%
% Revision 1.77  2002/02/21 21:08:31  dennis
% changed to floats/single for number defaults
%
% Revision 1.76  2002/02/21 04:50:04  dennis
% added :- include processing for consult and compile
%
% Revision 1.75  2002/02/19 19:38:09  ray
% Reaffirmation
%
% Revision 1.74  2002/02/19 04:11:39  dennis
% changed reals to use pass by reference, eliminating almost all needs
% for new and delete, seems to have eliminated most all leaks due to math,
% put in memcpy, memmove etc. for copying gigit arrays around.
%
% Revision 1.73  2002/02/15 02:28:15  dennis
% unicode I/O working again
%
% Revision 1.72  2002/02/13 03:19:58  dennis
% reals changed to have a LReal class, moved to file of same name,
% math functions moved out of termsvc and into lmath.cpp/h, eval rewritten
% to reflect various options for numbers, lexcept modified so anyone, even
% non-engine objects, can throw LExcept objects.
%
% Revision 1.71  2002/02/04 17:20:58  dennis
% New lreal wrapper class on reals, start of number options:
% decimals: real/float, floats: single/double.  Created lmath.h/cpp
% but nothing in them yet.  Work to be done on sorting out mixed mode
% arithmetic for different options.  Also - casts removed, as they
% were causing problems.
%
% Revision 1.70  2002/01/28 06:29:18  dennis
% changes for parsing numbers, handling different options
%
% Revision 1.69  2002/01/27 22:52:04  ray
% Corrections to power and fixed
%
% Revision 1.68  2002/01/20 20:48:04  ray
% revised real divide, printReal
%
% Revision 1.67  2002/01/06 20:31:26  dennis
% put in new memory leak LNEW that reports on lines of code, had
% to take out std:: classes from engine and replace with pointers
% to classes to get it to work, due to various news and deletes in
% the stl class libraries.
%
% Revision 1.66  2001/12/22 18:50:19  dennis
% allowed for repositioning of text streams, a bit of a tricky business.
%
% Revision 1.65  2001/12/08 03:48:21  dennis
% fixed bugs with reconsult, added unicode support
%
% Revision 1.64  2001/12/03 20:20:52  dennis
% fixed copyterm to allow infinite variables, not limited to maxvars.
%
% Revision 1.63  2001/11/21 00:31:50  dennis
% removed mode stuff, filled out prolog flags
%
% Revision 1.62  2001/11/17 18:40:23  dennis
% fixed reconsult and error handling in ide
%
% Revision 1.61 2001/11/17 12:38:14 dennis
% working on reconsult
%
% Revision 1.60 2001/11/16 02:11:38 dennis
% fixed bugs in positions, streams, get0 for stdin
%
% Revision 1.59 2001/11/15 13:54:06 dennis
% Fixed logging and apitrace, made logfile into its own entity,
% removed from the Prolog stream IO.
%
% Revision 1.58 2001/11/13 00:41:17 dennis
% updated documentation and use of .cfg and mode parameters, and
% created an documented amzi.cfg file.
%
% Revision 1.57 2001/11/11 13:47:56 dennis
% fixed bug in meta$ processing of consulted clauses
%
% Revision 1.56 2001/11/09 14:54:23 mary
% removed debugging stuff
%
% Revision 1.55 2001/11/09 02:28:09 dennis
% Finished, I hope, sorted and indexed predicates. Needed to rethink
% how mscws worked, given the new itertors for moving from clause to
% clause.
%
% Revision 1.54 2001/10/30 01:01:03 dennis
% sorted and indexed clauses supported in dynamic database
%
% Revision 1.53 2001/10/27 03:24:36 dennis
% sorted dynamic predicates working, with optimized queries
% when they fit the right pattern, of vars last
%
% Revision 1.52 2001/10/26 01:28:15 dennis
% sorted predicates partially implemented
%
% Revision 1.51 2001/10/21 14:37:59 dennis
% fixed open bug, wouldn't take strings as names, and then
% fixed string_atom, put back to C built-in rather than alib
%
% Revision 1.50 2001/10/19 01:37:59 dennis
% compiler bugs, still not found, but noted that X registers
% are really restricted to 255 because of flewrite in assemb.pro,
% should change some day.
%
% Revision 1.49 2001/10/13 12:36:37 dennis
% see/tell bugs
%
% Revision 1.48 2001/10/13 05:06:10 dennis
% speeded up system metapredicates with hard-wired properties
%
% Revision 1.47 2001/10/13 02:58:12 dennis
% see/tell bugs, used to close function streams
%
% Revision 1.46 2001/10/06 17:29:22 dennis
% fixed sort bug, compare put back in
%
% Revision 1.45 2001/10/05 17:08:55 ray
% Changed mode code for streams
%
% Revision 1.44 2001/09/19 02:39:48 dennis
% fixed is_list
%
% Revision 1.43 2001/09/19 02:35:33 dennis
% fixed io stuff
%
% Revision 1.42 2001/09/11 04:34:55 dennis
% cleaned up some io stuff, got consult working, etc.
%
% Revision 1.41 2001/09/08 16:11:52 dennis
% a6-1-44 fopen and fleopen removed, 61-44 set to current compatible version
%
% Revision 1.40 2001/09/08 15:27:56 dennis
% compiler working without calling fopen or fleopen, all open calls
%
% Revision 1.39 2001/09/04 01:57:00 dennis
% changed stream vector to reuse empty slots
%
% Revision 1.38 2001/08/29 19:34:33 ray
% Corrections to 'seen' in alib etc.
%
% Revision 1.37 2001/08/02 18:51:00 dennis
% merge of new streams complete
%
% Revision 1.36 2001/08/01 20:17:58 ray
% removed tswrite.cpp & sio.cpp, added streams.cpp sowrite.cpp
%
% Revision 1.35 2001/07/24 02:52:46 dennis
% discontiguous/multifile working again for 6.1
%
% Revision 1.34 2001/06/30 02:40:34 dennis
% added stream I/O version of loader from 5.0 work
%
% Revision 1.33 2001/06/27 15:15:09 dennis
% miscellaneous changes and bug fixes, work with leak detection
%
% Revision 1.32 2001/04/16 13:02:43 ray
% Restored primeFactors/2
%
% Revision 1.31 2001/04/16 05:21:12 dennis
% hacked together some fixes for sio/lex to be better friends,
% merged other changes, added new samples
%
% Revision 1.30 2001/04/15 14:51:46 ray
% changed list_real to real_list
%
% Revision 1.29 2001/04/04 03:12:35 dennis
% minor cleanups
%
% Revision 1.28 2001/04/02 21:50:12 dennis
% got debugger working again
%
% Revision 1.27 2001/04/01 15:54:51 ray
% Modified compiler and loader for fixed data.
%
% Revision 1.26 2001/03/28 15:07:21 ray
% char_code/2, number_chars/2 added
% 1 char macros proscribed
%
% Revision 1.25 2001/03/26 19:16:56 ray
% moved various string_ builtins to alib
% repaired arithmetic.
%
% Revision 1.24 2001/03/26 02:29:34 dennis
% Ray's fixed numbers in.
%
% Revision 1.23 2001/03/25 15:29:50 ray
% Implemented fixed
% repaired #include
%
% Revision 1.22 2001/03/16 00:29:06 dennis
% compiled metapredicates
%
% Revision 1.20 2001/03/13 22:04:02 dennis
% changed is$code
%
% Revision 1.19 2001/03/13 20:05:17 dennis
% added meta$call to try and isolate meta calls.
%
% Revision 1.18 2001/03/09 20:13:59 dennis
% consolidating for KW delivery, fixed jni memory leak
%
% Revision 1.17 2001/02/28 02:22:38 dennis
% fixed some number bugs, some assert/retract bugs
%
% Revision 1.16 2001/02/24 19:53:50 ray
% alib was modified
%
% Revision 1.15 2001/02/23 23:06:45 dennis
% fixed srcbuf bug
%
% Revision 1.14 2001/02/21 04:46:42 dennis
% debugger working, updated documentation for 6.1
%
% Revision 1.13 2001/02/18 18:47:14 ray
% Substituted end_of_file for !EOS
% Replaced 0' with 0' for char code denotation
% Introduced 0' quote mark as default for strings
%
% Revision 1.12 2001/02/08 22:56:45 dennis
% string bug fixes, modularized compiler and listener
%
% Revision 1.11 2001/02/06 04:06:34 dennis
% fixed throw bug, added puctuation error checking to alib.
%
% Revision 1.10 2001/02/05 20:17:08 dennis
% fixed some bugs
%
% Revision 1.9 2001/02/05 03:11:43 dennis
% Fixed nasty heap GC bug, other minor fixes
%
% Revision 1.8 2001/01/30 16:47:27 dennis
% Made, after many trials, alib into amzi_system module.
%
% Revision 1.7 2001/01/21 23:02:34 ray
% Rewrote real divide
% Added real sqrt
%
% Revision 1.6 2001/01/11 03:39:00 dennis
% Made metapredicates more efficient with is$meta/3
%
% Revision 1.5 2001/01/11 01:49:22 dennis
% Implemented rest of import/export and metapredicates, working
% for test cases.
%
% Revision 1.4 2001/01/06 03:46:01 dennis
% listener working with import/export
%
% Revision 1.3 2001/01/05 06:05:53 dennis
% fixed minor discrepancies
%
% Revision 1.2 2000/12/29 22:39:48 dennis
% added first cut at a system module
%
% Revision 1.1.1.1 2000/12/29 02:18:05 dennis
% moved to a6
%
% Revision 1.37 2000/12/28 16:30:35 dennis
% fixed nasty pets bug, true/false on CallInterp()
% and other changes, merged with ray's new stuff
% calling this a6-1-6
%
% Revision 1.36 2000/12/13 21:13:59 ray
% Added epsilon flag for reals.
%
% Revision 1.35 2000/11/30 15:47:27 ray
% made arg, nth and prime bilateral.
% made real length and exponent 12 bits for maximum range
%
% Revision 1.34 2000/11/10 17:09:18 ray
% Restricted real length to 255 in order to add max length, for protection
%
% Revision 1.33 2000/11/09 17:18:47 ray
% added primeFactors/2
%
% Revision 1.32 2000/11/08 17:13:54 ray
% Added makePrimes/4, primes/1, prime/2
%
% Revision 1.31 2000/10/30 15:11:45 ray
% Extended compiler, loader and linker for real data.
% Added arithmetic primitives for rational data in alib.pro
%
% Revision 1.30 2000/10/21 03:02:41 dennis
% temp fix of .pro.pro problem in compiler
%
% Revision 1.29 2000/10/20 12:40:51 ray
% Moved =:= out of alib to accomodate reals.
% Various repairs and enhancements
%
% Revision 1.28 2000/10/15 17:57:05 ray
% Eliminated 'L' notation
%
% Revision 1.27 2000/10/07 17:47:19 ray
% added q_cf and fourierPrime
%
% Revision 1.26 2000/10/01 16:20:03 dennis
% cleaned up modules, ddb, got defined, abolish and friends working
%
% Revision 1.25 2000/09/28 03:24:39 dennis
% debugger working for command line listener
%
% Revision 1.24 2000/09/27 01:42:02 dennis
% implemented listing, needed current_predicate, predicate_property,
% and current_module
%
% Revision 1.23 2000/09/25 02:11:18 dennis
% first version of modules working, runs the modular version of
% duck world. still needs import and export. release 6.1.1
%
% Revision 1.22 2000/09/15 21:42:24 dennis
% 12->13
%
% Revision 1.21 2000/09/02 02:10:18 dennis
% new version of get$db replacing dbrefgen for getting clauses
% from dynamic database
%
% Revision 1.20 2000/08/21 21:48:27 ray
% *** empty log message ***
%
% Revision 1.19 2000/06/11 05:22:56 ray
% added number_codes
%
% Revision 1.18 2000/05/15 11:24:46 dennis
% fixed some minor bugs, started modules
%
% Revision 1.17 2000/05/15 07:45:51 dennis
% new alib with iso stuff from Ray
%
% Revision 1.15 2000/05/11 00:52:16 ray
% open, close, put_char,get_char, put_code, get_code, write_term,
% current_input, current_output, write_canonical
%
% Revision 1.14 2000/05/04 21:28:11 ray
% directives initialization, include, ensure_loaded
%
% Revision 1.13 2000/04/21 02:36:59 ray
%
% new current_prolog_flags
%
% Revision 1.12 2000/04/10 01:00:43 ray
% *** empty log message ***
%
% Revision 1.11 2000/03/28 01:05:14 dennis
% merged Ray's changes with bigdig. bigdig is at point where
% new Cell class is used, but there are no modules in the system.
%
% Revision 1.10 2000/03/16 02:20:03 ray
%
% alib
%
% Revision 1.9 2000/03/06 18:32:55 ray
% *** empty log message ***
%
% Revision 1.8.2.3 2000/03/23 12:36:10 dennis
% *** empty log message ***
%
% Revision 1.8.2.2 2000/03/08 04:11:59 dennis
% builtin.cpp compiles
%
% Revision 1.8.2.1 2000/02/26 20:56:12 dennis
% Removed local atoms from compiler, and old module support, so
% compiler and listener are all global for now. Also made member/2
% and friends built-ins as well as the bug predicates.
%
% Revision 1.8 2000/02/14 04:06:04 dennis
% Everything working on Solaris, need "C" linkage for LSX entry
% point, InitPreds, and fixed bug with load/1 predicate, so it
% can load library .plm's from alib.
%
% Revision 1.7 2000/02/13 05:09:48 dennis
% Merging other changes with NT
%
% Revision 1.6 2000/01/31 14:13:09 dennis
% Cleaned up system dependent code, creating new module, lenv,
% that has it all, or at least most of it.
%
% Revision 1.5 2000/01/28 11:05:42 dennis
% Fixed 'listing' bug caused by cur$atom wanting a short int
% (as it should for an atom) but being fed a long one from the
% new for/4, which drives the cur_atom loop.
%
%
%
% - library of built-in predicates implemented in Prolog
% - initial operator definitions
% - main entry point for .xpl files (cp$top/0)
% - dynamic database evaluator (guts of interpreter)
% - DCG evaluator
%
% 12/15/99 Ray added properNames, properQuotes, controls, prep 
% & largeInts to mode$
% a4lib indented on 4/17/1999 by 'JOLI' 1.0.
% 08/20/99 Ray add ed catch and throw to DCG specials
% 01/20/99 Ray addded catch/4
% 11/16/99 Ray lib$ver(22) modified =:=
%----------------------------------------------------------------------
% Contents
% --------
% Imports & Exports
% Operator Definitions
% Main Entry Point
% Interpreter
% Built-Ins
% Arithmetic comparison predicates
% Consulting files
% Counter Manipulation
% Database predicates
% Display predicates
% File handling
% I/O predicates
% List manipulation predicates
% Mode control
% Set gathering functions
% Stream handling predicates
% Term ordering predicates
% Miscellaneous built-in predicates
% DCG Translator
% Error Handling
% Debugging Tools
% Local Utilities to amzilib.pro
%
%
% Note on err$exec/4 used for error reporting:
% arg1 - typeE, instanceE, execE, syntaxE
% arg2 - message string
% arg3 - Prolog terms to be tacked onto message
% arg4 - the predicate which triggered the error
% But, arg4 not working correctly, needs to be rethought

:-module(amzi_system).

%----------------------------------------------------------------------
% Operator Definitions
%
% define operators first, so they can be used in latent expressions
% during loading if necessary
%----------------------------------------------------------------------

op(Prec, Class, []) :- !.
op(Prec, Class, Symbol) :-
  atom(Symbol), !,
  op_(Prec, Class, Symbol).
op(Prec, Class, [H|T]) :-
  op(Prec, Class, H),
  op(Prec, Class, T).

:- (op(1200, xfx, [:-, -->])).
:- (op(1200, fx, [?-, :-])).
:- (op(1100, fx, [import, export, dynamic, multifile, discontiguous, sorted,
                 indexed])).
:- (op(1100, xfy, ';')).
:- (op(1050, xfy, ->)).
:- (op(1000, xfy, ',')).
:- (op(900, fy, [\+, not, once])).
:- (op(900, fy, [?, bug])).
:- (op(700, xfx, [=, \=, is, =.., ==, \==, =:=, ~=, =\=, <, >, =<, >=, @<,
                 @>, @=<, @>=])).
:- (op(600, xfy, :)).
:- (op(500, yfx, [+, -, /\, \/, xor])).       % moved \ here from 900 fy
:- (op(400, yfx, [rem, mod, divs, mods, divu, modu])). % new ones and mod
:- (op(400, yfx, [/, //, *, >>, <<])).
:- (op(200, xfx, **)).
:- (op(200, xfy, ^)).
:- (op(200, fy, [+, -, \])).

export(P/A) :- !,
  export$(P, A).
export([]) :- !.
export([P/A|Z]) :-
  export$(P, A), !,
  export(Z).
export(X) :-
  err$exec(instanceE, $Bad argument for export: $, X, export(X)).

:- export (->) /2.
:- export (?) /1.
:- export (@<) /2.
:- export (@=<) /2.
:- export (@>) /2.
:- export (@>=) /2.
:- export (\+) /1.
:- export (\=) /2.
:- export (=) /2.
:- export (=\=) /2.
:- export (=<) /2.
:- export (>=) /2.
:- export abolish/1.
:- export arg/3.
:- export assert/1.
:- export assert/2.
:- export asserta/1.
:- export asserta/2.
:- export assertz/1.
:- export assertz/2.
:- export at_end_of_stream/0.
:- export atom_concat/3.
:- export atomic/1.
:- export bagof/3.
:- export (bug)/1.
:- export bugclose/0.
:- export buginit/0.
:- export buginit/1.
:- export built_in/1.
:- export call/1.
:- export call_nometa/1.
:- export catch/3.
:- export char_code/2.
:- export clause/2.
:- export clause/5.
:- export close/1.
:- export cntr_dec/2.
:- export cntr_get/2.
:- export cntr_inc/2.
:- export cntr_set/2.
:- export compare/3.
:- export compound/1.
:- export consult/1.
:- export consult_project/1.
:- export current_module/1.
:- export current_op/3.
:- export current_predicate/1.
:- export current_prolog_flag/2.
:- export dcg$bug/3.
:- export dcg$call/3.
:- export dcg$terminal/3.
:- export debug_consult/1.
:- export debug_init/0.
:- export debug$call/7.
:- export debug$info/6.
:- export debug64/1.
:- export defined/1.
:- export display/1.
:- export expand_term/2.
:- export (export)/1.
:- export fclose/1.
:- export file_exists/1.
:- export file_exists/2.
:- export findall/3.
:- export findfiles/3.
:- export flow/1.
:- export flow/3.
:- export flush_in/0.
:- export flush_out/0.
:- export flush_output/0.
:- export flush_output/1.
:- export fopen/3.
:- export for/3.
:- export for/4.
:- export fseek/3.
:- export ftell/2.
:- export gc/0.
:- export gensym/2.
:- export get/1.
:- export get/2.
:- export get_char/1.
:- export get_char/2.
:- export get_code/1.
:- export get_code/2.
:- export get_debug_stack/1.
:- export get_mode/2.
:- export get_preds/1.
:- export get0/1.
:- export get0/2.
:- export get1/1.
:- export get1/2.
:- export halt/0.
:- export (import)/2.
:- export is_atom/1.
:- export is_float/1.
:- export is_list/1.
:- export is_number/1.
:- export is_string/1.
:- export islist/1.
:- export keysort/2.
:- export list/1.
:- export listing/0.
:- export listing/1.
:- export listing/2.
:- export load/1.
:- export load/4.
:- export makePrimes/0.
:- export meta$assert/1.
:- export meta$convert/4.
:- export nl/0.
:- export nl/1.
:- export nonvar/1.
:- export (not)/1.
:- export nth/3.
:- export number_codes/2.
:- export number_flags/2.
:- export numbervars/3.
:- export numeric_type/2.
:- export (once)/1.
:- export op/3.
:- export open/3.
:- export open/4.
:- export phrase/2.
:- export phrase/3.
:- export pp/1.
:- export pp/2.
:- export predicate_info/3.
:- export predicate_property/2.
:- export press_any_key/0.
:- export primeFactors/2.
:- export put/1.
:- export put/2.
:- export put_char/1.
:- export put_char/2.
:- export put_code/1.
:- export put_code/2.
:- export read/1.
:- export read/2.
:- export read_string/1.
:- export read_string/2.
:- export reconsult/1.
:- export reconsult_project/1.
:- export reload/1.
:- export repeat/0.
:- export respkey/1.
:- export retract/1.
:- export retract/2.
:- export retractall/1.
:- export retractall/2.
:- export see/1.
:- export seeing/1.
:- export seen/0.
:- export seentold/0.
:- export seetell/1.
:- export set_errors/1.
:- export set_mode/2.
:- export set_prolog_flag/2.
:- export setof/3.
:- export skip/1.
:- export skip/2.
:- export sort/2.
:- export stream_property/2.
:- export string_float/2.
:- export string_integer/2.
:- export string_number/2.
:- export stringlist_concat/3.
:- export string_query/2.
:- export structure/1.
:- export sub_atom/4.
:- export sub_string/4.
:- export substring/4.
:- export tab/1.
:- export tab/2.
:- export tell/1.
:- export tell/2.
:- export telling/1.
:- export term_type/2.
:- export throw/1.
:- export told/0.
:- export true/0.
:- export unget0/1.
:- export unget0/2.
:- export unify_with_occurs_check/2.
:- export unload/1.
:- export var/1.
:- export varlist_query/3.
:- export write/1.
:- export write/2.
:- export writeq/1.
:- export writeq/2.

set$discontiguous([]).
set$discontiguous([H|T]) :-
  set$discontiguous(H),
  set$discontiguous(T).
set$discontiguous( (A,B) ) :-
  set$discontiguous(A),
  set$discontiguous(B).
set$discontiguous(P/A) :-
  set$discontiguous(P, A).

:- discontiguous predicate$info/3.
%predicate$info(/, ``, ``).

% Built-in math functions and operators

predicate$info((+)/2, `X + Y`, `Sum of values of X and Y`).
predicate$info((-)/2, `X - Y`, `Value of X minus value of Y`).
predicate$info((-)/1, `-X`, `Evaluates to the negative of X evaluated`).
predicate$info((*)/2, `X * Y`, `Value of X multiplied by value of Y`).
predicate$info((/)/2, `X / Y`, `Value of X divided by value of Y`).
predicate$info((**)/2, `X ** Y`, `Evaluates to X raised to the Y power`).
predicate$info((//)/2, `X // Y`, `Integer division of X by Y truncates the result to the absolute integer`).
predicate$info(divs/2, `X divs Y`, `Integer division with a rounded rather than truncated answer`).
predicate$info(divu/2, `X divu Y`, `Integer division that is truncated`).
predicate$info(max/2, `max(X, Y)`, `The maximum of X and Y`).
predicate$info(min/2, `min(X, Y)`, `The minimum of X and Y`).
predicate$info(mod/2, `X mod Y`, `The positive remainder after dividing the value of X by the value of Y`).
predicate$info(mods/2, `X mods Y`, `The remainder after rounded integer division (divs)`).
predicate$info(modu/2, `X modu Y`, `The remainder, constrained so that the result is positive`).
predicate$info((/\)/2, `X /\ Y`, `Bitwise "and" of value of X and value of Y`).
predicate$info((\/)/2, `X \/ Y`, `Bitwise "or" of value of X and value of Y`).
predicate$info((<<)/2, `X << Y`, `Evaluates to X bit-shifted left by Y places`).
predicate$info((>>)/2, `X >> Y`, `Evaluates to X bit-shifted right by Y places`).
predicate$info((\)/1, `\ X`, `Evaluates to the bitwise complement of X`).
predicate$info(xor/2, `X xor Y`, `Evaluates to X exclusively or'd with Y`).
predicate$info(sin/1, `sin(X)`, `Evaluates to the sine of X (in radians). `).
predicate$info(cos/1, `cos(X)`, `Evaluates to the cosine of X (in radians) `).
predicate$info(tan/1, `tan(X)`, `Evaluates to the tangent of X (in radians`).
predicate$info(asin/1, `asin(X)`, `Evaluates to the angle (in radians) whose arcsine is X`).
predicate$info(acos/1, `acos(X)`, `Evaluates to the angle (in radians) whose arccosine is X`).
predicate$info(atan/1, `atan(X)`, `Evaluates to the angle (in radians) whose arctangent is X`).
predicate$info(abs/1, `abs(X)`, `Evaluates to the absolute value of `).
predicate$info(ceiling/1, `ceiling(X)`, `Evaluates to the smallest integer >= X`).
predicate$info(exp/1, `exp(X)`, `Evaluates to e raised to the power of X evaluated`).
predicate$info(float/1, `float(X)`, `Converts X to a double precision floating point number`).
predicate$info(floor/1, `floor(X)`, `Evaluates to the largest integer =< X`).
predicate$info(integer/1, `integer(X)`, `Converts X to an integer (truncating any fractional part)`).
predicate$info(ln/1, `ln(X)`, `Evaluates to the natural log (loge()) of X evaluated`).
predicate$info(log/1, `log(X)`, `Evaluates to the natural log (loge()) of X evaluated`).
predicate$info(log10/1, `log10(X)`, `Evaluates to the natural log (loge()) of X evaluated for log base 10`).
predicate$info(real/1, `real(X)`, `Converts X to a real number`).
predicate$info(round/1, `round(X)`, `Rounds X to the nearest integer and returns that value`).
predicate$info(sign/1, `sign(X)`, `Evaluates to 1 for positive numbers and -1 for negative numbers`).
predicate$info(sqrt/1, `sqrt(X)`, `Evaluates to the square root of X`).
predicate$info(cpuclock/0, `cpuclock`, `An integer with the number of milliseconds ticks expired`).
predicate$info(cputime/0, `cputime`, `A floating point number with the number of CPU seconds expired`).
predicate$info(e/0, `e`, `The value "e" (2.718281828459045)`).
predicate$info(pi/0, `pi`, `The value "pi" (3.141592653589793)`).
predicate$info(degtorad/0, `degtorad`, `The constant for converting degrees to radians, useful for trig functions. 2*pi/360`).
predicate$info(radtodeg/0, `radtodeg`, `The constant for converting radians to degrees, useful for trig functions. 360/2*pi`).
predicate$info(random/0, `random`, `A random floating point number >= 0.0 and < 1.0`).

% Built-in constants

predicate$info(end_of_file/0, `end_of_file`, `The constant that is returned by read predicates when end-of-file is reached`).


%----------------------------------------------------------------------
% Metapredicate Support
%
% Metapredicates require that their arguments have the
% current context module added as a module on arguments
% that are so marked with a ':' in a metapredicate
% mode indicator. The mode indicators are kept in the
% dynamic database.
%
% These predicates must be defined before any metapredicate
% definitions are given for module amzi_system.
%
% They are used by the interpreter, and also the compiler
% which is why they are exported.
%
% meta$assert/2
% meta$convert/3
%----------------------------------------------------------------------

get$meta$props(amzi_system, PRED, AR, PATTERN_ARGS) :-
  sys$meta$pred(PRED, AR, PATTERN_ARGS), !.
get$meta$props(MOD, PRED, AR, PATTERN_ARGS) :-
  sys$clause('{sys}meta$predicate'(MOD, PRED, AR, PATTERN_ARGS)).

set$meta$props(amzi_system, PRED, AR, PATTERN_ARGS) :-
  sys$meta$pred(PRED, AR, PATTERN_ARGS), !.
set$meta$props(MOD, PRED, AR, PATTERN_ARGS) :-
  sys$clause('{sys}meta$predicate'(MOD, PRED, AR, PATTERN_ARGS)), !.
set$meta$props(MOD, PRED, AR, PATTERN_ARGS) :-
  assert$(amzi_system, 0'a, 
         '{sys}meta$predicate'(MOD, PRED, AR, PATTERN_ARGS)).

% define system meta predicates here and below. this just hardwires
% their properties to avoid the ddb look up.

sys$meta$pred(dcg$bug, 3, [:,*,*]).
sys$meta$pred(dcg$call, 3, [:,*,*]).
sys$meta$pred(debug$call, 7, [:,*,*,*,*,*,*]).
sys$meta$pred(asserta, 1, [:]).
sys$meta$pred(assertz, 1, [:]).
sys$meta$pred(assert, 1, [:]).
sys$meta$pred(clause, 2, [:, *]).
sys$meta$pred(retract, 1, [:]).
sys$meta$pred(abolish, 1, [:]).
sys$meta$pred(retractall, 1, [:]).
sys$meta$pred(predicate_property, 2, [:, *]).
sys$meta$pred(defined, 1, [:]).
sys$meta$pred(once, 1, [:]).
sys$meta$pred(?, 1, [:]).
sys$meta$pred(not, 1, [:]).
sys$meta$pred(\+, 1, [:]).
sys$meta$pred(setof, 3, [*, :, *]).
sys$meta$pred(bagof, 3, [*, :, *]).
sys$meta$pred(findall, 3, [*, :, *]).
sys$meta$pred(catch, 3, [:, *, :]).
sys$meta$pred(phrase, 2, [:, *]).
sys$meta$pred(phrase, 3, [:, *, *]).

meta$assert(MOD, MP) :-
  functor(MP, PRED, AR),
  MP =.. [PRED|PATTERN_ARGS], !,
  set$meta(PRED, AR),
  set$meta$props(MOD, PRED, AR, PATTERN_ARGS).

meta$assert(MP) :-
  functor(MP, PRED, AR),
  MP =.. [PRED|PATTERN_ARGS],
  loading_module(MOD), !,
  set$meta(PRED, AR),
  set$meta$props(MOD, PRED, AR, PATTERN_ARGS).

meta$get$mode(MOD : PRED/AR, MODE) :-

% sys$clause('{sys}meta$predicate'(MOD, PRED, AR, PATTERN_ARGS)),
  get$meta$props(MOD, PRED, AR, PATTERN_ARGS),
  MODE =.. [PRED|PATTERN_ARGS].

meta$convert(CONTEXT_MOD, GIN, GOUT, DM) :-
  (
     GIN = M : GOAL_IN ->
     (integer(M) -> module$index(MA, M) ;  MA = M),
     GOUT = MA : GOAL_OUT ;

     GIN = GOAL_IN,
     GOUT = GOAL_OUT,
     MA = CONTEXT_MOD
  ),
  functor(GOAL_IN, PRED, ARITY),

% sys$clause('{sys}meta$predicate'(DM, PRED, ARITY, PATTERN_ARGS)),
  get$meta$props(DM, PRED, ARITY, PATTERN_ARGS),
  GOAL_IN =.. [PRED|IN_ARGS],
  meta$args(PATTERN_ARGS, MA, IN_ARGS, OUT_ARGS),
  GOAL_OUT =.. [PRED|OUT_ARGS], !.

meta$args([], _, [], []) :- !.
meta$args([*| X], CM, [A|Y], [A|Z]) :-
  meta$args(X, CM, Y, Z).
meta$args([:| X], CM, [A|Y], [AA|Z]) :-
  meta$simplify(CM : A, AA),
  meta$args(X, CM, Y, Z).

meta$simplify(M : X, M : X) :-
  var(X), !.
meta$simplify(M : MM : A, MM : A) :- !.
meta$simplify(M :(A, B), (MA, MB)) :- !,
  meta$simplify(M : A, MA),
  meta$simplify(M : B, MB).
meta$simplify(M :(A :- B), (MA :- MB)) :- !,
  meta$simplify(M : A, MA),
  meta$simplify(M : B, MB).
meta$simplify(M : !, !) :- !.
meta$simplify(M : true, true) :- !.
meta$simplify(M : fail, fail) :- !.
meta$simplify(M :(A -> B ;  C), (MA -> MB ;  MC)) :- !,
  meta$simplify(M : A, MA),
  meta$simplify(M : B, MB),
  meta$simplify(M : C, MC).
meta$simplify(M :(A ;  B), (MA ;  MB)) :- !,
  meta$simplify(M : A, MA),
  meta$simplify(M : B, MB).
meta$simplify(M :(call(A)), call(MA)) :- !,
  meta$simplify(M : A, MA).
meta$simplify(M : throw(A), throw(A)) :- !.
meta$simplify(M : catch(A, B, C), catch(MA, B, MC)) :- !,
  meta$simplify(M : A, MA),
  meta$simplify(M : C, MC).
meta$simplify(M : X, M : X).

% Exactly the same as above, but fails if there's
% a M:VAR pattern.

mcheck$convert(CONTEXT_MOD, GIN, GOUT, DM) :-
  (
     GIN = M : GOAL_IN ->
     (integer(M) -> module$index(MA, M) ;  MA = M),
     GOUT = MA : GOAL_OUT ;

     GIN = GOAL_IN,
     GOUT = GOAL_OUT,
     MA = CONTEXT_MOD
  ),
  functor(GOAL_IN, PRED, ARITY),

% sys$clause('{sys}meta$predicate'(DM, PRED, ARITY, PATTERN_ARGS)),
  get$meta$props(DM, PRED, ARITY, PATTERN_ARGS),
  GOAL_IN =.. [PRED|IN_ARGS],
  mcheck$args(PATTERN_ARGS, MA, IN_ARGS, OUT_ARGS),
  GOAL_OUT =.. [PRED|OUT_ARGS], !.

mcheck$args([], _, [], []) :- !.
mcheck$args([*| X], CM, [A|Y], [A|Z]) :-
  mcheck$args(X, CM, Y, Z).
mcheck$args([:| X], CM, [A|Y], [AA|Z]) :-
  mcheck$simplify(CM : A, AA),
  mcheck$args(X, CM, Y, Z).

mcheck$simplify(M : X, M : X) :-
  var(X), !,
  fail.
mcheck$simplify(M : MM : A, MM : A) :- !.
mcheck$simplify(M :(A, B), (MA, MB)) :- !,
  mcheck$simplify(M : A, MA),
  mcheck$simplify(M : B, MB).
mcheck$simplify(M :(A :- B), (MA :- MB)) :- !,
  mcheck$simplify(M : A, MA),
  mcheck$simplify(M : B, MB).
mcheck$simplify(M : !, !) :- !.
mcheck$simplify(M : true, true) :- !.
mcheck$simplify(M : fail, fail) :- !.
mcheck$simplify(M :(A -> B ;  C), (MA -> MB ;  MC)) :- !,
  mcheck$simplify(M : A, MA),
  mcheck$simplify(M : B, MB),
  mcheck$simplify(M : C, MC).
mcheck$simplify(M :(A ;  B), (MA ;  MB)) :- !,
  mcheck$simplify(M : A, MA),
  mcheck$simplify(M : B, MB).
mcheck$simplify(M :(call(A)), call(MA)) :- !,
  mcheck$simplify(M : A, MA).
mcheck$simplify(M : throw(A), throw(A)) :- !.
mcheck$simplify(M : catch(A, B, C), catch(MA, B, MC)) :- !,
  mcheck$simplify(M : A, MA),
  mcheck$simplify(M : C, MC).
mcheck$simplify(M : X, M : X).

% amzi_system metapredicates, safe to define now

:- metapredicate(dcg$bug(:, *,*)).
:- metapredicate(dcg$call(:, *,*)).
:- metapredicate(debug$call(:, *,*,*,*,*,*)).
:- metapredicate(clause(:, *)).
:- metapredicate(asserta(:)).
:- metapredicate(assertz(:)).
:- metapredicate(assert(:)).
:- metapredicate(retract(:)).
:- metapredicate(abolish(:)).
:- metapredicate(retractall(:)).
:- metapredicate(predicate_property(:, *)).
:- metapredicate(defined(:)).
:- metapredicate(once(:)).
:- metapredicate(?(:)).
:- metapredicate(not(:)).
:- metapredicate(\+(:)).
:- metapredicate(setof(*, :, *)).
:- metapredicate(bagof(*, :, *)).
:- metapredicate(findall(*, :, *)).
:- metapredicate(catch(:, *, :)).
:- metapredicate(phrase(:, *)).
:- metapredicate(phrase(:, *, *)).

%----------------------------------------------------------------------
% Main Entry Point
%
% cp$top/0
%
% called by codeProveMain() in cdexec.c to start execution 
% of an .xpl file. It first checks for main$, which is the
% main entry point of Amzi system modules, such as alis & acmp
%----------------------------------------------------------------------

lib$ver(50).                                  % alib version #

cp$top :-
  user : main$, !.                            % do Amzi main code if it exists
cp$top :-
  user : main.                                % do user code if it exists
%----------------------------------------------------------------------
% Interpreter
%
% call$i/1
%
% The guts of the interpreter - use a tag stack marking algorithm
% to handle '!'
%
% X is a whole procedure, so B is the choice point before
% the procedure is called. It is, therefore, the right place to return
% to, plus one more, when there is a cut. This is what cut$env does.
%
% Keep the call stack in a list, save it periodically so that if there
% is an error we can recover it for the user.
%----------------------------------------------------------------------
% The interpreter main entry, defined in alib

call$i(M : X) :- !,
  get$env(B),
  rb$(X, M, B, [X]).
call$i(X) :-
  get$modix(X, M),
  get$env(B),
  rb$(X, M, B, [X]).

% write(oops-call$i(X)), nl,
% err$exec(execE, $No module qualification for call/1$, X, call(X)).
% The debugging interpreter, implemented in
% debug.pro
%
% call$d is called by the engine for interpreted calls instead of
% call$i based on the setting of trace mode, which is set by
% the trace/1 predicate.

call$d(M : X) :- !,
  get$env(N),
  amzi_debugger : rbd$(X, M, N, 0).
call$d(X) :-
  get$modix(X, M),
  get$env(N),
  amzi_debugger : rbd$(X, M, N, 0).

% ----- Locals -----
% the order of these clauses is very important - avoid looping
% back to call$i through the call()

% rb$(CALL, M, B, S) :-
%   write(starting:rb$(CALL,M,B)), nl, fail.

rb$((X, Y), M, B, S) :- !,
  rb$(X, M, B, S),
  rb$(Y, M, B, S).
rb$((X -> Y ;  Z), M, B, S) :- !,
  (rb$(X, M, B, S) -> rb$(Y, M, B, S) ;  rb$(Z, M, B, S)).
% We should support ->/2 better, but there are problems with
% compiled code, this is a tricky business so this doesn't work
%rb$((X -> Y), M, B, S) :- !,
%  rb$(X, M, B, S) -> rb$(Y, M, B, S).
rb$((X ;  Y), M, B, S) :- !,
  (rb$(X, M, B, S) ;  rb$(Y, M, B, S)).
rb$(!, M, B, S) :- !,
  cut$env(B).                                 % Do not move this cut !!!!
rb$(debug64_cut, M, B, S) :- !,
  cut$debug64$env(B).                         % Do not move this cut !!!!
rb$(true, _, _, _) :- !.
rb$(G, M, _, S) :-
  (G = Mod : Goal -> true ;  Mod = M, Goal = G),
  is$code(Mod, Goal, CODE, META_DM), !,
  (
     CODE == 1 ->
     call(Mod : Goal) ;

     get$env(B),
     (
        META_DM == 0 ->
        (Goal = call_nometa(Goal2) -> true ;  Goal2 = Goal) ;

        meta$convert(Mod, Goal, Goal2, META_DM)
     ),
% (write(calling:clause$(Mod,Goal2)), nl; write(failing:clause$(Mod,Goal2)), nl, fail),
     clause$(Mod, Goal2, Body, _, NextMod),
% (write(exiting:clause$(Mod,Goal2)), nl; write(redoing:clause$(Mod,Goal2)), nl, fail),
% (write(calling:rb$(Body)), nl; write(failing:rb$(Body)), nl, fail),
     rb$(Body, NextMod, B, [Goal|S])
% (write(exiting:rb$(Body)), nl; write(redoing:rb$(Body)), nl, fail)
  ).

%% /* older (slower?) rb$
%% rb$(G, M, _, _) :-
%% (G = Mod:Goal ->
%% is$code(Mod,Goal)
%% ;
%% is$code(M,G),
%% Mod:Goal = M:G ),
%% !,
%% call(Mod:Goal).
%% % Keep this as the last clause so we can trim the environment
%% % when the last clause for G is tried
%% rb$(Goal, M, _, S) :-
%% get$env(B), % This is the NEW environment - do not move it !!
%% (Goal = Mod:Head ->
%% ThisMod = Mod
%% ;
%% ThisMod = M,
%% Head = Goal ),
%% (Head = call_nometa(Head2) ->
%% true
%% ;
%% (is$meta(ThisMod, Head, DefMod) ->
%% meta$convert(ThisMod, Head, Head2, DefMod)
%% ;
%% Head2 = Head) ),
%% clause$(ThisMod, Head2, Body, _, NextMod),
%% rb$(Body, NextMod, B, [Goal|S]).
%% */
% Only called from compiled code, with argument which
% is compiled code itself.

meta$call(M : X) :-
  is$meta(M, X, DM),
  (integer(M) -> module$index(MA, M) ;  MA = M),
  meta$convert(MA, X, X2, DM), !,
  call_nometa(MA : X2).
meta$call(X) :-
  get$modix(X, M),
  is$meta(M, X, DM),
  (integer(M) -> module$index(MA, M) ;  MA = M),
  meta$convert(MA, X, X2, DM), !,
  call_nometa(MA : X2).

% need the following so the interpreter can pick up the predicates
% - they are not defined internally by predicates but by compiled
% code sequences so we have to present them explicitly for the
% interpreter. For example, atomic/1 below will be compiled away,
% but the predicate definition will remain here for interpreted
% code to call.

predicate$info(var/1, `var(X)`, `Succeeds if X is an unbound variable`).

var(X) :-
  var(X).

predicate$info(nonvar/1, `nonvar(X)`, `Succeeds if X is not an unbound variable`).

nonvar(X) :-
  nonvar(X).

predicate$info(atomic/1, `atomic(X)`, `Succeeds if X is an atom or integer`).

atomic(X) :-
  atomic(X).

%not(X) :- not(X).

predicate$info(call/1, `call(Goal)`, `Tries to prove Goal, equivalent to simply Goal`).

call(X) :-
  call(X).

%\+(X) :- not(X).

predicate$info(call_nometa/1, ``, ``).

call_nometa(X) :-
  call_nometa(X).

predicate$info((=)/2, `Term1 = Term2`, `Succeeds if Term1 unifies with Term2`).

X = X.

%X \= Y :- not(X = Y).

predicate$info(repeat/0, `repeat`, `Succeeds the first time called, succeeds every time it's backtracked into`).

repeat :-
  repeat.

predicate$info(true/0, `true`, `Succeeds when called, fails on backtracking`).

true.

%(A -> B ; C) :- call(A), !, call(B).
%(A -> B ; C) :- !, call(C).
%(A -> B) :- call(A), !, call(B).

predicate$info(list/1, `list(X)`, `Succeeds if X is a list`).

list(X) :-
  list(X).

predicate$info(compound/1, `compound(X)`, `Succeeds if X is a compound term`).

compound(X) :-
  compound(X).

predicate$info(structure/1, `structure(X)`, `Succeeds if X is a structure`).

structure(X) :-
  structure(X).
  
% These were defined in the compiler with disjunctions, they are now
% just built-in predicates and called from here. This solves many of
% the problems with the optimized compiled versions of these which
% used disjunctions for implementation. That approach was very
% problematic for predicates that had cuts within them, as the scope
% of the cut was poorly defined.
% This experiment failed because of local predicates.
%fail_if(X) :-
% \+(X).

fail_if(X) :-
  call(X), !,
  fail.
fail_if(X).

predicate$info((not)/1, `not(Goal)`, `Succeeds if Goal fails`).

not(X) :-
  call(X), !,
  fail.
not(X).

predicate$info((\+)/1, `\\+Goal`, `Succeeds if Goal fails`).

\+(X) :-
  call(X), !,
  fail.
\+(X).

predicate$info((\=)/2, `Term1 \= Term2`, `Succeeds if Term1 cannot be unified with Term2`).

X \= Y :-
  X = Y, !,
  fail.
X \= Y.

/*
(A -> B ; C) :- call(A), !, call(B).
(A -> B ; C) :- !, call(C).

(A -> B) :- call(A), !, call(B).
*/

%(A -> (B ; C)) :- !, if$then$else(A,B,C).

predicate$info((->)/2, `Goal1 -> Goal2`, `Calls Goal2 if Goal1 succeeds`).

(A -> B) :-
  if$then(A, B).

if$then$else(A, B, _) :-
  call(A), !,
  call(B).
if$then$else(_, _, C) :-
  call(C).

if$then(A, B) :-
  call(A), !,
  call(B).

%----------------------------------------------------------------------
% Start of Built-ins
%
%----------------------------------------------------------------------
% Arithmetic comparison predicates
%
% =\= / 2
% =< / 2
% >= / 2
%----------------------------------------------------------------------

predicate$info((=\=)/2, `ArithExp1 =\= ArithExp2`, `Evaluates both arithmetic expressions, succeeds if they're not equal`).

X =\= Y :-
  not(X =:= Y).

predicate$info((=<)/2, `ArithExp1 =< ArithExp2`, `Succeeds if evaluation of ArithExp1 equal or less than evaluation of ArithExp2`).

X =< Y :-
  not(X > Y).

predicate$info((=>)/2, `ArithExp1 >= ArithExp2`, `Succeeds if evaluation of ArithExp1 greater than or equal to evaluation of ArithExp2`).

X >= Y :-
  not(X < Y).

% Primitives to support naive factorization over the integer domain.
% Depends on builtin makePrimes/4.


predicate$info(primeFactors/2, ``, ``).

primeFactors(N, Factors) :-
  integer(N),
  N > 0,
  primes(Got),
  Needed is integer(floor(1.5e*N/log(N))),
  % Needed is floor(1.5e*N/log(N)),
  (Got > Needed -> Last = Got ;  makePrimes(0, Needed, Last, _)),
  Last1 is Last - 1,
  primeFactorsLoop$(N, 1, Last1, Factors).

primeFactorsLoop$(1, _, _, []) :- !.
primeFactorsLoop$(N, From, Last, [Factor|Rest]) :-
  Root is floor(sqrt(N)),                     % highest possible factor
  (
     for(Pix, 1, Last, 1),
     nthPrime(Pix, Prime),
     Prime > From,
     Prime =< Root,
     0 is N mod Prime,                        % Prime is a factor of N
     Q is N//Prime,                           % quotient
     factorPower$(1, E, Prime, Q, CoFactor),  % Prime**E is factor of N
     (E == 1 -> Factor = Prime ;  Factor = Prime**E),
     primeFactorsLoop$(CoFactor, Prime, Last, Rest) ; % find more

     Factor = N,
     Rest = [], !
  ).


factorPower$(E, E, P, Q, Q) :-                % base case
  0 =\= Q mod P, !.                           % P no longer divides Q
factorPower$(Power, E, P, Q, CoFactor) :-     % E is exponent of P
  Q1 is Q//P,                                 % P does divide Q
  Power1 is Power + 1, !,
  factorPower$(Power1, E, P, Q1, CoFactor).


/*
Primitives to support modulo arithmetic, rationals and continued fractions.

In modulo arithmetic division is accomplished by inverse and multiply.
inverse/3 relates inverse residues through the modulus. It fails if the 
modulus is not prime. 
Instead of having inverseU and inverseS there is just one inverse, based on
modU, which is the typical case. If modS is wanted then the primitive
sig_unsig/2 will convert the result.

euclid/4, euclid1/3 and gcd/2 are various forms of the euclidean algorithm.
The first two arguments to euclid are consolidated into one rational.

q_cf relates a rational quotient to a finite continued fraction, denoted
as a list. Lowest/2 uses q_cf to reduce a quotient to lowest terms.
*/

makePrimes :-                                 % all you need to factorize ints
  X is 2**15,
  makePrimes(0, X, _, _).

realInt(R) :-                                 % R is real and an integer
  real(R),
  nth(0, R, Descr),                           % get descriptor
  Descr >= 0.                                 % Descr has same sign as exp

realDescr(D, L, E, S) :-                      % user not interested in Refs
  realDescr(D, L, E, S, _).

euclid(A/B, G, S, T) :-                       % /4, general purpose
  (B < 0 -> fail ;  true),
  euclid(A/B, 1, 0, 0, 1, G, S, T),
  (A < 0 -> N = - N1 ;  N = N1).              % same sign for A and N

euclid(G/0, S, S1, T, T1, G, S, T).
euclid(A0/A1, S0, S1, T0, T1, G, S, T) :-     % /8
	divmods(A0, A1, Q, R),                      % calculate Q and R together
  S2 is S0 - Q*S1,
  T2 is T0 - Q*T1,
  euclid(A1/R, S1, S2, T1, T2, G, S, T).

euclid1(A/B, G, T) :-                         % /3, optimised for inverse
  euclid1(A/B, 0, 1, G, T).

euclid1(G/0, T, T1, G, T).
euclid1(A0/A1, T0, T1, G, T) :-
   divmodu(A0, A1, Q, A2),
  T2 is T0 - Q*T1,
  euclid1(A1/A2, T1, T2, G, T).

gcd(G/0, G).
gcd(A/B, G) :-                                % greatest common divisor
  R is A modu B,
  gcd(B/R, G).

lcm(A/B, L) :-                                % least common multiple
  gcd(A/B, G),
  L is A*B/G.

inverse(A, I) :-                              % /2 inverse wrt modulo flag
  current_prolog_flag(modulo, M),
  inverse(A, M, I).

inverse(1, _, 1) :- !.
inverse(A, M, I) :-             % multiplicative inverse of A modu M. M prime.
  M > 0,
  (                                           % bilateral
     nonvar(A) ->
     I = Result,
     Input = A ;

     nonvar(I),
     A = Result,
     Input = I
  ),
  euclid1(M/Input, G, T), !,                  % G is gcd(M, A)
  1 =:= (G modu M),                           % G is a unit or fail
  Result is (T divu G) modu M.

q_cf(Q, CF) :-                                % continued fraction to rational
  nonvar(CF), !,
  (
		CF = [cf, [A1, A2|As]] ->
		P1 is A1*A2 + 1,
		epsilon(Epsilon),
		cyclicCnvgt(P1/A2, A1/1, Epsilon, [A1, A2|As], As, Q) ;
		
		(
			 CF = [cf, A1, [A2|As]] ->
			 P1 is A1*A2 + 1, 
			 epsilon(Epsilon), 
			 cyclicCnvgt(P1/A2, A1/1, Epsilon,  [A2|As], As, Q) ;

			 (
				  CF = [cf, A1, A2|As] ->          % length(cf) >= 2
				  P1 is A1*A2 + 1,                 % A1 is 1st cnvgt, P1/A2 is 2nd
				  cnvgt(P1/A2, A1/1, As, Q) ;

				  CF = [cf, Q]                     % length(cf) == 1
			 )
		)
  ).

q_cf(A/B, [cf|CF]) :-                         % rational to continued fraction
  q2cf(A/B, CF).

q2cf(GCD/Zero, []) :- !,
	Zero =:= 0.
%q2cf(GCD/0, []) :- !.
q2cf(X/Y, [A|As]) :- !,
   divmodu(X, Y, A, R),
  q2cf(Y/R, As).

cnvgt(Rat, _, [], Rat).                  % acyclic list is empty, rat is final
cnvgt(P1/Q1, P2/Q2, [[A|As]], Rat) :-  !,       
  P is A*P1 + P2,                             % make new cnvgt from A ...
  Q is A*Q1 + Q2, !,                          % and last two cnvgts
  cyclicCnvgt(P/Q, P1/Q1, As, As, Rat).       % go cyclic
cnvgt(P1/Q1, P2/Q2, [A|As], Rat) :-         
  P is A*P1 + P2,                             % make new cnvgt from A ...
  Q is A*Q1 + Q2, !,                          % and last two cnvgts
  cnvgt(P/Q, P1/Q1, As, Rat).

cyclicCnvgt(C1, C2, Epsilon, Cycle, [], Rat):-  % cycle list is empty, repeat
  cyclicCnvgt(C1, C2, Epsilon, Cycle, Cycle, Rat).
cyclicCnvgt(P1/Q1, P2/Q2, Epsilon, Cycle, [A|As], Rat) :-   
   PQ1 is P1/Q1,
   PQ2 is P2/Q2,    
/*
  ( 
		1/( Q1*Q2) < Epsilon ->
		 Rat = P2/Q2 ;                          % finished

		 P is A*P1 + P2,                        % make new cnvgt from A ...
		 Q is A*Q1 + Q2, !,                     % and last two cnvgts
		 cyclicCnvgt(P/Q, P1/Q1, Epsilon, Cycle, As, Rat)
	).
*/
	( 1/( Q1*Q2) < Epsilon,
	  !,
	  Rat = P2/Q2
	  ;                          % finished
	  P is A*P1 + P2,                % make new cnvgt from A ...
	  Q is A*Q1 + Q2,
	  !,                     % and last two cnvgts
	  cyclicCnvgt(P/Q, P1/Q1, Epsilon, Cycle, As, Rat)
	).

lowest(P/Q, Lowest) :-                        % fraction in lowest terms
  q_cf(P/Q, X),
  q_cf(Lowest, X).

sig_unsig(S, M, U) :-                         % unsigned to signed
  nonvar(U), !,
  M1 is M/2,
  U >= 0,
  U < M,                                      % 0 =< U < M
  (U >= M1 -> S is U - M ;  (U < - M1 -> S is U + M ;  S = U)).
sig_unsig(S, M, U) :-                         % signed to unsigned
  nonvar(S),
  M1 is M/2,
  (S < 0 -> U is S + M ;  U = S).

% fourierPrime(Index, Exp2, Prime, Alpha, J, TwoInverse)
% Exp2 is exponent of 2 in factor of (Prime - 1)
% Alpha is least primitive element of ring
% J is the square root of -1, modulo Prime
% TwoInverse is the mutiplicative inverse of two, modulo Prime
% Range(11) - 4.10 * 10**100

fourierPrime(N, M) :-
  fourierPrime(N, _, M, _, _, _).

fourierPrime( 1, 24, 2130706433,  3,  -16711679, -1065353216).
fourierPrime( 2, 20, 2114977793,  3,  456425726, -1057488896).
fourierPrime( 3, 25, 2113929217,  5,  911673634, -1056964608).
fourierPrime( 4, 21, 2099249153,  3, -217973919, -1049624576).
fourierPrime( 5, 21, 2095054849, 11, -862491351, -1047527424).
fourierPrime( 6, 23, 2088763393,  5,   78423909, -1044381696).
fourierPrime( 7, 20, 2077229057,  3,   -7128919, -1038614528).
fourierPrime( 8, 20, 2070937601,  6,  286480417, -1035468800).
fourierPrime( 9, 20, 2047868929, 13, -461970902, -1023934464).
fourierPrime(10, 20, 2035286017, 10,  105850368, -1017643008).
fourierPrime(11, 27, 2013265921, 31, -284861408, -1006632960).

% Rational relationships
/*
 * Rationals are of the form N/D (D > 0), where N and D are integer or realInt.
 * Should they ever get evaluated they would be demoted to floats or reals, 
 * so comparison must be performed by compareq, not <, >, or =:=.
 */

rational(N/D) :-
  (integer(N) ;  realInt(N)),
  (integer(D) ;  realInt(D)).

conformq(N1/D, N2/D, N1/D, N2/D).
conformq(N1/D1, N2/D2, N3/D, N4/D) :-         % make common denom for sumq
  D is D1*D2,
  N3 is N1*D2,
  N4 is N2*D1.

rationalize(A, B) :-                          % var(A) fails
  nonvar(A),
  (A = _/_ -> B = A ;  (is_integer(A) -> B = A/1 ;  fail) ).

sumq(Addend, Augend, Sum) :-                  % bilateral
  (
     rationalize(Addend, Add) ->
     (
        rationalize(Augend, Aug) ->
        conformq(Add, Aug, N1/D, N2/D),
        N is N1 + N2,
        lowest(N/D, Sum) ;

        rationalize(Sum, S),
        conformq(Add, S, N1/D, N2/D),
        N is N2 - N1,
        lowest(N/D, Augend)
     ) ;

     rationalize(Sum, S),
     rationalize(Augend, Aug),
     conformq(Aug, S, N1/D, N2/D),
     N is N2 - N1,
     lowest(N/D, Addend)
  ).

diffq(Subtrahend, Minuend, Difference) :-
  sumq(Difference, Minuend, Subtrahend).

prodq(AND, IER, PROD) :-                      % bilateral
  (
     rationalize(AND, N1/D1) ->
     (
        rationalize(IER, N2/D2) ->
        N is N1*N2,
        D is D1*D2,
        lowest(N/D, PROD) ;

        rationalize(PROD, N2/D2),
        N is N2*D1,
        D is N1*D2,
        lowest(N/D, IER)
     ) ;

     rationalize(PROD, N2/D2),
     rationalize(IER, N1/D1),
     N is N2*D1,
     D is N1*D2,
     lowest(N/D, AND)
  ).

divq(Dividend, Divisor, Quotient) :-
  prodq(Quotient, Divisor, Dividend).

compareq(A, B, Discrim) :-
  rational(A),
  rational(B),
  conformq(A, B, A1/D, B1/D),
  compared(A1, B1, Discrim).

compared(A, B, =) :-
  A =:= B.
compared(A, B, <) :-
  A < B.
compared(A, B, >) :-
  A > B.

%----------------------------------------------------------------------
% Consulting files
%
% consult_project/1
% reconsult_project/1
% consult/1
% reconsult/1
% consult_ops/1
% load/1
% import/2
% export/1
% set$discontiguous/1
% set$sorted/1
% set$$indexed/1
%----------------------------------------------------------------------

consult_project(Proj) :-
  file$name(Proj, ppj, PPJ),
  read$project(PPJ),
  reconsult$project.

reconsult_project(Proj) :-
  file$name(Proj, ppj, PPJ),
  read$project(PPJ),
  reconsult$project.

predicate$info(consult/1, `consult(FileA)`, `Consult source, object or load file, adding it onto the dynamic database`).

consult(X) :-
  var(X), !,
  fail.
consult([H|T]) :- !,
  consult$(H),
  consult(T).
consult([]) :- !.
consult(user) :-
  repeat,
  prompt$(`| `),
  current_input(H),
  read(H, X),
  exp$term(X, XE),
  do$cons(XE), !.
consult(X) :-
  consult$(X).

consult$(F) :-
  not( (atom(F); string(F)) ),
  err$exec(instanceE, $consult argument must be atom or string: $, F, consult(F)).
consult$(Fname1) :-
  tilt_slashes(Fname1, Fname),
  not Fname = user,
  file$name(Fname, pro, F),
  file_exists(F, Type),
  (
     Type == 1 ->
     load(F) ;

     (                                        % see(H, F),
            %fopen(H, F, r), %catch(consult$read(H), E, consult$except(E, H)),
        open(F, read, H),
        sys$assert('{sys}open$consult$file'(H)),
        % wide likes to catch the read error directly
        (sys$clause('{sys}wide') -> consult$read(H); catch(consult$read(H), E, (close(H),throw(E))) ),
%        consult$read(H),
        sys$retract('{sys}open$consult$file'(H)), % fclose(H)
        close(H),
        true
     )
  ), !.
consult$(Fname) :-
  file$name(Fname, plm, F),
  catch(load(F), _, fail).   % simply fail for now, need to rethink this.

consult$read(H) :-
  repeat,
  read(H, X),
  exp$term(X, XE),
  check$term(XE),
  do$cons(XE).

consult$except(E, H) :-
  close(H),
  throw(E).

predicate$info(reconsult/1, `reconsult(FileA)`, `Reconsult, replacing old predicate definitions, the source, object or load file File`).

reconsult(X) :-
  var(X), !,
  fail.
reconsult([]) :- !.
reconsult([H|T]) :-
  reconsult$(H),
  reconsult(T).
reconsult(user) :- !,
  % sys$retractall('{sys}done$'(_)),
  sys$abolish('{sys}done$'/1),
  repeat,
  write(`|* `), 
  current_input(H),
  read(H, X),
  exp$term(X, XE),
  check$term(XE),
  do$recons(XE), !.
reconsult(X) :-
  reconsult$(X).

reconsult$(Fname1) :-
  tilt_slashes(Fname1, Fname),
  file$name(Fname, pro, F),
  file_exists(F, Type),
  (
     Type == 1 ->
     load(F) ;                                % not source

     reconsult_file(F),                       % source
     % sys$retractall('{sys}done$'(_))
     sys$abolish('{sys}done$'/1)
  ), !.
reconsult$(Fname) :- !,
  fail.

predicate$info(load/1, `load(FileAL)`, `Loads one or a list of compiled files, File, into the static database`).

load(X) :-
  var(X), !,
  fail.
load([]) :- !.
load(A) :-
  atom(A),
  file$name(A, plm, FA),      %load_code$(FA), !.
%  load$file(FA),
  ensure_loaded(FA),
  !.
load([H|T]) :-
  load(H),
  load(T).

load(file, F) :-
%  load$file(F).
  ensure_loaded(F).

predicate$info(load/4, `load(memory, FilenameA, SizeN, AddressN)`, `Loads a plm file from the specified block of memory`).

load(memory, NAME, SIZE, ADDRESS) :-
  load$memory(NAME, SIZE, ADDRESS).

predicate$info(reload/1, `reload(FileAL)`, `Reload one or a list of compiled files, File, into the static database`).

reload(X) :-
  unload(X),
  load(X).

predicate$info(unload/1, `unload(FileAL)`, `Unloads one or a list of compiled files, File, from the static database`).

unload(X) :-
  var(X), !,
  fail.
unload([]) :- !.
unload(A) :-
  atom(A),
  file$name(A, plm, FA),
  unload_code$(FA), !.
unload([H|T]) :-
  unload(H),
  unload(T).

predicate$info((import)/2, ``, ``).

import(MOD, P/A) :- !,
  import$2(MOD, P, A).
import(MOD, []) :- !.
import(MOD, [P/A|Z]) :-
  import$2(MOD, P, A), !,
  import(MOD, Z).
import(MOD, X) :-
  err$exec(instanceE, $Bad argument for import: $, MOD : X, import(MOD, X)).


set$sorted([]).
set$sorted([H|T]) :-
  set$sorted(H),
  set$sorted(T).
set$sorted( (A,B) ) :-
  set$sorted(A),
  set$sorted(B).
set$sorted(P/A) :-
  set$sorted(P, A).

set$$indexed([]).
set$$indexed([H|T]) :-
  set$$indexed(H),
  set$$indexed(T).
set$$indexed( (A,B) ) :-
  set$$indexed(A),
  set$$indexed(B).
set$$indexed(P) :-
  set$indexed(P).

% ----- Locals -----

do$cons(quit) :- !.
do$cons('end_of_file') :- !.
do$cons(:- ([H|T])) :- !,
   consult([H|T]), !,
   fail.
do$cons(:-(module(M))) :-
   module$(M), !, fail.
do$cons(:-(end_module(M))) :-
   end_module$(M), !, fail.
do$cons(:-(body(M))) :-
   module$(M), !, fail.
do$cons(:-(end_body(M))) :-
   end_module$(M), !, fail.
do$cons(:-(import(M))) :-
   import(M), !, fail.
do$cons(:-(import(M,L))) :-
   import(M,L), !, fail.
do$cons(:-(export(L))) :-
   export(L), !, fail.
do$cons(:-(metapredicate(MP))) :-
   meta$assert(MP), !, fail.
do$cons(:-(sorted(P))) :-
   set$sorted(P), !, fail.
do$cons(:-(indexed(P))) :-
   set$$indexed(P), !, fail.
% not implemented for compiler, lets wait til then
% but is tricky for compiler, because it can't simply become
% a latent expression as all the clauses might not be loaded
% yet, or rather, it could only be a latent expression added
% at the end of the code, not the beginning...in which case
% its just latent expressions...
%do$cons(:-(initialization(Body))) :-   % redundant standard directive
%   do$cons(:- (call(Body))), !,
%   fail.
do$cons(:-(include(File))) :- 
   consult(File), !,  
   fail.
% need to implement for compiler as well, until then
% lets leave it out.
%do$cons(:-(ensure_loaded(File))) :-
%   (is$loaded(File) -> true ; consult(File)), !,
%   fail.
do$cons(:-(noNonTerminals)) :-
   sys$assertz('{sys}no$nt'), !,
   fail.
do$cons(:-(nonTerminal(F1/A))) :-
   sys$assertz('{sys}is$nt'(F1, A)), !,
   fail.
do$cons(:-(nonTerminal((F1/A, F2)))) :-         % can be many
   sys$assertz('{sys}is$nt'(F1, A)),
   do$cons(:-(nonTerminal(F2))), !.
do$cons(:-(dcg_terminal(F))) :-                 % usr-supplied terminal/3
   sys$assertz('{sys}is$t'(F)), !,
   fail.                                        % can only be one
do$cons(:- X) :- !,
  loading_module(M),
  call(M : X), !,
  fail.
do$cons(?- X) :- !,
  fail.                                       % ignore ?- on consult
do$cons( (M:P :- B) ) :- !,
  (current_module(M) -> true; 
     err$exec(execE, `module not defined: `, M, assert(M:P)) ),
  consult$assertz(M, (P:-B)),
  !, fail.
do$cons( (M:P) ) :- !,
  (current_module(M) -> true; 
     err$exec(execE, `module not defined: `, M, assert(M:P)) ),
  consult$assertz(M, P),
  !, fail.
do$cons(X) :-
  loading_module(M),
  consult$assertz(M, X), !,
  fail.

/*
reconsult_wfile(File) :-                      %fopen(H, File, rb),
  open(File, read, H, [type(wide_text)]),
  repeat,
  read(H, X),
  exp$term(X, XE),
  do$recons(XE),
  close(H), !.
*/

reconsult_file(File) :-
   %fopen(H, File, r), %catch( reconsult$read(H), E, reconsult$except(E, H) ),
  open(File, read, H), %catch( reconsult$read(H), E, reconsult$except(E, H) ),
  sys$assert('{sys}open$consult$file'(H)),
  reconsult$read(H),
  sys$retract('{sys}open$consult$file'(H)),
  close(H), !.

reconsult$read(H) :-
  repeat,
  read(H, X),                                 % read
  exp$term(X, XE),                            % expand
  do$recons(XE).                              % only succeed on eof or quit

reconsult$except(E, H) :-
  close(H),
  throw(E).

do$recons(quit) :- !.                           % repeat stopper for reconsult
do$recons('end_of_file') :- !.                  % repeat stopper for reconsult
do$recons(:-(nonTerminal(F1/A))) :-
   sys$assertz('{sys}is$nt'(F1, A)), !,
   fail.
do$recons(:-(nonTerminal((F1/A, F2)))) :-       % can be many
   sys$assertz('{sys}is$nt'(F1, A)),
   do$recons(:-(nonTerminal(F2))), !.
do$recons(:-(dcg_terminal(F))) :-               % usr-supplied terminal/3
   sys$assertz('{sys}is$t'(F)), !,              % can only be one 
   fail.
do$recons(:-(module(M))) :-
   module$(M), !, fail.
do$recons(:-(end_module(M))) :-
   end_module$(M), !, fail.
do$recons(:-(body(M))) :-
   module$(M), !, fail.
do$recons(:-(end_body(M))) :-
   end_module$(M), !, fail.
do$recons(:-(import(M))) :-
   import(M), !, fail.
do$recons(:-(import(M,L))) :-
   import(M,L), !, fail.
do$recons(:-(export(L))) :-
   export(L), !, fail.
do$recons(:-(metapredicate(MP))) :-
   meta$assert(MP), !, fail.
do$recons(:-(sorted(P))) :-
   set$sorted(P), !, fail.
do$recons(:-(indexed(P))) :-
   set$$indexed(P), !, fail.
%do$recons(:-(initialization(Body))) :-
%   do$recons(:- (call(Body))), !,
%   fail.
do$recons(:-(include(File))) :- 
   reconsult(File), !,  
   fail.
do$recons(:- X) :- !,
  loading_module(M),
  call(M : X), !,
  fail.
do$recons(?- X) :- !,
  fail.
do$recons( (M:P :- B) ) :- !,
  (current_module(M) -> true; 
     err$exec(execE, `module not defined: `, M, assert(M:P)) ),
  t$2(M : P),
  consult$assertz(M, (P:-B)),
  !, fail.
do$recons( (M:P) ) :- !,
  (current_module(M) -> true; 
     err$exec(execE, `module not defined: `, M, assert(M:P)) ),
  t$2(M : P),
  consult$assertz(M, P),
  !, fail.
do$recons(X) :-
  hd$$(X, H),
  loading_module(M),
  t$2(M : H),
  consult$assertz(M, X), !,
  fail.

hd$$((H :- B), H) :- !.
hd$$(H, H).

t$2(M:H) :-
   sys$clause('{sys}done$'(M:H)), !.       % not 1st cls with this head  
t$2(M:H) :-
   functor(H, F, A),                       % is 1st cls with this head
   functor(Proc, F, A),
   sys$asserta('{sys}done$'(M:Proc)),      % assert guard of most general type
   abolish(M:F/A), !.                      % and kill any clses that were there
%   retractall(M:Proc), !.      % don't abolish predicate, just remove clauses

t$2(M : H) :-
  sys$clause('{sys}done$'(M : H)), !.         % not 1st cls with this head 
t$2(M : H) :-
  functor(H, F, A),                           % is 1st cls with this head
  functor(Proc, F, A),
  sys$asserta('{sys}done$'(M : Proc)),     % assert guard of most general type
  abolish(M:F/A), !. % and kill any clses that were there
  % retractall(M : Proc), !.      % don't abolish predicate, just remove clauses
% add a default extension to a filename if none exists

file$name(FName, Ext, FNameExt) :-
  fname_charlist(FName, FNameL),
  get$ext(FNameL, X),                         % get extension
  var(X), !,                                  % there was no extension
  atom_codes(Ext, ExtL),
  append$(FNameL, [0'.|ExtL], FNameExtL),
  atom_codes(FNameExt, FNameExtL).
file$name(FName, _, FNameA) :-
  fname_charlist(FName, FNameL),
  atom_codes(FNameA, FNameL).

% get the file extension. 
% do not to be confused by directory names with extensions

get$ext(FName, Ext) :-
  reverse$(FName, RFName),
  append$(RExt, [0'.|RPathName], RFName),
  not(member$(0'\, RExt)),
  not(member$(0'/, RExt)),
  not(member$(0'], RExt)),
  reverse$(RExt, Ext), !.
get$ext(_, _).

fname_charlist([H|T], [H|T]) :- !.
fname_charlist(FNameA, FNameL) :-
  atom(FNameA), !,
  atom_codes(FNameA, FNameL).
fname_charlist(FNameS, FNameL) :-
  string(FNameS), !,
  string_list(FNameS, FNameL).

% Many common errors in Prolog are legal terms. 
% We are offering this check to find those wierd terms
% caused by bad punctuation (periods and commas misplaced).
% consult(F) not actually used by err$exec, and this is
% called by compiler as well, needs some more thinking.

check$term((A, B)) :-
  % sys$clause('{sys}open$consult$file'(F)),
  err$exec(syntaxE, $Probable missplaced period before: $, (A, B), 
          consult(F)).
check$term((A ;  B)) :-
  % sys$clause('{sys}open$consult$file'(F)),
  err$exec(syntaxE, $Probable missplaced period before: $, (A ;  B), 
          consult(F)).
check$term(((A, B) :- _)) :-
  % sys$clause('{sys}open$consult$file'(F)),
  err$exec(syntaxE, $Probable missplaced period before: $, (A, B), 
          consult(F)).
check$term(((A ;  B) :- _)) :-
  % sys$clause('{sys}open$consult$file'(F)),
  err$exec(syntaxE, $Probable missplaced period before: $, (A ;  B), 
          consult(F)).
check$term(((A :- B) :- _)) :-
  % sys$clause('{sys}open$consult$file'(F)),
  err$exec(syntaxE, $Probable missplaced period around: $, (A :- B), 
          consult(F)).
check$term((A :- B)) :-
  check$body(B, BADB),
  % sys$clause('{sys}open$consult$file'(F)),
  err$exec(syntaxE, $Probable missing period before: $, BADB, consult(F)).
check$term(_).

% succeeds if problem, fails if OK

check$body(X, X) :-
  var(X),
  !, fail.
check$body(X, (C :- D)) :-
  nonvar(X),
  X = (C :- D), !.
check$body((X, _), (C :- D)) :-
  nonvar(X),
  X = (C :- D), !.
check$body((_, REST), X) :- !,
  check$body(REST, X).

consult$assertz(M, (H :- B)) :- !,
  meta$goals(M, B, B2),
  assert$(M, 0'z, (H :- B2)).
consult$assertz(M, X) :-
  assert$(M, 0'z, X).

% The idea is to preprocess consulted clauses as
% the compiler does, but problem is we still can't
% guarantee we've got them, so can't take out
% the call to is$meta in the rb$ loop.

meta$goals(M, X, X) :-
  var(X), !.
meta$goals(M, (A, B), (A2, B2)) :- !,
  meta$goals(M, A, A2),
  meta$goals(M, B, B2).
meta$goals(M, (A ;  B), (A2 ;  B2)) :- !,
  meta$goals(M, A, A2),
  meta$goals(M, B, B2).
meta$goals(M, (A -> B ;  C), (A2 -> B2 ;  C2)) :- !,
  meta$goals(M, A, A2),
  meta$goals(M, B, B2),
  meta$goals(M, C, C2).
meta$goals(M, G, G4) :-
  is$meta(M, G, DM), !,
  (
     mcheck$convert(M, G, G3, DM) ->
     G4 = call_nometa(G3) ;

     G4 = (meta$convert(M, G, G2, DM), call_nometa(G2))
  ).
meta$goals(_, G, G).

%----- Project support -----

read$project(PPJ) :-
  % sys$retractall('{sys}project'(_, _)),
  sys$abolish('{sys}project'/2),
  sys$assertz('{sys}project'(`project`, PPJ)), %fopen(H, PPJ, r),
  open(PPJ, read, H),
  repeat,
  read_string(H, ATTRVAL),
  add$ppjattrval(ATTRVAL),
  close(H).

add$ppjattrval('end_of_file') :- !.
add$ppjattrval(ATTRVAL) :-
  string_tokens(ATTRVAL, [ATTR, '=', VAL], $=$),
  sys$assertz('{sys}project'(ATTR, VAL)), !,
  fail.

consult$project :-
  sys$clause('{sys}project'(library, LIB)),
  load(LIB),
  fail.
consult$project :-
  sys$clause('{sys}project'(file, FILE)),
  consult(FILE),
  fail.
consult$project.

reconsult$project :-
  sys$clause('{sys}project'(library, LIB)),
  load(LIB),
  fail.
reconsult$project :-
  sys$clause('{sys}project'(file, FILE)),
  reconsult(FILE),
  fail.
reconsult$project.

%----------------------------------------------------------------------
% Counter Manipulation
%
% cntr_set/2
% cntr_get/2
% cntr_inc/2
% cntr_dec/2
%----------------------------------------------------------------------

predicate$info(cntr_set/2, `cntr_set(CounterN, ValueN)`, `Set the value of Counter to Value`).

cntr_set(Num, Val) :-
  cntr$(Num, 1, Val).

predicate$info(cntr_get/2, `cntr_get(CounterN, ValueV)`, `Unify Value with current value of Counter`).

cntr_get(Num, Val) :-
  cntr$(Num, 0, Val).

predicate$info(cntr_inc/2, `cntr_inc(CounterN, ValueV)`, `Unify Value with current value of Counter, then increment Counter`).

cntr_inc(Num, Val) :-
  cntr$(Num, 0, Val),
  Valinc is Val + 1,
  cntr$(Num, 1, Valinc).

predicate$info(cntr_dec/2, `cntr_dec(CounterN, ValueV)`, `Unify Value with current value of Counter, then decrement Counter`).

cntr_dec(Num, Val) :-
  cntr$(Num, 0, Val),
  Valdec is Val - 1,
  cntr$(Num, 1, Valdec).

%---------------------------------------------------------------------- 
% Database manipulation predicates
%
% new_module/1
% assert/1
% asserta/1
% assertz/1
% retract/1
% clause/2
% retractall/1
% abolish/1
% get_preds/1
% current_module/1
% current_predicate/1
% predicate_property/2
% defined/1
%---------------------------------------------------------------------- 

predicate$info(new_module/1, ``, ``).

new_module(M) :-
  module$(M),
  end_module$(M).

predicate$info(assert/1, `assert(TermT)`, `Adds Term to the dynamic database`).

assert(X) :-
  assert$$(X, 0'z).

predicate$info(asserta/1, `asserta(TermT)`, `Adds Term to the dynamic database as first clause with its functor`).

asserta(X) :-
  assert$$(X, 0'a).

predicate$info(assertz/1, `assertz(TermT)`, `Adds Term to the dynamic database as last clause with its functor`).

assertz(X) :-
  assert$$(X, 0'z).

% assert is a metapredicate,
% so module added if missing

assert$$((MOD : HEAD :- BODY), AZ) :-
  !,
  (current_module(MOD) ->
     true
     ;
     err$exec(execE,
              `undefined module specification in assert: `, MOD,
              assert((MOD : HEAD :- BODY))) ),
  assert$(MOD, AZ, (HEAD :- BODY)).
assert$$(MOD : HEAD, AZ) :-
  !,
  (current_module(MOD) ->
     true
     ;
     err$exec(execE,
              `undefined module specification in assert: `, MOD,
              assert(MOD : HEAD)) ),
  assert$(MOD, AZ, HEAD).
assert$$(X, _) :-
  err$exec(execE, `bad module specification in assert: `, X, assert(X)).

% retract/1 is a metapredicate, so the module
% qualification will always be added to the argument
% before it is called.  Like clause, it must use
% a repeat before the retract$db call.
%
% note that first clause does not subsume second, because
% :- is domiment operator, not :.

predicate$info(retract/1, `retract(Term)`, `Retract the first term in the database that unifies with Term; on backtracking retract the next`).

retract(MOD : HEAD) :-
  nonvar(HEAD),
  !,
  retract$fact(MOD, HEAD, HEAD).
retract((MOD : HEAD :- BODY)) :-
  nonvar(HEAD),
  !,
  retract$rule(MOD, HEAD, (HEAD :- BODY)).
retract(M : X) :-
  err$exec(instanceE, `functor of clause to retract must be bound: `,
           X, retract(X)).

retract$rule( MOD, HEAD, (HEAD :- BODY) ) :-
  repeat,
  retract$db(MOD, HEAD, (HEAD :- BODY)).

retract$fact(MOD, HEAD, HEAD) :-
  repeat,
  retract$db(MOD, HEAD, HEAD).
  

% NOTE! clause$db is a strange duck, it must be
% called in a backtracking loop to get all answers,
% but when it fails it does an FCut, skipping
% over the repeat, defying all normal rules of
% Prolog gravity. So if you forget the repeat
% it skips over whatever else was there instead.
% Two versions, the $ version is for internal
% use and doesn't show up on call stack reports.

predicate$info(clause/2, `clause(Head, BodyV)`, `Designed to backtrack through all the clauses in the dynamic database`).

clause(MOD : X, Y) :-
  var(X), !,
  current_predicate(MOD : F/A),
  functor(X, F, A),
  clause(MOD : X, Y).
clause(MOD : HEAD, BODY) :- !,
  repeat,
  clause$db(MOD, HEAD, BODY, _, _).

clause$(MOD, HEAD, BODY, N) :-
  repeat,
  clause$db(MOD, HEAD, BODY, N, _).

clause$(MOD, HEAD, BODY, N, NEWMOD) :-
  repeat,
  clause$db(MOD, HEAD, BODY, N, NEWMOD).

% retractall(M:F(A1,...,An)) will retract just clauses that
% match the pattern.

predicate$info(retractall/1, `retractall(Term)`, `Retract all dynamic database terms that unify with Term`).

retractall(T) :-
   retract(T),
   fail.
retractall(_).

% abolish(M:F/A) abolishes all clauses for the predicate.

predicate$info(abolish/1, `abolish(NameA/ArityN)`, `Delete all clauses whose head is Name/Arity`).

/* If we have another tech support problem caused by abolish, then do this instead:

abolish(M:F/A) :-
  !,
  functor(T,F,A),
  retractall(M:T).
abolish(F/A) :-
  !,
  functor(T,F,A),
  retractall(T).
  
and maybe create a new dangerous_abolish/1 that does the real thing. */

abolish(P) :-
  abolish$(P).


% Versions of metapredicates with added module argument
% that avoid use of metapredicates.
% This is because metapredicates are forced through the interpreter
% for metapredicate processing, which means the debugger winds
% up debugging itself.

predicate$info(asserta/2, `asserta(ModuleA, TermT)`, `Adds Term to the dynamic database as the first clause with its functor in the specified Module`).

asserta(M, X) :-
  assert$(M, 0'a, X).

predicate$info(assert/2, `assert(ModuleA, TermT)`, `Adds Term to the dynamic database in the specified Module`).

assert(M, X) :-
  assert$(M, 0'a, X).

predicate$info(assertz/2, `assertz(ModuleA, TermT)`, `Adds Term to the dynamic database as the last clause with its functor in the specified Module`).

assertz(M, X) :-
  assert$(M, 0'z, X).

predicate$info(retract/2, `retract(ModuleA, TermT)`, `Deletes Term from the dynamic database in the specified Module`).

retract(M, X) :-
  !,
  repeat,
  retract$db(M, X, X).

predicate$info(retractall/2, `retractall(ModuleA, TermT)`, `Deletes all terms from the dynamic database in the specified Module that unify with Term`).

retractall(M, X) :-
  retract(M, X),
  fail.
retractall(_, _).

% M2 is the defining module for the called goal
% CN is the clause number

clause(M1, H, B, CN, M2) :-
  clause$(M1, H, B, CN, M2).% current$module is like clause, must have a repeat before
% it, so recognize two cases, one checking and the other
% getting all, and if we get all, don't return system ones.

predicate$info(current_module/1, `current_module(ModuleV)`, `Unifies Module with the currently defined modules in the logicbase; if Module is a variable, on backtracking, returns all the user-defined modules`).

current_module(M) :-
  nonvar(M),
  !,
  repeat,
  current$module(M).
current_module(M) :-
  repeat,
  current$module(M),
  not system$module(M).

predicate$info(current_predicate/1, `current_predicate(ModuleA:FunctorV/ArityV`, `Unifies its argument with the Functors and Arities in the specified Module; the Module must be bound to a valid module name`).

current_predicate(P/A) :-
  !,
  get$pred(PTR, user, P1, A1),
  current$predicate(PTR, user, P1, A1, P/A).
current_predicate(M : P/A) :-
  nonvar(M),
  !,
  get$pred(PTR, M, P1, A1),
  current$predicate(PTR, M, P1, A1, P/A).
current_predicate(X) :-
  err$exec(execE, $bad module specification in current_predicate $,
           X, current_predicate(X)).
   

current$predicate(PTR, _, P, A, P/A).
current$predicate(PTR, M, _, _, P/A) :-
  get$pred(PTR, M, P1, A1),
  current$predicate(PTR, M, P1, A1, P/A).

% built-in used by wide debugger, spy,
% is used by Delphi sample, proxs,
% so update that sample before removing it.  should
% be deprecated.  NOTE - if it calls the metapredicate
% findall/3, the debugger gets wrapped around the axle,
% so use non-metapredicate version findall$/3.

get_preds(Ls) :-
  findall(M : X, dynamic$predicate(M : X), Ls).

dynamic$predicate(M : X) :-
  current_module(M),
  not system$module(M),
  current_predicate(M : X),
  predicate_property(M : X, dynamic).

predicate$info(predicate_property/2, `predicate_property(ModuleA:FunctorA/ArityN, PropsL)`, `Returns the list of properties for the specified predicate Functor/Arity in Module`).

predicate_property(M : P/A, PROP) :-
  nonvar(M), nonvar(P), nonvar(A),
  !,
  predicate$property(M : P/A, LIST),
  member$(PROP, LIST),
  (
     PROP = metapredicate(MP) ->
     once member$(defined_in(DM), LIST),
     meta$get$mode(DM : P/A, MP) ;

     true
  ).
predicate_property(M:F, PROP) :-
  nonvar(M), nonvar(F),
  functor(F, P, A),
  !,
  predicate_property(M:P/A, PROP).
predicate_property(P/A, PROP) :-
  nonvar(P), nonvar(A),
  !,
  predicate_property(user:P/A, PROP).
predicate_property(F, PROP) :-
  nonvar(F),
  functor(F, P, A),
  !,
  predicate_property(user:P/A, PROP).
predicate_property(X, _) :-
  err$exec(instanceE, $arg 1 must be bound to predicate form, module:functor/arity$,
           X, predicate_property(X,_)).

predicate_info(M:F/A, ARGS, DESC) :-
  predicate$info(F/A, ARGS, DESC).
predicate_info(M:F/A, ARGS, DESC) :-
  findall(mfad(M,F,A,ARGS,DESC), (current_predicate(M:F/A), predicate$enginfo(M:F/A, ARGS, DESC)), L),
  !,
  member$(mfad(M,F,A,ARGS,DESC), L).

predicate$info(built_in/1, `built_in(Predicate)`, `Succeeds if predicate is an Amzi! system one`).

built_in(X) :-
  current_predicate(amzi_system:X),
  predicate_property(amzi_system:X, exported).

predicate$info(defined/1, `defined(Predicate)`, `Succeeds if predicate is defined`).

defined(X) :-
  defined$(X).

predicate$info(interpret/1, `interpret(Module:Functor)`, `Succeeds if the predicate is in the dynamic database in the specified module`).

interpret(M : X) :-
  repeat,
  interpret$db(M : X).

% ----- local predicates ----- 
% Splits clause to get head functor and arity and checks to see
% If the clause being (re)defined is protected.

get$key((H :- B), Name) :- !,
  functor(H, Name, Arity).                    % prot$db(Name, Arity).
get$key(H, Name) :-
  functor(H, Name, Arity).                    % prot$db(Name, Arity).

get$head((H :- B), H) :- !.
get$head(H, H).

%----------------------------------------------------------------------
% Display predicates
%
% display/1
% listing/0
% listing/1
% pp/1
%----------------------------------------------------------------------

predicate$info(display/1, `display(Term)`, `Displays Term without expanding operators`).

display(V) :-
  current_output(H),
  display(H, V).
  
predicate$info(display/2, `display(ID, Term)`, `Displays Term to stream ID without expanding operators`).

display(H, V) :-
  var(V),
  write(H, V), !.
display(H, V) :-
  real(V),
  !,
  write(H, V), write(H, 'r').
display(H, T) :-
  functor(T, F, A),
  A > 0,
  T =.. [F|As],
  writeq(H, F), write(H, '('), 
  display$args(H, As),
  write(H, ')'), !.
display(H, A) :-
  writeq(H, A).

predicate$info(listing/0, `listing`, `List all the clauses in the dynamic database`).

listing :-
  list$all.

predicate$info(listing/2, `listing(ID, Predicate)`, `Lists all the clauses for the Predicate in the dynamic database`).

listing(H, X) :-
  current_output(COUT),
  set_output(H),
  listing(X),
  set_output(COUT).

predicate$info(listing/1, `listing(ModuleA:FunctorA/ArityN)`, `Lists all the clauses for Functor/Arity in Module in the dynamic database`).

listing(MOD : NAME/ARITY) :- !,
  list$ing(MOD : NAME/ARITY).
listing(NAME/ARITY) :- !,
  list$ing(NAME/ARITY).
listing(MOD : NAME) :- !,
  list$mod$name(MOD : NAME).
listing(M) :-
  current_module(M), !,
  list$module(M).
listing(NAME) :- !,
  list$name(NAME).

list$all :-
  current_module(M),
  not system$module(M),
  current_predicate(M : P/A),
  predicate_property(M : P/A, dynamic),
  pp$nl,
  list$ing(M : P/A),
  fail.
list$all.

pp$module(user) :- !.
pp$module(M) :-
  ( write(':- module('), writeq(M), write(').'), nl
    ;
    write(':- end_module('), writeq(M), write(').'), nl, fail ).

list$module(M) :-
  current_predicate(M : P/A),
  predicate_property(M : P/A, dynamic),
  pp$nl,
  list$ing(M : P/A),
  fail.
list$module(_).

% Used by ARulesXL, not advertised at this point,
% NOTE - it exports all the predicates to support ARulesXL inheritance
module_listing(M) :-
  pp$module(M),
  current_predicate(M : P/A),
  predicate_property(M : P/A, dynamic),
%  write(':- export('), writeq(P/A), write(').'), nl,
  pp$nl,
  modlist$ing(M : P/A),
  fail.
module_listing(_).

modlist$ing(MOD : NAME/ARITY) :-
  functor(PRED, NAME, ARITY),
  clause(MOD : PRED, BODY),
  pp((PRED :- BODY)),
  fail.
modlist$ing(_).

% also called by ARulesXL to get listings that compile
user_listing(NAME/ARITY) :-
%  pp$module(user),
  functor(PRED, NAME, ARITY),
  clause(user : PRED, BODY),
  pp((PRED :- BODY)),
  fail.
user_listing(_).

list$mod$name(MOD : NAME) :-
  current_predicate(MOD : NAME/X),
  list$ing(MOD : NAME/X),
  fail.
list$mod$name(_).

list$name(NAME) :-
  current_module(M),
  current_predicate(M : NAME/X),
  list$ing(M : NAME/X),
  fail.
list$name(_).

list$ing(MOD : NAME/ARITY) :-
  functor(PRED, NAME, ARITY),
  clause(MOD : PRED, BODY),
  pp((MOD : PRED :- BODY)),
  fail.
list$ing(NAME/ARITY) :-
  functor(PRED, NAME, ARITY),
  clause(PRED, BODY),
  pp((PRED :- BODY)),
  fail.
list$ing(_).

% All of the Amzi! system modules begin with amzi_

system$module(M) :-
  atom_chars(M, [a, m, z, i, '_'|_]).

stub$(X) :-
  write(stubbed(X)), nl.

predicate$info(pp/2, `pp(ID, Term)`, `Pretty prints Term to stream ID`).

pp(H,X) :-
  current_output(COUT),
  set_output(H),
  pp(X),
  set_output(COUT).

predicate$info(pp/1, `pp(Term)`, `Pretty prints Term`).

pp(X) :-
  debug$pp(X), !.
pp(X) :-
  user_pp(X), !.                %% pick up a user pretty printer if it exists 
pp(X) :-
  pp$(X), !.

% ----- Locals -----

display$args(_, []).
display$args(H, [X]) :-
  display(H, X).
display$args(H, [X|Y]) :-
  display(H, X),
  write(H, `,`), 
  display$args(H, Y).

listing$(Name/Arity) :-
  atom(Name),
  not(sys$pred(Name)),
  nl, 
  pp$l,
  integer(Arity),
  functor(Head, Name, Arity),
  clause(Head, Body),
  pp$((Head :- Body)),
  fail. /* to pick up all clauses */
listing$(N/A).

sys$pred(A) :-
  atom_codes(A, [0'{, 0's, 0'y, 0's, 0'}|_]).

% can't use numbervars anymore because writeq now quotes
% atoms beginning with _, so need a vastly changed pp$,
% or special flag for a variable which is an atom for writeq,
% or, just like it is, live with H341 and the like for vars.
% clearly the best solution is to save names and use real
% variable names.

pp$(X) :-
  numbervars(X, 1, _),
  pp$w(X),
  write(`.\n`), 
  pp$l, !.

pp$w(X) :-
  var(X), !,
  write(X).
pp$w(call_nometa(X)) :-
  !, pp$w(X).
pp$w((A :- true)) :- !,
  pp$w(A).
pp$w((A :- B)) :- !,
  pp$w(A),
  write(` :- \n`), 
  pp$l,
  write(`    `), 
  pp$w(B).
pp$w((A ;  B)) :- !,
  write(`(\n    `), 
  pp$w(A),
  write(`\n     ;\n    `), 
  pp$w(B),
  write(`\n    )`).
pp$w((A, B)) :- !,
  pp$w(A),
  write(`,\n`), 
  pp$l,
  write(`    `), 
  pp$w(B).
pp$w(A) :-
  writeq(A).

pp$l :-
  cntr_get(21, N),
  NN is N - 1,
  pp$do(NN).

pp$do(0) :-
  write(`[More?]`), 
  respkey(C),
  pp$c(C),
  pp$init.
pp$do(N) :-
  N > 0, !,
  cntr_set(21, N).
pp$do(_).

pp$nl :-                                % a quick kludge so others can use it.
  nl, 
  cntr_get(21, N),
  NN is N - 1,
  pp$nldo(NN).

pp$nldo(0) :-
  write(`[More]`), 
  respkey(0'y),
  pp$c(C),
  pp$init.
pp$nldo(N) :-
  N > 0, !,
  cntr_set(21, N).
pp$nldo(_).

pp$init :-
  telling(CurOut),
  stream_type(CurOut, SType),
  pp$height(CurOut, SType).

pp$height(CurOut, 0) :-                       % NOTIO
  cntr_set(21, 23).                           % stdout
pp$height(CurOut, 1) :-                       % FILEIO
  cntr_set(21, -1).                           % a file, go forever
pp$height(CurOut, 2) :-                       % WINDOW
  cntr_set(21, 10).                           % set to 10 if no wn_attr
pp$height(CurOut, 3) :-                       % FUNCTIO
  cntr_set(21, -1).                           % a function, go forever

pp$c(0'N) :-
  cut_tag(listing_end),
  fail.
pp$c(0'n) :-
  cut_tag(listing_end),
  fail.
pp$c(0'Q) :-
  cut_tag(listing_end),
  fail.
pp$c(0'q) :-
  cut_tag(listing_end),
  fail.
pp$c(_).

%----------------------------------------------------------------------
% File handling
%
% fopen/3
% fclose/1
% file_exists/1
% file_exists/2
% findfiles/3
%----------------------------------------------------------------------
% valid modes are 'read' 'write' 'append'

predicate$info(open/3, `open(FileACS, ModeA {read, write, readwrite, append}, ID)`, `Opens a text File in Mode on stream ID`).

open(Name, Mode, H) :-
  open(Name, Mode, H, []).

predicate$info(open/4, `open(FileACS, ModeA {read, write, readwrite, append}, ID, OptionsL {alias(AliasA), type(TypeA {text, wide_text, binary})})`, `Opens a File in Mode on stream ID with the specified alias atom and file type`).

open(NameIn, Mode, H, OptList) :-
   ( 
		 checkname_atom(NameIn, Name) ->
		 true ;

		 err$exec(typeE, `open name must be atom, string, or charlist`,
		          NameIn, open(NameIn, Mode, H, OptList) ) 
	),
   (member$(reposition(TF), OptList) -> Repos = TF ; Repos = default), % maybe set Repos
   
   (member$(type(TYPE), OptList) ->
      enum(TYPE, [text=1, binary, wide_text], TCODE)
      ;
      TCODE = 4),
   (
       member$(alias(Alias), OptList) -> 
       (
           (current_input(Alias) ; current_output(Alias)) ->
            throw(permission(Name, alias(Alias))) ;

            true
        ) ;

        Alias = Name
    ),
    ( 
		  member$(eof_action(Action), OptList) ; Action = 0), !,
%nl, write(open$(H,Name,Mode,options(TCODE, Repos, Alias, Action))), nl,
		  open$(H, Name, Mode, options(TCODE, Repos, Alias, Action)
	 ).

%type$code(text, 1).
%type$code(binary, 2).
%type$code(wide_text, 3).

enum(ITEM, LIST, NUM) :-
   enum(ITEM, LIST, 0, NUM).

enum(ITEM, [ITEM = NUM|_], _, NUM) :- !.
enum(ITEM, [ITEM|_], NUM, NUM) :- !.
enum(ITEM, [_ = N|Z], _, NUM) :-
   NN is N + 1,
   !, enum(ITEM, Z, NN, NUM).
enum(ITEM, [_|Z], N, NUM) :-
   NN is N + 1,
   enum(ITEM, Z, NN, NUM).

checkname_atom(A, A) :- atom(A), !.
checkname_atom(S, A) :- string(S), string_atom(S,A), !.
checkname_atom(L, A) :- list(L), atom_codes(A,L), !.

% fopen_(Handle, Name, Mode).
% for backwards compatibility

predicate$info(fopen/3, `fopen(HandleV, FileACS, ModeA {r,w,a,rb,wb,ab})`, `Opens file named FileA in read/write mode Mode, unifies Handle with resulting handle`).

fopen(H, N, r) :- open(N, read, H), !.
fopen(H, N, w) :- open(N, write, H), !.
fopen(H, N, a) :- open(N, append, H), !.
fopen(H, N, rb) :- open(N, read, H, [type(binary)]), !.
fopen(H, N, wb) :- open(N, write, H, [type(binary)]), !.
fopen(H, N, ab) :- open(N, append, H, [type(binary)]), !.
fopen(H, N, wu) :- open(N, write, H, [type(wide_text)]), !.
fopen(H, N, ru) :- open(N, read, H, [type(wide_text)]), !.

predicate$info(close/1, `close(ID)`, `Closes stream ID`).

close(FileID) :-                                % std
   close$(FileID).

predicate$info(close/2, `close(ID, [])`, `Closes stream ID`).

close(FileID, []) :-                            % std
   close$(FileID).

predicate$info(fclose/1, `fclose(ID)`, `Closes stream ID`).

fclose(FileID) :-
  close$(FileID).

predicate$info(file_exists/1, `file_exists(FileACS)`, `Succeeds if File exists`).

file_exists(F) :-
  file_exists(F, _).

file_exists(F, T) :-
  catch(open(F, read, H, [type(binary)]), _, fail),
  fread(H, Byte1, 0),
  fread(H, Byte2, 0),
  file$test(Byte1, Byte2, T),
  close(H), !.                                %

file$test(0xff, 0xfe, 0) :- !.                % a little endian unicode file
file$test(0xff, _, 1) :- !.                   % a .plm or .xpl
file$test(_, _, 0).                           % else treat as source.

predicate$info(at_end_of_stream/0, `at_end_of_stream`, `Succeeds if the current_input stream is at its end`).

at_end_of_stream :-                           % current input stream
  current_input(H),
  at_end_of_stream(H).

%----------------------------------------------------------------------
% Find all files that match a given pattern and mask. The pattern can
% be any DOS file pattern, such as *.pro, and can be specified as a
% string, atom, or char list.
%
% The masks indicate the types of files to look for. They can be
% added together. Values are:
% 0 - normal files
% 2 - hidden files
% 4 - system files
% 8 - vol-id entry
% 16 - subdirectory
% 
% findfiles/3 is designed to work in a repeat/fail loop, and is based
% on the two C extended predicates findfirst/4 and findnext/2. These
% are typically implemented in the system specific extended predicate lsx.
%

predicate$info(findfiles/3, ``, ``).

findfiles(_, _, _) :-
  not defined(user : findfirst/4),
  err$exec(execE, $findfiles not implemented on this platform$, $$, 
          findfiles(A, B, C)), !,
  fail.
findfiles(Pattern, Mask, Fileinfo) :-
  findfirst(Handle, Pattern, Mask, FI),
  find$files(Handle, FI, Fileinfo).

find$files(Handle, FI, FI).
find$files(Handle, _, Fileinfo) :-
  findnext(Handle, FI),
  find$files(Handle, FI, Fileinfo).


%----------------------------------------------------------------------
% I/O predicates
%
% nl/0
% nl/1
% write/1
% write/2
% writeq/1
% writeq/2
% read/1
% read/2
% read_term/2
% read_term/3
% read_string/1
% read_string/2
% get_char/1
% get_char/2
% get_code/1
% get_code/2
% get/1
% get/2
% get0/1
% get0/2
% get1/1
% get1/2
% unget0/1
% unget0/2
% put_char/1
% put_char/2
% put_code/1
% put_code/2
% put_term/2
% put_term/3
% put/1
% put/2
% skip/1
% skip/2
% tab/1
% tab/2
% flush_in/0
% flush_input/0
% flush_out/0
% flush_output/0
% respkey/1
% press_any_key/0
%----------------------------------------------------------------------

predicate$info(nl/0, `nl`, `Write a newline character to the current output stream`).

nl :-
  write('\n').

predicate$info(nl/1, `nl(ID)`, `Write a newline character to stream ID`).

nl(H) :-
   write(H, '\n').

predicate$info(write_term/3, ``, ``).

write_term(H, Term, Options) :-                 % turn options list into flags
   (member$(quoted(true), Options) -> X = 1 ; X = 0),
   (member$(ignore_ops(true), Options) -> Y = 1 ; Y = 0),
   (member$(number_vars(true), Options) -> Z = 1 ; Z = 0), !, 
   (Y == 1 -> display(H, Term) ; write_(H, Term, options(X, Y, Z))).

predicate$info(write_term/2, ``, ``).

write_term(Term, Options) :-                    % std
   current_output(H),
   write_term(H, Term, Options).

write_canonical(Term) :-                        % std
   current_output(H),
   write_canonical(H, Term).

write_canonical(H, Term) :-                     % std
   write_term(H, Term, [quoted(true), ignore_ops(true)]).

predicate$info(write/1, `write(Term)`, `Write Term to the current output stream`).

write(Term) :-                                  % write to currout
   current_output(H),
   write_(H, Term, options(0, 0, 1)).          % [numbervars(true)]

predicate$info(write/2, `write(ID, Term)`, `Write Term to stream ID`).

write(H, Term) :-                               % write to H
   write_(H, Term, options(0, 0, 1)).          % [numbervars(true)]

predicate$info(writeq/1, `writeq(Term)`, `Write Term to the current output stream, quoting atoms as necessary`).

writeq(Term) :- 
   current_output(H),
   write_(H, Term, options(1, 0, 1)).      % [quoted(true), numbervars(true)]

predicate$info(writeq/2, `writeq(ID, Term)`, `Write Term to the stream ID, quoting atoms as necessary`).

writeq(H, Term) :- 
   write_(H, Term, options(1, 0, 1)).       % [quoted(true), numbervars(true)]

predicate$info(read_term/2, ``, ``).

read_term(Term, Options) :-                     % /2             
   current_input(H),
   read_term(H, Term, Options).

predicate$info(read_term/3, ``, ``).

read_term(H, Term, Options) :-                  % /3
   ( 
       list(Options) -> 
       (                                        % is list 
           member$(V, Options),
           var(V) -> 
           fail ;

           true                                 % no member is a var
       ) ; 

       Options = []                             % test for no options
   ),
   read_term$(H, Term, Options).

predicate$info(read/1, `read(TermV)`, `Read the next Term from the current input stream; note Term must end in a period`).

read(Term) :-                                   % read from curr input
   current_input(H),
   read_term(H, Term, []).                             

predicate$info(read/2, `read(ID, TermV)`, `Read the next Term from the stream ID; note Term must end in a period`).

read(H, Term) :-                                
   read_term(H, Term, []).                              

predicate$info(read_string/1, `read_string(StringV)`, `Read characters up to next newline character from current input stream, and unify with String`).

read_string(X) :-
  current_input(H),
  read_string_(X, H).

predicate$info(read_string/2, `read_string(ID, StringV)`, `Read characters up to next newline character from stream ID, and unify with String`).

read_string(H, X) :-
  read_string_(X, H).

predicate$info(get/1, `get(CharV)`, `Get next character from current input stream`).

get(X) :-
  get_char(X).

predicate$info(get/2, `get(ID, CharV)`, `Get next character from stream ID`).

get(H, Y) :-
  get_char(H, Y).

predicate$info(get_char/1, ``, ``).

get_char(X) :-
  current_input(H),
  get_char(H, X).

predicate$info(get_char/2, ``, ``).

get_char(H, Y) :-                             % std
  repeat,
  get0_(X, H),
  (X = 'end_of_file' ;  X > 0' ), !,
  Y = X.

predicate$info(get0/1, `get0(CharV)`, `Get next character from current input stream, return end_of_file if end of file reached`).

get0(X) :-
  current_input(H),
  get0_(X, H).

predicate$info(get0/2, `get0(ID, CharV)`, `Get next character from stream ID, return end_of_file if end of file reached`).

get0(H, X) :-
  get0_(X, H).                                % arg reversal!

predicate$info(get_code/1, ``, ``).

get_code(X) :-                                % std
  current_input(H),
  getcode_(X, H).

predicate$info(get_code/2, ``, ``).

get_code(H, X) :-                             % std
  getcode_(X, H).

% get1 designed for single character response, with
% nl after it, for systems without keyb/1

predicate$info(get1/1, `get1(CharV)`, `Gets the next character from the keyboard followed by [Enter] (for environments not supporting keyb/1`).

get1(X) :-
  current_input(H),
  get1(H, X).

predicate$info(get1/2, `get1(ID, CharV)`, `Gets the next character from stream ID followed by [Enter] (for environments not supporting keyb/1`).

get1(H, X) :-
  get0(H, X),
  get0(H, _).                                 % the newline

predicate$info(unget0/1, ``, ``).

unget0(C) :-
  current_input(H),
  unget0_(C, H).

predicate$info(unget0/2, ``, ``).

unget0(C, H) :-
  unget0_(C, H).

predicate$info(put_code/1, ``, ``).

put_code(X) :-                                % std - puts ints
  current_output(H),
  putc(H, X, 0).                              % 0 for code

predicate$info(put_code/2, ``, ``).

put_code(H, X) :-                             % std
  putc(H, X, 0).

predicate$info(put_char/1, ``, ``).

put_char(X) :-                                % std - should only put chars
  current_output(H),
  putc(H, X, 1).                              % 1 for char

predicate$info(put_char/2, ``, ``).

put_char(H, X) :-                             % std
  putc(H, X, 1).

predicate$info(put/1, `put(CharN)`, `Write the Char to the current output stream`).

put(X) :-                                     % make put same as put_char
  current_output(H),
  put(H, X).

predicate$info(put/2, `put(ID, CharN)`, `Write the Char to stream ID`).

put(H, X) :-
  (char(X) -> putc(H, X, 1) ;  putc(H, X, 0)).

predicate$info(skip/2, `skip(ID, Char)`, `Continuously read characters from stream ID until one matching Char is read. Fail if end_of_file is reached`).

skip(H, N) :-
  M is N,
  repeat,
  get0(H, X),
  (X == M ;  X == 'end_of_file'), !.          % skip until M

predicate$info(skip/1, `skip(Char)`, `Continuously read characters from current input stream until one matching Char is read. Fail if end_of_file is reached`).

skip(N) :-
  current_input(H),
  skip(H, N).

predicate$info(tab/2, `tab(ID, CountN)`, `Write Count spaces to stream ID`).

tab(_, 0) :-
  !.
tab(H, N) :-
  N > 0,
  put(H, 0' ), 
  M is N - 1,
  tab(H, M), !.

predicate$info(tab/1, `tab(CountN)`, `Write Count spaces to the current output stream`).

tab(N) :-
  current_output(H),
  tab(H, N).

predicate$info(respkey/1, `respkey(CharV)`, `Gets a key using keyb/1 if possible, otherwise get1/1`).

respkey(A) :-
  keyb(A),
  (A == 3 -> abort(3); true),
  nl, !.
respkey(A) :-
  key$b(A),
  (A == 3 -> abort(3); true),
  nl, !.
respkey(A) :-
  get1(A),
  (A == 3 -> abort(3); true).

predicate$info(ftell/2, ``, ``).

ftell(H, Posn) :-
  fseek(H, 0, 1, Posn).

predicate$info(fseek/3, `fseek(ID, OffsetN, MethodN{0=start,1=current,2-end NewOff) `, `Reposition file identified by ID according to Offset and Method; unify NewOff with new position`).

fseek(H, Offset, Method) :-
  fseek(H, Offset, Method, _).

predicate$info(flush_output/0, `flush_output`, `Flushes the current output stream`).

flush_output :-                               % std
  current_output(H),
  fflush(H).

predicate$info(flush_output/1, `flush_output(ID)`, `Flushes output for stream ID`).

flush_output(X) :-                            %std
  fflush(X).

flush_out :-
  current_output(H),
  fflush(H).

flush_in :-
  current_input(H),
  fflush(H).

predicate$info(press_any_key/0, ``, ``).

press_any_key :-
  write(`Press Enter to continue...`), 
  respkey(_).

%----------------------------------------------------------------------
% List manipulation predicates
%
% append$/3
% member$/2
% reverse$/2,3
% length$/2
% last$/2
%----------------------------------------------------------------------

append$([], X, X).
append$([H|T], W, [H|Z]) :-
  append$(T, W, Z).

member$(X, [X|_]).
member$(X, [_|Y]) :-
  member$(X, Y).

reverse$(F, R) :-
  reverse$(F, [], R).

reverse$([], X, X).
reverse$([H|T], X, Z) :-
  reverse$(T, [H|X], Z).

last$([X], X) :-
  !.
last$([X|Y], Z) :-
  last$(Y, Z).

length$(List, Len) :-
   length$(List, 0, Len).
   
length$([], L, L).
length$([_|As], Acc, Len) :-
   Acc2 is Acc + 1,
   !,
   length$(As, Acc2, Len).

%---------------------------------------------------------------------- 
% Mode control
%
% set_mode/2
% get_mode/2
% current_prolog_flag/2
% set_prolog_flag/2
%---------------------------------------------------------------------- 

% Prolog flags, note that prolog$flag is a built-in
% that sets if the value is instantiated, and unifies
% otherwise.

prolog$config$flags([
		heap, local, control, trail, heapbumper,
		readbuffer, readdepth, maxclauses,
		logfile, apitrace,
		string_esc, lsxload, macroheapsz, gcthingfreq,
		maxfiles, debug_port, debug_host ]).
		
prolog$settable$flags([
      locale, double_quote_strings,
      preprocessor, occurs_check,
      upper_case_atoms, decimals, floats,
      decimal_places, epsilon, delta, modulo ]).
      
prolog$information$flags([
		bounded, char_conversion, debug,
		integer_rounding_function,
		max_arity, max_integer, min_integer,
		prolog_copyright, prolog_date,
		prolog_name, prolog_version,
		undefined_predicate, unicode ]).

predicate$info(current_prolog_flag/2, `current_prolog_flag(FlagAV, ValueNV)`, `Returns the values of various system limits`).

current_prolog_flag(Flag, Value) :- 
   nonvar(Flag), !,
   prolog$flag(Flag, V),
   Value = V.
current_prolog_flag(Flag, Value) :- 
   var(Flag),
   prolog$config$flags(Flags),
   member$(Flag, Flags),
   prolog$flag(Flag, Value).
current_prolog_flag(Flag, Value) :- 
   var(Flag),
   prolog$settable$flags(Flags),
   member$(Flag, Flags),
   prolog$flag(Flag, Value).
current_prolog_flag(Flag, Value) :- 
   var(Flag),
   prolog$information$flags(Flags),
   member$(Flag, Flags),
   prolog$flag(Flag, Value).
   
predicate$info(set_prolog_flag/2, `Sets the values of various system limits`).

set_prolog_flag(Flag, Value) :-
   nonvar(Value),
   !, prolog$flag(Flag, Value).
set_prolog_flag(Flag, Value) :-
   err$exec(instanceE, `Value must be instantiated`, Value, 
	         set_prolog_flag(Flag, Value) ).
   
predicate$info(set_mode/2, `set_mode(ModeA, OnOffA)`, `Sets the Mode 'on' or 'off'`).

set_mode(F, V) :-
   set_prolog_flag(F, V).

predicate$info(get_mode/2, `get_mode(ModeA, OnOffV)`, `Gets the current setting of Mode: on or off`).

get_mode(F, V) :-
   current_prolog_flag(F, V).

% quicker way to set number flags
number_flags(DEC, FLT) :-
   var(DEC),
   var(FLT),
   !,
   current_prolog_flag(decimals, DEC),
   current_prolog_flag(floats, FLT).
number_flags(DEC, FLT) :-
   nonvar(DEC),
   nonvar(FLT),
   set_prolog_flag(decimals, DEC),
   set_prolog_flag(floats, FLT).
   

%----------------------------------------------------------------------
% Set gathering functions
%
% findall/3
% bagof/3
% setof/3
%----------------------------------------------------------------------

% non-metapredicate version for get_preds used by debugger
findall$(X, P, L) :-
  reserve$(G),
  ((call(P), stash$(G, X), fail) ;  get$(G, Got)),
  L = Got,
  stash$free(G).

predicate$info(findall/3, `findall(Instance, Goal, List)`, `Create a List of unified Instances that satisfy Goal`).

findall(X, P, L) :-
  reserve$(G),
  ((call(P), stash$(G, X), fail) ;  get$(G, Got)),
  L = Got,
  stash$free(G).

predicate$info(bagof/3, `bagof(Instance, Goal, List)`, `Create a List of unified Instances that satisfy Goal. Backtracking tries again if there are variables in Goal not in Instance`).

xt(G) :-
  (write( call(G) ), nl; write( fail(G) ), nl, fail),
  timer(T1),
  call(amzi_system:G),
  timer(T2),
  T3 is T2 - T1,
  ( write( exit(T3, G) ), nl; write( redo(G) ), nl, fail ).

xbagof(X, Goal, S) :-
  extract_goal(Goal, G),                      % Extract out ^ ... ^ .. G
  free_vars(X, Goal, F),                      % V
  (
     F == [] ->
     findall(X, G, S),
     S \= [] ;

     gensym(bagof, BagN),
     sys$assert('{sys}fvarlist'(BagN, [])),
     % copy_post((X, Goal), (XCopy, GoalCopy)),
     copy_term((X, Goal), (XCopy, GoalCopy)),
     free_vars(XCopy, GoalCopy, FCopy),
     extract_goal(GoalCopy, GCopy),
     call(GCopy),
     FCopy = F,
     sys$retract('{sys}fvarlist'(BagN, FVL)),
     (
        not(is_member(F, FVL)) ->
        true ;

        sys$assert('{sys}fvarlist'(BagN, FVL)),
        fail
     ),
     sys$assert('{sys}fvarlist'(BagN, [F|FVL])),
     findall(X, G, S),
     S \= []
  ).

:- sorted('{sys}fvarlist'/2).

bagof(X, Goal, S) :-
  extract_goal(Goal, G),                      % Extract out ^ ... ^ .. G
  free_vars(X, Goal, F),                      % V
  (
     F == [] ->
     findall(X, G, S),
     S \= [] ;

     gensym(bagof, BagN),
     (sys$assert('{sys}fvarlist'(BagN, [])); sys$retract('{sys}fvarlist'(BagN, _)), fail),  % clean up on failure
     % copy_post((X, Goal), (XCopy, GoalCopy)),
     copy_term((X, Goal), (XCopy, GoalCopy)),
     free_vars(XCopy, GoalCopy, FCopy),
     extract_goal(GoalCopy, GCopy),
     call(GCopy),
     FCopy = F,
     sys$retract('{sys}fvarlist'(BagN, FVL)),
     (
        not(is_member(F, FVL)) ->
        true ;

        sys$assert('{sys}fvarlist'(BagN, FVL)),
        fail
     ),
     sys$assert('{sys}fvarlist'(BagN, [F|FVL])),
     findall(X, G, S),
     S \= [],
     ( sys$retract('{sys}fvarlist'(BagN, _)); sys$assert('{sys}fvarlist'(BagN, [F|FVL])), fail )
  ).

predicate$info(setof/3, `setof(Instance, Goal, List)`, `Create an ordered List (without duplicates) of unified Instances that satisfy Goal. Backtracking tries again if there are variables in Goal not in Instance`).

setof(A, B, Set) :-
  bagof(A, B, Bag),
  sort(Bag, Sorted),
  undup(Sorted, Set).

undup([], []).
undup([X, X1|Y], Undup) :-
  X == X1, !,
  undup([X|Y], Undup).
undup([X|Y], [X|Rest]) :-
  undup(Y, Rest).

% ----- Locals -----

/*
copy_post(G, GC) :-
  reserve$(P),
  stash$(P, G),
  get$(P, [GC]),
  stash$free(P).
*/

free_vars(X, G, F) :-
  bound_vars(G, BV),         % Get bound variables (X ^ G)
  varsof((X, BV), XV),
  (debug64_loaded ->
     debug_varsof(G, GV)
     ;
     varsof(G, GV) ),
  v_diff(GV, XV, F), !.

% Parse out ^..^..^..Goal

extract_goal((M : A ^ B), G) :- !,
   extract_goal(M : B, G).
extract_goal(G, G).

bound_vars(G, BV) :-
   (debug64_loaded ->
      bound_vars_debug(G, [], BV)
      ;
      bound_vs(G, BV) ).

bound_vs((M : A ^ B), V) :- !,
  bound_vs(M : B, V1),
  (var(A) -> V = [A|V1] ;  V = V1).
bound_vs(_, []).

bound_vars_debug(M : A ^ B, Acc, V) :-
   !,
   bound_vars_debug(M : B, [A|Acc], V).
bound_vars_debug(M:B, Acc, V) :-
   !,
   bind_debug_vars(B, DVs),  % in debug64.pro
   append$(Acc,DVs,V).
bound_vars_debug(B, Acc, V) :-
   !,
   bind_debug_vars(B, DVs),  % in debug64.pro
   append$(Acc,DVs,V).
   

  
%---------------------------------------------------------------------- 
% Stream handling predicates
%
% see/1
% tell/1
% set_errors/1
% seetell/1
% seeing/1
% telling/1
% seen/0
% told/0
% seentold/0
%---------------------------------------------------------------------- 

predicate$info(see/1, `see(NameA)`, `Set the current input stream to the named stream`).

see(H) :-
  integer(H),
  current_streams(H, _, _).
see(user) :- !,
  current_user(H, _, _),
  current_streams(H, _, _).
see(SNameA) :-
  stream_property(H, alias(SNameA)),          % open already?
  current_streams(H, _, _), !.
see(SNameA) :-
  stream_property(H, file_name(SNameA)),      % open already?
  current_streams(H, _, _), !.
see(SNameA) :-                                %fopen(H, SNameA, r),
  open(SNameA, read, H),
  current_streams(H, _, _).

predicate$info(tell/1, `tell(NameA)`, `Set current output to the stream named NameA`).

tell(H) :-
  integer(H), !,
  current_streams(_, H, _).
tell(user) :- !,
  current_user(_, H, _),
  current_streams(_, H, _).
tell(user_output) :- !,
  current_user(_, H, _),
  current_streams(_, H, _).
tell(SNameA) :-
  tell(SNameA, a).

predicate$info(tell/2, ``, ``).

tell(SNameA, _) :-
  stream_property(H, alias(SNameA)),          % open already?
  current_streams(_, H, _), !.
tell(SNameA, _) :-
  stream_property(H, file_name(SNameA)),      % open already?
  current_streams(_, H, _), !.
tell(SNameA, a) :-                            %fopen(H, SNameA, w), 
  open(SNameA, write, H),
  current_streams(_, H, _), !.
tell(SNameA, u) :-                            %fopen(H, SNameA, wu),
  open(SNameA, write, H, [type(wide_text)]),
  current_streams(_, H, _).

predicate$info(set_errors/1, `set_errors(NameA)`, `Sets the current_error stream to NameA`).

set_errors(H) :-
  integer(H),
  current_streams(_, _, H).
set_errors(user) :- !,
  current_user(_, _, H),
  current_streams(_, _, H).
set_errors(SNameA) :-
  atom(SNameA),

              %fopen(H, SNameA, w), % return handle if open, else open as file
  open(SNameA, write, H),           % return handle if open, else open as file
  current_streams(_, _, H).

predicate$info(seetell/1, `seetell(NameA)`, `Set the current input and output streams to NameA`).

seetell(H) :-
  stream_type(H, 2),                          % its a window
  see(H),
  tell(H).

predicate$info(seeing/1, `seeing(NameA)`, `Unify NameA with the current input stream`).

seeing(X) :-
  var(X),
  current_streams(H, _, _),
  handle_name(H, X).

predicate$info(telling/1, `telling(NameA)`, `Unify NameA with the current output stream`).

telling(X) :-
  var(X),
  current_streams(_, H, _),
  handle_name(H, X).

predicate$info(seen/0, `seen`, `Close the current input stream, set current input stream to user`).

seen :-
  current_streams(H, _, _),
  close$stream(H),
  close(H).
seen.

predicate$info(told/0, `told`, `Close the current output stream, resetting it to user`).

told :-
  current_streams(_, H, _),
  close$stream(H),
  close(H).
told.

predicate$info(seentold/0, `seentold`, `Close the current input and output streams, setting both to user`).

seentold :-
  current_streams(I, O, _),
  close$stream(I).

% ----- Locals -----

close$stream(H) :-                      % don't close stdin, stdout, or stderr
  integer(H),                         % don't close function handle either (3)
  H < 4, !,                           % or anything less
  fail.
close$stream(H) :-                % reset current_user & _streams if necessary
  integer(H), !,
  current_user(I, O, E),
  (H == I -> current_user(0, _, _) ;  true),
  (H == O -> current_user(_, 1, _) ;  true),
  (H == E -> current_user(_, _, 2) ;  true),
  current_user(Iu, Ou, Eu),
  current_streams(Is, Os, Es),
  (H == Is -> current_streams(Iu, _, _) ;  true),
  (H == Os -> current_streams(_, Ou, _) ;  true),
  (H == Es -> current_streams(_, _, Eu) ;  true).
close$stream(stdin) :- !,
  fail.
close$stream(stdout) :- !,
  fail.
close$stream(stderr) :- !,
  fail.
close$stream(SNameA) :-
  atom(SNameA), !,
  handle_name(H, SNameA),
  close$stream(H).

push$streams :-
  (sys$retract('{sys}stream_stack'(Ss)) ;  Ss = []), !,
  current_streams(I, O, E),
  sys$assert('{sys}stream_stack'([streams(I, O, E)|Ss])).

pop$streams :-
  sys$retract('{sys}stream_stack'([streams(I, O, E)|Ss])), !,
  current_streams(I, O, E),
  sys$assert('{sys}stream_stack'(Ss)).

predicate$info(stream_property/2, `stream_property(ID, PropertyA)`, `Returns the named property for stream ID`).

stream_property(H, Prop) :-
  atomic(H),
  stream$property(H, PropList),
  member$(Prop, PropList).
stream_property(H, Prop) :-
  var(H),
  stream$list(Streams),
  member$(H, Streams),
  stream$property(H, PropList),
  member$(Prop, PropList).

%---------------------------------------------------------------------- 
% Term ordering predicates
%
% @< / 2
% @> / 2
% @>= / 2
% @=< / 2
% compare/3
%
% all based on system pred less$(T1, T2)
%---------------------------------------------------------------------- 

predicate$info((@<)/2, `Term1 @< Term2`, `Succeeds if Term1 collates before Term2`).

A @< B :-
  less$(A, B).

predicate$info((@>)/2, `Term1 @> Term2`, `Succeeds if Term1 collates after Term2`).

A @> B :-
  less$(B, A).

predicate$info((@>)/2, `Term1 @>= Term2`, `Succeeds if Term2 collates after or at the same place as Term2`).

A @>= B :-
  not(less$(A, B)).

predicate$info((@=<)/2, `Term1 @=< Term2`, `Succeeds if Term1 collates before or at the same place as Term2`).

A @=< B :-
  not(less$(B, A)).

compare(==, A, B) :-
   A == B.
compare(>, A, B) :-
   A @> B.
compare(<, A, B) :-
   A @< B.

%-----------------------------------------------------------
% String and atom handling built-in predicates
%
% atom_concat/2
% numberic_type/2
% substring/4
% sub_string/4
% sub_atom/4
% stringlist_concat/3   (supplements built-in stringlist_concat/2)
% term_type/2
% string_query/2
%----------------------------------------------------------

predicate$info(term_type/2, `term_type(Term, TypeA {atom, string, list, structure, number, var})`, `Returns the type of the Term`).

term_type(T, atom) :- atom(T).
term_type(T, string) :- string(T).
term_type(T, list) :- list(T).
term_type(T, structure) :- structure(T).
term_type(T, number) :- number(T).
term_type(T, var) :- var(T).

predicate$info(numeric_type/2, `numeric_type(Number, TypeA {integer, fixed_real, long_real, real, single_float, double_float, float})`, `Returns the type of the Number`).

numeric_type(T, integer) :- integer(T).
numeric_type(T, fixed_real) :- fixed_real(T).
numeric_type(T, long_real) :- long_real(T).
numeric_type(T, real) :- real(T).
numeric_type(T, single_float) :- single_float(T).
numeric_type(T, double_float) :- double_float(T).
numeric_type(T, float) :- float(T).

predicate$info(number_codes/2, ``, ``).

number_codes(N, C):-
   (
       number(N) ->                          % arg 1 a number
       string_number(S, N),
       string_list(S, C);
           
       list(C),                             % arg 1 not a number, arg 2 a list
       string_list(S, C),
       string_number(S, N),
       number(N)
   ).

predicate$info(is_atom/1, `is_atom(X)`, `Succeeds if X is an atom`).

is_atom(N):-
   atom(N).

predicate$info(is_float/1, `is_float(X)`, `Succeeds if X is a floating point number`).

is_float(N):-
   float(N).

predicate$info(is_number/1, `is_number(X)`, `Succeeds if X is a number`).

is_number(N):-
   number(N).

predicate$info(is_string/1, `is_string(X)`, `Succeeds if X is a string`).

is_string(S):-
   string(S).

predicate$info(char_code/2, `char_code(Atom, CharList)`, `Converts back and forth between an Atom and a list of characters`).

char_code(Char, Code):-
   atom_codes(Char, [Code]).

predicate$info(string_integer/2, `string_integer(String, Int)`, `Converts between integer and string`).

string_integer(S, N):-
   is_string_term(S, N), 
   is_integer(N).

predicate$info(string_number/2, `string_number(String, Number)`, `Converts between a string and a number`).

string_number(S, N):-
   is_string_term(S, N),
   is_number(N).
   
% string_atom(S, N)  implemented as built-in to allow strange characters in atom
   
predicate$info(string_float/2, `string_float(String, Number)`, `Converts between a string and floating point number`).

string_float(S, N):-
   is_string_term(S, N),
   is_float(N).
   

substring(A, B, C, D) :-
  repeat,
  '$substring'(A, B, C, D).

predicate$info(sub_string/4, `sub_string(String, IndexNV, LengthNV, SubstringV)`, `Substring of String starting at Index of Length`).

sub_string(S, I, L, SS) :-
  string(S), !,
  repeat,
  sub$string(S, I, L, SS).
sub_string(S, I, L, SS) :-
  not(string(S)),
  err$exec(instanceE, $arg1 must be string$, sub_string(A, I, L, SA), 
          sub_string(A, I, L, SA)).

predicate$info(sub_atom/4, `sub_atom(Atom, IndexNV, LengthNV, SubatomV)`, `Subatom of Atom starting at Index of Length`).

sub_atom(A, I, L, SA) :-
  atom(A), !,
  repeat,
  sub$string(A, I, L, SA).
sub_atom(A, I, L, SA) :-
  not(atom(A)),
  err$exec(typeE, $arg1 must be atom$, sub_atom(A, I, L, SA), 
          sub_atom(A, I, L, SA)).

/* save for now as possible template for error handling */

/*
sub_atom(A,B,C,D) :-
   atom(A),
   ( 
       (
           var(D), 
           integer(B), 
           integer(C) 
       ) ;
       (
           atom(D), 
           var$int(B), 
           var$int(C), 
           string_atom(Ds, D)
       ) 
   ),
   !,
   string_atom(As, A),
   substring(As, B, C, Ds),
   string_atom(Ds, D).
sub_atom(A,B,C,D) :-
   var(A),
   err$exec(instanceE, $arg1 must be instantiated$, 
            sub_atom(A,B,C,D), sub_atom(A,B,C,D)).
sub_atom(A,B,C,D) :-
   not(atom(D)), not(var(D)), !,
   err$exec(typeE, $arg4 must be an atom or variable$, 
            sub_atom(A,B,C,D), sub_atom(A,B,C,D)).
sub_atom(A,B,C,D) :-
   ( 
       (
           not(var$int(B))
       ) ;

       (
           not(var$int(C))
       ) 
   ), !,
   err$exec(typeE, 
   $arg2 and arg3 must be integers or variables$,
   sub_atom(A,B,C,D),
   sub_atom(A,B,C,D)).
sub_atom(A,B,C,D) :-
   var(D),
   ( var(B) ; var(C) ), !,
   err$exec(instanceE, $arg2 and arg3 must be instantiated when arg4 is not$,
            sub_atom(A,B,C,D), sub_atom(A,B,C,D)).

var$int(X) :- var(X), !.
var$int(X) :- integer(X).
*/

predicate$info(atom_concat/3, `atom_concat(Atom1AV, Atom2AV, Atom3AV)`, `Concatenates atoms Atom1 and Atom2 and unifies with Atom3, or generates all possible pairs of atoms, Atom1/Atom2, from Atom3`).

atom_concat(A1, A2, A12) :-
  atom(A1),
  atom(A2),
  (atom(A12) ;  var(A12)), !,
  atomlist_concat([A1, A2], A12).
atom_concat(A1, A2, A12) :-
  (var(A1); var(A2)),
%  var(A2),
  atom(A12), !,
  atom_codes(A12, L12),
  append$(L1, L2, L12),
  atom_codes(A1, L1),
  atom_codes(A2, L2).
atom_concat(A1, A2, A12) :-
  (
     (not(atom(A1)), not(var(A1))) ;

     (not(atom(A2)), not(var(A2))) ;

     (not(atom(A12)), not(var(A12)))
  ), !,
  err$exec(typeE, $args must be atoms or variables$, 
          atom_concat(A1, A2, A12), atom_concat(A1, A2, A12)).
atom_concat(A1, A2, A12) :-
  var(A12),
  (var(A1) ;  var(A2)), !,
  err$exec(instanceE, $arg1 and arg2 must be instantiated if arg3 is not$, 
          atom_concat(A1, A2, A12), atom_concat(A1, A2, A12)).

predicate$info(stringlist_concat/3, `stringlist_concat(StringList, SepStr, StringOut)`, `Concatenates the strings/atoms in StringList, separated by SepStr`).

stringlist_concat(List, Separator, String) :-
  build$separated$list(List, Separator, SepList),
  stringlist_concat(SepList, String).

build$separated$list([], _, []).
build$separated$list([S], _, [S]) :- !.
build$separated$list([S|Ss], Sep, [S, Sep|X]) :-
  build$separated$list(Ss, Sep, X).

predicate$info(string_query/2, `string_query(QueryString, VarsString)`, `Executes QueryString as a goal, returns variable bindings in VarsString`).

string_query(S, VarsOutStr) :-
   string_term(S, T),
   varlist(VarNames),
   varsof(T, Vars),
   call(T),
   varstr_out(VarNames, Vars, VarsOut),
   stringlist_concat(VarsOut, `, `, VarsOutStr).

% Map variable names to values, leaving out anonymous variables
% which begin with *.
varstr_out([], [], []) :-
   !.
varstr_out([[0'*|_]|Ns], [_|Vs], NVs) :-
   !,
   varstr_out(Ns, Vs, NVs).
varstr_out([NChars|Ns], [Var|Vs], [NVStr|NVs]) :-
   atom_codes(Name, NChars),
   string_term(NVStr, Name=Var),
   !,
   varstr_out(Ns, Vs, NVs).
   
predicate$info(string_query/2, `varlist_query(QueryString, VarsList)`, `Executes QueryString as a goal, returns alternating variable names and bindings in VarsList`).

% varlist_query/2 is used from various LSAPIs to allow the formating of the
% resulting query variables.
varlist_query(S, Len, VarsOutList) :-
   string_term(S, T),
   varlist(VarNames),
   varsof(T, Vars),
   call(T),
   varlist_out(VarNames, Vars, VarsOutList),
   length$(VarsOutList, Len).

% Map variable names to values, leaving out anonymous variables
% which begin with *.
varlist_out([], [], []) :-
   !.
varlist_out([[0'*|_]|Ns], [_|Vs], NVs) :-
   !,
   varlist_out(Ns, Vs, NVs).
varlist_out([NChars|Ns], [Var|Vs], [Name,Var|NVs]) :-
   atom_codes(Name, NChars),
   !,
   varlist_out(Ns, Vs, NVs).


%----------------------------------------------------------------------
% Miscellaneous built-in predicates
%
% current_op/3
% for/4
% halt/0
% gc/0
% gensym/2
% numbervars/3
% sort/2
% substring/4
% sub_string/4
% sub_atom/4
% catch/3
% throw/1
% atom_concat/2
% once/1
% islist/1
% is_list/1
% unify_with_occurs_check/2
%----------------------------------------------------------------------

predicate$info(current_op/3, `current_op(PrecedenceN, AssociativityA, Operator)`, `Unifies its arguments with the current operator definitions in the system; on backtracking it returns all the operator definitions that unify with the arguments`).

current_op(PREC, ASSOC, OP) :-
  get$op(PTR, P, A, O),
  current$op(PTR, P, A, O, PREC, ASSOC, OP).

current$op(PTR, P, A, O, P, A, O).
current$op(PTR, _, _, _, PREC, ASSOC, OP) :-
  get$op(PTR, P, A, O),
  current$op(PTR, P, A, O, PREC, ASSOC, OP).

predicate$info(for/3, `for(IndexN, StartN, EndN, IncrementN)`, `A for loop in Prolog, designed to increment the index on backtracking, succeeding if End hasn't yet been reached, failing if it has`).

for(I, Start, Finish) :-
  Start =< Finish,
  for(I, Start, Finish, 1).

for(I, Start, Finish, Step) :-
   repeat,
   for$(I, Start, Finish, Step).

predicate$info(arg/3, `arg(N, TermT, ArgV)`, `Unify Arg with the Nth argument of term Term`).

arg(Index, Term, Val):-
   (
       integer(Index) -> 
       arg$(Index, Term, Val) ; 

       nonvar(Val),
       functor(Term, _, A),
       for(I, 1, A),
       arg$(I, Term, Val),
       Index = I
   ).

predicate$info(nth/3, ``, ``).

nth(Index, Term, Val):-
   (
       integer(Index) -> 
       nth$(Index, Term, Val) ; 

       nonvar(Val),
       functor(Term, ., 2),
       nthmember(Val, Term, 1, Index)
   ).

predicate$info(nthmember/4, ``, ``).

nthmember(Val, [Val|Rest], Index, Index).
nthmember(Val, [_|Rest], I, Index) :-
  I1 is I + 1,
  nthmember(Val, Rest, I1, Index).

predicate$info(halt/0, `halt`, `Stop Prolog execution and return to either operating system or IDE`).

halt :-
  abort(1).

predicate$info(gc/0, ``, ``).

gc :-
  gc(7).

predicate$info(gensym/2, `gensym(RootA, SymbolV)`, `Generate a new atom from Root and unify it with Symbol`).

gensym(Root, Atom) :- gen$sym(Root, Atom).

/*
% gensym - generate a symbol for a root

gensym(Root, Atom) :-
  get$num(Root, Num),
  string_integer(S, Num),
  string_list(S, L),
  atom_codes(Root, RL),
  append$(RL, L, NL),
  atom_codes(Atom, NL).

get$num(Root, Num) :-
  sys$retract('{sys}current_num'(Root, N)), !,
  Num is N + 1,
  sys$assert('{sys}current_num'(Root, Num)).
get$num(Root, 1) :-
  sys$assert('{sys}current_num'(Root, 1)).
*/

% used to create special atoms, to replace variables,
% that will get printed by the system write predicate
% so that they can be read back in as variables.

predicate$info(numbervars/3, `numbervars(Term, StartN, EndV)`, `Unify variables of Term with atoms of the form _n, where n is an integer starting at Start for the first unique variable and ending at End. Called with Start bound and End unbound`).

numbervars(X, Lower, Upper) :-                % +Lower -Upper
  varsof(X, VList),
  nv$(VList, Lower, Upper).

nv$([], I, I).
nv$([V|Z], I, N) :-
  FA =.. ['$VAR', I],  % avoid '$VAR'(I) in code - writes funny
  V = FA,
  II is I + 1,
  nv$(Z, II, N).
/*
nv$([H|T], I, I1) :-
  string_integer(IS, I),
  string_list(IS, IL),
  XL = [0'_|IL],
  atom_codes(H, XL),
  J is I + 1,
  nv$(T, J, I1).
nv$([], I, I).
*/

predicate$info(sort/2, `sort(List, SortedListV)`, `Sort List into SortedList`).

sort(L, Sorted) :-
  dsort(L, Sorted - []).

dsort([X|L], R - R0) :-
  partition(L, X, L1, L2),
  dsort(L1, R - [X|R1]),
  dsort(L2, R1 - R0).
dsort([], R0 - R0).

partition([X|L], Y, [X|L1], L2) :-
   less$(X, Y), !,
   partition(L, Y, L1, L2).
partition([X|L], Y, L1, [X|L2]) :-
  partition(L, Y, L1, L2).
partition([], _, [], []).

predicate$info(keysort/2, `keysort(KeyListV, KeySortedListL)`, `Sorts a list of keyed elements of the form Key-Item; KeyedSortedList should be bound to a list`).

keysort(L, Sorted) :-                        % The sort routine - qsort for now
   ksort(L, Sorted - []).

ksort([X|L], R - R0) :-
   kpartition(L, X, L1, L2),
   ksort(L1, R - [X|R1]),
   ksort(L2, R1 - R0).
ksort([], R0 - R0).

kpartition([X|L], Y, [X|L1], L2) :-
   kless(X, Y), !,
   kpartition(L, Y, L1, L2).
kpartition([X|L], Y, L1, [X|L2]) :-
   kpartition(L, Y, L1, L2).
kpartition([], _, [], []).

kless(Kx - X, Ky - Y) :-
   !,
   less$(Kx,Ky).
kless(X, Y) :-
   err$exec(instanceE, $keysort items must be of form Key-Item. $, key_compare(X,Y), key_compare(X,Y)).

% catch & throw, for user-defined error handling

catch(Goal, Catcher, Recovery, OpenFiles) :-  % ray
  tellMe(OpenFiles, Told),
  catch(Goal, Catcher, Recovery),
  seekMe(OpenFiles, Told).

tellMe([], []).
tellMe([H|Hs], [Pos|Rest]) :-
  fseek(H, 0, 1, Pos).
tellMe(Hs, Rest).

seekMe([], []).
seekMe([H|Hs], [Pos|Rest]) :-
  fseek(H, Pos, 0, _).
seekMe(Hs, Rest).

predicate$info(catch/3, `catch(Goal, Catcher, Recover)`, `Tries to prove Goal, catching exception terms matching Catcher and proving Recover`).

catch(Goal, Catcher, Recovery) :-
%write(catch1(Goal, Catcher, Recovery)),nl,
  tag(Catcher),
  bt$assert('{sys}catch'(Catcher)),
%(write(catch_call:Goal),nl; write(catch_fail:Goal), nl, fail),
  call(Goal),
%(write(catch_exit:Goal),nl; write(catch_redo:Goal), nl, fail),
  bt$disable(Catcher).
catch(Goal, Catcher, Recovery) :-
%write(catch2(Goal, Catcher, Recovery)),nl,
  sys$clause('{sys}catch'(Catcher)),
  sys$retract('{sys}thrown'(Catcher)),
  sys$retract('{sys}catch'(Catcher)), !,
%(write(recov_call:Goal),nl; write(recov_fail:Goal), nl, fail),
  call(Recovery).

% call(Recovery),
% (write(recov_exit:Goal),nl; write(recov_redo:Goal), nl, fail).
% disable the tag after success, put it back on backtracking

bt$assert(X) :-
  sys$asserta(X).
bt$assert(X) :-
  sys$once(amzi_system : sys$retract(X)),
  fail.

bt$disable(Catcher) :-
  de$tag(Catcher),
  sys$once(amzi_system : sys$retract('{sys}catch'(Catcher))).
bt$disable(Catcher) :-
  re$tag(Catcher),
  sys$asserta('{sys}catch'(Catcher)),
  fail.

predicate$info(throw/1, `throw(Term)`, `Search for a matching catch Term`).

throw(X) :-
  sys$asserta('{sys}thrown'(X)),
  cut$tag(X),
  fail.

% Like call(X), but only executed once

predicate$info((once)/1, `once(Goal)`, `Tries to prove Goal once (without backtracking). `).

once(X) :-
  var(X),
  err$exec(instanceE, $arg must be instantiated: $, once(X), once(X)).
once(X) :-
  call(X), !.

predicate$info(islist/1, `islist(X)`, `Succeeds if X is a list or the empty list`).

islist(X) :-                            % like list/1, but gets the empty list
  var(X), !,
  fail.
islist([]) :- !.
islist(L) :-
  list(L).

predicate$info(is_list/1, `is_list(X)`, `Succeeds if X is a list or the empty list`).

is_list(L) :-
  list(L), !.
is_list(X) :-                           % like list/1, but gets the empty list
  var(X), !,
  fail.
is_list([]).

predicate$info(unify_with_occurs_check/2, `unify_with_occurs_check(Term1, Term2)`, `Attemps unification, also failing if there is a cyclic redundancy`).

unify_with_occurs_check(X,Y) :-
   current_prolog_flag(occurs_check, OCFlag),
   set_prolog_flag(occurs_check, on),
   (X = Y ->
      set_prolog_flag(occurs_check, OCFlag)
      ;
      set_prolog_flag(occurs_check, OCFlag),
      fail).

% End of builtins
%----------------------------------------------------------------------
%----------------------------------------------------------------------
% DCG Translator
%
% expand_term/2
%----------------------------------------------------------------------
% New DCG, compliments of Ray Reeves.
% Users can replace the terminal with
% dcg_terminal/3 of their own choosing.

rhs$(A, B, C, D) :-
  rhs(A, B, C, D).

/*
phrase(Grammar, Phrase) :-                    % needed at runtime
  phrase(Grammar, Phrase, []).

phrase(Grammar, Phrase, Tail) :-
  rhs(Grammar, Code, Phrase, Tail),
  call(Code).
*/

predicate$info(phrase/2, `phrase(GrammarGoal, TokenList)`, `Calls a DCG predicate with token list to parse`).

phrase(Grammar, Phrase) :-
   phrase(Grammar, Phrase, []).

predicate$info(phrase/3, `phrase(GrammarGoal, TokenList, RemainderList)`, `Calls a DCG predicate with token list to parse and expected remainder list`).

phrase(ModGrammar, Phrase, Tail) :-
   ( islist(ModGrammar), Gram = ModGrammar;
    ModGrammar = _:Gram, islist(Gram) ),
   !,
   dcg$terminal(Gram, Phrase, Tail).
phrase(ModGrammar, Phrase, Tail) :-
   (ModGrammar = Mod:Grammar ->
      true
      ;
      Grammar = ModGrammar,
      Mod = user),
   Grammar =.. [G|Args],
   append$(Args, [Phrase,Tail], DCGArgs),
   DCGGrammar =.. [G|DCGArgs],
   call(Mod:DCGGrammar).
   
predicate$info(expand_term/2, `expand_term(DCGtermT, PROLOGtermV)`, `Expands a DCG term into a Prolog term`).

expand_term(T1, T2) :-
  term_expansion(T1, T2),  % if there's a user defined one, use it
  !.
expand_term((P0 --> Q0), (P :- Q)) :-
  lhs(P0, P, PB, S0, S),
  rhs(Q0, Q1, S0, S),
  and(PB, Q1, Q2),
  flatten$(Q2, Q).

lhs((NT, Ts), P, PB, S0, S) :- !,             % special
  nonvar(NT),
  is_list(Ts),
  tag(NT, P, S0, S1),
  rhs(Ts, PB, S1, S).
lhs(NT, P, true, S0, S) :-                    % regular
  nonvar(NT),
  tag(NT, P, S0, S).

rhs(X, Code, S0, S) :-
  var(X), !,                                  % resolve var at runtime
  Code = phrase(X, S0, S).
rhs((X1, X2), P, S0, S) :- !,
  rhs(X1, P1, S0, S1),
  rhs(X2, P2, S1, S),
  and(P1, P2, P).
rhs((X1 -> X2), (P1 -> P2), S0, S) :- !,
  rhs(X1, P1, S0, S1),
  rhs(X2, P2, S1, S).
rhs((X1 ;  X2), (P1 ;  P2), S0, S) :- !,
  or(X1, P1, S0, S),
  or(X2, P2, S0, S).
rhs({P}, (P), S0, S) :- !,
  S0 = S.
rhs(!, !, S, S) :- !.
rhs(- X, Code, S0, S) :- !,
  rhs(X, Code, S, S0).                        % -X means reverse the thread
rhs([], true, S0, S) :- !,
  S0 = S.
rhs(catch(Try, Filter, Else), P, S0, S) :- !, % ray
  or(Try, T, S0, S),
  or(Else, E, S0, S),
  P = catch(T, Filter, E).
rhs(call(T), P, S0, S) :- !,                  % ray
  rhs(dcg$call(T), P, S0, S).                 % threading occurs at run time
rhs(? T, P, S0, S) :- !,                  % ray
  rhs(dcg$bug(T), P, S0, S).                 % threading occurs at run time
rhs(bug(T), P, S0, S) :- !,                  % ray
  rhs(dcg$bug(T), P, S0, S).                 % threading occurs at run time
rhs([T], Code, S0, S) :-
  sys$clause('{sys}is$t'(Name)), !,           % if terminal supplied ...
  functor(Code, Name, 3),                     % use it
  arg(1, Code, [T]),
  arg(2, Code, S0),
  arg(3, Code, S).
rhs([T|Ts], Code, S0, S) :-
  sys$clause('{sys}is$t'(Name)), !,           % if terminal supplied ...
  functor(Code, Name, 3),                     % use it
  arg(1, Code, [T|Ts]),
  arg(2, Code, S0),
  arg(3, Code, S).
rhs([T], dcg$terminal([T], S0, S), S0, S) :- !.
rhs([T|Ts], dcg$terminal([T|Ts], S0, S), S0, S) :- !.
rhs([T|Ts], Code, S0, S) :- !,                % if terminal not supplied 
  terminal(T, Code1, S0, S1),
  rhs(Ts, Code2, S1, S),
  and(Code1, Code2, Code).
rhs(T, T, S, S) :-                            % if there are no non terminals
  sys$clause('{sys}no$nt'), !.
rhs(T, P, S0, S) :-
  sys$clause('{sys}is$nt'(_, _)),             % if there are any declarations
  functor(T, Name, Arity),                    % only tag those
  (
     sys$clause('{sys}is$nt'(Name, Arity)) -> % if functor is declared nt ...
     tag(T, P, S0, S) ;                       % then tag it ...

     S0 = S,                                  % else don't
     P = T
  ), !.
rhs(T, P, S0, S) :-                           % tag it
  tag(T, P, S0, S).

terminal(T, S0 = [T|S], S0, S).

% dcg$terminal([T], S0, S) :-                   % only needed by june96 compiler
%   (
%      dcg_terminal([T], S0, S), ! ;            % user can define dcg_terminal.
% 
%      S0 = [T|S]                               % user did not
%   ), !.
% dcg$terminal(Ts, S0, S) :-
%   (
%      dcg_terminal(Ts, S0, S), ! ;             % user can define dcg_terminal.
% 
%      append$(Ts, S, S0)
%   ).

dcg$terminal(Ts, S0, S) :-
   dcg_terminal(Ts, S0, S),   % user defined dcg_terminal
   !.
dcg$terminal(Ts, S0, S) :-
   nonvar(S0),
   %S0 = prolog_source(Line,Lines),
%   S0 = file(Line,Lines),
   S0 = file(_,_,_),
   !,
   dcg$lines$terminal(Ts, S0, S).
dcg$terminal(Ts, S0, S) :-
   append$(Ts, S, S0).        % default lists

% This is the case where a file is being parsed.  The lines
% of the file are read into a list of strings, and as it
% comes up, a string is converted to a list of chars.

dcg$lines$terminal(Ts, file(F,Ts,[]), file(F,[],[])) :- !.
dcg$lines$terminal(Ts, file(F,T0,Lines), file(F,T1,Lines)) :-
   length$lte(Ts, T0),
   !, append$(Ts, T1, T0).
dcg$lines$terminal(Ts, file(F,T0,[line(N,L)|Lines]), file(F,T1,Ls)) :-
   string_list(L, T),
   append$(T0, T, T00),
   dcg$lines$terminal(Ts, file(F,T00,Lines), file(F,T1,Ls)).

length$lte([], _) :- !.
length$lte([X|Xs], [Y|Ys]) :-
   !, length$lte(Xs,Ys).

or(X, P, S0, S) :-
  rhs(X, X1, S1, S),
  or1(X1, P, S0, S1, S).

or1(X, P, S0, S1, S) :-
  S1 == S, !,
  and(S1 = S0, X, P).
or1(P, P, S, S, _).

and(true, P, P) :- !.                         % remove trues
and(P, true, P) :- !.
and((X, Xs), Y, (X, P)) :- !,
  and(Xs, Y, P).
and(P, Q, (P, Q)).

tag(M:X, M:P, S0, S) :-                           % P is X with args S0 S added
  X =.. [F|A],
  append$(A, [S0, S], AX),
  P =.. [F|AX],
  !.
tag(X, P, S0, S) :-                           % P is X with args S0 S added
  X =.. [F|A],
  append$(A, [S0, S], AX),
  P =.. [F|AX].

flatten$(A, A) :-
  var(A), !.
flatten$((A, B), C) :- !,
  flattenl(A, C, R),
  flatten$(B, R).
flatten$(A, A).

flattenl(A, (A, R), R) :-
  var(A), !.
flattenl((A, B), C, R) :- !,
  flattenl(A, C, R1),
  flattenl(B, R1, R).
flattenl(A, (A, R), R).

dcg$call(T, S0, S) :-
  amzi_system:rhs(T, P, S0, S),
  !,
  call(P).

dcg$bug(M:T, S0, S) :-
  amzi_system:rhs(T, P, S0, S),
  !,
  bug(M:P).
dcg$bug(T, S0, S) :-
  amzi_system:rhs(T, P, S0, S),
  !,
  bug(P).

%----------------------------------------------------------------------
% Debugging Predicates
%
% buginit/1,
% buginit/0,
% bug/1,
% ? /1,
% bugclose/0,
% flow/1,
% flow/3.
%
%----------------------------------------------------------------------

predicate$info(buginit/1, `buginit(FilenameA)`, `Opens Filename for debugging output`).

buginit(FileName) :-                          % fopen(B, FileName, w), 
  open(FileName, write, B),
  assert(bug$file(B)).

predicate$info(buginit/0, `buginit`, `Opens 'bug.log' for debugging output`).

buginit :-
  cntr_set(22, 0),
  buginit('bug.log').

predicate$info(bugclose/0, `bugclose`, `Closes the debug log file`).

bugclose :-
  retract(bug$file(B)), !,                    % !, fclose(B).
  close(B).
bugclose.

predicate$info((bug)/1, `bug(Goal)`, `Brackets a call to a Goal with debugging information in the debug log`).

bug(X) :-
  bug$callfail(X),
  get_mode(trace, T),
  set_mode(trace, off),                       % so we can debug debugger
  call(X),
  set_mode(trace, T),
  bug$exitredo(X).

predicate$info((?)/1, `? Goal`, `rackets a call to a Goal with debugging information in the debug log`).

? X :-
  bug(X).

% Try user's bugwrite first, else see if there's a bugfilet, else just writeq.

bug$write(I, X) :-
  user_bugwrite(I, X), !.
bug$write(I, X) :-
  bug$file(B), !,
  tab(B, I), writeq(B, X), nl(B),
  flush_output(B).
bug$write(I, X) :-
  tab(I), writeq(X), nl.

% bug$callfail/1 - writes a quoted term with the message
% CALL when called, and FAIL when backtracked through
% bug$exitredo/1 - writes a quoted term with the message
% EXIT when called, and REDO when backtracked through

bug$callfail(X) :-
  cntr_inc(22, I),
  bug$write(I, 'CALL' : X).
bug$callfail(X) :-
  cntr_dec(22, _),
  cntr_get(22, I),
  bug$write(I, 'FAIL' : X),
  fail.

bug$exitredo(X) :-
  cntr_dec(22, _),
  cntr_get(22, I),
  bug$write(I, 'EXIT' : X).
bug$exitredo(X) :-
  cntr_inc(22, I),
  bug$write(I, 'REDO' : X),
  fail.

predicate$info(flow/1, ``, ``).

flow(X) :-
  put(0'>), write(X), nl.
flow(X) :-
  put(0'<), write(X), nl, 
  fail.

predicate$info(flow/3, ``, ``).

flow(X, Y, Y) :-
  put(0'>), write(X), nl.                     % threaded for dcg rules
flow(X, Y, Y) :-
  put(0'<), write(X), nl, 
  fail.

/*
flow(X) :- 
   bug$write('>':X).
flow(X):- 
   bug$write('<':X),
   fail.

flow(X, Y, Y) :- 
   flow(X).                                     % threaded for dcg rules
*/

%----------------------------------------------------------------------
% Debug64 Predicates
%
% debug$info/6,
% debug$call/7,
% debug_init/0.
% unify_vars/1.
%
%----------------------------------------------------------------------

debug_init :-
   not(debug64_loaded),
   !,
   err$exec(execE, `Debug library not loaded for debugging run `, debug_init, debug_init/0).
debug_init :-
   debug64_init.
 
debug$info(Head, _,_,_,_,_):-
   not(debug64_loaded),
   !,
   err$exec(execE, `Debug library not loaded for debugging run `, Head, debug$info/6).
debug$info(A,B,C,D,E,F) :-
   debug64$info(A,B,C,D,E,F).

debug$call(Goal, _,_,_,_,_,_):-
   not(debug64_loaded),
   !,
   err$exec(execE, `Debug library not loaded for debugging run `, Goal, debug$goal/7).
debug$call(A,B,C,D,E,F,G) :-
   debug64$call(A,B,C,D,E,F,G).

% Unify a list of vars with their names, used by debugger for display,
% varlist might look like ['X' = H342, 'Y' = H456, ....
unify_vars([]).
unify_vars([V=V|Vs]) :-
   unify_vars(Vs).


%----------------------------------------------------------------------
% Local Utilities to amzilib.pro 
%----------------------------------------------------------------------


% system-specific versions of metapredicates, assuming
% all work is done in the amzi_system module.

sys$asserta(X) :-
  assert$(amzi_system, 0'a, X).

sys$assert(X) :-
  assert$(amzi_system, 0'a, X).

sys$assertz(X) :-
  assert$(amzi_system, 0'z, X).

sys$retract(X) :- !,
  repeat,
  retract$db(amzi_system, X, X).

sys$retractall(X) :-
  sys$retract(X),
  fail.
sys$retractall(_).

sys$abolish(F/A) :-
  abolish$(amzi_system:F/A).

sys$clause(X) :-
  clause$(amzi_system, X, _, _).

sys$clause(M, X) :-
  clause$(M, X, _, _).

sys$not(X) :-
  clause$(amzi_system, X, _, _), !,
  fail.
sys$not(_).

sys$not(M, X) :-
  clause$(M, X, _, _), !,
  fail.
sys$not(_, _).

sys$once(M : X) :-
  call(M : X), !.

/*
remove_pdb(PDB) :-
   protect_db(PDB),
   noprotect_db.
*/

/*
v_diff([], _, []).
v_diff([H|T], L, D) :-
   v_diff(T, L, D1),
   (is_member(H, L) -> D = D1 ; D = [H | D1]).
*/

v_diff([], _, []).
v_diff([H|T], L, D) :-
  is_member(H, L), !,
  v_diff(T, L, D).
v_diff([H|T], L, [H|D]) :-
  v_diff(T, L, D).

% Messages to the user

message$(H, X) :-
  message$(X).                                % disable different windows

message$([]) :-
  nl, !.
message$([X|Y]) :-
  write(X), !, 
  message$(Y).
message$(X) :-
  write(X), nl.

prompt$(H, X) :-
  prompt$(X).                                 % disable different windows

prompt$([]) :- !.
prompt$([X|Y]) :-
  write(X), !, 
  prompt$(Y).
prompt$(X) :-
  write(X).

% Force call$i of expand_term so debugger won't pick it up

exp$term('end_of_file', 'end_of_file') :- !.
exp$term(quit, quit) :- !.
exp$term(X, XE) :-
  call$i(amzi_system : expand_term(X, XE)), !.
exp$term(X, X).

:- end_module(amzi_system).

% ----- End of code -----
%
% ,_O<
% ( )
% ~~~~~

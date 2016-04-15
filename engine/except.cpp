/****************************************************************************
*
* except.c -- The Exception classes
*
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
*
****************************************************************************/


#include "inc.h"
#include "pch.h"

ErrFmt ErrFmts::Fmts[]=
{
   // Initialization Errors )

   { unknowniniE, INIT, aS("Unknown ini parameter: %s") },
   /* 100
      The parameter %s is not a valid Amzi! initialization parameter.  It was
      encountered in either an Amzi! initialization file (.ini) or an 
      initialization string passed by a host language application during 
      Logic Server initialization. */

   { nologfileE,  INIT, aS("Unable to open log file: %s") },
   /* 101
      An attempt to open the logfile %s failed. */

   { openlogE,    INIT, aS("Attempt to open log %s before closing log %s") },
   /* 102
      The application tried to open logfile %s when is was already open. */

#if defined(WINDOWS)
   { lsxinitE,    INIT, 
     aS("A Windows error, %d, occurred when calling InitPreds() for LSX %s.")},
#elif defined(UNIX)
   { lsxinitE,    INIT, 
     aS("Calling InitPreds() for LSX %s, caused an error: %s") },
#endif
   /* 103
      Once a Logic Server Extension (LSX) has been loaded by the Logic
      Server, the Logic Server looks for and calls the function InitPreds() 
      which should be defined in the LSX.  This error message indicates that 
      there was an error when attempting to get the address
      of InitPreds() in the LSX.
      
      This error code indicates there was a problem other than the most common
      problem of not being able to find the InitPreds() entry point. */

   { lsxnoprocE,  INIT, 
     aS("The initialization procedure InitPreds() was not found for LSX %s.")},
   /* 104
      Once a Logic Server Extension DLL (LSX) has been loaded by the Logic 
      Server, the Logic Server looks for and calls the function InitPreds() 
      which should be defined in the LSX.  This error message indicates that 
      the LSX was successfully loaded, but the Logic Server could not find 
      the InitPreds() entry point.
      
      The problem could be simply that InitPreds() is not defined in the LSX, 
      or, if it is defined, that it is not defined correctly.  Look at the 
      sample LSXs for your environment and make sure that InitPreds() in your 
      LSX is defined the same way. */

#if defined(WINDOWS)
   { lsxloadE,    INIT, 
     aS("A Windows error, %d, occurred when attempting to load LSX %s.") },
   /* 105
      There are a few common errors that can occur when the Logic Server
      attempts to load a Logic Server Extension (LSX).  This message
      indicates an error was encountered that is not that common.
      Consult the Windows documentation to determine the nature of the problem.
      */

#elif defined(UNIX)
   { lsxloadE,    INIT, 
     aS("When attempting to load LSX %s, an error occurred: %s") },
   /* 105
      This message
      indicates an error was encountered during initial load of an LSX.
      */

#endif
   { lsxnomodE,   INIT, aS("Windows could not find LSX %s.") },
   /* 106
      Windows was unable to find the named Logic Server Extension DLL.  This 
      is probably due to a simple environment path error, or other such 
      directory error. */

   { lsxnodllE,   INIT, 
     aS("Windows could not find one of the DLLs necessary to load LSX %s.") },
   /* 107
      The Logic Server Extension DLL (LSX) %s could not be loaded.  Apparently 
      it requires other DLLs as part of its load, and one of those DLLs could 
      not be loaded. */

   { lsxwinitE,   INIT, aS("Windows failed to initialize LSX %s.") },
   /* 108
      Windows failed to initialize the Logic Server Extension DLL (LSX) %s 
      correctly. Make sure main DLL entry points in the LSX are specified 
      correctly.  See the sample LSXs for examples. */

   { lsxdebugE,   INIT, aS("Remote debug failed to connect to host %s on port %d.") },
   /* 109
      Either the remote port and host wasn't set, or for some reason
      adebug.lsx couldn't be initialized. */

   { lsxsysdebugE,   INTERNAL, aS("Internal failure initializing remote debug LSX: %s.") },
   /* 110
      Either the remote port and host wasn't set, or for some reason
      adebug.lsx couldn't be initialized. */


   // API Errors
   { noloadE,     API, 
     aS("API function was called before the .XPL file was loaded") },
   /* 200
      Most Logic Server API (LSAPI) functions require a loaded Prolog load 
      module (.XPL file) before they can be executed.  This is required 
      because part of the Prolog runtime system is implemented in Prolog, 
      and that Prolog code (A5LIB.PLM) is linked with every .XPL file.  
      The .XPL file can be your full Prolog program, or just a stub to get 
      the Logic Server started. 

      Check the sequence of LSAPI calls in the host language program to make 
      sure an lsLoad is called before any LSAPI calls to manipulate the logic 
      base (but after the Logic Server is initialized).  If you just want a 
      stub .XPL file, you can use AMZI.XPL, which was linked using just 
      A5LIB.PLM. */

   { noninitE,    API, aS("Logic Server call to uninitialized engine.") },
   /* 201
      The first Logic Server API (LSAPI) call must be either lsInit or lsInit2.
      Without one of these occurring first, you can't execute any other LSAPI 
      functions. */

   { afterloadE,  API, 
     aS("Can't add extended predicates after a .XPL file is loaded.") },
   /* 202
      Extended predicates, entered with lsAddPred or lsInitPreds, must be 
      defined after the Logic Server is initialized and before the .XPL file 
      is loaded.  This is because the .XPL loader uses extended predicate 
      definitions to resolve references to extended predicates. */

   { argrangeE,   API, 
     aS("API function called with invalid index (%d) for structure %s.") },
   /* 203
      In a Logic Server API call that takes a structure and argument index, 
      the index was not valid for the structure.  That is, it was either 
      less than 1 or greater than the arity of the structure. */

   { arglrangeE,  API, 
     aS("API function called with invalid list index (%d).  Must be 1 or 2.")},
   /* 204
      A list can be considered as a structure with two arguments.  In this 
      case the term in the Logic Server API call was a list and it was given 
      an index that was not either 1, the head, or 2, the tail. */

   { notcstrE,    API, 
     aS("API function cannot map Prolog term to a string: %s.") },
   /* 205
      A Logic Server API call is trying to map the Prolog term '%s' to a 
      string, but that term is not an atom, character list or string. */

   { notcintE,    API, 
     aS("API function cannot map Prolog term to an integer: %s.") },
   /* 206
      A Logic Server API call is trying to map the Prolog term '%s' to an 
      integer, but that term cannot be converted to an integer. */

   { notcfltE,    API, 
     aS("API function cannot map Prolog term to a float: %s.") },
   /* 207
      A Logic Server API call is trying to map the Prolog term '%s' to a float,
      but that term cannot be converted to a float. */

   { notcadrE,    API, 
     aS("API function cannot map Prolog term to an address: %s.") },
   /* 208
      A Logic Server API call is trying to map the Prolog term '%s' to an 
      address, but that term cannot be converted to an address. */

   { badctypeE,   API, 
     aS("API function called with bad type code for conversion, %d.") },
   /* 209
      A Logic Server API call is trying to map a Prolog term to a host 
      language type, but the type, '%d', is not a valid host language type 
      specification. */

   { badgetfaE,   API, 
     aS("API function GetFA called with term that is not an atom, structure or list: %s.") },
   /* 210
      The term '%s' was passed to the Logic Server API function GetFA, but 
      the term is not a structure, atom or list, so it can't be broken up 
      into a functor and arity. */

   { badlistE,    API, 
     aS("API function List function called with non-list argument.") },
   /* 211
      A non-list term was passed to a Logic Server API function that requires 
      a list argument. */

   { badstreamE,  API, aS("API function called with a bad stream ID.") },
   /* 212
      A Logic Server API function that either sets or gets stream information 
      was passed an invalid stream identifier. */

   { badstrlenE,  API, 
     aS("API function request length of term that was not an atom or string.") },
   /* 213
      A Logic Server API function that attempts to calculate the length of an 
      atom or string Prolog term was not passed a term that is an atom or 
      string. */

   { badtypeE,    API, 
     aS("API function requested type of invalid Prolog term.") },
   /* 214
      A Logic Server API function that returns the type of a Prolog term was 
      given a Prolog term that is not a known type.  This error should not 
      happen, so it is likely that the term pointer has been corrupted. */

   { nonullE,     API, 
     aS("API function called with string too long, or not null-terminated: %s...") },
   /* 215
      A Logic Server API function was called with a string argument that is 
      either longer than allowed or not null-terminated. If you think the 
      string is formatted correctly then increase the Amzi! initialization 
      parameter readbuffer. */

   { badhosttypeE, API, 
     aS("API function called with bad host language type.") },
   /* 216
      A Logic Server API function that maps Prolog terms to host language 
      variables was called with a bad type indicator for the host language. */

   { extE,        EXEC, aS("Extended predicate error: %s") },
   /* 217
      An extended predicate has thrown an error with the descriptive string
      '%s'. */


   // Load Errors

   { loadfileE,      LOAD, aS("%s is not a valid Prolog load file, trying recompiling and relinking") },
   /* 300
      '%s' has either been corrupted or is not a valid load file for this
      release.  Rebuild the file.  If the problem persists, contact
      Amzi! technical support. */

   { compverE,       LOAD, 
     aS("Load module compiled with earlier version, recompile") },
   /* 302
      The load module was not compiled and linked using the current version.  
      Rebuild the file and try again. */

   { labelsE,        LOAD, 
     aS("Too many clause references (%d), increase MAXCLAUSES") },
   /* 303
      While loading a load module, one of the predicates was found to have 
      too many clauses.  Either split the predicate into multiple predicates 
      with fewer clauses or increase the Amzi! initialization parameter 
      'maxclauses'.

      If you decide to split the predicate, the following model can be used.  
      If the original predicate had clauses such as:

      duck(a1).
      duck(a2).
      ...
      duck(b1).
      duck(b2).
      ...

      You can rewrite the predicate as follows, without changing program 
      behavior.

      duck(X) :- duck1(X).
      duck(X) :- duck2(X).

      duck1(a1).
      duck1(a2).
      ...
      duck2(b1).
      duck2(b2).
      ...
      */

   { localatomsE,    LOAD, aS("Too many local atoms, limit %d") },
   /* 304
      The load module had too many atoms, increase the Amzi! initialization
      parameter 'maxatoms'.  If the problem persists, you might be able to 
      replace some of the atom definitions in your program with strings.  
      Strings are slower for pattern matching, but are not limited in space. */

   { destbufE,       FATAL, aS("Load buffer overflow, increase amzi.cfg parameter destbuf, try %d") },
   /* 305
      An internal buffer has overflowed while loading a program.  Increase
      the Amzi! initialization parameter 'destbuf'. */

   { badfixupS,      LOAD, aS("Missing local predicates:\n%s") },
   /* 306
      Obsolete error message */

   { checksumE,      LOAD, aS("Corrupted .XPL file: %s") },
   /* 307
      The .XPL file '%s' has become corrupted.  Recreate the file and try the
      application again. If the problem persists, call Amzi! technical 
      support. */

   { filemaxE,        LOAD, aS("Maximum load modules exceeded") },
   /* 308
      You have exceeded the system limit for load modules in a single 
      execution. Contact Amzi! technical support if your application needs 
      require a larger number of load modules. */

   { bufmismatS,  ABORT, aS("Error in load file") },
   /* 309
      While loading a .PLM or .XPL file, it was discovered that the .PLM or
      .XPL file was corrupted.  Try rebuilding the offending file.  If that
      doesn't fix the problem, contact Amzi! technical support. */

   { latomS,      ABORT, aS("Error loading local atom table") },
   /* 310
      This is an internal error, indicating a problem during load with the
      local atom table of a module.  Contact Amzi! technical support. */

   { longatomS,   ABORT, aS("Too long an atom in compiled code") },
   /* 311
      An atom name was encountered during a load that was longer than the
      read buffer.  If you have an extremely long atom name, then increase
      the Amzi! initialization paratemeter 'readbuffer'. */

   { longcodeS,   ABORT, 
	  aS("Code too long to load, increase srcbuf from %d to %d") },
   /* 312
      The buffer used to hold each compiled predicate during load was not
      big enough for one of your predicates.  Increase the Amzi! initialization
      parameter 'srcbuf'. */

   { manyvarS,    ABORT, aS("Too many variables in clause") },
   /* 313
      During loading, one of the clauses was found to have more variables than
      the system had space for.  Increase the Amzi! initialization paramenter
      'maxvars'. */

   { badbytecntE, ABORT, aS("Load file corrupted, aborting.") },
   /* 314
      This is an internal error caused by a corrupted load file.  Rebuild the
      file, and if the problem persists contact Amzi! technical support. */

   { labjumpE, LOAD, aS("Predicate too long to load, subdivide it.") },
   /* 315
      This error is caused by a predicate that is too long for the internal
      16-bit integer jumps.  It must be broken up into smaller predicates,
      that might have a master predicate that calls each of the smaller chunks
      in turn. */

   { add_compiledE, LOAD, aS("Attempt to add compiled code to non-compiled code for predicate %s/%d") },
   /* 316
      This is error is triggered when the loader is attempting to load
      a block of compiled code to a predicate that is already defined
      as dynamic, extended or built-in. */

   { disconE, LOAD, aS("Attempt to add discontiguous compiled code to non-discontiguous predicate %s/%d") },
   /* 317
      This is error is triggered when the loader is attempting to load
      a block of compiled code to a predicate that is already defined
      as dynamic, extended or built-in. */

   { loadloadE, LOAD, aS("Cannot load file from directive in compiled code, link or use ensure_loaded.  File: %s") },
   /* 318
      This is error is triggered when the loader is attempting to load
      a block of compiled code to a predicate that is already defined
      as dynamic, extended or built-in. */

   { latentE, LOAD, aS("A latent expression failed during loading, possible unresolved calls") },
   /* 319
      This error occurs when a latent expression fails.  Latent expressions
      must succeed.  Often the failure is due to calls to predicates that
      have not yet been loaded. */

   { redefinitionE, LOAD, aS("Attempt to redefine %s/%d") },
   /* 320
      This error occurs when an application attempts to redefine a predicate
      that is already defined in the application. */

   { syspredE, LOAD, aS("Attempt to redefine built-in system predicate %s/%d") },
   /* 321
      This error occurs when an application attempts to redefine a system
      built-in predicate. */

   { free_xplE, LOAD, aS("Attempt to run a file not supported by the free version") },
   /* 322
      This error occurs when an application attempts to run a non-system .xpl file. */

   // I/O Errors

   { stringlE,    READ, aS("String/atom too long") },
   /* 400
      A string or atom has exceeded an internal buffer used for manipulating
      strings and atoms.  Increase the Amzi! initialization parameter 
      'readbuffer'. */

   { charE,       READ, aS("Bad character %d '%c'") },
   /* 401
      The reader encountered a character it could not parse.  Fix the source 
      code so the bad character is no longer used. */

   { hexE,        READ, aS("Zero length integer denotation") },
   /* 402
      The reader was attempting to read what it thought was an integer
      denotation but it was not formatted correctly.  Check the format, fix the
      program and try again. */

   { maxvarrE,    READ, aS("Too many variables") },
   /* 403
      The reader encountered a clause with too many variables.  
      Either redesign the clause or increase the Amzi! initialization 
      parameter 'maxvars'. */

   { rdbufE,     READ, aS("Term too long to read") },
   /* 404
      The reader encountered a term that overflowed the internal read buffer.
      Either break the term up or increase the Amzi! initialization parameter
      'readbuffer'. */

   { pstackE,     READ, aS("Read stack full, increase .cfg parameter readdepth") }, 
   /* 405
      The reader encountered a term whos complexity caused an internal 
      overflow. Either simplify the term or increase the Amzi! initialization 
      parameter 'readdepth'. */

   { delimE,      READ, aS("Unexpected delimiter") },
   /* 406
      The reader encountered a delimiter character when it did not expect one.
      The term being read is syntactically incorrect. */

   { randE,       READ, aS("Unexpected operator") },
   /* 407
      The reader has been confused by the term being read.  The term is 
      syntactically incorrect, although the problem might be something other 
      than an operator definition problem.  Fix the problem and try again. */

   { inargE,      READ, aS("Read parsing error, parse stack: %d") },
   /* 408
      The reader ran into a problem interpreting an operator expression.  
      If you don't find an error in your code, contact Amzi! technical 
      support with the code that causes the error. */

   { abuffE,      READ, aS("Atom name too long to read, max size: %ld") },
   /* 409
      An atom in the code had a name longer than the read buffer.  Increase 
      the Amzi! initialization parameter 'readbuffer', or use a shorter name 
      for the atom. */

   { rdnotatomE,  READ, aS("Reader confused, expected an atom") },
   /* 410
      The reader encountered an internal error.  If the code appears correct 
		to you, contact Amzi! technical support with the offending code. */

   { parsetypeS,  READ, aS("Bad internal type in read parse") },
   /* 411
      An internal reader error occurred.  If the problem isn't obvious with
      your code, contact Amzi! technical support with the offending code. */

   { stringioE,   READ, aS("Unable to read valid term in string") },
   /* 412
      While trying to read a term from a string, the reader was unable to
      parse a valid term from that string. */

   { handleE,        EXEC, aS("Bad I/O ID passed in, %d") },
   /* 413
      An I/O predicate was called with the invalid ID '%d'. */

   { unopenedE,      EXEC, 
     aS("Attempt to redirect stream to unopened file/window, %s") },
   /* 414
      A file I/O predicate was called with handle that does not correspond
      to an open file. */

   { wr_buf_overE,   EXEC, aS("Buffer has overflowed by %d during a write.") },
   /* 415
      Write operations use the same internal buffer as read operations.  
      This message indicates that a write operation has exceeded the buffer 
      length.  Increase the Amzi! initialization parameter 'readbuffer'. */

   { curstrE,        EXEC, 
     aS("Attempt to set stream to non-open I/O handle, %d") },
   /* 416
      When attempting to change the current or user default I/O stream, an 
      invalid handle was specified.  This problem might occur from use of 
      see or tell predicates or Logic Server API calls that set the current 
      streams. */

   { iofE,           EXEC, aS("Prolog file I/O error") },
   /* 417
      An undetermined I/O error occur when writing to a file. */

   { pathlenE,       EXEC, aS("Path length too long") },
   /* 418
      A file predicate was called with a file name/path specification that 
      is longer than the allowable path name for the platform. */

   { eofE,        READ, aS("Unexpected end-of-stream on stream: %s") },
   /* 419
      An unexpected end-of-file error occurred while reading a file.  
      This error can occur with an improperly formatted binary file, .PLM or 
      .XPL, or with a file of Prolog source in which the end-of-file was 
      encountered in the middle of reading a Prolog term. */

   { fopenE,      EXEC, aS("Can't open file %s") }, 
   /* 420
      An attempt to open the file '%s' failed. */

   { ioE,         EXEC, aS("IO error: %s") },
   /* 421
      An I/O error occurred. '%s' provides additional information on where
      the error occurred. */

   { maxfileE,    FATAL, aS("Too many files opened") },
   /* 422
      There are too many files opened from Prolog.  Close some before trying 
      to open others.  If your application really needs this many files open
      contact Amzi! technical support. */

   { oopsE,       EXEC, aS("Oops error (%s), call Amzi!") },
   /* 423
      An internal error has occurred during Prolog I/O.  If it is not clear how
      your program could have caused such an error, contact Amzi! technical
      support. */

   { modsurfeitE,       EXEC, aS("Module %s already loaded") },
   /* 424
      This module is already loaded. 
      If you have a new version do UNLOAD first.*/

   { streamsurfeitE,       EXEC, aS("Stream %s already open with mode %d") },
   /* 425
      This stream is already open. */

   { escseqE, READ, aS("No closing backslash in %s denotation.") },
   /* 426
		Closing backslash not found in octal or hex constant denotation. */

   { syntaxE,       EXEC, aS("Syntax error: %s") },
   /* 427
      During a consult or compile a legal Prolog term was
      encountered which is most likely wrong,
      such as caused by punctuation errors in Prolog source. */

   { permE,       EXEC, aS("permission_error(%s, %s, %d)" ) },
   /* 428
		This operation not permitted on this object. */

   { long_macroE,       EXEC, aS("A macro exceeded 1000 characters" ) },
   /* 429
		This operation not permitted on this object. */

   { aliasE,        EXEC, aS("Bad I/O alias passed in, %s") },
   /* 430
      An I/O predicate was called with the invalid alias '%s'. */

   { setinputE,        EXEC, aS("Can't set_input to the output stream %d") },
   /* 431
       */

   { setoutputE,        EXEC, aS("Can't set_output to the input stream %d") },
   /* 432
       */

   { reposE,        EXEC, aS("Repositioning not allowed for stream %d") },
   /* 433
       */

   { invalid_sopE,  EXEC, aS("Invalid stream operation on stream %d: %s") },
   /* 434
      An attempt was made to perform an operation on a stream that the
      the stream does not support.
      */

   { longlineE, EXEC, aS("Input line longer than read buffer (%d) at line %d") },
   /* 435
      An input line longer than the read buffer was encountered.
      */
   { prevopenE, EXEC, aS("Attempt to open a previously opened file: %s") },
   /* 436
      Open a file which was already open.
      */

   { no_cinE, EXEC, aS("No response from attempt to read from standard input") },
   /* 437
      Under Windows, stdin and stdout are not defined, which is not a problem
      for output, it just disappears, but for input, causes endless waiting.
      */

   { outputdepthE, EXEC, aS("Output term nested too deep, possible cyclic term or increase .cfg outputdepth") },
   /* 438
      Nesting level of output term was too deep.
      */

   { commentE, EXEC, aS("Unbalanced block comment, missing closing */") },
   /* 439
      A block comment wasn't closed
      */

   { balanceE, EXEC, aS("Unbalanced delimiter: %c") },
   /* 440
      No closing delimiter found.
      */

   // Arithmetic Errors

   { arithopE,       EXEC, aS("Bad arithmetic operator in term: %s") },
   /* 500
      An invalid arithmetic operator was found in term. */

   { arithargE,      EXEC, aS("Bad arithmetic argument in term: %s") },
   /* 501
      A term being evaluated arithmetically contained the value '%s' which is
      not valid for arithmetic evaluation.  Arithmetic evaluation occurs for 
      arguments of the 'is' operator and arithmetic comparison operators. */

   { zero_divideE,       ARITH, aS("Divide by zero") },
   /* 502
      An arithmetic expression included a division by zero. */

	{ type_error,  ARITH, aS("Type should be %s, but arg %s is not.") },
   /* 503
      The operator argument is not a valid type for evaluation.  
		Arithmetic evaluation occurs for arguments of the 'is' operator 
		and arithmetic comparison operators. */

	{ calculationE,  ARITH, aS("Calculation error in term: %s") },
   /* 504
      Some kind of calculation error occurred.  
		Arithmetic evaluation occurs for arguments of the 'is' operator 
		and arithmetic comparison operators. */
      
	{ number_castE,  INTERNAL, aS("Invalid number cast in term: %s") },
   /* 505
      A number was cast to a non-numeric type */
      
	{ number_domainE,  ARITH, aS("Invalid numeric value in evaluable term: %s") },
   /* 506
      A number has a value which it shouldn't have. */
      
	{ number_indexE,  INTERNAL, aS("Index in real array out of bounds: %s") },
   /* 507
      Some kind of calculation error occurred.  
		Arithmetic evaluation occurs for arguments of the 'is' operator 
		and arithmetic comparison operators. */
      
	{ number_sizeE,  ARITH, aS("Number too big in term: %s") },
   /* 508
      A number is too large for its type. */
      
   
   // Execution Errors

   { sysassertE,  EXEC,  
     aS("Attempt to assert a previously compiled or protected predicate: %s")},
   /* 1000
      The application tried to assert the predicate '%s', which has already 
      been loaded as a compiled predicate, or is a system predicate.

      This error often occurs when a program contains discontiguous clauses
      defining a predicate.  This type of program will run interpreted OK, but
      when compiled, each set of clauses compiles into a separate block.  When
      the compiled module is loaded this error will be generated when the 
      loader tries to load the second block of clauses.
      
      This error can also result from a simple typing error.  If, for example,
      a clause ends in a comma, rather than a period, or a clause has a period
      in the middle of the goals, rather than a comma, the compiler might 
      think you are defining the comma operator.

      Another common typing error is to miss an argument in one clause of a 
      predicate that has many clauses.  This will insert a predicate of the 
      wrong arity in between the clauses with the correct arity.

      Look at the compiler output to make sure the clauses that are compiled
      represent the predicates you think should be defined in your program. */

   { userexitE,   ABORT, aS("User exit") },
   /* 1001
      The application program executed the goal abort(1) */

   { userfatalE,  FATAL, aS("User reset") },
   /* 1002
      The application program executed the goal abort(0). */

   { userabortE,  ABORT, aS("User abort") },
   /* 1003
      The application program executed the goal abort(2). */

   { badopcodeS,  ABORT, aS("Bad op code in compiled code") },
   /* 1004
      While executing compiled Prolog code, a bad Prolog op code was
      encountered. This should not happen, and indicates that the compiled
      code was somehow corrupted.  Unless you suspect your host language
      application is corrupting memory, contact Amzi! technical support. */

   { badrefS,     INTERNAL, aS("Bad choice point heap reference") },
   /* 1005
      During heap garbage collection, a bad heap reference was found.  This
      is an internal error that shouldn't occur.  Contact Amzi! technical
      support.  In the meantime you can work around the problem by increasing
      the heap size with an Amzi! initialization parameter or, if the code
      is running interpreted, by compiling the code. */

   { badtagS,     ABORT, aS("Bad data type in load file") },
   /* 1006
      While loading a .PLM or .XPL file a bad tag was found.  This is an
      internal error, indicating the file being loaded is corrupted.  Recreate
      the file and if the problem persists contact Amzi! technical support. */

   { bigchunkS,   ABORT, aS("Attempt to allocate too big a chunk") },
   /* 1007
      This is an internal error, contact Amzi! technical support. */

   { hbadtermS,   ABORT, aS("Bad term on heap during GC") },
   /* 1008
      This is an internal error triggered during heap garbage collection. 
      Contact Amzi! technical support.  In the meantime you can work around 
      the problem by increasing the heap size with an Amzi! initialization 
      parameter or, if the code is running interpreted, by compiling it. */

   { heapoverS,   ABORT, aS("Heap has overflowed before garbage collection") },
   /* 1009
      A percentage of the heap is reserved for overflow.  This error indicates
      that that percentage was not enough to prevent the heap from overflowing
      before garbage collection could take place.  Increase the Amzi!
      initialization parameter 'heapbumper'. */

   { memallocS,   ABORT, aS("Memory allocation error") },
   /* 1010
      This is an internal error indicating an error in the memory management
      sub-system.  Contact Amzi! technical support. */

   { stroverS,    ABORT, aS("String to char list string buffer overflow") },
   /* 1011
      This is an internal error indicating a string being converted
      to a list of codes was longer than its internal buffer. This shouldn't
      happen.  Contact Amzi! technical support. */

   { thingS,      ABORT, aS("No more space for things") },
   /* 1012
      Certain Prolog types are stored in a separate area of memory.  These
      include long integers, strings and addresses.  This message indicates
      that storage area is full.  You can increase this storage by increasing
      the Amzi! initialization parameter 'thingblksz'.  Garbage collection
      should make this message relatively rare.  If you don't think your
      application should run out of this storage, contact Amzi! technical
      support. */

   { wrongwriteS, ABORT, aS("Write error parsing term") },
   /* 1013
      This is an internal error indicating the write function was unable
      to process a term to be written.  Contact Amzi! technical support. */

   { outofmemE,   ABORT, aS("Out of memory allocating: %s") },
   /* 1014
      There is insufficient memory available to continue running the
      application. The resource being allocated when memory ran out
      was '%s'. */

   { sysretractE, EXEC,  aS("Attempt to retract a protected predicate: %s") },
   /* 1015
      The application as attempted to retract the predicate '%s', which is
      either a system predicate or a protected predicate. */

   { heapE,       FATAL, 
     aS("Heap space full, compile code or increase heap") },
   /* 1016
      The heap became full before the garbage collector could be called. You
      can increase the heap with the Amzi! initialization parameter 'heap', 
      or you can give the garbage collector a better chance by increasing 
      the 'heapbumper' initialization parameter.

      If your program is running interpreted, you can greatly reduce the heap
      usage by compiling the program.  This is because compiled code optimizes
      both speed and the use of internal resources, such as the heap.

      If the problem still persists, you might have to replace some recursive 
      control structures in your application with repeat/fail control 
      structures. */

   { heapgcE,     FATAL, 
     aS("Heap fully compacted, no more space, compile code or increase heap")},
   /* 1017
      The heap is full and can not be further garbage collected.  
      You can increase the heap with the Amzi! initialization parameter 'heap'.

      If your program is running interpreted, you can greatly reduce the heap
      usage by compiling the program.  This is because compiled code optimizes
      both speed and the use of internal resources, such as the heap.

      If the problem still persists, you might have to replace some recursive 
      control structures in your application with repeat/fail control 
      structures. */

   { alignE,      ABORT, 
	  aS("Memory not aligned on 4-byte boundary for %s.  Contact Amzi!") },
   /* 1018
      This is an internal error that should not occur. It indicates that the 
		internal control structure '%s' is not properly aligned.  
		Contact Amzi! technical support. */

   { manyretracE, FATAL, aS("Too many references to the same clause") },
   /* 1019
      The dynamic database garbage collection routines keep track of the 
      active use of each clause.  This error means one particular clause is 
      being simultaneously accessed a great many times.  This should not be 
      an ordinary occurence. */

   { trailE,      FATAL, 
     aS("Trail space full, %ld of %ld. Compile code or increase trail") },
   /* 1020
      The trail stack has overflowed.  If your program is running interpreted
      you can decrease trail use by compiling the code.  You can also 
      increase the amount of trail space with the Amzi! initialization 
      parameter 'trail'. */

   { choiceE,     FATAL, 
	  aS("Control stack full. Compile code or increase .cfg parameter 'control'") },
   /* 1021
      The Prolog execution control stack is full.  Compiling your code will 
      decrease the impact on the control stack for recursive predicates.  
      You can also increase the Amzi! initialization parameter 'control' to 
      create a larger control stack.
      If the problem persists, you might have to replace some recursive loops 
      in your program with repeat/fail loops. */

   { instanceE,   EXEC, aS("Argument instantiation error: %s.") },
   /* 1022
      An argument to a built-in predicate was not correctly instantiated, as
      indicated by the additional information '%s' provided in the message. */

   { typeE,       EXEC, aS("Argument type error: %s.") },
   /* 1023
      An argument to a built-in predicate was not the correct type, as
      indicated by the additional information '%s' provided in the message. */

   { natomsE,     EXEC, 
     aS("Maximum number of atoms exceeded (%d). Use strings or increase atomtable") }, 
   /* 1024
      The atom table is full.  You can increase the size of the atom table 
      using the Amzi! initialization parameter 'maxatoms'.
      
      You might also consider using strings instead of atoms for some 
      situations in your program.  Strings are slower to unify because 
      they compare character by character, but are not as limited in storage. 
      So if there are places where you use atoms simply to display 
      information, then those terms are probably better stored as strings. */

   { maxvarE,     EXEC, aS("Too many variables") },
   /* 1025
      A call to the built-in predicate 'functo'r requested an arity that 
      is greater than the maximum number of variables allowed.  
      Either correct the call or increase the Amzi! initialization parameter 
      'maxvars'. */

   { opopE,       EXEC, aS("Operator definition error: %s") },
   /* 1026
      An attempt to create an operator has failed.  Check the operator 
      definition statements for syntactical correctness. */

   { sysargE,     EXEC, aS("System pred - bad argument") },
   /* 1027
      A built-in predicate had a bad argument.  
      Either it was the wrong type or it was not correctly instantiated 
      (a variable when it should be bound, or bound when it should be a 
      variable). */

   { intargE,     EXEC, aS("Needed integer argument") },
   /* 1028
      A call to the built-in predicate arg had an argument number that was
      not an integer. */

   { breakE,      FATAL, aS("Break hit, or equivalently character 0x03 read in") },
   /* 1029
      The user hit break during execution of a program, or while reading input
      */

   { localE,      FATAL, 
     aS("Local stack full, compile code or increase local") },
   /* 1030
      The local stack is full.  
      You can minimize impact on the local stack by compiling your code.  
      You can increase the local stack with the Amzi! initialization
      parameter 'local'. */

//   { variE,       EXEC, aS("Too many variables in term (copyt)") },
   /* 1031
      A term with too many variables was being added to the dynamic database.
      Either simplify the term or increase the Amzi! initialization parameter
      'maxvars'. */

   { badopE,      FATAL, aS("Bad op code, %d, at offset %d") },
   /* 1032
      A bad opcode was encountered when loading compiled code.  
      Try rebuilding the file and if that doesn't correct the problem 
      contact Amzi! technical support. */

   { argE,        EXEC, aS("Bad system argument: %s") },
   /* 1033
      A built-in predicate had an invalid argument.  The text '%s' provides
      further information. */

   { badthrowtagE,EXEC, aS("User throw: %s") },
   /* 1034
      A throw or cut_tag predicate was called with a tag for which there was
      no catch or tag.  The offending tag is '%s'. */

   { execE,       EXEC, aS("Execution error: %s") },
   /* 1035
      An execution error occurred during the call to a built-in predicate in
      the Prolog portion of the runtime library.  The message '%s' provides
      more information. */

   { cuttagE,     FATAL, aS("Secondary error during LSAPI error recovery; use apitrace to find primary error.") },
   /* 1036
      A tag for a catch wound up in a position on the control stack where it
      could not be reached.  This can easily happen when there is an error
      in a NewProve or friend, like under an API call. */

   { cutdE,       FATAL, aS("Bad cut") },
   /* 1037
      A cut, probably in a compiled not or if-then-else ( -> ; ), did not
      cut correctly.  If the problem occurs in a cut in one of the two
      constructs mentioned above, recode that section without the
      embedded cut. */

   { stringoverE, EXEC, 
     aS("String too long, increase .ini parameter 'readbuffer'") },
   /* 1038
      A string predicate or operation created a string that is longer 
      than the internal buffer.  
      Increase the Amzi! initialization parameter 'readbuffer'. */

   { faultytowerE,ABORT, aS("Unlock software before use.") },
   /* 1039
      The system was still locked when it was used.  
      Unlock the software and then continue. 
      Contact Amzi! technical support if you have questions. */

   { badbipE,     INTERNAL, aS("Bad built-in predicate") },
   /* 1040
      An internal error occurred accessing a built-in predicate.  
      Contact Amzi! technical support. */

   { badxpredE,   INTERNAL, 
     aS("Built-in or extended predicate %s/%d had a bad return code: %d") },
   /* 1041
      A built-in predicate encountered an internal error.  
      Contact Amzi! technical support. */

   { atomtablesizeE,   LOAD, aS("maxatoms specified is larger than 32.") },
   /* 1042  maxatoms specified is larger than 32. */

   { badprepE,   LOAD, aS("\nPreprocessor error, file: %s line: %d\n%s") },
   /* 1043 Preprocessor error. */

   { retractE,   EXEC, 
	  aS("Attempt to retract clause which is being used: %s") },
   /* 1044 The program attempted to retract a clause that was being used
      from some early, on the control stack, predicate. */

   { abolishE,   EXEC, 
	  aS("Attempt to abolish a non-dynamic predicate: %s:%s/%d") },
   /* 1046 The program attempted to abolish a predicate that was not
      dynamic, probably either compiled or a built-in or extended predicate. */

   { indexE, EXEC, aS("%s array index %d out of bounds") },
   /* 1047 index out of bounds. */

   { heapofloE, EXEC, aS("%s heap overflow") },
   /* 1048 heap overflow. */

   { heapufloE, EXEC, aS("attempt to extract from empty %s heap") },
   /* 1049 heap underflow. */

   { struntermE, READ, 
      aS("missing closing backquote for string '%c' (use 0'x for characters, not `x)")	},
   /* 1050 string unterminated. */

   { nestedexecE, READ, 
      aS("lsExec or lsCall was called before completing a previous read()")	},
   /* 1051 An a nested execProve loop was started when the system
      was in the middle of a read.  For now, this is an error. */

   { badgoalE, EXEC, 
      aS("attempt to process an invalid goal: %s")	},
   /* 1052 An invalid term for a goal was passed to the goal constructor. */

   { badindexedE, EXEC, 
      aS("indexed directive applied to existing non-indexed predicate: %s/%d")	},
   /* 1053 An invalid term for a goal was passed to the goal constructor. */

   { unknown_modeE, EXEC, 
      aS("set_mode/get_mode was called with an unknown mode")	},
   /* 1054 an unknown mode was specified in a set/get mode goal. */

   { unknown_flagE, EXEC, 
      aS("attempt to set an unsettable Prolog flag, %s")	},
   /* 1055 an unsettable flag was specified in set_prolog_flag/2 */

   { maxmodsE, EXEC, 
      aS("too many modules have been created")	},
   /* 1056 too many modules created */

   /*
   { locked_featureE, EXEC,
      aS("%s feature locked in this version - contact Amzi! for professional or academic upgrade.") },
      */
   /* 1057 the feature is locked in this version */

   { cyclicE, EXEC,
      aS("A runtime error, possible cyclic term.") },
   /* 1058 a runtime error while looping on a term, probably a cyclic redundancy */

   { choice_debugE,     FATAL, 
	  aS("Control stack full running debugger. Set 'cut display' debug option to 'hide cut stack frames'") },
   /* 1059
      The Prolog execution control stack is full.  Compiling your code will 
      decrease the impact on the control stack for recursive predicates.  
      You can also increase the Amzi! initialization parameter 'control' to 
      create a larger control stack.
      If the problem persists, you might have to replace some recursive loops 
      in your program with repeat/fail loops. */

   { undefinedE, EXEC, 
      aS("undefined predicate: %s/%d")	},
   /* 1060 An undefined predicate was called, when the undefined predicate option is on. */

   { securityE, INTERNAL, aS("Error in security code: %s") },
   /* 1200 internal error in security checking */

   { no_local_amziE, SECURITY, aS("Attempt to run non-distributable .xpl file on a machine without an Amzi! installation.") },
   /* 1201 */

   { expired_xplE, SECURITY, aS("Thank you for trying Amzi!  The evaluation period has ended.\nPlease either\n:   1) purchase a license at www.amzi.com, or\n   2) contact Amzi! for an extension on the evaluation period.\nIn either case, you will have to rebuild your application.") },
   /* 1202 */

   { locked_featureE, SECURITY, aS("Attempt to use a feature not available with current license: %s.") },
   /* 1203 */

   { registryE, SECURITY, aS("Error accessing Amzi! environment: %s") },
   /* 1204 */

   { not_activeE, SECURITY, aS("Amzi! not activated. Please activate as either:\n   1) a free (IDE only),\n   2) evaluation, or\n   3) licensed copy\nusing the IDE menu File/Activate, or\nthe 'activate' command in the amzi/apls/bin directory.") },
   /* 1205 */

   { reg_corruptE, SECURITY, aS("Activation information out of date, please reactivate.") },
   /* 1206 */

   { eval_expiredE, SECURITY, aS("Thank you for trying Amzi! The evaluation period is over, please either:\n   1) purchase a license from www.amzi.com, or\n   2) contact Amzi! for an extension, or\n   3) re-activate as a free, IDE only, version.") },
   /* 1207 */

   { stubE,   EXEC, aS("Function not implemented: %s") },
   /* 8888 Attempt to execute stubbed out code. */

   { internalE, INTERNAL, aS("Contact Amzi! - internal error: %s") },
   /* 8889 Internal error, shouldn't occur, contact Amzi! */

   { unknownE,    UNKNOWN, aS("Unknown error") }
};

ErrFmt *ErrFmts::GetFmt(ErrNo err)
{
  ErrFmt* pfmt;
  
  for(pfmt = Fmts; pfmt->err != unknownE; pfmt++)
	 if (pfmt->err == err) 
		break;
  return pfmt;
}

LExcept::LExcept()
{
   //m_peng = peng;
   LNEWX(m_apicall, LString);
   LNEWX(m_msg, LString(aS("Uninitialized Exception")));
   LNEWX(m_predinfo, LString);
   LNEWX(m_callstack, LString);
   LNEWX(m_readfilename, LString);
   LNEWX(m_readtext, LString);
   LNEWX(m_loadfilename, LString);

   m_err        = unknownE;
   m_extype     = UNKNOWN;
   m_readlineno = 0;
   m_bExecErr   = LFALSE;
   m_bReadErr   = LFALSE;
   m_bLoadErr   = LFALSE;
}

LExcept::LExcept(ExType extype, ErrNo err, LString msg)
{
#ifdef BUG_SYNC
   *g_sync_file << "LExcept/3 " << err << NL << FLUSH;
#endif
   LNEWX(m_apicall, LString);
   LNEWX(m_msg, LString(msg));
   LNEWX(m_predinfo, LString);
   LNEWX(m_callstack, LString);
   LNEWX(m_readfilename, LString);
   LNEWX(m_readtext, LString);
   LNEWX(m_loadfilename, LString);

   m_err        = err;
   m_extype     = extype;
   m_readlineno = 0;
   m_bExecErr   = LFALSE;
   m_bReadErr   = LFALSE;
   m_bLoadErr   = LFALSE;
}

LExcept::LExcept(const LExcept &ex)
{
#ifdef BUG_SYNC
   *g_sync_file << "LExcept/1 " << NL << FLUSH;
#endif
   LNEWX(m_apicall, LString(*ex.m_apicall));
   LNEWX(m_msg, LString(*ex.m_msg));
   LNEWX(m_predinfo, LString(*ex.m_predinfo));
   LNEWX(m_callstack, LString(*ex.m_callstack));
   LNEWX(m_readfilename, LString(*ex.m_readfilename));
   LNEWX(m_readtext, LString(*ex.m_readtext));
   LNEWX(m_loadfilename, LString(*ex.m_loadfilename));

   m_err        = ex.m_err;
   m_extype     = ex.m_extype;
   m_readlineno = ex.m_readlineno;
   m_bExecErr   = ex.m_bExecErr;
   m_bReadErr   = ex.m_bReadErr;
   m_bLoadErr   = ex.m_bLoadErr;
}


LExcept::LExcept(ErrNo err, ...)
{
#ifdef BUG_SYNC
   *g_sync_file << "LExcept/... " << err << NL << FLUSH;
#endif
   va_list  args;
   aCHAR   msg[MAXMSG];

   ErrFmt  *pfmt = ErrFmts::GetFmt(err);

   va_start(args, err);
// For portability we opt for the more dangerous, but
// standard non 'n' version
//   Lvsnprintf(msg, MAXMSG-1, pfmt->fmt, args);
   Lvsprintf(msg, pfmt->fmt, args);
   va_end(args);

// std::cout << "==> LExcept" << NL;
// std::cout << SP2 << "# " << err << SP2 << msg << NL;
// std::cout << "<== Throwing Exception" << NL << FLUSH;

   LNEWX(m_apicall, LString);
   LNEWX(m_msg, LString(msg));
   LNEWX(m_predinfo, LString);
   LNEWX(m_callstack, LString);
   LNEWX(m_readfilename, LString);
   LNEWX(m_readtext, LString);
   LNEWX(m_loadfilename, LString);

   m_err        = err;
   m_extype     = pfmt->type;
   m_readlineno = 0;
   m_bExecErr   = LFALSE;
   m_bReadErr   = LFALSE;
   m_bLoadErr   = LFALSE;

#ifdef LANDFILL
*g_dump << "==> Error" << NL;
*g_dump << SP2 << "# " << err << SP2 << msg << NL;
*g_dump << "<== Throwing Exception" << NL << FLUSH;
#endif
}


LExcept::~LExcept()
{
   delete m_apicall;
   delete m_msg;
   delete m_predinfo;
   delete m_callstack;
   delete m_readfilename;
   delete m_readtext;
   delete m_loadfilename;

   m_apicall      = NULL;
   m_msg          = NULL;
   m_predinfo     = NULL;
   m_callstack    = NULL;
   m_readfilename = NULL;
   m_readtext     = NULL;
   m_loadfilename = NULL;
}

LExcept& LExcept::operator=(const LExcept &ex)
{
   if (&ex == this)
      return *this;

   m_err           =  ex.m_err;
   m_extype        =  ex.m_extype;
   *m_msg          = *ex.m_msg;
   *m_apicall      = *ex.m_apicall;
   *m_predinfo     = *ex.m_predinfo;
   *m_callstack    = *ex.m_callstack;
   *m_readfilename = *ex.m_readfilename;
   *m_readtext     = *ex.m_readtext;
   m_readlineno    =  ex.m_readlineno;
   m_bExecErr      =  ex.m_bExecErr;
   m_bReadErr      =  ex.m_bReadErr;
   m_bLoadErr      =  ex.m_bLoadErr;
   return *this;
}

STRptr LExcept::GetMsg()
{   return *m_msg; }

STRptr LExcept::GetAPICall()
{   return *m_apicall; }

STRptr LExcept::GetPredInfo()
{   return *m_predinfo; }

STRptr LExcept::GetCallStack()
{   return *m_callstack; }

STRptr LExcept::GetReadFileName()
{   return *m_readfilename; }

STRptr LExcept::GetReadText()
{   return *m_readtext; }

STRptr LExcept::GetLoadFile()
{   return *m_loadfilename; }

void LExcept::SetAPICall( const LString s)
{   *m_apicall = s; }

void LExcept::AddExecInfo(STRptr func, ARITY ar)
{
   const int bufsize = 120;
   aCHAR buf[bufsize+1];
   if (m_err != faultytowerE)
     {
#ifdef _UNICODE
       Lsprintf(buf, bufsize, aS("%ls/%d"), func, ar);
#else
       Lsprintf(buf, bufsize, aS("%s/%d"), func, ar);
#endif
       *m_predinfo = buf;
     }
   else
     *m_predinfo = aS("");  // No need to say where they came from in this case

   m_bExecErr = LTRUE;
}

void LExcept::AddExecCallStack(LString sB)
{
   *m_callstack = sB;
}

void LExcept::AddReadInfo(LString read_buf, int err_index, long lineno, 
								  LString filename)
{
   int start, left;

   *m_readfilename = filename;
   m_readlineno = lineno;
//   *m_readtext = aS("no read buffer information");
   if ((start=err_index-READTEXTLEN+NEARHERELEN-1) < 0)
      start=0;
   else
     {
       read_buf[start]   = '.';
       read_buf[start+1] = '.';
       read_buf[start+2] = '.';
     }
   left = READTEXTLEN - (err_index - start) - NEARHERELEN + 1 + READTEXTTAIL;
   *m_readtext = read_buf.Substring(start, err_index-start)
     + aS(" **NEAR HERE** ")
     + read_buf.Substring(start+err_index, left);
   if (read_buf.Length() > err_index + left)
     *m_readtext += aS("...");
   m_bReadErr = LTRUE;
}

void LExcept::AddLoadInfo(LString loadfile)
{
   *m_loadfilename = loadfile;
   m_bLoadErr = LTRUE;
}

void LExcept::AddToMsg(LString more)
{
   *m_msg = *m_msg + more;
}

// CLSException is a simple cover on LExcept, designed
// to be used externally to the engine.  These exceptions
// are caught by the application program.
CLSException::CLSException(LExcept &ex)
{
   LNEWX(m_px, LExcept(ex));
}

// Don't just copy the pointer, disaster results
// as C++ cleans up exceptions at multiple levels
// in the stack.
CLSException::CLSException(CLSException &lsex)
{
   LNEWX(m_px, LExcept(*lsex.m_px));
}

CLSException::~CLSException()
{
   delete m_px;
   m_px = NULL;
}

/*
TF CLSException::IsExec()
{
   return LTRUE == m_px->IsExec() ? TRUE : FALSE;
}

TF CLSException::IsRead()
{
   return LTRUE == m_px->IsRead() ? TRUE : FALSE;
}

TF CLSException::IsLoad()
{
     return LTRUE == m_px->IsLoad() ? TRUE : FALSE;
}
*/

int CLSException::GetRC()
{
   return m_px->GetRC();
}

void CLSException::GetMsg(aCHAR* s, int len)
{
   LString msg = m_px->GetMsg();
   const int bufsize = 80;
   aCHAR buf[bufsize+1];

   if (m_px->IsLoad())
     {
#ifdef _UNICODE
       Lsprintf(buf, bufsize, aS("\nWhile loading: %ls"), 
                (aCHAR*)m_px->GetLoadFile());
#else
       Lsprintf(buf, bufsize, aS("\nWhile loading: %s"), 
                (aCHAR*)m_px->GetLoadFile());
#endif
       msg += buf;
     }
   if (m_px->IsExec())
     {
#ifdef _UNICODE
       Lsprintf(buf, bufsize, aS("\nWhile executing: %ls"), 
                (aCHAR*)m_px->GetPredInfo());
#else
       Lsprintf(buf, bufsize, aS("\nWhile executing: %s"), 
                (aCHAR*)m_px->GetPredInfo());
#endif
       msg += buf;
     }
   
   Lstrncpy(s, msg, len);
   return;
}

#ifdef _UNICODE
void CLSException::GetMsg(char* s, int len)
{
   LString msg = m_px->GetMsg();
   const int bufsize = 80;
   aCHAR buf[bufsize+1];

   if (m_px->IsLoad())
     {
       Lsprintf(buf, bufsize, aS("\nWhile loading: %ls"), 
					 (aCHAR*)m_px->GetLoadFile()); 
       msg += buf;
     }
   if (m_px->IsExec())
     {
       Lsprintf(buf, bufsize, aS("\nWhile executing: %ls"), 
					 (aCHAR*)m_px->GetPredInfo());
       msg += buf;
     }
   
   if (wcstombs(s, msg, len) >= len)
      s[len-1] = 0;
   return;
}
#endif

ExType CLSException::GetType()
{
   ExType x;
   x = m_px->GetType();
   return x;
   //return m_px->GetType();
}

/*
STRptr CLSException::GetAPICall()
{
   return m_px->GetAPICall();
}

STRptr CLSException::GetPredInfo()
{
   return m_px->GetPredInfo();
}

STRptr CLSException::GetCallStack()
{
   return m_px->GetCallStack();
}
*/

void CLSException::GetReadFileName(aCHAR* s, int len)
{
   Lstrncpy(s, m_px->GetReadFileName(), len);
   return;
}

#ifdef _UNICODE
void CLSException::GetReadFileName(char* s, int len)
{
   if (wcstombs(s, m_px->GetReadFileName(), len) >= len)
      s[len-1] = 0;
   return;
}
#endif


void CLSException::GetReadBuffer(aCHAR* s, int len)
{
   Lstrncpy(s, m_px->GetReadText(), len);
   return;
}

#ifdef _UNICODE
void CLSException::GetReadBuffer(char* s, int len)
{
   if (wcstombs(s, m_px->GetReadText(), len) >= len)
      s[len-1] = 0;
   return;
}
#endif

void CLSException::GetCallStack(aCHAR* s, int len)
{
   Lstrncpy(s, m_px->GetCallStack(), len-1);
   return;
}

#ifdef _UNICODE
void CLSException::GetCallStack(char* s, int len)
{
   if (wcstombs(s, m_px->GetCallStack(), len) >= len)
      s[len-1] = 0;
   return;
}
#endif



intC CLSException::GetReadLineno()
{
   return m_px->GetReadLineno();
}

/*
STRptr CLSException::GetLoadFile()
{
   return m_px->GetLoadFile();
}
*/

Lostream& operator<<( Lostream &os, const LExcept &ex )
{
   switch(ex.m_extype)
     {
     case(INIT):
       os << aS("INITIALIZATION ERROR: ");
       break;
     case(LOAD):
       os << aS("LOAD ERROR: ");
       break;
     case(EXEC):
       os << aS("EXECUTION ERROR: ");
       break;
     case(ARITH):
       os << aS("EVALUATION ERROR: ");
       break;
     case(READ):
       os << aS("READ ERROR: ");
       break;
     case(ABORT):
       os << aS("ABORT ERROR: ");
       break;
     default:
       os << aS("UNKNOWN ERROR TYPE: ");
     }
   os << ex.m_apicall << SP;
   os << (int)ex.m_err << SP;
   os << ex.m_msg;
   return os;
}

//void cow_catcher()
//{
//   int i;
//   int a = 3;
//   int b = 2;
//   i = 7 / (a - b - 1);
//}

void LExHandler::Error(ErrNo err, ...)
{
#ifdef BUG_SYNC
   *g_sync_file << "LExHandler/... " << err << NL << FLUSH;
#endif
//   cow_catcher();

   va_list  args;
   aCHAR   msg[MAXMSG];

   ErrFmt  *pfmt = ErrFmts::GetFmt(err);

   va_start(args, err);
// For portability we opt for the more dangerous, but
// standard non 'n' version
//   Lvsnprintf(msg, MAXMSG-1, pfmt->fmt, args);
   Lvsprintf(msg, pfmt->fmt, args);
   va_end(args);

// std::cout << "==> Error" << NL;
// std::cout << SP2 << "# " << err << SP2 << msg << NL;
// std::cout << "<== Throwing Exception" << NL << FLUSH;

#ifdef LANDFILL
   int iii;
   iii = 0;
   FILL("==> Error" << SP2 << "# " << err << SP2 << msg);
 //DUMP << "==> Error" << NL;
 //DUMP << SP2 << "# " << err << SP2 << msg << NL;
 //DUMP << "<== Throwing Exception" << NL << FLUSH;
#endif
   LOG( "Error handling for error # " << err );
   LOG( "Error message: " << msg );  
   //LExcept *pex = new LExcept(pfmt->type, err, msg);
   // Quietly record some types of errors
   //m_exstack.AddLast(pex);
   //if (pfmt->type == FILL)
   //   return;
   //if (pfmt->type == WARNING && m_warnerr == OFF)
   //   return;
   // But throw the rest.
   //throw(pex);
   throw LExcept(pfmt->type, err, msg);
}


TF LExHandler::p_err_fatal()
{                                        // create a fatal error, for testing
#ifdef BUG_SYNC
   *g_sync_file << "err_fatal " << NL << FLUSH;
#endif
   pXCPT->Error(userfatalE);
   return TRUE;
}


TF LExHandler::p_err_abort()
{                                        // create an abort error, for testing 
#ifdef BUG_SYNC
   *g_sync_file << "err_abort " << NL << FLUSH;
#endif
   pXCPT->Error(userabortE);
   return TRUE;
}

TF LExHandler::p_errZexec()
// err$exec(+errtype, +message, +args, +term)
//
// errtype can be 1 for type errors
//                2 for instance errors
//                3 for other execution errors
//                4 for read errors
// message is the message
// args is a Prolog term appended to the message
// term is the predicate that triggered the error
{
#ifdef BUG_SYNC
   *g_sync_file << "err$exec " << NL << FLUSH;
#endif
   int    errtype;
   TERM   ttype;
   PATOM  atype;
   int    len, plen;
   aCHAR  message[256];
   STRptr p;

   ttype = X(0);
   if (ttype->IsInt())
      errtype = ttype->getInt();
   else if (ttype->IsAtom())
   {
      atype = ttype->getAtom();
      if (atype == pATAB->typeEA) errtype = 1;
      else if (atype == pATAB->instanceEA) errtype = 2;
      else if (atype == pATAB->execEA) errtype = 3;
      else if (atype == pATAB->syntaxEA) errtype = 4;
      else
         Error(typeE, aS("arg1 is not a known error type"));
   }

   Lstrncpy(message, X(1)->getStr(), 255);
   len = (int)Lstrlen(message);
   if (len < 240)
   {
      p = message + len;
      plen = 255 - len;
   }

   pWRIT->termWriteString(X(2), p, plen, TRUE);

   // fourth argument not really used, err$exec always
   // becomes the predicate reported.  This needs to be
   // rethought.
   /*
   TERM t;
   int i;
   t = X(3);
   if (!t->IsStruct())
      Error(typeE, aS("arg4 must be a structure"));

   //t = pTSVC->StrucFA(t);
   t = t->getTerm();
   pEXEC->SetEsc_predA(t->getAtom());
   pEXEC->SetEsc_arity(t->getArity());
   for (i = 0; i < (int)pEXEC->GetEsc_arity(); i++)
      (pHXL->XVar(i))->setTerm( ++t);
   */

   switch(errtype)
     {
     case 1: Error(typeE, message); break;
     case 2: Error(instanceE, message); break;
     case 3: Error(execE, message); break;
     case 4: Error(syntaxE, message); break;
     }
   return TRUE;
}













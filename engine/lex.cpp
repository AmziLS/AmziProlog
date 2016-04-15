/****************************************************************************\
* lex.cpp
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
* author: Ray Reeves December 1999
*
* $Log: lex.cpp,v $
* Revision 1.5  2006/09/11 15:19:31  mary
* Updated to Visual Studio 8 and .NET 2.0 in version 7-5-0.
*
* Revision 1.4  2006/01/04 16:42:29  dennis
* /* bug hack fix
*
* Revision 1.3  2005/02/21 17:40:08  dennis
* vba addition
*
* Revision 1.2  2005/02/09 17:35:39  dennis
* ?var added
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.52  2002/12/02 18:25:12  dennis
* Converted to Visual Studio .NET.
*
* Revision 1.51  2002/05/15 16:59:08  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.50  2002/05/02 17:39:30  dennis
* various minor bug fixes, added locale as a settable prolog flag
*
* Revision 1.49  2002/04/19 19:41:43  dennis
* fixed retract bug with sorted/indexed clauses, implemented abolish for
* those types as well
*
* Revision 1.48  2002/04/09 17:31:18  ray
* corrected error in divide and mod arith
*
* Revision 1.47  2002/03/25 22:24:06  dennis
* Added read/write_binary predicates to replace fwrite, also repositioning
* for binary files. Note that even though the standard says there's a read
* and write position, they are one and the same.
*
* Revision 1.46  2002/03/21 01:25:09  dennis
* allow separator in file names to be either tilt on slash, converts
* to correct one for Unix or Windows
*
* Revision 1.45  2002/02/19 04:11:39  dennis
* changed reals to use pass by reference, eliminating almost all needs
* for new and delete, seems to have eliminated most all leaks due to math,
* put in memcpy, memmove etc. for copying gigit arrays around.
*
* Revision 1.44  2002/02/13 03:20:00  dennis
* reals changed to have a LReal class, moved to file of same name,
* math functions moved out of termsvc and into lmath.cpp/h, eval rewritten
* to reflect various options for numbers, lexcept modified so anyone, even
* non-engine objects, can throw LExcept objects.
*
* Revision 1.43  2002/02/04 17:20:59  dennis
* New lreal wrapper class on reals, start of number options:
* decimals: real/float, floats: single/double.  Created lmath.h/cpp
* but nothing in them yet.  Work to be done on sorting out mixed mode
* arithmetic for different options.  Also - casts removed, as they
* were causing problems.
*
* Revision 1.42  2002/01/28 06:29:19  dennis
* changes for parsing numbers, handling different options
*
* Revision 1.41  2002/01/27 22:52:05  ray
* Corrections to power and fixed
*
* Revision 1.40  2002/01/20 20:48:06  ray
* revised real divide, printReal
*
* Revision 1.39  2002/01/06 20:31:28  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.38  2001/12/23 20:08:19  dennis
* made reposition of text files work for both gets and reads
*
* Revision 1.37  2001/12/22 18:50:19  dennis
* allowed for repositioning of text streams, a bit of a tricky business.
*
* Revision 1.36  2001/11/11 13:47:56  dennis
* fixed bug in meta$ processing of consulted clauses
*
* Revision 1.35  2001/10/13 02:58:13  dennis
* see/tell bugs, used to close function streams
*
* Revision 1.34  2001/10/06 03:08:00  dennis
* running on Linux mostly, compiling compiler at least
*
* Revision 1.33  2001/10/05 19:15:17  dennis
* string streams, function streams working, reals set up to not require
* m_peng, just passed in parameters as necessary
*
* Revision 1.32  2001/10/02 16:05:21  dennis
* changed streams, cleaner interface to lex, reimplemented
* readers as buffer between the two
*
* Revision 1.31  2001/09/19 02:35:33  dennis
* fixed io stuff
*
* Revision 1.30  2001/09/11 04:34:56  dennis
* cleaned up some io stuff, got consult working, etc.
*
* Revision 1.29  2001/09/08 15:27:57  dennis
* compiler working without calling fopen or fleopen, all open calls
*
* Revision 1.28  2001/09/04 01:57:00  dennis
* changed stream vector to reuse empty slots
*
* Revision 1.27  2001/08/29 19:34:33  ray
* Corrections to 'seen' in alib etc.
*
* Revision 1.26  2001/08/16 17:07:25  ray
* merged everything, repaired errors, added BINSTREAM
*
* Revision 1.25  2001/08/08 00:21:16  dennis
* unworking commit - stream bugs need fixing
*
* Revision 1.24  2001/08/02 18:51:00  dennis
* merge of new streams complete
*
* Revision 1.23  2001/08/01 20:17:59  ray
* removed tswrite.cpp & sio.cpp, added streams.cpp sowrite.cpp
*
* Revision 1.22  2001/07/21 00:39:46  dennis
* added garbage collector for strings and things
*
* Revision 1.21  2001/07/10 16:51:32  dennis
* added more memory leak checking tools, all clean so far
*
* Revision 1.20  2001/06/27 15:15:10  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.19  2001/04/18 15:30:50  ray
* removed enum as builtin
* allowed it as prep directive and .h file statement
*
* Revision 1.18  2001/04/16 05:21:14  dennis
* hacked together some fixes for sio/lex to be better friends,
* merged other changes, added new samples
*
* Revision 1.17  2001/04/02 21:50:13  dennis
* got debugger working again
*
* Revision 1.16  2001/03/28 15:07:21  ray
* char_code/2, number_chars/2 added
* 1 char macros proscribed
*
* Revision 1.15  2001/03/25 15:29:51  ray
* Implemented fixed
* repaired #include
*
* Revision 1.14  2001/02/27 21:09:13  ray
* removed ? from properNames
* implemented integer/1 for reals
* repaired -ve reals error
*
* Revision 1.13  2001/02/26 16:44:14  ray
* repaired properNames to not accept digit initials
*
* Revision 1.12  2001/02/25 16:23:01  ray
* repaired macro string args
*
* Revision 1.11  2001/02/24 19:59:10  ray
* sio tsread and lload were modified
*
* Revision 1.10  2001/02/24 13:20:20  ray
* repaired comment bug
*
* Revision 1.9  2001/02/22 15:52:00  ray
* repaired rpoper names and proper quotes
*
* Revision 1.8  2001/02/22 15:27:20  ray
* repaired proper names
*
* Revision 1.7  2001/02/18 18:47:14  ray
* Substituted end_of_file for !EOS
* Replaced ` with 0' for char code denotation
* Introduced ` quote mark as default for strings
*
* Revision 1.6  2001/02/11 19:59:33  ray
* fixed reconsult bug
*
* Revision 1.5  2001/02/10 05:02:57  dennis
* fixed assert/retract gc bug
*
* Revision 1.4  2001/02/08 22:56:45  dennis
* string bug fixes, modularized compiler and listener
*
* Revision 1.3  2001/02/07 18:42:25  ray
* made grab_str accept \n
*
* Revision 1.2  2001/02/02 15:22:31  ray
* corrected read(X)
* enabled arithmetic with char args
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.36  2000/12/28 16:30:35  dennis
* fixed nasty pets bug, true/false on CallInterp()
* and other changes, merged with ray's new stuff
* calling this a6-1-6
*
* Revision 1.35  2000/12/19 20:35:27  ray
* Modified lex for '.' operators
* Added float_real/2 (bilateral)
*
* Revision 1.34  2000/11/10 17:09:18  ray
* Restricted real length to 255 in order to add max length, for protection
*
* Revision 1.33  2000/10/25 18:35:27  ray
* Corrected unary minus.
*
* Revision 1.32  2000/10/22 17:54:13  ray
* Made char lists and char integers produce char type
* Made string-integer test for non-digits
*
* Revision 1.31  2000/10/21 03:02:42  dennis
* temp fix of .pro.pro problem in compiler
*
* Revision 1.30  2000/10/15 18:04:23  ray
* Eliminated requirement to annotate integers > 1 billion with 'L'
* Now, only numbers greater than MAXintC get promoted to Real
*
* Revision 1.29  2000/10/03 02:03:01  dennis
* patched linux back to windows, both OK now
*
* Revision 1.28  2000/10/03 01:37:17  dennis
* Got it running under Linux, had to ifdef out reals for now
*
* Revision 1.27  2000/09/20 17:09:12  ray
* added casting operators to cell and rand to clean up code.
* added prolog flag 'modulo' and adapted arithmetic to use it.
* added fraction, realSpan and nth.
* added fast ** for reals.
*
* Revision 1.26  2000/08/27 01:57:03  ray
* corrected list_real
*
* Revision 1.25  2000/08/26 00:32:06  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.24  2000/08/24 20:46:05  dennis
* fixed Lfgetc bug
*
* Revision 1.23  2000/08/21 20:36:53  ray
* reversed the order of gigadigits in real array
* real arithmetic installed for add, subtract, multiply.
*
* Revision 1.22  2000/08/14 02:05:37  ray
* Added Real class.
* No division yet.
* No error reporting yet.
*
* Revision 1.20  2000/06/11 05:26:32  ray
* introduced STRINGREADER
*
* Revision 1.19  2000/06/08 20:30:55  ray
* Changed readers to be line oriented
*
* Revision 1.18  2000/05/15 11:24:48  dennis
* fixed some minor bugs, started modules
*
* Revision 1.17  2000/05/14 03:52:33  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
* Revision 1.16  2000/05/11 00:44:01  ray
* open, close, get_char, put_char, get_code, put_code, write_term, 
  write_canonical, current_input, current_output, compound.
*
* Revision 1.15  2000/05/04 21:47:55  ray
* log, log10, xor, round, floor, ceiling, truncate/1, realpart, fractionpart
*
* Revision 1.14  2000/04/21 02:49:05  ray
*
* Added current_prolog_flags
*
* Revision 1.13  2000/04/18 01:32:01  ray
*
* Added builtin stream_attrs/1 for users and error reporting.
*
* Revision 1.12  2000/04/12 17:08:01  ray
*
* fixed fgetc bug
*
* Revision 1.11  2000/04/10 00:57:27  ray
*
* #include
*
* Revision 1.10  2000/04/02 23:54:44  ray
*
* Fixed + and - bug
*
* Revision 1.9  2000/04/01 02:52:51  dennis
* add getGraphic cast to keep gcc happy
*
* Revision 1.8  2000/04/01 00:36:11  ray
*
* fixed -ve number error
*
* Revision 1.7  2000/03/31 08:51:37  dennis
* Small bug fixes for Linux, still not running on Linux
*
* Revision 1.6  2000/03/30 07:35:42  dennis
* some minor changes to make gcc happy on Linux
*
*
* Ray Reeves December 1999
\****************************************************************************/

#include "inc.h"
#include "pch.h"

#ifdef LANDFILL
#define noBUG_LEX
#define noBUG_LEX_NUMBERS
#endif

//aCHAR LEX::c;

int branchLevel = 0; 
int skipLevel; 
int gotLevel;

// here are the dynamic routines for pre-defined macros
// dynamic means that the definition is an expression, not a constant

void getTime(LEngine *peng, aCHAR **arg)
{ 
  time_t     t;
  struct tm *st;
  short  xargs[3];

  time(&t);
  st = localtime(&t);

  xargs[0] = (short) st->tm_hour;
  xargs[1] = (short) st->tm_min;
  xargs[2] = (short) st->tm_sec;
  Lsprintf(arg[0], 22, aS("$%2d:%2d:%2d$"), xargs[0], xargs[1], xargs[2]); 
}

void getDate(LEngine *peng, aCHAR **arg)
{ 
  time_t     t;
  struct tm *st;
  short      xargs[3];
  
  time(&t);
  st = localtime(&t);
  
  xargs[0] = (short) st->tm_mon + 1;
  xargs[1] = (short) st->tm_mday;
  xargs[2] = (short) st->tm_year + 1900;
  Lsprintf(arg[0], 24, aS("$%2d/%2d/%4d$"), xargs[0], xargs[1], xargs[2]); 
}

void getLino(LEngine *m_peng, aCHAR **arg)
{ 
  Lsprintf(arg[0], 20, aS("%d"), pLEX->stream_reader->getStream()->getLineNo()); 
}

void ifNull(LEngine *peng, aCHAR **arg)
{
  *arg[0] = aS('#');
  //  *(arg[0]+1) = arg[1] == NULL || *arg[1] == aS(' ') ? aS('2') : aS('3');
  *(arg[0]+1) = arg[1] == NULL || *arg[1] == 0 ? aS('2') : aS('3');
  *(arg[0]+2) = aS('#');
  *(arg[0]+3) = 0;
}

void doEnum(LEngine *m_peng, aCHAR**)
{
  pLEX->scanEnum();
}


MacroReader::MacroReader(LEngine *peng, MACRO *m, int bufsize)
{
   m_peng = peng;
   //kind    = MACRO_;
   //nestLevel = parent ? parent->nestLevel + 1 : 1;
   //path = pATAB->macroA;
   macro = m;                                    // our very own macro
   arity = m->getArity();                             // copy macro arity
   //name  = 1 + m->name;                          // point to macro name
   bix   = 0;
   argSpaceFree = 0;
   LNEW(arg, aCHAR *[1 + arity], aS("macro"));    // 1 more than arity
   arg[0] = body = p = m->body;                  // point to macro body
   // need to separate this from dynamically allocated ones:
   for(int i = 1; i <= arity; i++)
      arg[i] = NULL;

   if (arity > 0)
   {
      argSpacesz = 1000;
      LNEW(argSpace, aCHAR[argSpacesz], aS("macro"));
   }
   else
      argSpace = NULL;
   col = 0;
   //is_macro = true;
}

MacroReader::~MacroReader()
{
  //delete [] arg;
   // note that arg is an array of string pointers,
   // and that the strings are all allocated from bits
   // and pieces of argSpace.
  delete[] arg;
  if(argSpace)
  {
     delete argSpace;
  }
   // delete [] argSpace;
}

aCHAR MacroReader::rawchar()
{
  aCHAR c = *p;     

  if(c == EOS) 
    if(bix) 
      closeArg(c);                              // may now be end of body too
    else;                       // end of body, but can't pop reader from here!
  else 
    {                                           // c was not EOS
      p++;
      col++;
    }

  if(c == aS('#') && !bix)
    if(!openArg(c))                             // start reading arg
      c = *p++;                                 // no arg, no default 
      
  return c;                                     // c may still be 0
}

void MacroReader::backupScan()
{ 
    if ( (bix > 0 && p > arg[bix]) ||
               bix == 0 && p > body)    // stop at origin, col > 0
      {
        p--;
        col--;
      }
}

bool MacroReader::openArg(aCHAR &c)
{
  aCHAR *pin = p;
  bix = 0;
  c = *p++;                                     // 1st decimal digit
  for(int i = 0; i < 4 && c != aS('#'); i++)
    {                                           // less than 5 decimal digits
      if(c < '0' || c >= '9')
        {                                       // invalid arg reference
          p = pin;
          return false;
        }
      bix *= 10;                                // accumulate
      bix += c - '0'; 
      c = *p++;                                 // consume trailing #
    } 

  //  if(bix < 1 || bix > arity)
  //    pXCPT->Error(badprepE, FILO, LINO, 
  //                 aS("macro buffer index range error") );
  if(!arg[bix] || *arg[bix] == 0)
    if(macro->defalt && macro->defalt[bix])     // NULL arg
      arg[bix] = macro->defalt[bix];            // substitute default
    else
      {                                         // no arg, no default
        bix = 0;                                // back to body
        return false;                           // c is crud
      }
                                                // we have an arg
  resumep = p;                                  // resumption point
  resumecol = col;
  p = arg[bix];
  c = *p ? *p++ : 0;                            // get arg char, could be EOS
  return true;
}

void MacroReader::closeArg(aCHAR &c)
{                                               // bix is not 0
  bix = 0;                                      // back to body
  p = resumep;
  col = resumecol;
  c = *p ? *p++ : 0;                            // get body char, could be EOS
}

void MacroReader::macCall(MACRO *macRef)
{                             // a debug routine to see the args to macro calls
  Lprintf(aS(" applying %s"), macRef->name+1);
  if(macRef->arity)
    {
     Lprintf(aS("("));
     for(int ix = 1; ix < macRef->arity; ix++)
       Lprintf(aS(" %s,"), arg[ix]);
     Lprintf(aS(" %s)\n"), arg[macRef->arity]);
    }
  else
    Lprintf(aS("\n"));
}

void MacroReader::getMacroArgs(aCHAR &c, intCH arity)
{                                               // sets c to delim
  uintCH ix;
  aCHAR  *s, delim;
  
  s = argSpace;                                 // where args go
  for(ix = 1; ix <= (uintCH)arity; ix++)
    {                                           // get each arg
      arg[ix] = s;
      delim = ix == arity ? aS(')') : aS(',');
      if(pLEX->getBalancedText(s, c, delim, argSpaceFree, 0)) 
        *s++ = 0;
      else
        arg[ix] = NULL;                         // empty arg

      if (s-argSpace >= 1000)
         pXCPT->Error(long_macroE);
      
      if(c == aS(')'))
        break;
    }
  if(ix < (uintCH)arity)   
    arg[arity] = NULL;                          // last arg was missing
  // Do not read at end, as we are about to switch readers
}

StreamReader::StreamReader(LEngine *peng, PrologStream *s, int bufsize)
{
   m_peng = peng;
   m_buflen = bufsize;
   stream = s;
   LNEW(buffer, aCHAR[bufsize], aS("reader"));
   p = buffer;
   *p = EOS;
   parent = NULL;
}

StreamReader::~StreamReader()
{
   delete buffer;
}

aCHAR StreamReader::rawchar()
{
   if (*p == EOS)
   {
      if (stream->isEndOfStream())
         return LEOF;

      getLine();
      // newline might start with EOS, so read it this way:
      return rawchar();
   }
   return *p++;
}

void StreamReader::getLine()
{
      stream->get_line(buffer, m_buflen);

      p = buffer;
}

/*
 * Soliloquy
 * There is only one instance of LEX; and it points to a stack of Readers.
 * Each reader has a body and possibly arguments (arg[n]}.
 * Each arg is a null terminated string buffer; the one in use is identified
 * by bix (the buffer index). Each Reader has a pointer p pointing to the
 * next char to be read by rawchar(). If, while reading the body, an arg 
 * reference is identified then a marker (resumep) saves the current p, bix 
 * is set to the arg and p is set to scan that arg. When a null char is read 
 * while bix is not 0 it simply means that the argument is ended and p must 
 * resume scanning the body. On the other hand, if bix is 0 then the body has 
 * ended. If the Reader kind is FILELINE or COMMANDLINE the body must be 
 * replenished with getLine(), but if the Reader kind is MACROLINE or 
 * STRINGLINE the Reader must be popped off the stack, because macros consist 
 * of only one line. Unfortunately, rawchar() is a Reader method and cannot 
 * pop itself, so popping has to be deferred. For this purpose LEX has a 
 * rawchar() which calls the Reader rawchar() and does the popping.
 */

LEX::LEX(LEngine *peng) 
{
  int i;

  m_peng = peng;
  m_c = aS(' ');                                  // start with WHITE
  acc  = 0;
  expanding = true;                             // expand mode is default
  anon_index = 0;
  preprocessing = false;
  for(i = 0; i < 32; i++)
      bucket[i] = NULL;
  for(i = 0; i < 16; i++)
      incdir[i] = NULL;
  incfree = &incdir[0];
  macroHeap      = NULL;
  topLevelBuf    = NULL;
  readBuffer     = NULL;
  nameBuffer     = NULL;
  macroFree      = NULL;
  incsave        = NULL;

  macro_reader = NULL;
}

LEX::~LEX() 
{
   delete topLevelBuf;
   delete readBuffer;
   delete nameBuffer;
   delete macroHeap;

   // delete the macros in the macro chains

   int i;
   MACRO *m, *m2;
   for (i=0; i<32; i++)
   {
      m = bucket[i];
      while (m)
      {
         m2 = m->chain;
         //delete m;
         delete m;
         m = m2;
      }
   }

   //delete[] incsave;
   delete incsave;
}

aCHAR LEX::rawchar()
{
   // sole purpose is to catch macro stream endings, but maybe
   // there is a better way to single the end of stream, as
   // returning EOS is not right for get_char.  It can return
   // the '\n' so we can see end-of-lines, and maybe EOS when
   // the stream is finished?  No should be LEOF I think.

   aCHAR c;

   if (macro_reader)
   {
      c = macro_reader->rawchar();

      // this code copied from blackchar, idea - lets have macro
      // servers which are stacked.
      if(c == EOS)
      {
			//if(stream_reader->is_macro)
			//	if(((MACROSTREAM *)stream_reader)->getBix())
			//	  ((MACROSTREAM *)stream_reader)->closeArg(c); // close out the arg
			//	else
			//	  return(c);
         if (macro_reader->getBix())
            macro_reader->closeArg(c);
         else
         {
            MacroReader *old = macro_reader;
            macro_reader = old->parent;
            delete old;
         }

         // this is some code from balancedText which may be
         // pertinent in popping macros:
         //if(c == EOS)                         // some string ended
         //   if(stream_reader->is_macro)              // an arg
         //   {
         //      MACROSTREAM * mp = (MACROSTREAM *)stream_reader;
         //      mp->closeArg(c);
         //      if(c == aS('#'))
         //         mp->closeArg(c);
         //  }

			 //if(pLEX->preprocessing) 
          //  return(c);                        // prep & macros care about LFs

			 c = rawchar();                     // read another char
      }
   }
   else
   {
      c = stream_reader->rawchar();

      // may have been a nested stream, a #include?
      while (c == LEOF && pop_stream())
         c = stream_reader->rawchar();
   }

   return c;

   //aCHAR c = stream_reader->rawchar();
   //while(!c && stream_reader->is_macro)
   //{
   //   pIO->pop_stream(read_handle);
   //   c = stream_reader->rawchar();  // is this an error? blackchar needs EOS...
   //}
  //return c;                                     // could be eof
}

void LEX::backupScan()
// input_line is the line we're working on, so backup the
// scan to the previous character, keeping the blackcount
// as we go.
{
   if (macro_reader)
      macro_reader->backupScan();
   else
      stream_reader->backupScan();


//  void backupScan()                        // stop at origin, col > 0
//  { stream_reader->backupScan(is_black(*(stream_reader->p)));}

//   if ( input_char > input_line )    // stop at origin, col > 0
//   {
//      if(is_black(input_char))
//         blackCount--; 
//      input_char--;
//      col--;
//   }
}

aCHAR LEX::blackchar(aCHAR c) 
{  // skip comments, observe EOF, get next black. do nothing if already black.
  int wasWhite;
  //STREF strmp;

  while(true)
    {
      // this won't happen because rawchar does this, so blackchar
      // won't see the LEOF unless its really the end.
      //if (c == LEOF)
      //{
      //   if (pIO->pop_stream(read_handle))
      //      c = rawchar();
      //   else
      //      return LEOF;
      //}

      // If end-of-string, then maybe we were pre-processing
      // or maybe we were really reading a string?
      // wait, rawchar is supposed to catch '0s, which is same
      // as EOS, so how could blackchar get one?
      if (c == LEOF)
         return LEOF;
      
      for(wasWhite = 0; c && pLEX->is_white(c); wasWhite|= 1) // get black
        c = rawchar();                       // EOS is black if pre-processing
      if(!c || wasWhite)                      // c is new if was_white(c)
        continue;                             // so restart

      if(c == aS('%'))                        // suppress comments
        {                                     // line comment
          pLEX->lineComment(c);
          continue;
        }

      if(c == aS('/') && stream_reader->lookahead() == aS('*'))
        {                                     // block comment
          pLEX->blockComment(c);
          continue;
        }
      break;                                  // not WHITE & not special
    }                                         // end while
  return(c);
}

int LEX::char_class(aCHAR c)
{
   // preprocessor cares about line feeds
   if (c == '\n')
      return preprocessing ? FEED : WHITE;

   if (c <= 127)
      return proclass[c];

   if (c >= 0x0080 && c <= 0x00a0)
      return WHITE;

   if (c >= 0x00a1 && c <= 0x00bf)
      return GRAPHIC;

#ifdef _UNICODE
   if ( (c >= 0x2000 && c <= 0x200f) ||
        (c >= 0xfff0 && c <= 0xfffe) ||
         c == 0xfeff )  // do this first, as in a range in LOWER
      return WHITE;

   if ( (c >= 0x2010 && c <= 0x303f) )
      return GRAPHIC;

   if ( (c >= 0x00c0 && c <= 0x1fff) ||
        (c >  0x3040 && c <= 0xd7ff) ||
        (c >= 0xe000 && c <= 0xffef) )
      return LOWER;
#endif

   return 0;
}

void LEX::lineComment(aCHAR &c)
{
   c = rawchar();
   while(c != '\n' && c != LEOF)
      c = rawchar();
  //while( (c = rawchar()) != '\n');
}

void LEX::blockComment(aCHAR &c)
{
  c = rawchar();                              // consume the leading '/'
  do
    {
      c = rawchar();                          // consume a '*'
      skip(c, aS('*'));
    } while(stream_reader->lookahead() != aS('/'));
  c = rawchar();                              // consume the exit '*'
  c = rawchar();                              // consume the exit '/'
}

bool LEX::is_atominitial(const aCHAR c)
{                 // The chars that can start an atom (the lowercase letters).
    if(pSTATE->m_properNames)
      return (char_class(c) & LETTER) && c != aS('_') && c != aS('?') ? true : false;
    else    
      return char_class(c) == LOWER ? true : false;
}

bool LEX::is_varinitial(const aCHAR c)
{                                             // chars that can start a var
  if(pSTATE->m_properNames)
    return (c == aS('_') || c == aS('?')) ?  true : false;
  else
    return (char_class(c) == UPPER) ? true : false;
}

bool LEX::is_letter(const aCHAR c)              // lower, upper or $
{                                         
  if(c == aS('$'))
    return true;

  return (char_class(c) & LETTER) ? true : false;
}

void LEX::skip(aCHAR &c, const aCHAR delim)
{  // skips to unbalanced delim
// this should be handled by rawchar
//  if (c == LEOF && stream_reader->kind == FILE_)
//    {                                         // EOF exceptional case
//      pIO->pop_stream(read_handle);
//      c = rawchar();
//    }

  if(delim == aS(')'))                        // special case
    while(c != delim)                         
      if(c == aS('('))
        {
          c = rawchar();
          skip(c, delim);                     // skip nesting
          c = rawchar();
        }
      else
        c = rawchar();
  else
    while(c != delim)
    {
       if (c == LEOF)
          if (delim == aS('*')) pXCPT->Error(commentE);
          else pXCPT->Error(balanceE, c);

      c = rawchar();                          // rawchar counts the LFs
    }
}

bool LEX::hexdigit(uintCH &value, aCHAR c)
{                                             // set value or fail
  switch(c)
    {
    case '0': value = 0; break;
    case '1': value = 1; break;
    case '2': value = 2; break;
    case '3': value = 3; break;
    case '4': value = 4; break;
    case '5': value = 5; break;
    case '6': value = 6; break;
    case '7': value = 7; break;
    case '8': value = 8; break;
    case '9': value = 9; break;
    case 'a': case 'A': value = 10; break;
    case 'b': case 'B': value = 11; break;
    case 'c': case 'C': value = 12; break;
    case 'd': case 'D': value = 13; break;
    case 'e': case 'E': value = 14; break;
    case 'f': case 'F': value = 15; break;
    default: return false;
    }
  return true;
}

bool LEX::hexchar(intC h, aCHAR &c)
{                                             // return 'hex char or X
  switch(h)
    {
    case 0:  c = '0'; break;
    case 1:  c = '1'; break;
    case 2:  c = '2'; break;
    case 3:  c = '3'; break;
    case 4:  c = '4'; break;
    case 5:  c = '5'; break;
    case 6:  c = '6'; break;
    case 7:  c = '7'; break;
    case 8:  c = '8'; break;
    case 9:  c = '9'; break;
    case 10: c = 'a'; break;
    case 11: c = 'b'; break;
    case 12: c = 'c'; break;
    case 13: c = 'd'; break;
    case 14: c = 'e'; break;
    case 15: c = 'f'; break;
    default: return false;
    }
  return true;
}

uintC LEX::decimalacc(aCHAR &c, aCHAR * &s, bool fraction)
// accumulate integer to any length, return length - integer
// might overflow, no big deal, it'll get redone as a real
// just used by prep now for enums I think.
{
   uintC length = 0;
   acc = 0;                                    // longlong
   for(c = rawchar(); is_digit(c); c = rawchar(), length++)
   {
      *s++ = c;                               // save text
      acc *= 10;                              // accumulate
      acc += c - '0';                         // add decimal digit value
   }
   *s = EOS;
   if(fraction && length < 9)                  // last fractional digit
      acc*= (longlong)pow((double)10, (int)(9 - length));      // left justify
   c = blackchar(c);
   return length;                      // length is true even if acc overflowed
}


bool LEX::nestPair(aCHAR c, aCHAR & d)
{
  switch(c)
    {
    case aS('['): d = aS(']'); return true;
    case aS('{'): d = aS('}'); return true;
    case aS('('): d = aS(')'); return true;
    default: return false;
    }
}

/*
 * get the string in the input q to buffer at pc:
 * the string started with a <quote> it must end with a <quote>
 * (both of which are stripped out of the processed string).
 * \ is the escape character mapped according to C standard
 */
void LEX::grab_str(aCHAR * &pc, aCHAR &c, const aCHAR quote, intC buf_size)
{
  int i = 0;
  aCHAR c_next;
  
  while(true)
    {
      c = rawchar();
      if(c == '\n')
        {
          *pc++ = aS('\n');
          continue;
        }
      if (++i == buf_size)
        pXCPT->Error(stringlE);
      if(c == LEOF)
        pXCPT->Error(struntermE, quote);
      if (c == aS('\\') && pREAD->GetStrEsc())
        {                              // control escape sequence
          c = rawchar();
          if (++i == buf_size)
            pXCPT->Error(stringlE);
          
          switch(c)
            {
            case aS('a'): *pc++ = aS('\a'); break;
            case aS('b'): *pc++ = aS('\b'); break;
            case aS('f'): *pc++ = aS('\f'); break;
            case aS('n'): *pc++ = aS('\n'); break;
            case aS('r'): *pc++ = aS('\r'); break;
            case aS('t'): *pc++ = aS('\t'); break;
            case aS('v'): *pc++ = aS('\v'); break;
            case aS('x'):
               c_next = stream_reader->lookahead();
              //if(is_digit(stream_reader->lookahead()))
               if (isxdigit((int)c_next))
                {                      // note: absorbs up to 4 hex digits max 
                  c = binaryacc(4, 4, pc);
                  *pc++ = (aCHAR)acc; 
						if(c != aS('\\'))
						  pXCPT->Error(escseqE, aS("hex"));
                }
              else
                *pc++ = aS('x');
              break;
            default:
              if (is_digit(c) && c < aS('8'))  // octal denotation of a char
                {                    // note: absorbs up to 3 octal digits max 
                  backupScan();
                  c = binaryacc(3, 3, pc);
                  *pc++ = (aCHAR)acc; 
						if(c != aS('\\'))
						  pXCPT->Error(escseqE, aS("octal"));
                  break;
                }
               else
                 *pc++ = c;                    // literal char
            }                                  // end switch
        }                                      // end if '\\'
      else if (c == quote)
        {                                      // encountered another quote
          c = rawchar();                       // the next raw char
          if (++i == buf_size)
            pXCPT->Error(stringlE);
          
          if (c == quote)                      // it was shielded
            *pc++ = quote;                     // insert the quote char
          else
            if(c == '\n')
              break;                           // EOL
            else
              {                                // not an inserted quote
                c = blackchar(c);              // the next black char
                if(c != quote)                 // not a new opening quote
                  {                            // genuine delimiter
                    backupScan();
                    break;
                  }                            // forget new quote & continue
              }       
        }                                      // end quote char
      else
        *pc++ = c;                             // vanilla char
    }                                          // end while 
  *pc = EOS;
}                                              // returns black

void LEX::getName(aCHAR *s, aCHAR &c, uintCH &size) // returns c = delim
{                                              // last 2 args get bumped
  size = 0;
#ifdef BUG_LEX
  aCHAR *gname = s;
#endif
  if(pSTATE->m_properNames && c == aS('?'))
    {
      *s++ = c; 
      c = rawchar();
      size = 1;
    }      

  while(is_lud(c)) // includes _ as an UPPER
    {
      *s++ = c; 
      c = rawchar();
      size++;
    }
  *s = EOS;                                    // null terminate name
#ifdef BUG_LEX
 DUMP << "  getName got: " << gname << NL << FLUSH;
#endif
}                                              // now c is raw delim

void LEX::getNest(aCHAR *s, aCHAR &c, uintCH &size, aCHAR delim, 
                     const aCHAR * endSpace)
{                                              // not used ??
  aCHAR d;

  while(c != delim)
    {
      c = rawchar();
      if(char_class(c) == NEST)
        if(nestPair(c, d))                    // ensures it was a nest opener
          getNest(s, c, size, d, endSpace);
      if(char_class(c) == QUOTE)
        pLEX->grab_str(s, c, c, (intC)(endSpace - s));
    }
}

bool LEX::dfined(aCHAR *name, intC hix, MACRO * &previous, MACRO * &macRef) 
{                     // returns macro object which points to macro definition 
  aCHAR *p, *s;
  intCH cp, cs;
  int  i;

  macRef = NULL;   
  if(!name)
    return false;

  if(hix == 0)
    hix = macroHash(name);

  previous = NULL;
  for(macRef = bucket[hix]; macRef; macRef = macRef->chain) 
    {                                           // walk chain 
      p = name - 1;                             // reset for each 
      s = macRef->name;                         // macro name 
      for(i = 0; i < 32; i++)                   // while non zero 
        {
          cp = *(p++);
          cs = *(s++);
          if(cp == 0 && cs == 0)
            return true;                        // success 

          if(cp ^ cs)                           // symmetric difference 
            break;                              // fail, go on walking 
        }
      previous = macRef;                        
    }          
  return false;                                 // not found 
}

MACRO * LEX::addEnvVar(aCHAR *macName, intC hix, aCHAR *body)
{                                               // make local copy of env macro
  MACRO * macRef;
  intCH bodylen;

  //macRef = new MACRO(m_peng, macName, 0);       // assume arity is 0
  LNEW(macRef, MACRO(m_peng, macName, 0), aS("macro")); // assume arity is 0
  macRef->chain = bucket[hix];                  // insert new macro
  bucket[hix] = macRef;   

  bodylen = (uintCH)Lstrlen(body);
  if(((macroFree + bodylen) -  macroHeap) > macroHeapsz - 1) 
    pXCPT->Error(badprepE, FILO, LINO, aS("macro heap full"));

  *macroFree++ = aS('$');
  Lstrcpy(macroFree, body);
  macroFree+= bodylen;
  *macroFree++ = aS('$');
  *macroFree++ = 0;

  return macRef;
}

MACRO::MACRO(LEngine *peng, aCHAR *macName, intCH a)
{
   m_peng = peng;
  arity = a;
//  defalt = a ? new aCHAR *[1+a] : NULL;         // default arg ptrs
  if (a > 0)
  {
     LNEW(defalt, aCHAR *[1+a], aS("macro"));
  }
  else
     defalt = NULL;

  if(defalt)
    for(int i = 0; i <= arity; i++)
      defalt[i] = NULL;
  niladic = false;                              // not a niladic syntax
  noParNames = false;                           // expect par names
  body = pLEX->macroFree;                       // address for body
  dynamic = NULL;                               // static body by default
  name[0] = macName[-1];                        // copy size of name
  Lstrcpy(name+1, macName);                     // copy name 
}

MACRO::~MACRO()
{
  if(defalt)
  {
     delete[] defalt;
  }
}

void LEX::Init(intC buflen, intC macSize)
{
   //blackCount = 0;
   //LNEW(input_line, aCHAR[buflen], aS("reader"));
   //input_char = input_line;

   macroHeapsz = macSize;
   bufSize = pSTATE->m_buflen = buflen;
   LNEW(topLevelBuf, aCHAR[buflen], aS("reader"));
   //if (topLevelBuf == NULL)
   //  pXCPT->Error(outofmemE, aS("Token Buffer"));
   LNEW(readBuffer, aCHAR[buflen], aS("reader"));
   LNEW(nameBuffer, aCHAR[buflen], aS("reader"));
   //if (readBuffer == NULL)
   //  pXCPT->Error(outofmemE, aS("Read Buffer"));
   //if (nameBuffer == NULL)
   //  pXCPT->Error(outofmemE, aS("Name Buffer"));

   //if(pSTATE->m_prep)
   //{
   // always initialize because prep maybe turned on later
      LNEW(macroHeap, aCHAR[macSize], aS("reader"));
      macroFree = macroHeap;
      //macroFree = macroHeap = (STRptr) new aCHAR[macSize];
      //if (macroHeap == NULL)
      //  pXCPT->Error(outofmemE, aS("Macro Heap Buffer"));

      preDefMacros();                           // init the pre-defined macros
      setIncDirs();                             // intern all the search paths
   //}
   //else
   //   macroFree = macroHeap = NULL;
}

void LEX::preDefMacros()
{                                               // set up the pre-def macros
  MACRO *macRef;
  aCHAR buff[34];
  aCHAR *name = &buff[1];

  // note - name[-1] is set to  length for use of
  // macro hashing algorithm

  //date  
  buff[0] = Lsprintf(name, 20, aS("__DATE__")); 
  macRef = addMacro(name, 0, 0);
  macRef->dynamic = getDate;
  macroFree+= 14;                               // reserve body space
  //time
  buff[0] = (aCHAR)Lsprintf(name, 20, aS("__TIME__"));
  macRef = addMacro(name, 0, 0);
  macRef->dynamic = getTime;
  macroFree+= 12;                               // reserve body space  
  // line number
  buff[0] = (aCHAR)Lsprintf(name, 20, aS("__LINE__"));
  macRef = addMacro(name, 0, 0);
  macRef->dynamic = getLino;
  macroFree+= 12;                               // reserve body space  
  // IFNULL
  buff[0] = (aCHAR)Lsprintf(name, 20, aS("IFNULL"));
  macRef = addMacro(name, 3, 0);
  macRef->dynamic = ifNull;
  macroFree+= 4;                                // reserve body space  
  /*
  // enum
  buff[0] = (aCHAR)Lsprintf(name, 20, aS("enum"));
  macRef = addMacro(name, 0, 0);
  macRef->dynamic = doEnum;
  macRef->body = NULL;
  macroFree+= 4;                                // reserve body space  

  // file arg
  buff[0] = (aCHAR)Lsprintf(name, 20, aS("fileArg"));
  macRef = addMacro(name, 0, 0);
  macRef->dynamic = fileArg;
  macRef->body = NULL;
  */
  /*  // include
  buff[0] = (aCHAR)Lsprintf(name, 20, aS("include"));
  macRef = addMacro(name, 1, 0);
  macRef->dynamic = NULL;
  macroFree+= 30;                               // reserve body space  
  Lsprintf(macRef->body, 20, aS("consult(#1#).")); 
  */
}

MACRO * LEX::addMacro(aCHAR *macName, intCH arity, intC hix)
{               // If macro already there do nothing, else copy macro to table
  MACRO *macRef, *previous = NULL;              // Return ref to macro
  
  if(hix == 0)
    hix = macroHash(macName);                   // hash index
  if(!dfined(macName, hix, previous, macRef))
    {                                           // not found 
      //macRef = new MACRO(m_peng, macName, arity); // make a new one
      LNEW(macRef, MACRO(m_peng, macName, arity), aS("macro")); // make new one
      macRef->chain = bucket[hix];              // thread new macro

      bucket[hix] = macRef;   
    }
  return macRef;
}

void LEX::initReader(int h)
{
   stream_reader = pIO->stream[h]->getStreamReader();
   if (! stream_reader)
   {
      LNEW(stream_reader, StreamReader(m_peng, pIO->stream[h], bufSize), aS("stream"));
      pIO->stream[h]->setStreamReader(stream_reader);
   }
}

void LEX::finishReader()
{
   //delete stream_reader;
   stream_reader = NULL;
}

void LEX::errorResetReader()
{
   stream_reader->clear();
}

bool LEX::pop_stream()
{
   StreamReader *parent = stream_reader->parent;

   if (parent)
   {
      delete stream_reader;
      stream_reader = parent;
      return true;
   }
   else
      return false;
}


TOKEN_ LEX::getLexeme(EXPECT expect)
{                                          // read next token onto parse stack
   int       value;
   uintCH    size;
   TERM      t, u;
   aCHAR     preop;
   aCHAR    *pc;
   MACRO    *macRef, *previous = NULL;
   RAND_     rand;
   GCDouble  *pf;
   GCReal    *pr;
   int       arity;

start:

#ifdef BUG_LEX
  DUMP << "LEX::getLexeme() " << NL << FLUSH;
#endif
   // note, nameBuffer uninitialized at this point, might
   // have garbage or value from previous scan.  it's set
   // later by getName.
   pc = nameBuffer + 1;
   m_c = blackchar(' ');

   // should be handled OK by rawchar
   //while(!c && (stream_reader->kind == MACRO_ || stream_reader->kind == STRING_))
   //  {
   //		pIO->pop_stream(read_handle);
   //    c = blackchar(' ');
   //  }
   if (m_c == (CharInt)LEOF)
   {
      pREAD->PushStack(T_ATOM, (void *) &(pATAB->eofA));
      return T_EOF;                             // tell tsread
   }

   // something to be skipped by #ifdef and friends
   if (pSTATE->m_prep && skipLevel && m_c != aS('#'))
   {
      lineComment(m_c);
      goto start;
   }

   if (is_atominitial(m_c))                        // atom or func or op 
   {
      //      savebp = p;
      getName(pc, m_c, size);      
      *(pc-1) = size;
      if ((pc-nameBuffer) >= pSTATE->m_buflen)
         pXCPT->Error(abuffE, pSTATE->m_buflen);
      
      if(size > 1 && expanding && pSTATE->m_prep)
      {                                       // only if doing macros
         if(dfined(pc, 0, previous, macRef))   // a macro name
         {
            arity = macRef->getArity();
            if(macRef->vacant())
            {
               if(macRef->dynamic)
                  macRef->callDynamic(m_peng, NULL);
            }
            else   // vacant & not dynamic
            {                               // not vacant   
               //						if(m_c != aS('('))
               if(arity == 0)
               {
                  backupScan();             // save delim until done macro
                  macRef->applyMacro(m_c);  // make new reader
                  goto start;
               }
               else
               {
                  if(m_c == aS('('))
                  {
                     macRef->applyMacro(m_c);  // make new reader, get args
                     goto start;
                  }
               }
            }
         }
      }
      do_atom(pc, m_c);
      return m_c == aS('(') ? T_FUNCTOR : T_ATOM;
   }
  
  if (is_varinitial(m_c))                         // variable 
    {
#ifdef xBUG_READ
      errDebugMsg("a variable\n");
#endif
      //      savebp = p;
      getName(pc, m_c, size);      
      *(pc-1) = size;
      
      if (*pc == aS('_') && size == 1)
        {                                       // make anonymous var name 
          *(pc-1) = Lsprintf(pc, 32, aS("**%d**"), anon_index);
          ++anon_index;
        }
      else if (*pc ==('?') && size == 1)  // even if in vba mode, a single ? is an atom
      {
       do_atom(pc, m_c);
       return m_c == aS('(') ? T_FUNCTOR : T_ATOM;
     }
      else
        {
          if(expanding && pSTATE->m_prep)
            {
              if(dfined(pc, 0, previous, macRef))
                if(macRef->vacant())
                  if(macRef->dynamic)
                    macRef->callDynamic(m_peng, NULL);
                  else;                         // vacant & not dynamic
                else
                  {                             // not vacant   
                    if(m_c != aS('('))                    
                      backupScan();             // save delim until done macro
                    macRef->applyMacro(m_c);      // make new reader, get args
                    goto start;
                  }
            }
        }
      /*
      if(!c && stream_reader->parent)
        delete stream_reader;
        */
      backupScan(); 
      value = pREAD->varid(pc);
      pREAD->PushStack(T_VAR, (void *) &value);
      return T_VAR;
    }                                           // end is_varinitial
  
  if( expect == Erand && (m_c == aS('-') || m_c == aS('+')) )
    {
      preop = m_c;
      if(is_digit(stream_reader->lookahead()))
        {
          m_c = rawchar();
          goto digit;
        }
    }
  
  if (is_digit(m_c))                              // number 
    {
      preop = 0;
    digit:
      scanNumber(m_c, pc, preop, rand);
      backupScan();
      if (rand.kind == T_DOUBLE)
      {
         pf = pGCTH->make_float(rand.getDouble());
         pREAD->PushStack(T_DOUBLE, &pf);
      }
      else if (rand.kind == T_REAL)
      {
         pr = pGCTH->make_real(rand.getReal());
         pREAD->PushStack(T_REAL, &pr);
         //pREAD->PushStack(rand.kind, rand.Value());
      }
      else
         pREAD->PushStack(rand.kind, rand.Value());
      return rand.kind;
    }
  
  switch(char_class(m_c))
    {
    case NEST:
      pREAD->PushStack((TOKEN_)m_c, (void *) NULL);
      return T_NEST;
    case CNTRLZ:           // some editors end files with ^Z
      pREAD->PushStack(T_ATOM, (void *) &(pATAB->eofA));
      return T_ATOM;       
    default:
      switch(m_c)
        {  
        case aS('!'):                           // cut
          pREAD->PushStack(T_ATOM, (void *) &(pATAB->cutA));
          return T_CUT;
          /*
        case aS('.'):
          if(stream_reader->kind == TOPLEVEL_)
            stream_reader->col = 1;
          pREAD->PushStack(T_DOT, (void *) NULL);
          return T_DOT;
          */
        case aS(','):
          pREAD->PushStack(T_COMMA, (void *) NULL);
          return T_COMMA;
        case aS('|'):
          pREAD->PushStack(T_VBAR, (void *) NULL);
          return T_VBAR;
        case aS('\''):                          // quoted atom 
          pc = nameBuffer;
          grab_str(pc, m_c, aS('\''), pSTATE->m_buflen);
          m_c = rawchar();                        // do_atom will backup
          do_atom(nameBuffer, m_c);
          return T_ATOM;
        case aS('"'):
          if(!(pSTATE->m_properQuotes))
            {                                   // else fall through
              pc = nameBuffer;
              grab_str(pc, m_c, aS('"'), pSTATE->m_buflen);
              u = t = pHXL->heapGET();
              t->setList(t+1);
              pc = nameBuffer;
              while( *pc != EOS )
                {
                  pHXL->heapGET()->setChar(*pc++);
                  t = pHXL->heapGET();
                  t->setList(t+1);
                }          
              pHXL->heapPOP();                  // pop last listT cell 
              pHXL->heapGET()->setAtom(pATAB->nilA); // replace with []
              pREAD->PushStack(T_TERM, (void *) &u);
              return T_TERM;
            }
        case aS('`'):    
        case aS('$'):                           // $..$ string 
          pc = nameBuffer;
          grab_str(pc, m_c, m_c, pSTATE->m_buflen);
          do_str(nameBuffer);
          return T_STR;
        case aS(';'):                           // `; not a graphic
          nameBuffer[0] = aS(';');
          nameBuffer[1] = 0;
          m_c = rawchar();
          //do_atom(nameBuffer, 0);               // lose the c
          do_atom(nameBuffer, m_c);               // lose the c
          return T_RATOR;
        case aS('#'):
          if(pSTATE->m_prep)
            if(stream_reader->getCol() == 1)
              {                                 // 1st black char is #
                doPrepControl(getPrepControl(m_c), m_c);
                goto start;
              }
          //pXCPT->Error(charE, (int)m_c, (aCHAR)m_c);
          //return T_ATOM;                        // just to keep compiler happy
        // else fall through to default, as # is a graphic
        default:
#ifdef xBUG_READ
          errDebugMsg("That '%c' was something special\n", m_c);
#endif
          if (is_graphic((aCHAR) m_c))
            {                                   // we classify `\ as graphic
              if(m_c == '.' && !(is_graphic(stream_reader->lookahead())))
                {                               // plain dot, not an op
                  // not sure why we need the restart, this is only place
                  // so taking it out for now.
                  //if(stream_reader->kind == TOPLEVEL_)
                  //  stream_reader->restart();
                  pREAD->PushStack(T_DOT, (void *) NULL);
                  return T_DOT;
                }
              getGraphic(m_c, nameBuffer);
              do_atom(nameBuffer, m_c);           // may or may not be a rator
            }
          else
            pXCPT->Error(charE, (int)m_c, (aCHAR)m_c);

          return expect == Eop ? T_RATOR : T_ATOM;
        }                                       // end switch(c)
    }                                           // end switch(proclass[c])
}                                               // end 

/*
// These two functions used by scanNumber to restart the
// scan of the number once its determined its a real.
void LEX::saveScanPosition()
{
   if (macro_reader)
   {
      saved_col = macro_reader->getCol();
      saved_curse = macro_reader->getCurse();
   }
   else
      saved_col = stream_reader->getCol();
}

void LEX::restoreScanPosition()
{
   if (macro_reader)
   {
      macro_reader->setCol(saved_col);
      macro_reader->setCurse(saved_curse);
   }
   else
      stream_reader->setCol(saved_col);
}
*/
//--------------------------------------------------------
// Number scanning stuff
//

bool LEX::scanNumber(aCHAR &c, aCHAR * &pc, aCHAR preop, RAND_ &rand)
// c - the character just read, always a number
// pc - points to nameBuffer+1, which may be uninitialized
// preop - is 1 means we had a '-' first, 0 means no minus.
// rand - the token to be returned
{
   int lvalue;
   aCHAR cnext;
   bool explicit_real = false;
   bool explicit_float = false;
   bool explicit_double = false;
   bool explicit_single = false;
   bool dont_fix = false;
   bool int_real = false;
   aCHAR peek;
   int whole_size;
   int frac_size;
   int exp_size;
   aCHAR *the_number;

   clear_number_flags();

   cnext = stream_reader->lookahead();
   // this is the case that an integer starts with
   // 0x7f5e (a hex integer) or 0B1101 (binary) or the like
   if (c == aS('0'))
   {
      cnext = stream_reader->lookahead();
      // if the next character indicates this is to be an
      // integer in hex or binary or the like, go get it.
      if ( cnext == aS('x') || cnext == aS('X') ||
           cnext == aS('w') || cnext == aS('W') ||
           cnext == aS('o') || cnext == aS('O') ||
           cnext == aS('b') || cnext == aS('B') ||
           cnext == aS('\'') )
      {
         c = intDenoter(pc);  // accumulates integer in acc, sets flags as needed
         lvalue = (int)((preop == aS('-')) ? -acc : acc);   // sign on int denoter??
         if (char_flag)  // would have been set by intDenoter
         {
            rand.c = lvalue;
            rand.kind = T_CHAR;
         }
         else
         {
            rand.l = lvalue;
            rand.kind = T_LONG;
         }
         return true;  // we're done and out of here
         //goto done;
      }
   }

   // now copy the number piece by piece into nameBuffer,
   // so we can know what we've got and then pass it to
   // the appropriate number maker.

   the_number = pc;
   if (preop == aS('-'))
      *pc++ = preop;
   *pc++ = c;

   whole_size = 1 + copy_digits(c, pc);

   if (c == aS('.') && is_digit(stream_reader->lookahead()))
   {
      *pc++ = c;
      frac_size = copy_digits(c, pc);
   }
   else
      frac_size = 0;

   // r is for the real world, but for now we use g internally
   // to force a real, and f also means real but follows fixed.
   if ( c == aS('r') || c == aS('R') ||
        c == aS('g') || c == aS('G') || c == aS('f') )
   {
      explicit_real = true;
      dont_fix = (c == aS('g') || c == aS('G')) ? true : false;
      *pc++ = c;
   }

   if ( c == aS('e') || c == aS('E') ||
        c == aS('d') || c == aS('s') )
   {
      explicit_float = true;
      if (c == aS('d'))
      {
         explicit_double = true;
         *pc++ = aS('e');    // d allowed under Windows, but no help...
      }
      else if (c == aS('s'))
      {
         explicit_single = true;
         *pc++ = aS('e');
      }
      else
         *pc++ = aS('e');
   }

   if (explicit_real || explicit_float)
   {
      peek = stream_reader->lookahead();
      if ( peek == aS('+') || peek == aS('-') )
         *pc++ = rawchar();
      exp_size = copy_digits(c, pc);
   }

   if ( (pc - nameBuffer) > bufSize )
      pXCPT->Error(rdbufE);

   // Now we've got a copy of the number, so its
   // time to do the right thing with it.

   if (explicit_real)
   {
      //rand.r = string_to_real(the_number);
      LNEW(rand.r, LReal(the_number), aS("scanning number"));
      rand.kind = T_REAL;
      if (!dont_fix && rand.r->isFixable())
      {
         fixedC f = rand.r->toFixed();
         delete rand.r;
         rand.f = f;                        // coerce to fixed
         rand.kind = T_FIXED;
      }
#ifdef BUG_LEX_NUMBERS
 if (rand.kind == T_REAL) rand.r->Dump();
 else rand.f.Dump();
#endif
      return true;
   }

   if (explicit_float)
   {
      rand.d = Latof(the_number);
      rand.kind = T_DOUBLE;
      if ( !explicit_double && (pSTATE->m_floats == single_ || explicit_single) )
      {
         rand.fl = (float)rand.d;
         rand.kind = T_FLOAT;
      }
      float_flag = true;
      return true;
   }

   if (frac_size == 0)  // an integer
   {
      if (whole_size < 10)   // easily a long
         int_real = false;
      else if (whole_size > 10)
         int_real = true;
      else if ( (the_number[0] != aS('-') && Lstrcmp(the_number, aS("2147483647")) > 0) ||
                (the_number[0] == aS('-') && Lstrcmp(the_number, aS("-2147483648")) > 0) )
         int_real = true;
      else
         int_real = false;

      if (int_real)
      {
         if (pSTATE->m_decimals == real_)
         {
            LNEW(rand.r, LReal(the_number), aS("scanning number"));
            rand.kind = T_REAL;
            return true;
         }
         else  // decimals float_
         {
            rand.d = Latof(the_number);
            rand.kind = T_DOUBLE;
            if (pSTATE->m_floats == single_)
            {
               rand.fl = (float)rand.d;
               rand.kind = T_FLOAT;
            }
            float_flag = true;
            return true;
         }
      }
      else
      {
         rand.l = Latol(the_number);
         rand.kind = T_LONG;
         return true;
      }
   }

   if (pSTATE->m_decimals == real_)
   {
      //rand.r = string_to_real(the_number);
      LNEW(rand.r, LReal(the_number), aS("scanning number"));
      rand.kind = T_REAL;
      if(rand.r->isFixable())
      {
         fixedC f = rand.r->toFixed();
         delete rand.r;
         rand.f = f;
         rand.kind = T_FIXED;
      }
      return true;
   }
   else  // m_decimals == float_
   {
      rand.d = Latof(the_number);
      rand.kind = T_DOUBLE;
      if (pSTATE->m_floats == single_)
      {
         rand.fl = (float)rand.d;
         rand.kind = T_FLOAT;
      }
      float_flag = true;
      return true;
   }

   return true;
}

int LEX::copy_digits(aCHAR &c, aCHAR* &s)
{
   int length = 0;
   for(c = rawchar(); is_digit(c); c = rawchar())
   {
      *s++ = c;
      length++;
   }
   *s = EOS;
   return length;                      // length is true even if acc overflowed
}



aCHAR LEX::intDenoter(aCHAR *pc)
// only called from scanNumber when first char is 0 
// followed by (learned from peeking) one of the integer
// denoter characters listed in the case statement.
// pc is buffer being filled with results of scan
// acc has longlong version of integer.
{
   aCHAR key, c;

   key = rawchar();   // get the key
   *pc++ = key;
   switch((char)key)
   {
//#ifdef REALS
//   case 'G':
//   case 'g':
//      STUB(aS("intDenoter - working with 0g format"));
//      acc = 0;
      // does this work?  pc still has the key?
//      backupScan();
//      backupScan();
//      c = rawchar();  // read the zero again
//      real_flag = true;
//      return c;
//#endif
    // note that binaryacc reads an integer into
    // the member variable acc which is a longlong.
    // pc is now null terminated and contains the
    // characters read, such as 0x8f7e.
   case 'x': case 'X':
      hex_flag = true;
      return binaryacc(4, 8, pc);
   case 'w': case 'W':
      char_flag = true;
      return binaryacc(4, 8, pc);
   case 'o': case 'O':
      return binaryacc(3, 10, pc);
   case 'b': case 'B':
      return binaryacc(1, 32, pc);
   case '\'':
      c = (int)rawchar();
      *pc++ = c;
      acc = (int)c; 
      char_flag = true; 
      c = rawchar();                         // get delimiter
      return c;
   default:            // no key just an int or mantissa
      pXCPT->Error(oopsE, aS("Bad integer denoter"));
      return 0;  // a formality for compiler
      //backupScan();   
      //backupScan();   
      //decimalacc(c, pc, false);
      //return c;
   }
}

aCHAR LEX::binaryacc(intC logRadix, intC stop, aCHAR *pc)
// read an integer in some base specified by logRadix.
// fill pc with the characters read and processed, ending with EOS
// fill the longlong acc with the result,
// return the delimiter character
{
  aCHAR c, *start; 
  char *p, *q;
  int i, value, RADIX;

  RADIX = logRadix == 1 ? 2 : (logRadix == 3 ? 8 : (logRadix == 4 ? 16 : 0));
  acc = 0;
  start = pc;
  for(i = 0, c = rawchar(); i < stop; i++, c = rawchar())
    { 
      if(char_class(c) == DIGIT)
        {
          value = c - '0';
			 if(value < RADIX)
				goto l1;
        }
		else
		  if(10 < RADIX)
			 {
				p = x_digits;                     // set p and q
				q = p + 6;
				for(value = 10; value < RADIX; value++, p++, q++) 
				  if((c == *p) || (c == *q))
					 goto l1;
			 }
      break;                                  // not valid, so c is delimiter
    l1:                                       // valid
      *pc++ = c;                              // save text
      acc <<= logRadix;                       // accumulate
      acc |= value; 
    }                                         // end for

  if (start == pc+1)                          // can't have zero length
    pXCPT->Error(hexE);

  c = blackchar(c);                           // maybe delete reader
  *pc = EOS;                                  // end the text accumulator
  return c;
}

//-----------------------------------------


/*
void LEX::applyString(aCHAR *s)
{                                               // push reader
  STRINGREADER *stringReader = new STRINGREADER(m_peng, s); 
  
  stringReader->parent = stream_reader;         // push onto current reader
  stream_reader = stringReader;                 // connect the stringreader
}
*/

void MACRO::applyMacro(aCHAR &c)
{                       // While applying, nothing gets pushed onto parse stack
  MacroReader *macroReader;                     // push reader

  //macroReader = 
  //	 new MACROSTREAM(this, pLEX->bufSize);       // new rdr with this macro
  LNEW(macroReader, MacroReader(m_peng, this, pLEX->bufSize), aS("reader"));

  if(arity)
    {
      if(c != aS('('))
        pXCPT->Error(badprepE, FILO, LINO, aS("Missing '('"));
      macroReader->getMacroArgs(c, arity); // this can pop macro's parent
    }

  if(dynamic) 
    (*dynamic)(m_peng, macroReader->getArg());  // load dynamic body & arg refs

  if(pSTATE->m_macrotrace)
    macroReader->macCall(this);                 // output trace

//  pIO->pushNewIstream(macroReader);             // connect the macroreader 
//  pIO->push_stream(pLEX->get_read_handle(), macroReader);
//  macro_reader = macroReader;
   macroReader->parent = pLEX->macro_reader;
   pLEX->macro_reader = macroReader;
}
/*
void MACROSTREAM::getMacroArgs(aCHAR &c, const intCH arity)
{                                               // sets c to delim
  uintCH ix;
  aCHAR  *s, delim;
  
  s = argSpace;                                 // where args go
  for(ix = 1; ix <= arity; ix++)
    {                                           // get each arg
      arg[ix] = s;
      delim = ix == arity ? aS(')') : aS(',');
      if(pLEX->getBalancedText(s, c, delim, argSpaceFree, 0)) 
        *s++ = 0;
      else
        arg[ix] = NULL;                         // empty arg
      
      if(c == aS(')'))
        break;
    }
  if(ix < arity)   
    arg[arity] = NULL;                          // last arg was missing
  // Do not read at end, as we are about to switch readers
}
*/
bool LEX::getBalancedText(aCHAR *&s, aCHAR &c, const aCHAR delim, 
                          const aCHAR *end, const uintC depth)
{                                              // let caller record nest chars
   aCHAR d;

   c = blackchar(' ');                          // lose input initial
   if(c == delim)
      return false;                              // empty nest, c == delim

   while(c != delim)
   {
      if(depth == 0 && c == aS(')'))           // ',' alternative at level 0
         break;

      if(c == LEOF)                            // unexpected in nest
         pXCPT->Error(badprepE, FILO, LINO, aS("Unexpected EOF in nest"));

      if(char_class(c) == NEST)
      {
         if(nestPair(c, d))                     // ensures c was a nest opener
         {
            *s++ = c;                          // record nest open char
            getBalancedText(s, c, d, end, depth+1);
         }
         else
            pXCPT->Error(badprepE, FILO, LINO, aS("unbalanced argument "));
      }
      //     else
      //if(ascii_class(c) == QUOTE)
      //{
      //grab_str(s, c, c, end - s);
      //            c = rawchar();
      //continue;
      //}
      
      //else
         // EOS won't happen, handled by rawchar
         //if(c == EOS)                         // some string ended
         //   if(stream_reader->is_macro)              // an arg
         //   {
         //      MACROSTREAM * mp = (MACROSTREAM *)stream_reader;
         //      mp->closeArg(c);
         //      if(c == aS('#'))
         //         mp->closeArg(c);
         //  }
         //else
         //if(stream_reader->getParent())    // body
         //  {
         //    pIO->delete_current_reader();
         //    c = blackchar(' ');
         //  }                              // else an error
         //if(stream_reader->getParent())    // body
         //{
         //   pIO->pop_stream(read_handle);
         //   c = blackchar(' ');
         //}                              // else an error
      *s++ = c;
      c = rawchar();    
   }                                          // end for, c == delim
   return true;                                 // caller wants to see delim
}

void LEX::do_atom(aCHAR *p, aCHAR c)
{                             // process atom in buffer p, last char read was c
   PATOM    a;

#ifdef xBUG_READ
   errDebugMsg("do_atom(%s, %d, %c)\n", p, c, c);
#endif

   a = pATAB->EnterAtom(p);
   if (c == aS('('))                            // a functor 
      pREAD->PushStack(T_FUNCTOR, (void *) &a);
   else
   {                                          // a non-op atom 
      c = blackchar(c);
      if (c != '\n')         
         if(c != LEOF)
            backupScan();
      pREAD->PushStack(pREAD->OpClass(a), (void *) &a);
   }
   return;
}

void LEX::getGraphic(aCHAR &c, aCHAR *buf)
{                                              // get operator string
  for(;
      (is_graphic(c) && c != aS('.')) ||
      (c == aS('.') && EOS != stream_reader->lookahead()) ;
      c = rawchar())
    *buf++ = c; 
  *buf = EOS;
}

void  LEX::do_str(aCHAR *p)
{                                              // process string in buffer p
  //STRptr   ps;

  //pPSTR->strEnterString( (STRptr) p, &ps );
  //pREAD->PushStack(T_STR, (void *) &ps);
   GCString *ps;
   ps = pGCTH->make_string(p);
   pREAD->PushStack(T_STR, &ps);
}

void LEX::ReadErr(LExcept &pE)
{
  //aCHAR c, *read_err_p;;
  intC linecount;

  // in case string reader set it on
  pREAD->unsetReading();

  //read_err_p = stream_reader->p;
  linecount = stream_reader->getStream()->getLineNo();          // get line count before reading on
  int col = stream_reader->getCol();
/*
  c = blackchar(' ');
  while( (aS('.') != c)  && (LEOF != c) && (0 != c) )
    {
      c = rawchar();                            // attempt to recover (crude) 
    }
*/
  //*stream_reader->p = EOS;
  //*read_err_p = EOS;
  //aCHAR* prb = nameBuffer+1;
  //aCHAR* prb = stream_reader->get_errline();
  
/*
  while (*prb && (is_white(*prb) || *prb == LEOF))
    {                    // trim leading whitespace, also adjust index to error
      prb++;
      read_err_p--;
    }
  if(read_err_p < prb)
    read_err_p = prb;
*/
	 STRptr namep = FILO;  //stream_reader->path;
	 pE.AddReadInfo(stream_reader->getBuffer(), col, linecount, namep);
	 //stream_reader->getStream()->close();
	 //  pE.AddReadInfo(stream_reader->get_errline(), col, linecount, 
	 //                 pSIO->GetioNowFileName());
	 //  pSIO->CloseioNow();                     // will close a file only
}

//TF LEX::p_stream_attrs(void)
//{
//  TERM t;
//  LSTERMptr listtp = (void **)&t;

//  pENG->MakeList(listtp);                       // list of active readers
//  stream_attrs(stream_reader, listtp);          // push readers in rev order
//  return(pLOGSERV->UnifyParm(1, cTERM, listtp));
//}

/*
void LEX::stream_attrs(PROSTREAM *thisRdr, LSTERMptr listtp)
{
  TERM t;
  void ** tp = (void **)&t;

  if(thisRdr->getParent())
    stream_attrs(thisRdr->getParent(), listtp); // do parents first
  pENG->MakeFA(tp, aS("stream"), 4);            // a list element
  pENG->UnifyArg(tp, 1, cINT, &(thisRdr->kind)); // attributes
  pENG->UnifyArg(tp, 2, cWSTR, thisRdr->path);
  pENG->UnifyArg(tp, 3, cINT, &(thisRdr->lino));
  pENG->UnifyArg(tp, 4, cINT, &(thisRdr->col));
  pENG->PushList(listtp, *tp);
}
*/
    
ENUMEL::ENUMEL(aCHAR * n, int v)
{
  name = n;
  value = v;
}

ENUMLIST::ENUMLIST(LEngine *peng, int extent)
{
   m_peng = peng;
  //enumList = new ENUMEL[extent];
  LNEW(enumList, ENUMEL[extent], aS("stream"));
}

ENUMLIST::~ENUMLIST()
{
  delete[] enumList;
}

bool ENUMLIST::find(aCHAR * name, intCH size, ENUMEL * datum, int & result)
{                                               // walk tree to find name
  intCH size2, size3;
  int discrim;
  ENUMEL *datum2, *datum3;
  
  size2 = size >> 12;
  size3 = size - size2;
  datum2 = enumList + 1 + (size2 >> 1);
  datum3 = enumList + size + 1 + (size3 >> 1);
  
  discrim = Lstrcmp(name, datum->name);         // is this it?
  
  if(discrim == EOS)
    {                                           // yes
      result = datum->value;
      return true;
    }
  else
    if(discrim < 0)
      if(size2)                                 // recurse left
        return find(name, size2, datum2, result);
      else;
    else
      if(size3)                                 // recurse right
        return find(name, size3, datum3, result);
  return 0;                                     // not found
}


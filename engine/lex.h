//-*-C++-*-
/****************************************************************************\
*
* lex.h
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
* author: Ray Reeves December 1999
*
* $Log: lex.h,v $
* Revision 1.3  2005/12/21 19:27:43  dennis
* paren
*
* Revision 1.2  2005/12/19 17:40:49  dennis
* minor fix
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.24  2002/05/15 16:59:08  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.23  2002/05/02 17:39:30  dennis
* various minor bug fixes, added locale as a settable prolog flag
*
* Revision 1.22  2002/03/21 01:25:09  dennis
* allow separator in file names to be either tilt on slash, converts
* to correct one for Unix or Windows
*
* Revision 1.21  2002/02/19 04:11:39  dennis
* changed reals to use pass by reference, eliminating almost all needs
* for new and delete, seems to have eliminated most all leaks due to math,
* put in memcpy, memmove etc. for copying gigit arrays around.
*
* Revision 1.20  2002/02/13 03:20:00  dennis
* reals changed to have a LReal class, moved to file of same name,
* math functions moved out of termsvc and into lmath.cpp/h, eval rewritten
* to reflect various options for numbers, lexcept modified so anyone, even
* non-engine objects, can throw LExcept objects.
*
* Revision 1.19  2002/02/04 17:20:59  dennis
* New lreal wrapper class on reals, start of number options:
* decimals: real/float, floats: single/double.  Created lmath.h/cpp
* but nothing in them yet.  Work to be done on sorting out mixed mode
* arithmetic for different options.  Also - casts removed, as they
* were causing problems.
*
* Revision 1.18  2002/01/28 06:29:19  dennis
* changes for parsing numbers, handling different options
*
* Revision 1.17  2001/12/23 20:08:20  dennis
* made reposition of text files work for both gets and reads
*
* Revision 1.16  2001/12/22 18:50:19  dennis
* allowed for repositioning of text streams, a bit of a tricky business.
*
* Revision 1.15  2001/10/05 17:07:01  ray
* Added real things. Fixed some bugs. Stopped adding '.' in prep
*
* Revision 1.14  2001/10/02 16:05:21  dennis
* changed streams, cleaner interface to lex, reimplemented
* readers as buffer between the two
*
* Revision 1.13  2001/09/11 04:34:56  dennis
* cleaned up some io stuff, got consult working, etc.
*
* Revision 1.12  2001/08/16 17:07:25  ray
* merged everything, repaired errors, added BINSTREAM
*
* Revision 1.11  2001/08/08 00:21:17  dennis
* unworking commit - stream bugs need fixing
*
* Revision 1.10  2001/08/02 18:51:00  dennis
* merge of new streams complete
*
* Revision 1.9  2001/08/01 20:17:59  ray
* removed tswrite.cpp & sio.cpp, added streams.cpp sowrite.cpp
*
* Revision 1.8  2001/06/27 15:15:10  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.7  2001/04/18 15:30:50  ray
* removed enum as builtin
* allowed it as prep directive and .h file statement
*
* Revision 1.6  2001/04/16 05:21:14  dennis
* hacked together some fixes for sio/lex to be better friends,
* merged other changes, added new samples
*
* Revision 1.5  2001/03/25 15:29:51  ray
* Implemented fixed
* repaired #include
*
* Revision 1.4  2001/02/24 19:59:10  ray
* sio tsread and lload were modified
*
* Revision 1.3  2001/02/08 22:56:45  dennis
* string bug fixes, modularized compiler and listener
*
* Revision 1.2  2001/02/02 15:22:31  ray
* corrected read(X)
* enabled arithmetic with char args
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.24  2000/10/15 18:04:23  ray
* Eliminated requirement to annotate integers > 1 billion with 'L'
* Now, only numbers greater than MAXintC get promoted to Real
*
* Revision 1.23  2000/10/03 01:37:17  dennis
* Got it running under Linux, had to ifdef out reals for now
*
* Revision 1.22  2000/09/20 17:09:12  ray
* added casting operators to cell and rand to clean up code.
* added prolog flag 'modulo' and adapted arithmetic to use it.
* added fraction, realSpan and nth.
* added fast ** for reals.
*
* Revision 1.21  2000/08/26 00:32:06  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.20  2000/08/14 02:05:37  ray
* Added Real class.
* No division yet.
* No error reporting yet.
*
* Revision 1.18  2000/06/11 05:26:32  ray
* introduced STRINGREADER
*
* Revision 1.17  2000/06/08 20:30:55  ray
* Changed readers to be line oriented
*
* Revision 1.16  2000/05/14 03:52:34  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
* Revision 1.15  2000/05/11 00:44:01  ray
* open, close, get_char, put_char, get_code, put_code, write_term, 
* write_canonical, current_input, current_output, compound.
*
* Revision 1.14  2000/05/04 21:47:55  ray
* log, log10, xor, round, floor, ceiling, truncate/1, realpart, fractionpart
*
* Revision 1.13  2000/04/21 02:49:05  ray
*
* Added current_prolog_flags
*
* Revision 1.12  2000/04/18 01:32:01  ray
*
* Added builtin stream_attrs/1 for users and error reporting.
*
* Revision 1.11  2000/04/12 17:08:01  ray
*
* fixed fgetc bug
*
* Revision 1.10  2000/04/10 00:57:27  ray
*
* #include
*
* Revision 1.9  2000/04/02 23:54:44  ray
*
* Fixed + and - bug
*
* Revision 1.8  2000/04/01 00:36:11  ray
*
* fixed -ve number error
*
* Revision 1.7  2000/03/31 08:51:38  dennis
* Small bug fixes for Linux, still not running on Linux
*
* Revision 1.6  2000/03/30 07:35:42  dennis
* some minor changes to make gcc happy on Linux
*
*
\****************************************************************************/

#ifndef LEX_H
#define LEX_H

//#define stream_reader pIO->stream[read_handle]
#define LINO pLEX->stream_reader->getStream()->getLineNo()
//#define FILO *(pIO->stream[pLEX->get_read_handle()]->path)
#define FILO  (STRptr)*( pLEX->stream_reader->getStream()->get_name() )


static const int LOWER   =    1;
static const int UPPER   =    2;
static const int LETTER  =    3;
static const int DIGIT   =    4;
static const int LUD     =    7;
static const int GRAPHIC =    8;
static const int QUOTE   =   16;
static const int SOLO    =   32;
static const int NEST    =   64;
static const int WHITE   =  128;
static const int BCKSLSH =  256;
static const int FEED    =  512;
static const int CNTRLZ  = 1024;

static const int DIGITVALUE = 0x000f;

static const int LF     = 10;
static const int CR     = 13;
static const int POUND  = 35;
static const int STAR   = 42;
static const int FSLASH = 47;
static const int SEMIC  = 59;
static const int BSLASH = 92;

static const int FORMAL = 0x007F;

                                     // character class constants for Prolog 
static const uintCH proclass[] = {
/* 0|0x0    1|0x1    2|0x2    3|0x3    4|0x4    5|0x5    6|0x6    7 |0x7   */
/* NUL      SOH      STX      ETX      EOT      ENQ      ACK      BEL */
   WHITE,   0,       0,       0,       0,       0,       0,       0,

/* 8 |0x8   9 |0x9   10|0xa   11|0xb   12|0xc   13|0xd   14|0xe   15|0xf   */
/* BS       HT       LF       VT       FF       CR       SO       SI  */
   0,       WHITE,   WHITE,   WHITE,   WHITE,   WHITE,   0,       0,

/* 16|0x10  17|0x11  18|0x12  19|0x13  20|0x14  21|0x15  22|0x16  23|0x17  */
/* DLE      DC1      DC2      DC3      DC4      NAK      SYN      ETB  */
   0,       0,       0,       0,       0,       0,       0,       0,

/* 24|0x18  25|0x19  26|0x1a  27|0x1b  28|0x1c  29|0x1d  30|0x1e  31|0x1f  */
/* CAN      EM       SUB      ESC      FS       GS       RS       US  */
   0,       0,       CNTRLZ,  0,       0,       0,       0,       0,

/* 32|0x20  33|0x21  34|0x22  35|0x23  36|0x24  37|0x25  38|0x26  39|0x27  */
/* SPACE    !        "        #        $        %        &        '   */
   WHITE,   SOLO,    QUOTE,   GRAPHIC, GRAPHIC, SOLO,    GRAPHIC, QUOTE,

/* 40|0x28  41|0x29  42|0x2a  43|0x2b  44|0x2c  45|0x2d  46|0x2e  47|0x2f  */  
/* (        )        *        +        ,        -        .        /   */
   NEST,    NEST,    GRAPHIC, GRAPHIC, SOLO,    GRAPHIC, GRAPHIC, GRAPHIC,  

/* 48|0x30  49|0x31  50|0x32  51|0x33  52|0x34  53|0x35  54|0x36  55|0x37  */
/* 0        1        2        3        4        5        6        7   */
   DIGIT,   DIGIT,   DIGIT,   DIGIT,   DIGIT,   DIGIT,   DIGIT,   DIGIT,

/* 56|0x38  57|0x39  58|0x3a  59|0x3b  60|0x3c  61|0x3d  62|0x3e  63|0x3f  */
/* 8        9        :        ;        <        =        >        ?   */
   DIGIT,   DIGIT ,  GRAPHIC, SOLO,    GRAPHIC, GRAPHIC, GRAPHIC, GRAPHIC,  

/* 64|0x40  65|0x41  66|0x42  67|0x43  68|0x44  69|0x45  70|0x46  71|0x47  */
/* @        A        B        C        D        E        F        G   */
   GRAPHIC, UPPER,   UPPER,   UPPER,   UPPER,   UPPER,   UPPER,   UPPER,

/* 72|0x48  73|0x49  74|0x4a  75|0x4b  76|0x4c  77|0x4d  78|0x4e  79|0x4f  */
/* H        I        J        K        L        M        N        O   */
   UPPER,   UPPER,   UPPER,   UPPER,   UPPER,   UPPER,   UPPER,   UPPER,

/* 80|0x50  81|0x51  82|0x52  83|0x53  84|0x54  85|0x55  86|0x56  87|0x57  */
/* P        Q        R        S        T        U        V        W   */
   UPPER,   UPPER,   UPPER,   UPPER,   UPPER,   UPPER,   UPPER,   UPPER,

/* 88|0x58  89|0x59  90|0x5a  91|0x5b  92|0x5c  93|0x5d  94|0x5e  95|0x5f  */
/* X        Y        Z        [        \        ]        ^        _   */
   UPPER,   UPPER,   UPPER,   NEST ,   GRAPHIC, NEST,    GRAPHIC, UPPER,

/* 96|0x60  97|0x61  98|0x62  99|0x63  100|0x64 101|0x65 102|0x66 103|0x67 */
/* `        a        b        c        d        e        f        g   */
   QUOTE,   LOWER,   LOWER,   LOWER,   LOWER,   LOWER,   LOWER,   LOWER,

/* 104|0x68 105|0x69 106|0x6a 107|0x6b 108|0x6c 109|0x6d 110|0x6e 111|0x6f */
/* h        i        j        k        l        m        n        o   */
   LOWER,   LOWER,   LOWER,   LOWER,   LOWER,   LOWER,   LOWER,   LOWER,

/* 112|0x70 113|0x71 114|0x72 115|0x73 116|0x74 117|0x75 118|0x76 119|0x77 */
/* p        q        r        s        t        u        v        w   */
   LOWER,   LOWER,   LOWER,   LOWER,   LOWER,   LOWER,   LOWER,   LOWER,

/* 120|0x78 121|0x79 122|0x7a 123|0x7b 124|0x7c 125|0x7c 126|0x7e 127|0x7f */
/* x        y        z        {        |        }        ~        DEL */
   LOWER,   LOWER,   LOWER,   NEST,    SOLO,    NEST,    GRAPHIC, 127  };

enum histoArg_ {macNames = 1, histogram};
enum control_{HUHTK, IFTK, IFDEFTK, IFNDEFTK, ELSETK, ELIFTK, ENDIFTK, ENUMTK,
              LINETK, DEFINETK, INCLUDETK, UNDEFTK, ERRORTK, PRAGMATK};
//enum kind_{TOPLEVEL_ = 1, FILE_, WINDOW_, MACRO_, STRING_};
//enum kind_{COMMANDLINE = 1, FILELINE = 3, MACROLINE, STRINGLINE, WINDOWLINE};
enum lang_ {unknown, cpp, pro, plm, xpl};
enum CRator_{NOCOP, 
             PARENCOP = 0x10, BRACKETCOP, DOTCOP, ARROWCOP, POSTINCCOP, 
             POSTDECCOP, DEFINEDCOP,
             PREINCCOP = 0x20, PREDECCOP, NOTCOP, BITNOTCOP, SIZEOFCOP, 
             ADDRCOP, PREPLUSCOP, PREMINUSCOP, STARCOP,
             CASTCOP = 0x40,
             MULTCOP = 0x80, DIVCOP, MODCOP, 
             PLUSCOP = 0x100, MINUSCOP, 
             LSHIFTCOP = 0x200, RSHIFTCOP, 
             LESSCOP = 0x400, LESSEQCOP, GRCOP, GREQCOP,
             EQCOP = 0x800, NOTEQCOP,
             BITANDCOP = 0x1000,
             XORCOP = 0x2000,
             BITORCOP = 0x4000,
             ANDCOP = 0x8000,
             ORCOP = 0x10000,
             QUERYCOP = 0x20000, COLONCOP,
             ASSCOP = 0x40000, INCASSCOP, DECASSCOP, MULTASSCOP, DIVASSCOP, 
             MODASSCOP, PLUSASSCOP, MINUSASSCOP, XORASSCOP, ORASSCOP, 
             ANDASSCOP, LSHIFTASSCOP , RSHIFTASSCOP,
             COMMACOP = 0x80000,
             NOCPREC = 0x100000};
/*
 * The above, fiendishly clever enumeration, lists all the C operators
 * at their levels of precedence. At each level the first item is a
 * witness to the precedence level of all the operators at that level,
 * and the precedence of each can be found by masking it with ~0xf, 
 * or right shifting it by 4.
 * In C only the operator order is defined, not the level, so we are 
 * free to be arbitrary about this. In Prolog, on the other hand, the
 * actual precedence of each operator is prescribed to be a round number
 * in decimal, which is useless for this approach. Moreover, Prolog
 * allows user defined operators, so enumerations are out.
 */
static char x_digits[] = {"abcdefABCDEF"};
static aCHAR parenPair[2] = {aS('('),aS(')')};

class RAND_;
class MACRO;
//class PROSTREAM;

class MacroReader
{
private:
   LEngine *m_peng;
   MACRO  *macro;                             // this macro
   aCHAR  *argSpace;                          // macro args for this call
   aCHAR  *argSpaceFree;                      // where next arg goes
   intC    argSpacesz;
   aCHAR **arg;                               // arg pointers
   uintCH  bix;                               // buffer (arg) index 
   int     arity;
   aCHAR  *body;
   aCHAR  *p;
   uintC   col;                               // column number of p
   aCHAR  *resumep;                           // interrupted body scan mark
   uintC   resumecol;                         // interrupted body col mark

public:
   MacroReader *parent;

public:
   MacroReader(LEngine *peng, MACRO *m, int bufsize);
   ~MacroReader();
   aCHAR rawchar();
   void backupScan();
   uintCH getBix()
   { return bix; }
   bool openArg(aCHAR &);
   void closeArg(aCHAR &);
   void getMacroArgs(aCHAR &c, intCH arity);
   aCHAR **getArg()
   { return arg; }
   void macCall(MACRO *m);

   // used when LEX saves and restores state of scan, for
   // rethinking reals and the like.
   aCHAR *getCurse()
   { return p; }
   uintC getCol()
   { return col; }
   void setCurse(aCHAR *curse)
   { p = curse; }
   void setCol(uintC c)
   { col = c; }
};

class StreamReader
{
private:
   LEngine *m_peng;
   PrologStream *stream;
   aCHAR *buffer;
   aCHAR *p;

public:
   StreamReader *parent;
   int m_buflen;

public:
   StreamReader(LEngine *peng, PrologStream *s, int bufsize);
   ~StreamReader();
   PrologStream *getStream()
   { return stream; }

   aCHAR rawchar();
   void backupScan()
   { if (p > buffer) p--; }
   aCHAR lookahead()
   { return *p; }
   void clear()
   { p = buffer; *p = EOS; }

   // these two called by prep
   void change_next_char(aCHAR c)
   { *p = c; }
   void change_nextnext_char(aCHAR c)
   { *(p+1) = c; }

   // column of the last char read
   intC getCol() { return (intC)(p - buffer); }
   void setCol(int i) { p = buffer + i; }
   aCHAR *getBuffer()
   { return buffer; }
   void getLine();
};

class LEX
{
private:
   LEngine *m_peng;

public:
  LEX(LEngine *peng);
  ~LEX(void);
  
  void Init(intC, intC);
  void initReader(int h);
  void finishReader();
  void errorResetReader();

  TOKEN_ getLexeme(EXPECT expect);
  void scanEnum(void);

  // used in writing terms
  bool is_graphic(aCHAR c)
  { return (char_class(c) == GRAPHIC); }
  bool is_atominitial(const aCHAR);
  bool is_noquotes(aCHAR c)
  { return ( is_graphic(c) || is_solosolo(c) ); }
  bool is_lud(aCHAR c)                         // lower, upper or digit
  { return is_letter(c) || char_class(c) == DIGIT;}

  bool getBalancedText(aCHAR *&, aCHAR &, const aCHAR,
                       const aCHAR *, const uintC);
  void ReadErr(LExcept &);
private:
  bool scanNumber(aCHAR &, aCHAR * &, aCHAR, RAND_ &);
  int copy_digits(aCHAR &, aCHAR * &);
  //Real string_to_real(aCHAR*);

  void do_atom(aCHAR *, aCHAR );
  void do_str(aCHAR *p);
  void getGraphic(aCHAR &, aCHAR *);
  //void applyString(aCHAR *s);
  bool EqPrec(CRator_ tok1, CRator_ tok2)
  { return (tok1 & ~0xf) == (tok2 & ~0xf);}
  //void backupScan()                        // stop at origin, col > 0
  //{ pIO->reader()->backupScan(is_black(*(pIO->reader()->p)));}
  void backupScan();
  // stop at origin, col > 0
  //{ stream_reader->backupScan(is_black(*(stream_reader->p)));}
  aCHAR blackchar(aCHAR c);
  //{ return blackchar(pIO->current_input, c);}
  //aCHAR blackchar(intC, aCHAR);
  aCHAR rawchar();
  //{ return rawchar(pIO->current_input);}
  //aCHAR rawchar(intC);
  void skip(aCHAR &, const aCHAR);
  void lineComment(aCHAR &);
  void blockComment(aCHAR &);
  MACRO *scanDefiniend(aCHAR &);
  void scanDefinition(MACRO *, aCHAR &);
  void getName(aCHAR *, aCHAR &, uintCH &);
  int char_class(aCHAR c);
  bool is_char(aCHAR c)
  { return is_lud(c) || is_graphic(c) || is_solo(c) || 
      is_layout(c) || is_meta(c);}
  bool is_letter(aCHAR);                        // lower, upper 
  bool is_solo(aCHAR c)
  { return char_class(c) & (SOLO | NEST) ? true :  false;}
  bool is_solosolo(aCHAR c)
  { return char_class(c) & SOLO ? true :  false;}
  bool is_layout(aCHAR c)
  { return c == 10 || c == 32;}
  bool is_meta(aCHAR c)
  { return char_class(c) == QUOTE || c == 92;}
  bool is_varinitial(const aCHAR);
  bool is_CRator(const aCHAR);
  bool is_digit(const aCHAR c)
    { return (char_class(c) == DIGIT);}
  bool is_formalName(MACRO *, uintCH &);
  bool is_white(aCHAR c)
  {   return (char_class(c) == WHITE); }
  bool is_black(aCHAR c)
  { return (char_class(c) != WHITE); }
  bool skipWhite(aCHAR & );
  bool nestPair(aCHAR, aCHAR &);
  void getNest(aCHAR *, aCHAR &, uintCH &, aCHAR, const aCHAR *);
  bool hexdigit(uintCH &, const aCHAR);
  bool hexchar(const intC, aCHAR &);

  aCHAR intDenoter(aCHAR *);
  aCHAR binaryacc(intC, intC, aCHAR *);                  
  uintC  decimalacc(aCHAR &, aCHAR * &, bool);                  
  //Real realacc(aCHAR &, aCHAR *, aCHAR);
  void grab_str(aCHAR * &, aCHAR &, aCHAR, intC);
  void doPrepControl(control_, aCHAR &);
  control_ getPrepControl(aCHAR &);
  bool getCAtom(aCHAR &, aCHAR *, RAND_ &);
  bool CExpr(aCHAR &, CRator_, bool, aCHAR, RAND_ &);
  bool CPrefixToken(aCHAR *, uintC, CRator_ &);
  bool CPostfixToken(aCHAR *, uintC, CRator_ &);
  bool CInfixToken(aCHAR *, uintC, CRator_ &);
  uintCH scanCRator(aCHAR &, aCHAR *);

  intC macroHash(aCHAR *name)            // name[-1] has the length of the name
  {return(name[-1] ^ name[0] ^ name[name[-1] >> 1] ^ name[name[-1]-1]) & 0x1f;}
  void macHisto(histoArg_ histo);
  void doDefine(void);
  void doIncl(aCHAR &);
  void doPragma(aCHAR &);
  void doUndef(aCHAR &c);
  void preDefMacros();
  bool dfined(aCHAR *, intC, MACRO *&, MACRO *&);
  bool is_defined(aCHAR &);
  bool p_stream_attrs(void);
  MACRO *addMacro(aCHAR *, intCH, intC);
  MACRO *addEnvVar(aCHAR *, intC, aCHAR *);
  intC  macroSpaceLeft()
  { return (intC)(macroHeap + macroHeapsz - macroFree);}
  void getEnumItem(aCHAR &, intC &);
  void setIncDirs(void);
  void skipComments(aCHAR &);
  lang_ classify(aCHAR *);
  //void set_read_handle(int h)
  //{ read_handle = h; }
  //int get_read_handle()
  //{ return read_handle; }
  bool pop_stream();
  aCHAR *topLevelBuf;

  // used by scanNumber to allow restart for reals
  int  saved_col;
  aCHAR *saved_curse;
  //void saveScanPosition();
  //void restoreScanPosition();
  //   void set_current_reader(READER *r);
#ifdef LANDFILL
   void dump_readers();
#endif

public:
  //static aCHAR m_c;                            // char last seen
  aCHAR      m_c;                            // char last seen
  aCHAR     *macroFree;                      // space for macro bodies
  aCHAR     *readBuffer;                     // only used in ReadErr
  aCHAR     *nameBuffer;                     // temp for names (long for Msoft)
  longlong   acc;                            // integer accumulator
  intC       bufSize;                        // ini.readbuffer
  bool       expanding;                      // ok to expand macros
  bool       preprocessing;                  // want to see LFs
  bool       real_flag, float_flag, hex_flag, char_flag, long_flag;
  MacroReader  *macro_reader;
  StreamReader *stream_reader;

  void clear_number_flags()
  { real_flag = false; float_flag = false; hex_flag = false;
  char_flag = false; long_flag = false; }

private:  
  //void stream_attrs(PROSTREAM *, LSTERMptr);

  int     anon_index;                        // used to generate anon var names
  intC    macroHeapsz;
  aCHAR  *macroHeap;                         // space for macro definitions
  aCHAR   formalSpace[256];                  // temp for macro formal pars
  MACRO  *bucket[32];                        // root of macro chains 
  // (keep at 32 or change macroHash and other references)
  aCHAR  *incdir[16];                        // -I directories 
  aCHAR  *incsave;                     // where the new char array was for incs
  aCHAR **incfree;                           // -> free space in incdir[] 
  lang_   fileLang;                          // class of include file language
  //int     read_handle;                       // handle of input stream
  //int     blackCount;
};                                           // end class LEX

class RAND_
{                                            // operand for const arith expn
public:
  RAND_(void) {kind = T_ATOM; l = 0;}
  ~RAND_(void)
  { if((kind == T_VAR) && s) delete [] s;
    if (kind == T_REAL) delete r; }

  void *Value()
  { return (void *)&f;}                     // addr is same for all
  double getDouble()
  { return d; }
  Real getReal()
  { return r; }
  fixedC getFixed()
  { return f; }

  //  void setFloatFlag()
  //  { space|= 1; }
  bool evalPreop(CRator_);                    // operations on rands and rators
  bool evalPostop(CRator_);
  bool evalAssop(CRator_, RAND_);
  bool evalIInfix(CRator_, RAND_, RAND_);
  bool evalFInfix(CRator_, RAND_, RAND_);
  bool promote(RAND_ &);

  //operator fixedC();

  TOKEN_ kind;
  union
  {
	 Real   r;
    double d;
    float  fl;
	 fixedC f;
    long   l;
    aCHAR  c;
    aCHAR *s;
  };  
};

class ENUMEL
{
  friend class ENUMLIST;

public:
  ENUMEL(aCHAR *, int);
  ENUMEL() {}

protected:
  aCHAR * name;
  int value;
};

class ENUMLIST
{
public:
  LEngine *m_peng;
  ENUMLIST(LEngine*, int);
  ~ENUMLIST();

  bool find(aCHAR *, intCH, ENUMEL *, int &);

private:
  ENUMEL * enumList;
};

/*
 * A macro may be:
 * o    vacant     body == NULL, and can't be applied
 * o    niladic    arity == 0, but defined as name()
 * o    dynamic    definition is not constant, fixed at expand time
 * o    noParNames User used parameter indices, not names
 */
class MACRO
{                // body is aCHAR array of indefinite extent in the macro heap 
  //friend class MACROSTREAM;

   LEngine *m_peng;
public:
  friend class LEX;
  // friend class MACROSTREAM;

  MACRO(LEngine*, aCHAR *, intCH);
  ~MACRO();

  void applyMacro(aCHAR &);
  bool vacant()
  { return body == NULL;}
  void callDynamic(LEngine* m, aCHAR ** arg)
  { (*dynamic)(m, arg);}
  void display();
  intCH getArity()
  { return arity; }

public:
  intCH arity;                             
  aCHAR name[64];                        // macro name, length in name[0]
  aCHAR **defalt;                        // array of default arg ptrs
  aCHAR *body;                           // macro body, ends in 0
  void (*dynamic)(LEngine*, aCHAR **);   // method to load body (or whatever)
private:  
  MACRO *chain;                          // Next define in bucket chain   
  bool    noParNames;                    // user used indices
  bool    niladic;                       // defined as name()
};                                       // end class MACRO

#endif                                   // LEX_H






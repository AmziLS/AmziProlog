/****************************************************************************
* 
* LExec -- the pcode interpreter
* 
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: lexec.h,v $
* Revision 1.2  2003/12/26 18:26:14  dennis
* some reader fixes
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.11  2003/09/01 11:23:16  dennis
* aaarghh
*
* Revision 1.10  2003/08/23 03:27:31  dennis
* cut working for debug64 compiled code
*
* Revision 1.9  2003/08/21 18:24:02  dennis
* latest fixes to debug64
*
* Revision 1.8  2003/06/13 18:37:01  dennis
* date_time and other fixes
*
* Revision 1.7  2002/06/02 03:50:56  dennis
* all the XStr forms of logic server calls call strterm and grow the
* heap, so made new ExecProve and CallProve that to the strterm inside
* so that the heap can rolled back to before the Exec/Call.  Important
* that this be done in the Prove, so that if heapgc is encountered,
* the new heap is used for the rollback.
*
* Revision 1.6  2002/05/15 16:59:08  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.5  2002/04/02 22:52:43  dennis
* Moved the hotel two feet to the right, changing arity and xi
* in .plm files to be 2 bytes rather than 1.
*
* Revision 1.4  2001/04/02 21:50:13  dennis
* got debugger working again
*
* Revision 1.3  2001/03/13 20:05:18  dennis
* added meta$call to try and isolate meta calls.
*
* Revision 1.2  2001/02/05 03:11:44  dennis
* Fixed nasty heap GC bug, other minor fixes
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.10  2000/09/15 21:42:24  dennis
* 12->13
*
* Revision 1.9  2000/08/26 00:32:06  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.8  2000/05/23 03:37:43  dennis
* changed name of various variables that used to be called mod this
* and that for keeping track of load modules.  Those are now 'file' variables
* and 'mod' is used for real modules.
*
* Revision 1.7  2000/05/14 03:52:34  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
* Revision 1.6  2000/03/28 01:05:16  dennis
* merged Ray's changes with bigdig.  bigdig is at point where
* new Cell class is used, but there are no modules in the system.
*
* Revision 1.5  2000/02/21 20:36:22  ray
* *** empty log message ***
*
* Revision 1.4.2.3  2000/03/22 22:36:40  dennis
* duckworld now working
*
* Revision 1.4.2.2  2000/03/17 03:43:31  dennis
* hello.xpl running right at this point
*
* Revision 1.4.2.1  2000/03/08 04:12:01  dennis
* builtin.cpp compiles
*
* Revision 1.4  2000/02/12 10:19:55  dennis
* minor linker changes
*
* Revision 1.3  2000/01/20 10:25:43  dennis
* Put in Ray's 1999-11-11 release, which included the fix for
* large predicates, which also requires 32-bit alignment.  r5-0-2
*
* Revision 1.2  2000/01/17 09:51:51  dennis
* original a5x modified for new directory structure and
* names, sans the 5, like alnk and alis
*
* 99/10/02 Ray changed Cur_mod_ix to int
* 99/10/02 Ray changed P32 default to ALIGN, but cdATOM to intCH  
* 12/03/98 Ray Added breaks flag
* Revision 4.1  1998/01/13 07:36:29  pete
* Initial changes for solaris
*
*
****************************************************************************/

#ifndef LEXEC_H
#define LEXEC_H

/* CODE is used to define a pointer to a block of code in memory.
   The various lengths, *_L, are used to increment the pointer, so are the
   length in units of CODE.  TOP_L is the length of the startup code in
   pmain.c, it is one cdOP, two ATOMS, one cdSMINT and one cdSINT.

   In order for call and call_local to work, it is essential that ATOM_L
   be at   least 1/2 of PTR_L.

   cdEXIT_L is the length of the kludged exit sequence used in codeNewProve
   and cpCall.  It is an op plus a term.  */

class EscapePredicate;

#if  defined(P64)
typedef  intC    cdSINT;
typedef    uintC   cdSMINT;
typedef    uintC   cdOP;
typedef  PATOM    cdATOM;
typedef  intC    cdMODIX;
typedef  EscapePredicate *cdESCAPE;
#define  cdOP_L    1
#define  CELL_L    3   // a 64-bit pointer + a uintC (32)
#define  PTR_L     2
#define  cdATOM_L  2
#define  cdMODIX_L  1
#define  cdSINT_L  1
#define  cdSMINT_L 1
#define  cdTOP_L   8  // cdOP_L + cdMODIX_L + 2 * cdATOM_L + cdSMINT_L +1
#define  cdEXIT_L  3
#define  cdESCAPE_L 2
#define  cdCODE_L   1

#elif defined(P32)
//#ifdef   ALIGN32
typedef  intC    cdSINT;
typedef  uintC   cdSMINT;
typedef  uintC   cdOP;
typedef  PATOM   cdATOM;   // new atoms are same in code as for real
typedef  intC    cdMODIX;
typedef  EscapePredicate *cdESCAPE;

#define  cdOP_L     1
#define  CELL_L     2  // new cells are two code units
#define  PTR_L      1
#define  cdATOM_L   1
#define  cdMODIX_L  1
#define  cdSINT_L   1
#define  cdSMINT_L  1
#define  cdTOP_L    6
#define  cdEXIT_L   2
#define  cdESCAPE_L 1
#define  cdCODE_L   1
// Allowing large jumps in compiled code requires
// all 32-bit versions to be aligned on 32-bit
// boundaries.
/*
#else
typedef    intCH   cdSINT;
//typedef    intC   cdSINT;                     // ray
typedef    uintCH  cdSMINT;
typedef    uintCH  cdOP;
typedef    intCH   cdATOM;
#define    cdOP_L    1
#define    CELL_L    2
#define    PTR_L     2
#define    cdATOM_L  1
#define    cdSINT_L  1
#define    cdSMINT_L 1
#define    cdTOP_L   5
#define    cdEXIT_L  3
#endif
*/



#elif defined(P16)
typedef    intCH   cdSINT;
typedef    uintCH  cdSMINT;
typedef    uintCH  cdOP;
typedef    intCH   cdATOM;
#define    cdOP_L    1
//#define    CELL_L    2
#define    PTR_L     2
#define    cdATOM_L  1
#define    cdSINT_L  1
#define    cdSMINT_L 1
#define    cdTOP_L   5
#define    cdEXIT_L  3

#endif



typedef   cdSINT *   cdSINTptr;
typedef   cdSMINT *  cdSMINTptr;
typedef   cdOP *     cdOPptr;
typedef   cdATOM *   cdATOMptr;
typedef   cdMODIX *  cdMODIXptr;
typedef   cdESCAPE * cdESCAPEptr;

// values taken by UM 
#define    WRITE_MODE  0
#define    READ_MODE   1

// miscellaneous defines 
//#define  Y_REG   128
//#define  Y_MASK  127
#define  Y_REG   0x8000
#define  Y_MASK  0x7fff

// Time slice used to interrupt prove loop
#define    EXEC_SLICE  100;

// By defining WAMPROFILE, we can generate a profile
// of execution.
#define xWAMPROFILE
#define N_WAMOPS 55

class CompiledPredicate;

struct CODE_HEADER
{
   cdOP     d_wam;
   cdSMINT  file_ix;
   cdATOM   pred_atom;
   cdSMINT  pred_arity;
   MODIX    mod_ix;
   CompiledPredicate *block;
public:
#ifdef LANDFILL
   void Dump(LEngine *m_peng);
#endif
};
typedef CODE_HEADER * CODE_HEADERptr;

const CODE_HEADER NULL_CODE_HEADER = {0, 0, 0, 0};

    // the op-codes 

#define     Ono_op        '\000'    //  0 - 0x00 
#define     Owho_am_i     '\063'    // 51 - 0x33 

    // gets 
#define     Oget_x_var    '\037'    // 31 - 0x1f 
#define     Oget_y_var    '\043'    // 35 - 0x23 
#define     Oget_con      '\001'    //  1 - 0x01 
#define     Oget_nil      '\002'    //  2 - 0x02 
#define     Oget_struc    '\003'    //  3 - 0x03 
#define     Oget_list     '\004'    //  4 - 0x04 
#define     Oget_x_val    '\057'    // 47 - 0x2f 
#define     Oget_y_val    '\060'    // 48 - 0x30 

    // puts 
#define     Oput_x_var    '\061'    // 49 - 0x31 
#define     Oput_y_var    '\062'    // 50 - 0x32 
#define     Oput_unsafe   '\005'    //  5 - 0x05 
#define     Oput_con      '\006'    //  6 - 0x06 
#define     Oput_nil      '\007'    //  7 - 0x07 
#define     Oput_struc    '\010'    //  8 - 0x08 
#define     Oput_list     '\011'    //  9 - 0x09 
#define     Oput_x_val    '\054'    // 44 - 0x2c 
#define     Oput_y_val    '\055'    // 45 - 0x2d 

    // sequencing codes 
#define     Oexit           '\042'    // 34 - 0x22 
#define     Ofail           '\040'    // 32 - 0x20 
#define     Otrust_me_2_else  '\041'  // 33 - 0x21 
#define     Ocall           '\012'    // 10 - 0x0a 
#define     Ocall_direct    '\050'    // 40 - 0x28 
#define     Omod_call       '\067'    // 55 - 0x37
#define     Oproceed        '\013'    // 11 - 0x0b 
#define     Oexec           '\014'    // 12 - 0x0c 
#define     Oexec_direct    '\051'    // 41 - 0x29
#define     Omod_exec       '\070'    // 56 - 0x38
#define     Oescape         '\015'    // 13 - 0x0d 
#define     Oalloc          '\016'    // 14 - 0x0e 
#define     Odealloc        '\017'    // 15 - 0x0f 
#define     Ocut            '\020'    // 16 - 0x10 
#define     Ocutd           '\021'    // 17 - 0x11 
#define     Ocut64          '\071'    // 57 - 0x39 // last number
#define     Otry_me_else    '\022'    // 18 - 0x12 
#define     Otry_me_or_else '\066'    // 54 - 0x36 
#define     Otry            '\023'    // 19 - 0x13
#define     Oretry_me_else  '\024'    // 20 - 0x14
#define     Oretry          '\025'    // 21 - 0x15 
#define     Otrust_me_else  '\026'    // 22 - 0x16 
#define     Otrust          '\027'    // 23 - 0x17 
#define     Oswitch_on_term '\033'    // 27 - 0x1b 
#define     Ogoto           '\034'    // 28 - 0x1c 
#define     Oswitch_on_cons '\035'    // 29 - 0x1d 
#define     Oswitch_on_struc '\036'   // 30 - 0x1e 
#define     Olabel          '\045'    // 37 - 0x25 

    // Ounify ops 
#define     Ounify_x_var    '\044'    // 36 - 0x24 
#define     Ounify_y_var    '\056'    // 46 - 0x2e 
#define     Ounify_unsafe   '\046'    // 38 - 0x26 
#define     Ounify_con      '\030'    // 24 - 0x18 
#define     Ounify_void     '\031'    // 25 - 0x19 
#define     Ou_var_getlist  '\047'    // 39 - 0x27 
#define     Ounify_nil      '\032'    // 26 - 0x1a 
#define     Ounify_x_val    '\052'    // 42 - 0x2a 
#define     Ounify_y_val    '\053'    // 43 - 0x2b 

    // special noops for first clause when switches not needed 
#define     Ono_switch      '\064'    // 52 - 0x34 
#define     Ono_try         '\065'    // 53 - 0x35 

// The names of the WAM codes, defined in LExecDbg.cpp
#ifdef LANDFILL
extern aCHAR * ops[];   // the names of the WAM codes 
#endif

struct ENV;
struct CHOICE_POINT;

class Cell;

class LExec
{
private:
   LEngine *m_peng;
   TF  codePLMTraceB;
   TF  codeTraceInitB;
   TF  breaks;                                  // break button flag
   // the current predicate being executed (pcode.c) 

   PATOM   Cur_goala;
   ARITY   Cur_arity;
   MODIX   Cur_modix;
   STRptr  Cur_goal;
   int     Cur_clause;
   STRptr  Cur_loadfile;
   int     Cur_file_ix;          // ray
   Cell    Cur_cStack;

   aCHAR   *Port;
   bool    pause_flag;

#ifdef TIMEX
   float time1;
   float time2;
#endif

   // code pointers 

   CODEptr  CP;
   CODEptr  P;

   CODE_HEADERptr  Who;

   CODE_HEADER  Who_Or;
   CODE_HEADER  Who_Nil;

   CODE     ExitCode[cdEXIT_L];

   PATOM    Esc_predA;
   ARITY    Esc_arity;
   CODEptr  Esc_code_ptr;

   TF       Esc_CF;
   TERM     Esc_S;
   short    Esc_UM;   // READ or WRITE mode 
   ENV     *Esc_tos;

   int      Slice;    // number of iterations before checking Win msgs 

#ifdef WAMPROFILE
   int    wbi;
   int    bucket[N_WAMOPS];
   FILE*  wamprf;
#endif

   int depth;  // used by plm debug functions


public:
   LExec(LEngine*, TROPT);
   ~LExec();
   void Reset(void);
   int  ProveMain(void);
   int  Prove(CODEptr, CODEptr);
   int  NewProve(CODEptr);
   int  CallProve(TERMptr);
   int  CallProve(STRptr, TERMptr);
   int  ExecProve(TERMptr);
   int  ExecProve(STRptr, TERMptr);
   int  InsertProve(CODEptr);
   int  RedoProve(void);
   int  ClearCall(void);
   CODEptr  failure(void);                      // process failure 

   void InitDebug(TF);

#ifdef LANDFILL
   void cdDebugPause(void);
   void cdDebugChoice(void);
   void cdPrintChoice(CHOICE_POINT *);
   void cdDebugTrail(void);
   void cdDebugCode(CODEptr, int);
   void cdHexDump(aBYTEptr, int);
#endif

   TF GetPLMTraceB()
   {   return codePLMTraceB; }
   TF GetTraceInitB()
   {   return codeTraceInitB; }
   void SetPLMTraceB(TF tf)
   {   codePLMTraceB = tf; }
   void SetTraceInitB(TF tf)
   {   codeTraceInitB = tf; }

   PATOM GetEsc_predA()
   {   return Esc_predA; }
   void SetEsc_predA(PATOM p)
   {   Esc_predA = p; }
   ARITY GetEsc_arity()
   {   return Esc_arity; }
   void SetEsc_arity(ARITY a)
   {   Esc_arity = a; }
   CODEptr GetCP()
   {   return CP; }
   void SetCP(CODEptr cp)
   {   CP = cp; }
   CODEptr GetExitCode()
   {   return ExitCode; }

   void SetCurFA(PATOM f, ARITY a)
   {   Cur_goala = f; Cur_arity = a; }

   TF p_cut_env(void);
   TF p_cut_debug64_env(void);
   TF p_get_env(void);
   TF p_get_env1(void);

   TF p_cuttag(void);
   TF p_cutZtag(void);
   TF p_deZtag(void);
   TF p_reZtag(void);
   TF p_tag(void);
   TF p_saveZstack(void);
   TF p_debugZpause(void);

//   TF p_spy(void);

private:
   TF       winbreak();
   int      choice_point(TF, CODEptr);       // add a choice point 
   int      put_unsafe(CODEptr);
   CODEptr  switch_on_cons(CODEptr);
   CODEptr  switch_on_struc(CODEptr);
   CHOICE_POINT *FindTag(TERM, TF);
   LBOOL    throwPrologError(LExcept);

   // cddebug.c - debugging support
   void  plm_debug_header(void);
   void  plm_trace(void);
   void  plmtrace_msg(aCHAR *, ...);
   void  plm_xs(ARITY);
   void  setwho(void);
//   TF    spying(MODIX, PATOM, ARITY);
   TF    check_this_out(void);
   void  plmprint_cell(TERM);
   void  DumpStack(void);
};

#endif // LEXEC_H

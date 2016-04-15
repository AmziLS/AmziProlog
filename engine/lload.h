/****************************************************************************
* 
* LLoad -- the loader
* 
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: lload.h,v $
* Revision 1.2  2004/04/28 18:30:58  mary
* Added LoadFromMemory for XPL files to the LSAPI.
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.10  2002/07/04 16:20:25  dennis
* support academic registration
*
* Revision 1.9  2002/05/15 16:59:08  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.8  2002/04/02 22:52:44  dennis
* Moved the hotel two feet to the right, changing arity and xi
* in .plm files to be 2 bytes rather than 1.
*
* Revision 1.7  2002/03/15 06:30:59  dennis
* added ensure loaded directive, changed .xpl format to hold
* list of files loaded with that .xpl
*
* Revision 1.6  2002/03/10 22:18:58  dennis
* fixed some small bugs, updated documentation
*
* Revision 1.5  2001/10/06 03:08:00  dennis
* running on Linux mostly, compiling compiler at least
*
* Revision 1.4  2001/10/05 17:07:01  ray
* Added real things. Fixed some bugs. Stopped adding '.' in prep
*
* Revision 1.3  2001/08/05 19:11:20  dennis
* made unload work for .plm files again
*
* Revision 1.2  2001/06/30 02:40:35  dennis
* added stream I/O version of loader from 5.0 work
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.11  2000/10/07 17:57:09  ray
* deleted realSpan, modified nth, added packLong (for compiler),
* made lload load reals
* added inverse/3 for modulo divide.
*
* Revision 1.10  2000/10/01 17:02:12  dennis
* cleaned up bigdig comments
*
* Revision 1.9  2000/08/26 00:32:07  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.8  2000/05/23 03:37:44  dennis
* changed name of various variables that used to be called mod this
* and that for keeping track of load modules.  Those are now 'file' variables
* and 'mod' is used for real modules.
*
* Revision 1.7  2000/05/04 21:47:56  ray
* log, log10, xor, round, floor, ceiling, truncate/1, realpart, fractionpart
*
* Revision 1.6  2000/03/28 23:47:51  dennis
* Changed all tabs to three spaces, and also changed Logic Server
* to use void* for TERM externally and cast to Cell* in LEngine
* implementation.
*
* Revision 1.5  2000/03/28 01:05:17  dennis
* merged Ray's changes with bigdig.  bigdig is at point where
* new Cell class is used, but there are no modules in the system.
*
* Revision 1.4.2.2  2000/03/08 04:12:02  dennis
* builtin.cpp compiles
*
* Revision 1.4.2.1  2000/02/28 23:57:56  dennis
* removed local atoms from engine
*
* Revision 1.4  2000/01/31 14:13:11  dennis
* Cleaned up system dependent code, creating new module, lenv,
* that has it all, or at least most of it.
*
* Revision 1.3  2000/01/20 10:25:43  dennis
* Put in Ray's 1999-11-11 release, which included the fix for
* large predicates, which also requires 32-bit alignment.  r5-0-2
*
* Revision 1.2  2000/01/17 09:51:52  dennis
* original a5x modified for new directory structure and
* names, sans the 5, like alnk and alis
*
* 1999/10/23 Ray changed intC to uintC for lengths
* 1999/01/04 Ray added Large flag to private
* 1998/12/16 Ray added MaxFiles, extra arg to Init and unsigned File_Ix.
*
* Revision 4.8  1998/02/03 23:14:02  dennis
* fixed bug with banner in W95, the old A/W versions of SDK functions problem.
* the textmetric structure comes in two flavors...
*
* Revision 4.7  1998/02/03 21:12:05  dennis
* Increased banner size in hopes of fixing Win95 bug.
*
* Revision 4.6  1998/01/26 16:18:58  mary
* Changed version number to 4.1.
*
* Revision 4.5  1998/01/25 20:07:54  dennis
* Fixed minor problem with timelocked registration messages.
*
* Revision 4.4  1998/01/25 19:29:47  mary
* Put up banner for timelocked registrations.
*
* Revision 4.3  1998/01/24 20:51:47  dennis
* added some code for unload
*
* Revision 4.2  1998/01/22 05:22:58  dennis
* Fixed minor bugs
*
* Revision 4.1  1998/01/13 07:36:56  pete
* Initial changes for solaris
*
* 
****************************************************************************/

#ifndef LLOAD_H
#define LLOAD_H

//#define MAX_FILES         256        // maximum number of modules in pld_code 

struct   LMODFILE
{
   //LString fname;
   aCHAR *fname;
public:
   bool in_use;
   LMODFILE();
   ~LMODFILE() { delete fname; }
};
typedef LMODFILE * LMODFILEptr;


// The fixup datastructure is used to punt references in call(exec). 
// The code pointer points to the atom portion of the 
// Atom/Arity arguments in the code. This code may get overwritten with
// an address and the corresponding op code turned into a _direct
// if the code can be found.

struct FIXUP
{
   FIXUP *link;
   CODEptr fbase;
   intC   foff;                       // location of call/exe op for fixup 
};
typedef FIXUP * FIXUPptr;

#define  MAX_FIXUP_ERRS  10   
#define  CODESIZE         1           // size of opcode in object files 
#define  PLM_CLHEAD_L     6           // number of bytes in each clause header 
#define  XATOM_L          2           // size of atom in object files 

// Load module type flags applied to type byte in .plm & .xpl files
const aBYTE   LONG_CODE_FLAG = 0x40;
const aBYTE   LARGE_FLAG     = 0x20;
const aBYTE   UNICODE_FLAG   = 0x10;
const aBYTE   LOADMOD_FLAG   = 0x03;


class LLoad
{
private:
   LEngine *m_peng;

   LBOOL   IsUnicode;
   LMODFILE *Load_Files;
   std::vector<LPathString> v_short_fnames;
   std::vector<LPathString> v_ensure_loaded;
   intC    MaxClauses;              // maximum clauses, used for labels etc. 
   uintC   MaxFiles;                // maximum number of load module files 
   uintC   DestBufLen;              // size of code destination buffer 
   uintC   SrcBufLen;               // size of code source buffer 
   TF      Large;                   // Large code linker was used 
   TF      LongCode;                // Long code linker was used 
   static TF AmziBanner;

   STRptr   loadNameBuf;
   bool    b_loading;

   aBYTE   m_build;
   aBYTE   m_version;
   
   unsigned short  File_Ix;

   //PATOMptr        pglobal_atoms;               // global atom table 
   // use a vector for the temporary global atom table.  In the future
   // a better solution is to store the number of atoms in the .xpl/.plm
   // file and simply allocate an array of the right size.

   std::vector<PATOM>   v_global_atoms;
   FIXUPptr        FixupTop, FixupHead;

   aBYTEptr  SourceBuf;
   CODEptr  DestBuf;

   TF  bXPLfile;
   TF  bPLMfile;

/* arrays of pointers for resolving labels and references during loading,
   and indices for accessing them. */

   CODEhnd  Clabels;
   CODEhnd  Clabrefs;
   CODEhnd  Ccllabs;

   int      Clnum;
   int      Lref;
   TF       SwitchB;                         // first clause switch flag 
   CODEptr  Elseptr;                         // point to else clause 

   aCHAR    LoadFile[_MAX_PATH];

public:
   LLoad(LEngine *peng);
   ~LLoad();

   int   Load(std::istream *cs);
   int   load_file(STRptr fname);
   int   LoadOps(std::istream *cs);
   int   load_from_memory(STRptr xplname, int length, unsigned char* p);

   void  Init(intC, uintC, uintC, uintC, TF);
   //int   Load(aCHAR *);
   //int   LoadOps(aCHAR * fname);

   aCHAR *GetLMfname(int ix)
   {   return Load_Files[ix].fname; }
   TF    p_loadZfile(void);
   TF    p_loadZops(void);
   TF    p_unload(void);
   TF    p_is_loaded(void);
   TF    p_loadZmemory(void);
   TF    p_loadZmemoryZtest(void);
   TF    p_ensure_loaded(void);

private:
   int GET_INT16(aBYTEptr p)
   {   return ( ((aINT16) *(aSBYTE *)(p+1) << 8) | (aINT16) *(aBYTEptr)(p) ); }
   // For binary files, which this is, fgetc reads a sb char
   // and fgetwc reads a wide char.
   //short  ISUNIGETC(FILE * f)
   //  {   return ( IsUnicode ? fread_int16(f) : fgetc(f) ); }
   int    rd_atom_table(std::istream *cs);
   //int    rd_atom(std::istream *cs);
   CODEptr  rd_xpl_pred(std::istream *cs);
   CODEptr  rd_plm_pred(std::istream *cs);
   LBOOL    rd_plm_ops(std::istream *cs);
   CODEptr  process_code_buf(CODEptr , int);
   int    add_code(CODEptr );
   void   close_down();
   void   FixAll(void);
   void   fixfix(CODEptr);
   void   fixup(CODEptr, TF);
   int    fread_int16(std::istream *cs);
   unsigned int    fread_uint32(std::istream *cs);
   unsigned int    fread_uint16(std::istream *cs);
   TERM   read_xi(aBYTEptr &sp);
   ARITY    read_arity(aBYTEptr &sp);
   cdSMINT    read_yi(aBYTEptr &sp);
   Cell   read_const(aBYTEptr &);
   void   read_xpl_filenames(std::istream *cs);
   void   resolve_labels(void);
   bool   is_same_fname(LPathString f1, LPathString f2);
   int    load_file2(STRptr fname);
   void   un_ensure_loaded(aCHAR *fn);
};

#endif // LLOAD_H


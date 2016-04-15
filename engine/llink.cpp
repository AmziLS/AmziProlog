/****************************************************************************\
*
* LLink.cpp -- the .plm file linker
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
*
\****************************************************************************/


/*
 *
 *   usage: plink target-file  plm-files
 *
 *       puts all plm-files into one bit target file with only one
 *                         symbol table
 *
 *       
 *       Target files which are standard .plm files as emitted by the compiler:
 *               a series of variable length records of the form
 *
 *       length-of-atom-table          16                     |
 *       atom table           (variable length strings)       |--> atom seg
 *       code_length                   16                     |
 *       functor                       16                     |--> code seg
 *       arity                         16                     |
 *       code-buffer          (code-length> bytes             |
 *
 *       Thus .plm files are of the form atom-seg code-seg atom-seg code-seg..
 *
 *       The new file produced will have one atom-seg and then all the code
 *       segs of all the files in one
 */

#include "inc.h"
#include "pch.h"

#include "llink.h"

OSVer g_osver;                                // The Windows OS version
enum ErrCode { LABORT=1 };

#define CHAR_SIZE 2             // how many aCHAR bytes are read/written
#define TERM_BUFSIZE 4096
#define ucFWRITE(a,b,c,d) fwrite((a),(b),(c),(d));

//#define CODESIZE 1;                // size of opcode in object files
//#define PLM_CLHEAD_L 6;            // number of bytes in each clause header
//#define XATOM_L 2;                 // size of atom in object files
const int MAX_ATOM_TABLE_SZ = 512;

int license_info(int ip);

#ifdef BUG_LINK
const char * lops[] =
{
   "no_op",
   "get_con",
   "get_nil",
   "get_struc",
   "get_list",
   "put_unsafe",
   "put_con",
   "put_nil",
   "put_struct",
   "put_list",
   "call",
   "proceed",
   "exec",
   "escape",
   "alloc",
   "dealloc",
   "cut",
   "cutd",
   "try_me_else",
   "try",
   "retry_me_else",
   "retry",
   "trust_me_else",
   "trust",
   "unify_con",
   "unify_void",
   "unify_nil",
   "switch_on_term",
   "goto",
   "switch_on_cons",
   "switch_on_struc",
   "get_x_var",
   "fail",
   "trust_me_2_else",
   "exit",
   "get_y_var",
   "unify_x_var",
   "label",
   "unify_unsafe",
   "u_var_getlist",
   "call_local",   // bigdig not used
   "exec_local",   // bigdig not used
   "unify_x_val",
   "unify_y_val",
   "put_x_val",
   "put_y_val",
   "unify_y_var",
   "get_x_val",
   "get_y_val",
   "put_x_var",
   "put_y_var",
   "who_am_i",
   "no_switch",                     /* special no-op */
   "no_try",                        /* special no-op */
   "try_me_or_else",
   "mod_call",
   "mod_exec"
};
#endif //BUG_LINK

Linker::Linker( void(*pfM)(aCHAR*) )
{
   initialize(pfM);
}


#ifdef _UNICODE
Linker::Linker( void(*pfM)(char*) )
{
   initialize(pfM);
}

#endif

Linker::~Linker()
{
   shutdown();
}

void Linker::zero_fill(aCHAR* s, int len)
{
   for (int i=0; i<len; i++)
      s[i] = 0;
}

void Linker::initialize( void(*pfM)(aCHAR*) )
{
#ifdef BUG_LINK
 lout = fopen("buglink.txt","w");
// printf("buglink open\n");
 fprintf(lout, "** Buglink open\n");
 fprintf(lout, "%s\n", __TIME__);
 fflush(lout);
// return;
#endif
   aCHAR  outbuf[512];

#ifdef BUG_LINK
   pfMsg = NULL;
#else
   pfMsg = pfM;
#endif
   eMaxAtoms = 1024 * MAX_ATOM_TABLE_SZ;
   GAtomTable = NULL;
   LAtomTable = NULL;
   zero_fill(m_ver, 256);
   Lstrcpy(m_ver, AMZI_VERSION);

   Lstrncpy(outbuf, aS("\nAmzi! Prolog Linker "), 256);
   Lstrncat(outbuf, m_ver, 256);
#ifdef BUG_LINK
   return;
#endif
   output(outbuf);
   //output(aS(""));
}

#ifdef _UNICODE
void Linker::initialize( void(*pfM)(char*) )
{
#ifdef BUG_LINK
 lout = fopen("buglink.txt","w");
// printf("buglink open\n");
 fprintf(lout, "** Buglink open\n");
 fprintf(lout, "%s\n", __TIME__);
 fflush(lout);
// return;
#endif
   aCHAR  outbuf[120];
   
   pfMsgA = pfM;
   pfMsg = NULL;
   eMaxAtoms = 1024 * MAX_ATOM_TABLE_SZ;
   GAtomTable = NULL;
   LAtomTable = NULL;
   Lstrcpy(m_ver,  AMZI_VERSION);

   Lstrncpy(outbuf, aS("\nAmzi! Prolog Linker "), 120);
   Lstrncat(outbuf, m_ver, 120);
   output(outbuf);
}
#endif

void Linker::shutdown()
{
   delete[] GAtomTable;
   GAtomTable = NULL;

   delete[] LAtomTable;
   LAtomTable = NULL;
}

#ifdef _UNICODE
int Linker::Link(int argctr, char* *pargvA)
{
   paCHAR *pargvW = new paCHAR[argctr];
   int i, len, rc;

   for (i = 0; i < argctr; i++)
     { 
       len = 1 + (int)strlen(pargvA[i]);
       pargvW[i] = new aCHAR[len];
       mbstowcs(pargvW[i], pargvA[i], len);
     }

   rc = Link(argctr, pargvW);

   for (i = 0; i < argctr; i++)
      delete[] pargvW[i];

   return rc;
}
#endif

int Linker::Link(int argctr, aCHAR* *pargv)
{
   FILE   *pFobj, *pFtarg;
   int    filei, i, j, count, pos;
   int    fpos_eoa;
   aBYTE  ibuf[CL_HEAD_LENGTH+4];
   aBYTE  checksum, curbyte;
   bool   is_system = false;

#if defined(_WIN32)
   DWORD v = ::GetVersion();
   g_osver = v < 0x80000000 ? WNT : W95;
#else
   g_osver = OtherOS;
#endif

   try {
//   locking();   // get the registration status and info
   output(aS("Linking: %s"), pargv[0]);

   if (Lstrstr(pargv[0], aS("alis.xpl")) != NULL ||
       Lstrstr(pargv[0], aS("acmp.xpl")) != NULL ||
       Lstrstr(pargv[0], aS("aidl.xpl")) != NULL ||
       Lstrstr(pargv[0], aS("axrf.xpl")) != NULL ) {
          is_system = true; }

   // Open output .xpl file
   if (NULL == (pFtarg = Lfopen(pargv[0], aS("w+b"))))
      abort_linker(aS("Error: Cannot open xpl file %s"), pargv[0]);

	// make two passes through files - first one builds
	// complete atom table - second pass constructs new file
	// header and atom table then writes out clauses

	// pass 0 - initialise
   
   if (NULL == (GAtomTable = (G_ATOM*) new G_ATOM[eMaxAtoms] ))
      abort_linker(aS("Insufficient memory for global atom table"));
   //for(i = 0; i < eMaxAtoms; ++i)
   //   GAtomTable[i].name = NULL;

   if (NULL == (LAtomTable = (L_ATOM*) new L_ATOM[eMaxAtoms] ))
      abort_linker(aS("Insufficient memory for local atom table"));
   for(i = 0; i < eMaxAtoms; ++i)
      LAtomTable[i] = NULL;

	// pass1
   for (filei = 1; filei < argctr; ++filei)
   {
      output(aS("Opening: '%s'"), pargv[filei]);
      if (NULL == (pFobj = Lasys_fopen(pargv[filei], aS("rb"), aS("abin"))))
         abort_linker(aS("Error: Cannot open plm file '%s'"), pargv[filei]);
      else
      {
         //output(aS("Reading Atom Table: %s"), pargv[filei]);
         read_g_atoms(pFobj, filei);
         fclose(pFobj);
      }
   }

	// now read through global atom table setting index field
	// to the index of the atom as it will be when the table
	// is written out
   count = 0;
   for(i = 0; i < eMaxAtoms; ++i)
      if (GAtomTable[i].name)
      {
          GAtomTable[i].index = count;
          ++count;
      }

   for (i = 0; i < CL_HEAD_LENGTH+4; i++)
      ibuf[i] = 0;

   /* Header bytes
   0 - ff indicating a load module
   1 - xpl/plm flag + unicode flag + large + longcode

   2 - version
   3 - build
   4 - check sum of remaining header stuff + atom table
   5 - 0, 1, 2 for professional or personal or evaluation edition
   6 - platform id based on linker - 1 - Win32
   7-? - serial number product code (ex. APX)
       - serial number platform code (ex. PC)
       - serial number sequence ID, as a long (ex. 93309)
   */

                                      // write header 
   ibuf[0] = 0xFF;                    // the tag 
#ifdef _UNICODE
   ibuf[1] = 0x72;                    // .xpl + Unicode + large + long code

#else
   ibuf[1] = 0x22;                    // signifying .xpl file + large
#endif
   ibuf[2] = VERSION_NUMBER;          // set module bit 
   ibuf[3] = BUILD_NUMBER;            // oldest compiler version supported 

#ifdef DISTVER
#ifdef BUG_LINK
   fprintf(lout, "** Starting Initialization\n");
   fflush(lout);
#endif

	ibuf[8] = 0;
    ibuf[9]  = 0;
    ibuf[10] = 0;
    ibuf[11] = 0;
	ibuf[12] = 0;
	ibuf[13] = 0;
	ibuf[14] = 0;
	ibuf[15] = 0;

	for (i = 16, j=0; i < CL_HEAD_LENGTH; i++, j++)
		ibuf[i] = ibuf[8+j] ^ (aBYTE)i;

#endif // DISTVER

	// fill out header and first four bytes of atom table length with 0s
   ucFWRITE(ibuf, 1, CL_HEAD_LENGTH+4, pFtarg);

   count = 0;

   for(i = 0; i < eMaxAtoms; ++i)             // now write the atom table 
   {
      if (GAtomTable[i].name)
      {
         aCHAR *nptr = GAtomTable[i].name;
           
         write_wstring(GAtomTable[i].name, pFtarg);
         ++count;
      }
   }
   pos = (int) ftell(pFtarg);                 // currrent pos 
   fpos_eoa = pos;                            // end of atom table file pos
   fseek(pFtarg, CL_HEAD_LENGTH, SEEK_SET);   // posn for atom table length 
   pos = pos - (CL_HEAD_LENGTH + 4);          // length of table 
   //printf("pos = 0x%x\n",pos);        ray
   write_int32(pos, pFtarg);                  // write it out 
//   write_int16(pos, pFtarg);                  // write it out 
#ifdef BUG_LINK
   fprintf(lout, "** Atom Table Done\n");
   fflush(lout);
#endif


   // compute check sum, starting with byte #5 and ending before fpos_eoa.
   
   fseek(pFtarg, 5, SEEK_SET);                // write it in byte #4.
   checksum = 0xaa;
   for (i=5; i<fpos_eoa; i++)
   {
      fread(&curbyte, 1, 1, pFtarg);
      curbyte += (aBYTE)i;
      checksum ^= curbyte;
   }
   fseek(pFtarg, 4, SEEK_SET);
   ucFWRITE(&checksum, 1, 1, pFtarg);

#ifdef BUG_LINK
   fprintf(lout, "** Check Sum Done\n");
   fflush(lout);
#endif
   // Back to the end of the atom table to continue the real work.
   fseek(pFtarg, (long)(pos+CL_HEAD_LENGTH+4), SEEK_SET);

   for (filei = 1; filei < argctr; ++filei)
   {
      if (NULL == (pFobj = Lasys_fopen(pargv[filei], aS("rb"), aS("abin"))))
         abort_linker(aS("Warning: Cannot open PLM file %s"), pargv[filei]);
      else
      {
         //output(aS("Reading Code Segments: %s"), pargv[filei]);
         read_code_segs(pFobj, pFtarg, filei);
         fclose(pFobj);
      }
   }
   
#ifdef BUG_LINK
   fprintf(lout, "** Code Segments Done\n");
   fflush(lout);
#endif
   // originally added to catch a bug with binary files and VMS,
   // but is now used to determine end of code, so leave in.
   ibuf[0] = 0;
   for (i=0; i<4; i++)
      ucFWRITE(ibuf, 1, 1, pFtarg);

   // write file names at end of file, so that ensure_loaded, if
   // used, can check to see if a given file was loaded as part
   // of an .xpl file, end with zeroes as above.
   for (filei = 1; filei < argctr; ++filei)
   {
      write_wstring(pargv[filei], pFtarg);
   }

   // originally added to catch a bug with binary files and VMS,
   // but is now used to determine end of file as well, so leave in.
   ibuf[0] = 0;
   for (i=0; i<4; i++)
      ucFWRITE(ibuf, 1, 1, pFtarg);


   fclose(pFtarg);
   for(i = 1, count = 0; i < eMaxAtoms; ++i)
   {
      if (GAtomTable[i].name)
            ++count;
   }

   //output(aS("%d Global Atoms"), count);
   output(aS("Link Done"));
   } catch (ErrCode)
   {
#ifdef BUG_LINK
   fprintf(lout, "** Returning NOTOK 1\n");
   fflush(lout);
#endif
#ifdef BUG_LINK
 fclose(lout);
#endif
      return NOTOK;
   }
   catch (...)
   {
#ifdef BUG_LINK
   fprintf(lout, "** Returning NOTOK 2\n");
   fflush(lout);
#endif
#ifdef BUG_LINK
 fclose(lout);
#endif
      output(aS("\nUnrecognized system error"));
      return NOTOK;
   }

#ifdef BUG_LINK
   fprintf(lout, "** Returning OK\n");
   fflush(lout);
#endif
#ifdef BUG_LINK
 fclose(lout);
#endif

   return OK;
}

/****************   ATOM MANIPULATING CODE **************************/

int  Linker::AtomHash(aCHAR* ps)      // simple character hash fn 
{
   int val = 0;
   short i;
   aCHAR *s;

   i = -1;
   s = ps;
   while (*s)
     {
       i += 2;
       val = val * i + *s++;
     }
   return (int)(val & (eMaxAtoms - 1));
}

void Linker::read_g_atoms(FILE* f, int filei) 
{                                              // Build the Global atom table
   aUINT16     atom_table_length, code_length;
   short     posn;
   aCHAR     buf[TERM_BUFSIZE], *p;
   int       ichar;
   aBYTE     type, version, build;

   if (0 == fread(&type, 1, 1, f))             // 0
      aborteof(aS("header"));
   if (type != 0xFF)
     abort_linker(aS("Error: File is not an Amzi! Prolog object file (.PLM)"));
   if (0 == fread(&type, 1, 1, f))             // 1
      aborteof(aS("header"));
   if (0x03 != (type & 0x03))
     abort_linker(aS("Error: File is not an Amzi! Prolog object file (.PLM)"));
   if (type & 0x10)
      isunicode = LTRUE;
   else
      isunicode = LFALSE;

   if (0 == fread(&version, 1, 1, f))             // 1
      aborteof(aS("header"));
   if (version < COMPATIBLE_VERSION)
      abort_linker(aS("Error: Out-of-date .PLM file needs to be recompiled"));
   if (0 == fread(&build, 1, 1, f))             // 1
      aborteof(aS("header"));
   if (version == COMPATIBLE_VERSION && build < COMPATIBLE_BUILD)
      abort_linker(aS("Error: Out-of-date .PLM file needs to be recompiled"));

   fseek(f, CC_HEAD_LENGTH, SEEK_SET);         // 8  skip header 

   while(LTRUE)                 // for each  atomtable / code segment sequence 
   {
      if (!read_uint16(&atom_table_length, f) || // 8
            atom_table_length == 0)
         return;                                // all done
      if (isunicode)
         atom_table_length = atom_table_length / 2;

      posn = 0;
      while (posn < atom_table_length)         // read atom table
      {
         p = buf;
         do
         {
            if ( p >= (buf + TERM_BUFSIZE))
            {
               //buf[511] = EOS;
               //Lprintf(aS("%d %d %d %d"), buf[0], buf[1], buf[2], buf[3]);
               abort_linker(aS("Error: Global atom too long in code"));
            }
            if (EOF == (ichar = ISUNIGETC(f))) // read chars
               aborteof(aS("global atom table"));
            ++posn;
           }
         while((*p++ = (aCHAR) ichar) != 0);   // until null char
         
         //Lprintf(aS("atom = '%s'\n"), buf);
         EnterGAtom(buf, filei);

      }
      
      if (!read_uint16(&code_length, f))        // 10   get code seg length 
         aborteof(aS("code segment length"));
      
      fseek(f, (long) code_length, SEEK_CUR);  // and seek past it 
   }
}

void Linker::EnterGAtom(aCHAR * abuf, int filei)
{                                  // Enter one global atom whose name is abuf 
   int ahash;
   int oldhash;
   aCHAR *pb;
   const int bbufsize = TERM_BUFSIZE;
   //aCHAR bbuf[bbufsize+1];
   aCHAR *buf;

   buf = abuf;
#ifdef BUG_LINK
   fprintf(lout, "atom: %ls\n", buf);
#endif

   oldhash = ahash = AtomHash(buf);     

   while (GAtomTable[ahash].name)      // while slot full 
   {
      if (0 == Lstrcmp(buf, GAtomTable[ahash].name))
         return;    // symbol already installed
      // if collision, bump it else wrap around
      ahash = (ahash < eMaxAtoms  - 1) ? ahash + 1 :   0;
      if (ahash == oldhash)       // full circle
         abort_linker(aS("Error: Global atom table full"));
   }
   
   if (NULL == (pb = (aCHAR*) new aCHAR[1+Lstrlen(buf)]))
      abort_linker(aS("Error: Out of memory"));

   Lstrcpy(pb, buf);
   GAtomTable[ahash].name = pb;
}

void Linker::read_l_atoms(FILE * f, aUINT16 length, int filei)
{                                       // read local atom table for predicate 
   int i;
   int ccount, ichar;
   aCHAR *pbuf, *qbuf;
   const int bufsize = TERM_BUFSIZE;
   aCHAR abuf[bufsize+1];
   //aCHAR bbuf[bufsize+1];
   static int id;

   for(i = 0; i < eMaxAtoms; ++i) // 1st, reset local atom table from last time
      if (LAtomTable[i])
      {
         delete[] LAtomTable[i];
         LAtomTable[i] = NULL;
      }

   ccount = 0;
   if (isunicode)
      length = length / 2;
   
   for(i=0; i < eMaxAtoms; ++i)
   {
      if(ccount < length)    // read atom table
      {
         pbuf = abuf;
         do
         {
            if (pbuf >= (abuf + bufsize))
            {
               abort_linker(aS("Error: Local atom too long in code"));
            }
            if (-1 == (ichar = ISUNIGETC(f)))
               aborteof(aS("local atom table"));
            ++ccount;
         } while((*pbuf++ = (aCHAR) ichar) != EOS);
           
         qbuf = abuf;
           
         if (NULL == (pbuf = (aCHAR*) new aCHAR[Lstrlen(qbuf) + 1] ))
            abort_linker(aS("Error: Out of memory"));
         Lstrcpy(pbuf, qbuf);
         LAtomTable[i] = pbuf;
      }
      else
         return;
   }
   
   abort_linker(aS("Error: Local atom table full"));
}

int Linker::map_atom(int anumber)
{
  aCHAR *name;
  int oldhash;
  int ahash;
  
  name = LAtomTable[anumber];

#ifdef BUG_LINK
  fprintf(lout, "map_atom: %d, %ls\n", anumber, name);
#endif
  
   if (name == NULL)
      abort_linker(aS("Error: Local atom table disaster (# %d)"), anumber);
  
   oldhash = ahash = AtomHash(name);
  
   while (GAtomTable[ahash].name)    // while slot full
   {
      if(0 == Lstrcmp(name, GAtomTable[ahash].name)) // found it 
      {
         ahash = GAtomTable[ahash].index ;
         // bigdig if(anumber < 0)
         // bigdig    ahash = -ahash;
         return(ahash);
      }
      // if collision, bump it
      ahash = (ahash < eMaxAtoms  - 1) ? ahash + 1 : 0;
      if (ahash == oldhash)    // full circle
         abort_linker(aS("Error: Local atom %s not found in GAtomTable"), name);
    }
  abort_linker(aS("Error: Local atom %s not found in GAtomTable"), name);
  return 0;   // a formality
}


/************************* SEGMENT READING CODE ***********************/

void Linker::read_code_segs(FILE *f, FILE *pFtarg, int filei)
{
   aUINT16 length;

   aBYTE mod, type;

   fseek(f, 1L, SEEK_SET);                     // get to byte 1 
   fread(&type, sizeof(aBYTE), 1, f);          // 1
   if ((type & 0x03) != 0x03)
      abort_linker(aS("Error: Input file not a .PLM file"));
   isunicode = type & 0x10 ? LTRUE :  LFALSE;
   fread(&mod, sizeof(aBYTE), 1, f);           // 2  is it a module 

   //fread(&ver, sizeof(aBYTE), 1, f);           // 3  get the version number 
   //if (ver < CUR_COMPILER)
   //  abort_linker(aS("Error: .PLM file not at current level, %d"), 
   //               CUR_COMPILER);

   fseek(f, CC_HEAD_LENGTH, SEEK_SET);         // 8  skip header 

   // check for 00 at end of .plm, put there for VMS bug 
   while(read_uint16(&length, f) && length) 
     { read_linked_segs(f, length, pFtarg, filei); }
}
                                   
void Linker::read_linked_segs(FILE * f, aUINT16 atom_table_length, 
                              FILE * pFtarg, int filei)
{                             //  reads the linked segments for one PROCEDURE 
  aINT32      proc_code_length;
  aUINT16      clause_code_length;
  CODE       buf[6];
  aUINT16     last_clause, first_clause;
  aINT32      start_of_proc, end_of_proc;  // changed from long
  aINT16     zero = 0;
  char       op;   
  
  start_of_proc = ftell(pFtarg);
  
  /* build the procedure prefix segment
     
     proc_code_length
     name
     arity

     */
  read_l_atoms(f, atom_table_length, filei);    // Read the local atom table 
  
  if (!read_uint16(&clause_code_length, f))      // code length 
    aborteof(aS("linked segment length"));
#ifdef BUG_LINK
  fprintf(lout, "clause_length = %u, local_atom_table_length = %u\n", clause_code_length, atom_table_length);
#endif
  //  ucFWRITE(&zero, sizeof(aINT16), 1, pFtarg);      // reserve this 
  // ucFWRITE(&zero, sizeof(intC), 1, pFtarg);      // reserve this  ray
  ucFWRITE(&zero, sizeof(aINT32), 1, pFtarg);      // no ray, should be int32, intC might be 64
  
  if (!read_uint16(&last_clause, f))             // last clause marker 
    aborteof(aS("linked segment clause marker"));
  
  read_atom(f, pFtarg);                         // functor 
  
  if (0 == fread(buf, sizeof(aINT16), 1, f))     // arity 
    aborteof(aS("linked segment arity"));
  ucFWRITE(buf, 2, 1, pFtarg);
  
  proc_code_length = clause_code_length - 2;     // size of first clause 
  /* 
     There is no last clause marker in the created prefix segment 
     so subtract this off
     */
   first_clause = 1;
   while (1)
   {                                            // We are now at the code

#ifdef BUG_LINK
      fprintf(lout, "\nNext clause");
#endif
      if (first_clause)
      {
         first_clause = 0;
         fread(&op, sizeof(aBYTE), 1, f);       // see if  switch is active
         if (op == Ono_switch)
         {
#ifdef xBUG_LINK
            fprintf(lout, "\nno switch");
#endif
            fseek(f, 6L, SEEK_CUR);            // skip the remaining labels
            clause_code_length -= 7;
            proc_code_length -= 7;
              
            fread(&op, sizeof(aBYTE), 1, f);   // now look at try_me_else
            if (op == Ono_try)
            {
#ifdef xBUG_LINK
               fprintf(lout, "\nno try_me_else");
#endif
               clause_code_length -= 5;
               proc_code_length -= 5;
               fseek(f, 4L, SEEK_CUR);         // skip label and NTV
            }
            else
               fseek(f, -1L, SEEK_CUR);          // get back to the try_me
         }
         else
            fseek(f, -1L, SEEK_CUR);              // get back to first op code
      }
      
      read_code(f, pFtarg, PLM_CLHEAD_L, clause_code_length, (aBYTE*) buf);
      if (last_clause)                            // was last clause 
         break;
      
      if (!read_uint16(&atom_table_length, f))
         aborteof(aS("linked atom table length"));
      read_l_atoms(f, atom_table_length, filei);
      
      if (!read_uint16(&clause_code_length, f))    
         aborteof(aS("linked clause length"));
#ifdef BUG_LINK
  fprintf(lout, "clause_length = %d\n", clause_code_length);
#endif

      /* We don't include junk in prefix as part of length 
         after the first clause (we only have ONE name, arity etc) */
      
      proc_code_length += clause_code_length - PLM_CLHEAD_L;
      
      if (!read_uint16(&last_clause, f))
        aborteof(aS("linked last clause"));
      
      fseek(f, 4L, SEEK_CUR);                 // skip Name, ARITY 
    }
#ifdef BUG_LINK
  fprintf(lout, "\n  ----- end of predicate ----- \n");
#endif
                         // seek back to fill in proc_size & proc length 
  end_of_proc = ftell(pFtarg);
  fseek(pFtarg, start_of_proc, SEEK_SET);
#ifdef BUG_LINK
  fprintf(lout, "proc_length = %d\n", proc_code_length);
#endif
  write_int32(proc_code_length, pFtarg);
  fseek(pFtarg, end_of_proc, SEEK_SET);

}

int Linker::read_code(FILE* f, FILE* pFtarg, long init_pos, int length, 
							 aBYTE* buf)
{
  aBYTE op;
  int   x;
#ifdef BUG_LINK
  int   i;
#endif
  
  while( init_pos < length)
    {
#ifdef BUG_LINK
      fprintf(lout, "\nstart length %d init_pos %d ", length, init_pos);
#endif
      if (0 == fread(buf, CODESIZE, 1, f))
        aborteof(aS("code"));
      ucFWRITE(buf, CODESIZE, 1, pFtarg);
      init_pos += CODESIZE;
      op = *buf;
#ifdef BUG_LINK
      fprintf(lout, "\nop %d, %s: ", (int) op, lops[op]);
#endif
      switch(op)
        {    
        case Ono_op:                           // no args 
        case Ofail:
        case Oproceed:
        case Odealloc:
        case Ocut:
        case Ocut64:
        case Otrust_me_else:
        case Ou_var_getlist:
        case Ounify_nil:
          break;
          
        case Ounify_y_var:                     // Xi or Yi 
        case Ounify_y_val:
        case Ounify_unsafe:
        case Oget_nil:
        case Oget_list:
        case Oput_nil:
        case Oput_list:
        case Ounify_x_var:
        case Ounify_x_val:
          if (0 == fread(buf, 2, 1, f))
            aborteof(aS("unify x val"));
          ucFWRITE(buf, 2, 1, pFtarg);
          init_pos += 2;
          break;
          
        case Oget_y_var:                       // Xi and Yi or Xj 
        case Oget_y_val:
        case Oput_y_var:
        case Oput_y_val:
        case Oput_unsafe:
        case Oget_x_var:
        case Oget_x_val:
        case Oput_x_var:
        case Oput_x_val:
          if (0 == fread(buf, 2, 2, f))
            aborteof(aS("put x val"));
          ucFWRITE(buf, 2, 2, pFtarg);
          init_pos += 4;
          break;
          
        case Ounify_void:                      // short int 
        case Oalloc:
        case Ocutd:
        case Oretry_me_else:
        case Oretry:
        case Otrust:
        case Ogoto:
        case Otrust_me_2_else:
        case Olabel:
          if (0 == fread(buf, 2, 1, f))
            aborteof(aS("label"));
          ucFWRITE(buf, 2, 1, pFtarg);
          init_pos += 2;
#ifdef BUG_LINK
          fprintf(lout, "%d", (int) getint16(buf));
#endif
          break;
          
        case Otry_me_else:
        case Otry_me_or_else:
        case Otry:                             // 2 short ints 
          if (0 == fread(buf, 2, 2, f))
            aborteof(aS("try"));
          ucFWRITE(buf, 2, 2, pFtarg);
          init_pos += 4;
#ifdef BUG_LINK
          fprintf(lout, "%d, %d", 
                  (int) getint16(buf), (int) getint16(buf+2));
#endif
          break;
          
        case Oget_con:                         // Constant, Xi 
        case Oput_con:
          read_const(f, pFtarg, &init_pos);
          if (0 == fread(buf, 2, 1, f))
            aborteof(aS("put con"));
          ucFWRITE(buf, 2, 1, pFtarg);
          init_pos += 2;
          break;
          
        case Oexec:
        case Oescape:
        case Ocall:                          
        case Omod_call:
        case Omod_exec:
        case Oget_struc:              // functor, arity, (Xi or short or null)
        case Oput_struc:
          read_atom(f, pFtarg);                // functor 
          init_pos += 2;
          if (op == Ocall || op == Oexec || op == Omod_call || op == Omod_exec)      // new style call/exec 
            {
              //if (0 == fread(buf, 2, 1, f)) 
              //  aborteof(aS("put struc"));
              //ucFWRITE(buf, 2, 1, pFtarg);
             // shouldn't this be a read atom, not just a buf??
             read_atom(f, pFtarg);
              init_pos += 2;
            }       
          
          if (0 == fread(buf, 2, 1, f))
            aborteof(aS("put struc 2"));
          ucFWRITE(buf, 2, 1, pFtarg);
          init_pos += 2;
#ifdef BUG_LINK
          fprintf(lout, "  arity = %d", (int) getint16(buf));
#endif
                                             // now figure third (optional arg 
          if (op == Oget_struc || 
              op == Oput_struc)                // Xi 
            {
              if (0 == fread(buf, 2, 1, f))
                aborteof(aS("put struc 3"));
              ucFWRITE(buf, 2, 1, pFtarg);
              init_pos += 2;
#ifdef BUG_LINK
          fprintf(lout, "  xi(get/put_struc) = %d", (int) getint16(buf));
#endif
            }
          else if (op == Ocall || op == Omod_call)                // short 
            {
              if (0 == fread(buf, 2, 1, f))
                aborteof(aS("put struc 4"));
              ucFWRITE(buf, 2, 1, pFtarg);
              init_pos += 2;
#ifdef BUG_LINK
          fprintf(lout, "  short(call/mod_call) = %d", (int) getint16(buf));
#endif
            }
                                              // else no 3rd arg 
          break;
          
        case Ounify_con:
          read_const(f, pFtarg, &init_pos);
          break;
          
          
        case Oswitch_on_term:
          if (0 == fread(buf, 2, 3, f))        // lab1, lab2, lab3 
            aborteof(aS("switch on term"));
          init_pos += 6;
          ucFWRITE(buf, 2, 3, pFtarg);
#ifdef BUG_LINK
          for (i=0; i<3; i++) 
            fprintf(lout, " %d ",(int) getint16(buf + 2 * i));
#endif
          break;
          
        case Oswitch_on_cons:           // short size, (size x |CELL|LABEL|) 
          if (0 == fread(buf, 2, 1, f))
            aborteof(aS("switch on cons"));
          ucFWRITE(buf, 2, 1, pFtarg);
          x = getint16(buf);
          init_pos += 2;
          while(x--)
            {       
              read_const(f, pFtarg, &init_pos); // get const 
              if (0 == fread(buf, 2, 1, f))     // branch label 
                aborteof(aS("switch on cons 2"));

#ifdef BUG_LINK

              fprintf(lout, " branch[%d] ", (int) getint16(buf));
#endif
              ucFWRITE(buf, 2, 1, pFtarg);
              init_pos += 2;
            }
          break;
          
        case Oswitch_on_struc:    // short size, (size x |NAME|ARITY|LABEL|) 
          if (0 == fread(buf, 2, 1, f))
            aborteof(aS("switch on struc"));
          ucFWRITE(buf, 2, 1, pFtarg);
          x = getint16(buf);
          init_pos += 2;
          while(x--)
            {       
              read_atom(f, pFtarg);
              if (0 == fread(buf, 2, 1, f)) // arity 
                aborteof(aS("switch on struc 2"));
              ucFWRITE(buf, 2, 1, pFtarg);
              
              if (0 == fread(buf, 2, 1, f))    // label 
                aborteof(aS("switch on struc 2"));

#ifdef BUG_LINK
              fprintf(lout, " branch[%d] ", (int) getint16(buf));

#endif
              ucFWRITE(buf, 2, 1, pFtarg);
              init_pos += 2 + 2 + 2;
            }
          break;
          
        default:
          abort_linker(aS("Error: Bad opcode in CodeStream %d"), op);
        }
#ifdef BUG_LINK
      fprintf(lout, "\nend length %d init_pos %d ", length, init_pos);
#endif
    }
  return(1);
}

void Linker::read_atom(FILE* f, FILE* pFtarg)
{
   aBYTE buf[2];
   aINT16 temp, gatom;


   if (0 == fread(buf, 2, 1, f))
      aborteof(aS("read atom"));
   temp = getint16(buf);
   gatom = map_atom(temp);
   //fwrite(&gatom, 2, 1, pFtarg);
   write_int16(gatom, pFtarg);
}

void Linker::read_const(FILE *f, FILE *pFtarg, long *pi)
{
   /* a constant is  a tag followed by constant data 
   tag = 1 -> constant is short atom
   tag = 2 -> constant is short int
   tag = 3 -> constant is arb string terminated by EOS
   tag = 4 -> constant is 4 byte float
   tag = 5 -> constant is 4 byte long
   tag = 6 -> constant is 4 byte long wide char
   tag = 7 -> constant is 8 byte double
   tag = 8 -> constant is real
    */
   aBYTE buf[8];
   int   ccount;
   aBYTE ibyte;
   aINT16 temp, length;
	aINT32 temp32;
   aCHAR ichar;

   if (0 == fread(buf, 1, 1, f))
      aborteof(aS("read const"));
   ucFWRITE(buf, 1, 1, pFtarg);
   (*pi) += 1;
   switch(*buf)
     {
     case 1:
       read_atom(f, pFtarg);
       (*pi) += 2;
       break;
       
     case 2:
       if (0 == fread(buf, 2, 1, f))
         aborteof(aS("read const 2"));
       ucFWRITE(buf, 2, 1, pFtarg);
       (*pi) += 2;     
       break;
       
     case 3:                                      // single byte string 
       ccount = 1;
       if (0 == fread(&ibyte, 1, 1, f))
         aborteof(aS("read const 3"));
       ichar = (aCHAR)ibyte;
       while(ichar)
         {
           ++ccount;
           ucFWRITE(&ichar, 1, 1, pFtarg);
           if (0 == fread(&ibyte, 1, 1, f))
             aborteof(aS("read const 3b"));
           ichar = (aCHAR)ibyte;
         }
       ucFWRITE(&ichar, 1, 1, pFtarg);
       (*pi) += ccount*sizeof(char);
       break;
       
     case 4:                                      // float
       if (0 == fread(buf, 4, 1, f))
         aborteof(aS("read const 4"));
       ucFWRITE(buf, 4, 1, pFtarg);
       (*pi) += 4;     
       break;
       
     case 5:                                      // long
       if (0 == fread(buf, 4, 1, f))
         aborteof(aS("read const 5"));
       ucFWRITE(buf, 4, 1, pFtarg);
       (*pi) += 4;     
       break;
       
     case 6:                                      // wide character string 
       ccount = 1;
       //    if (0 == fread(&ichar, sizeof(aCHAR), 1, f))
       if (!read_int16(&temp, f))
         aborteof(aS("read const 3"));
       while(temp)
         {
           ++ccount;
           //       ucFWRITE(&ichar, sizeof(aCHAR), 1, pFtarg);
           write_int16(temp, pFtarg);
           //       if (0 == fread(&ichar, sizeof(aCHAR), 1, f))
           if (!read_int16(&temp, f))
             aborteof(aS("read const 3b"));
         }
       //    fwrite(&ichar, sizeof(aCHAR), 1, pFtarg);
       write_int16(temp, pFtarg);
       (*pi) += ccount*2;
       break;
       
	  case 9:                                   // fixed
     case 7:                                   // C double
       if (0 == fread(buf, 8, 1, f))
         aborteof(aS("read const 7"));
       ucFWRITE(buf, 8, 1, pFtarg);
       (*pi) += 8;     
       break;

	  case 8:
		 for(ccount = 0; ccount < 6; ccount += 2)
			{                                    // copy the real descr
			  if (!read_int16(&temp, f))
				 aborteof(aS("read const 3"));
			  if(ccount == 0) 
				 length = temp;                   // length
           write_int16(temp, pFtarg);
			}
		 while(length)
			{                                    // copy the gigits
			  if (!read_int32(&temp32, f))
				 aborteof(aS("read const 3b"));
           write_int32(temp32, pFtarg);
			  ccount+= 4;
			  length--;
			}
       (*pi) += ccount;
     }                                        // end switch
}

void Linker::output(aCHAR* fmt, ...)
{
  va_list   arg_ptr;
  
  va_start(arg_ptr, fmt);
  Lvsprintf(msgBuf, fmt, arg_ptr);
  va_end(arg_ptr);
  
#ifdef BUG_LINK
  fprintf(lout, "output: %ls\n", msgBuf);
  fflush(lout);
  return;
#endif
  if (pfMsg)
    (*pfMsg)(msgBuf);

#ifdef _UNICODE
  else
    {
      int len = 1 + (int)Lstrlen(msgBuf);
      char *msgA = new char[len];
      wcstombs(msgA, msgBuf, len);
      (*pfMsgA)(msgA);
      delete[] msgA;
    }
#endif
}

void Linker::abort_linker(aCHAR* s,...)
{
   va_list   arg_ptr;

   va_start(arg_ptr, s);
   Lvsprintf(msgBuf, s, arg_ptr);
   va_end(arg_ptr);

   if (pfMsg)
      (*pfMsg)(msgBuf);

   throw(LABORT);
}

void Linker::aborteof(aCHAR * s)
{
   abort_linker(aS("Error: Unexpected end-of-file reading %s"), s);
}

int Linker::scampi(aCHAR * s1, aCHAR * s2)
{                                          // string compare, case insensitive 
  int i;
  
  for (i=0; Ltolower(s1[i]) == Ltolower(s2[i]); i++)
    if (s1[i] == '\0') 
      return 0;
  
  return(s1[i] - s2[i]);
}

int Linker::fread_int16(FILE *f)
{                        // read a 2-byte integer -- if you can't then blow up 
  aINT16 i;
  
  if (!read_int16(&i, f))
    //   if (0 == fread((aBYTE *) &i, 2, 1, f))
    aborteof(aS("while processing"));
  return((int) i);
}

int Linker::write_wstring(aCHAR *str, FILE *fp)
{
   aINT16 buf[1024];
   int count;

   count = 0;
   while(str[count])
     {
       aINT16 val = str[count];
       RevEndian16(val);
       buf[count] = val;
       count++;
     }
   buf[count] = 0;
   count++;
   return (int)ucFWRITE(buf, count*2, 1, fp);
}

int Linker::read_int32(aINT32 *val, FILE *fp)
{
   long addr = ftell(fp);
   if (1 != fread(val, sizeof(aINT32), 1, fp))
      return 0;
   RevEndian32((*val));
   return 1;
}

int Linker::read_int16(aINT16 *val, FILE *fp)
{
   long addr = ftell(fp);
   if (1 != fread(val, sizeof(aINT16), 1, fp))
      return 0;
   RevEndian16((*val));
   return 1;
}
   
int Linker::read_uint16(aUINT16 *val, FILE *fp)
{
   long addr = ftell(fp);
   if (1 != fread(val, sizeof(aUINT16), 1, fp))
      return 0;
   RevEndian16((*val));
   return 1;
}
   
int Linker::write_int32(int val, FILE *fp)
{
   long addr = ftell(fp);
   RevEndian32(val);
   return 1 == fwrite(&val, sizeof(int), 1, fp)  ? 1 : 0;
}

int Linker::write_int16(aINT16 val, FILE *fp)
{
   long addr = ftell(fp);

   RevEndian16(val);
   return 1 == fwrite(&val, sizeof(aINT16), 1, fp)  ? 1 : 0;
}

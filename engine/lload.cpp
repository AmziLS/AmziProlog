/****************************************************************************
*
* LLoad -- the loader
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
****************************************************************************/

#include "inc.h"
#include "pch.h"

#ifdef LANDFILL
#define noBUG_LOAD

#ifdef BUG_LOAD
#define BUG_ADD
#define BUG_XPL
#define BUG_PLM
#define BUG_FIXUP
#define noBUG_LABELS
#define BUG_CALL
#define BUG_BUFS
#endif

#endif

#ifdef xBUG_LOAD
#define EOFCHECK(cs)  if(!(*cs)) pXCPT->Error(eofE, cs->aS("loading"))
#else
#define EOFCHECK(cs)
#endif

LMODFILE::LMODFILE()
{
   fname = NULL;
   in_use = false;
}

LLoad::LLoad(LEngine *peng)
{
   m_peng = peng;

   bXPLfile = FALSE;
   bPLMfile = FALSE;
   b_loading = false;

   SourceBuf     = NULL;
   DestBuf       = NULL;
   loadNameBuf   = NULL;
   Clabels       = NULL;
   Clabrefs      = NULL;
   Ccllabs       = NULL;
   Load_Files    = NULL;
   //pglobal_atoms = NULL;
// bigdig   plocal_atoms  = NULL;
// bigdig   plocal_map    = NULL;
}

LLoad::~LLoad()
{
   // Should already be deleted by close_down, but just in case:
   delete SourceBuf;
   delete DestBuf;
   delete loadNameBuf;
   delete Clabels;
   delete Clabrefs;
   delete Ccllabs;
   delete[] Load_Files;
}

TF LLoad::AmziBanner = TRUE;

void LLoad::Init(intC maxcl, uintC maxfile, uintC dbufl, uintC sbufl, TF ban)
// We've moved AmziBanner to a static variable. 
// For now at least, we no longer recognizing the .ini parameter override
// of the banner.
{
   MaxClauses = maxcl;
   File_Ix     = 0;

   MaxFiles = maxfile;
   LNEW(Load_Files, LMODFILE[MaxFiles], aS("load"));

// bigdig   plocal_map = NULL;

   DestBufLen = dbufl;
   DestBuf    = NULL;

   SrcBufLen  = sbufl;
   SourceBuf  = NULL;

//   AmziBanner = ban;

   return;
}


void LLoad::close_down()
{
   delete SourceBuf;
   delete DestBuf;
   delete loadNameBuf;
   delete Clabels;
   delete Clabrefs;
   delete Ccllabs;

   SourceBuf     = NULL;
   DestBuf       = NULL;
   loadNameBuf   = NULL;
   Clabels       = NULL;
   Clabrefs      = NULL;
   Ccllabs       = NULL;

   b_loading = false;
   //File_Ix++;
}

int LLoad::Load(std::istream *cs)
  /*
   load code in file  fname -- the string fname should be permanent
   since we are going to plant pointers to it in the Clause Info blocks
   of all predicates loaded herein .. so far this is the case.
   Taking Alan at his word above, we will also store pointers in load_modules.
*/
{
  CODEptr  buf;
  aBYTE    type, checksum;
//  aBYTE    prod;
  aBYTE    curbyte, curchecksum;
  aBYTE    unused;  // space to dump bytes not being used right now
  int      bump, status;
  uintC    i;
  int      atomtablelength, checksumlength;

  try
    {
      for (i = 0; i < MaxFiles; i++)              // do not load module twice
      {
        if(Load_Files[i].in_use && Lstrcmp(Load_Files[i].fname, LoadFile) == 0)
        {
           // made this change so reconsult of a project with .plm files works
           // without complaining.  not sure this is a good choice, but needed
           // for the galileo class.
//          pXCPT->Error(modsurfeitE, fname);
            return TRUE;
        }
      }

      b_loading = true;

      // find the first unused module slots
      for (i = 0; i < MaxFiles; i++)
      {
         if (! Load_Files[i].in_use)
            break;
      }
      if (i >= MaxFiles)
         pXCPT->Error(modsurfeitE, LoadFile);
      else
         File_Ix = (unsigned short)i;

      FixupTop  = FixupHead  = NULL;

      /* Header bytes
         0 - ff indicating ia load module
         1 - xpl/plm flag + unicode flag
         2 - version
         3 - build
         4 - check sum of remaining header stuff + atom table
         5 - 0, 1, 2 for professional, personal or evaluation edition
         6 - platform id based on linker - 1 - Win32
         7-? - serial number product code (ex. APX)
         - serial number platform code (ex. PC)
         - serial number sequence ID, as a long (ex. 93309)
         */
                                                // inspect the header
      cs->read((char*)&type, 1);                // byte 0
      EOFCHECK(cs);
      if (type != 0xFF)
        pXCPT->Error(loadfileE, LoadFile);

      cs->read((char*)&type, 1);                // byte 1
      EOFCHECK(cs);
      Large = type & LARGE_FLAG;
      LongCode = type & LONG_CODE_FLAG;

      cs->read((char*)&m_version, 1);              // byte 2
      EOFCHECK(cs);
      if (m_version < COMPATIBLE_VERSION)
         pXCPT->Error(compverE);

      cs->read((char*)&m_build, 1);                 // byte 3
      EOFCHECK(cs);
      if (m_version == COMPATIBLE_VERSION && m_build < COMPATIBLE_BUILD)
         pXCPT->Error(compverE);

      cs->read((char*)&checksum, 1);              // byte 4
      EOFCHECK(cs);

      switch(type & LOADMOD_FLAG)
        {
        case 0x03:                             // .plm file
          cs->seekg( CC_HEAD_LENGTH, std::ios::beg);  // CC_HEAD_LENGTH == 8
          break;

        case 0x02:         // compressed .xpl - one atom table then code segs
          cs->read((char*)&unused, 1);  // bytes 5, 6, 7 not used now
          EOFCHECK(cs);                 // leave this
          cs->read((char*)&unused, 1);
          EOFCHECK(cs);
          cs->read((char*)&unused, 1);
          EOFCHECK(cs);

/* keep til we make sure we've got this right
		  cs->read((char*)&key_flags, 1);
          EOFCHECK(cs);
          cs->read((char*)expiration_date, 3);
          EOFCHECK(cs);
          cs->read((char*)&xpl_fingerprint, 4);
          EOFCHECK(cs);
          RevEndian32(xpl_fingerprint);
*/
		  
		  cs->read((char*)&unused, 1);
          EOFCHECK(cs);
          cs->read((char*)&unused, 1);
          EOFCHECK(cs);
          cs->read((char*)&unused, 1);
          EOFCHECK(cs);
          cs->read((char*)&unused, 1);
          EOFCHECK(cs);
          cs->read((char*)&unused, 1);
          EOFCHECK(cs);
          cs->read((char*)&unused, 1);
          EOFCHECK(cs);
          cs->read((char*)&unused, 1);
          EOFCHECK(cs);
          cs->read((char*)&unused, 1);
          EOFCHECK(cs);
		            
          cs->seekg( CL_HEAD_LENGTH, std::ios::beg);
          atomtablelength = Large ? fread_uint32(cs) : fread_uint16(cs);
          bump = Large ? 4 : 2;
          checksumlength = CL_HEAD_LENGTH + bump + atomtablelength;
          cs->seekg( 5, std::ios::beg);
#ifdef BUG_XPL
          DUMP << aS("atom table length = ") << atomtablelength;
          DUMP << aS(" checksum length = ") << checksumlength;
          DUMP << aS(" checksum = ") << checksum << NL << FLUSH;
#endif
          // This is the same as the linker code that sets the checksum.
          curchecksum = 0xaa;
          for (int i = 5; i < checksumlength; i++)
            {
              cs->read((char*)&curbyte, 1);
              EOFCHECK(cs);
              curbyte += (aBYTE)i;
              curchecksum ^= curbyte;
            }

          if (curchecksum != checksum)
            pXCPT->Error(checksumE, LoadFile);
          cs->seekg( CL_HEAD_LENGTH, std::ios::beg);
          break;

        default:
          pXCPT->Error(loadfileE, LoadFile);
	  }

      LNEW(SourceBuf, aBYTE[SrcBufLen+1], aS("load"));
      LNEW(DestBuf, CODE[DestBufLen], aS("load"));
      LNEW(loadNameBuf, aCHAR[pSTATE->m_buflen], aS("load"));
      LNEW(Clabels, CODEptr[MaxClauses], aS("load"));
      LNEW(Clabrefs, CODEptr[MaxClauses], aS("load"));
      LNEW(Ccllabs, CODEptr[MaxClauses], aS("load"));

      int fname_len;
      if (File_Ix < MaxFiles)
      {
         //Load_Files[File_Ix].fname = LString(LoadFile);
         fname_len = (int)Lstrlen(LoadFile);
         LNEW(Load_Files[File_Ix].fname, aCHAR[fname_len + 1], aS("load file name"));
         Lstrcpy(Load_Files[File_Ix].fname, LoadFile);
         Load_Files[File_Ix].in_use = true;
      }
      else
         pXCPT->Error(filemaxE, LoadFile);

      //if (File_Ix < MaxFiles)
      //{
      //  Load_Files[File_Ix].fname = LString(LoadFile);
      //  Load_Files[File_Ix].in_use = true;
      //}
      //else
      //  pXCPT->Error(filemaxE, LoadFile);

      IsUnicode =                            // Check Unicode flag
        type & UNICODE_FLAG ? LTRUE : LFALSE;

      switch(type & LOADMOD_FLAG)
        {
        case 0x03:                           // .plm file
          bXPLfile = FALSE;
          while(buf = rd_plm_pred(cs)) 
            add_code(buf);
          break;
      
        case 0x02:            // cmprssd .xpl - one atom table then code segs
          //if (prod != 0)      // if 0, professional version, else show banner 
          //   banner(prod);
          bXPLfile = TRUE;
          status = rd_atom_table(cs);
          while(buf = rd_xpl_pred(cs))
            add_code(buf);

          read_xpl_filenames(cs);
          break;
      
        default:
          pXCPT->Error(loadfileE, LoadFile);
        }             
                                              // Our work is done here Tonto 
      FixAll();

      close_down();
       
    } catch (LExcept &E)
      {
        // This seemed to have been working, and then broke.  Changing
        // it so it creates a new exception and throws it instead, fixed
        // the problem.  It also worked under debug mode but not release mode.
        //LExcept E2 = E;
        //close_down(f);
        //E2.AddLoadInfo(fname);
        // New note - setting optimization to default, rather than speed
        // also fixes the problem.  So now it works like before.

        E.AddLoadInfo(LoadFile);
        close_down();

        // And more, putting this kludge back in fixed the problem with
        // thrown errors during loading.  Who knows what evil lurks
        // in the heart of Microsoft optimization... (it works without
        // the kludge with speed optimization off)

        LExcept E2 = E;
        throw E2;
      }
    catch (...)
      {
        close_down();
        throw;
      }

    return(TRUE);
}

int LLoad::LoadOps(std::istream *cs)
{                           // Load the operator definitions from a .PLM file.
  aBYTE    type, checksum;

  try {
    FixupTop = FixupHead = NULL;

    cs->read((char*)&type, 1);  // byte 0
    EOFCHECK(cs);
    if (type != 0xFF)
      pXCPT->Error(loadfileE, LoadFile);

    cs->read((char*)&type, 1);  // byte 1
    EOFCHECK(cs);
    Large = type & LARGE_FLAG;

   cs->read((char*)&m_version, 1);  // byte 2
   EOFCHECK(cs);
   if (m_version < COMPATIBLE_VERSION)
      pXCPT->Error(compverE);

   cs->read((char*)&m_build, 1);  // byte 3
   EOFCHECK(cs);
   if (m_version == COMPATIBLE_VERSION && m_build < COMPATIBLE_BUILD)
      pXCPT->Error(compverE);

    cs->read((char*)&checksum, 1);  // byte 4
    EOFCHECK(cs);

/*
    cs->read((char*)&prod, 1);
    EOFCHECK(cs);
*/
    switch(type & LOADMOD_FLAG)
      {
      case 0x03:                             // .plm file
        cs->seekg( CC_HEAD_LENGTH, std::ios::beg);  // CC_HEAD_LENGTH == 8
        break;

      case 0x02:             // compressed .xpl - one atom table then code segs
        cs->seekg( CL_HEAD_LENGTH, std::ios::beg);
        break;

      default:
        pXCPT->Error(loadfileE, LoadFile);
      }


   LNEW(SourceBuf, aBYTE[SrcBufLen+1], aS("load"));
   LNEW(DestBuf, CODE[DestBufLen], aS("load"));
   LNEW(loadNameBuf, aCHAR[pSTATE->m_buflen], aS("load"));
   LNEW(Clabels, CODEptr[MaxClauses], aS("load"));
   LNEW(Clabrefs, CODEptr[MaxClauses], aS("load"));
   LNEW(Ccllabs, CODEptr[MaxClauses], aS("load"));

   int fname_len;
   if (File_Ix < MaxFiles)
   {
      //Load_Files[File_Ix].fname = LString(LoadFile);
      fname_len = (int)Lstrlen(LoadFile);
      LNEW(Load_Files[File_Ix].fname, aCHAR[fname_len + 1], aS("load file name"));
      Lstrcpy(Load_Files[File_Ix].fname, LoadFile);
      Load_Files[File_Ix].in_use = true;
   }
   else
      pXCPT->Error(filemaxE, LoadFile);

                                            // Check for the Unicode flag
   IsUnicode = type & UNICODE_FLAG ? LTRUE : LFALSE;

   switch(type & LOADMOD_FLAG)
     {
     case 0x03:                             // .plm file
       bXPLfile = FALSE;
       while (LTRUE == rd_plm_ops(cs))
         ;
       break;

     default:
       pXCPT->Error(loadfileE, LoadFile);
     }

   close_down();

   } catch (LExcept &E)
     {
      // This seemed to have been working, and then broke.  Changing
      // it so it creates a new exception and throws it instead, fixed
      // the problem.  It also worked under debug mode but not release mode.
      //LExcept E2 = E;
      //close_down(f);
      //E2.AddLoadInfo(fname);
      // New note - setting optimization to default, rather than speed
      // also fixes the problem.  So now it works like before.

      E.AddLoadInfo(LoadFile);
      close_down();

      // And more, putting this kludge back in fixed the problem with
      // thrown errors during loading.  Who knows what evil lurks
      // in the heart of Microsoft optimization... (it works without
      // the kludge with speed optimization off)

      LExcept E2 = E;
      throw E2;
     }
     catch (...)
     {
      close_down();
      throw;
     }

   return(TRUE);
}


int LLoad::add_code(CODEptr  code_buffer)
{  // Add code to appropriate module, or execute if a latent expression.
   //PRED_BLKptr       pi;
   //CLAUSE_BLKptr     ci, ciprev;
   //CompiledPredicate *ci;
   //CompiledPredicate *ciprev;
   PATOM             goalatom;
   // bigdig HIDDEN_PROCptr    php;
   ARITY             arity;
   CODE_HEADERptr    head;
   TF                xB;

   // code_buffer is of the form code_header; code
   head = (CODE_HEADERptr) code_buffer;
   head->block = NULL;
   goalatom = (PATOM)head->pred_atom;

   if (goalatom == pATAB->latentA || goalatom == pATAB->latentodA)  
   {                                // If its a latent expression, execute now
      xB = pEXEC->GetPLMTraceB();
      pEXEC->SetPLMTraceB(FALSE);
      // we need to throw an error if it fails, because the failure
      // might be due to an unresolved call waiting for a fixup,
      // and the delete code_buffer pulls the rug out from under
      // fixup leading to a system failure.
      if (TRUE != pEXEC->NewProve(code_buffer))
         pXCPT->Error(latentE);
      pEXEC->SetPLMTraceB(xB);
      delete code_buffer;
   }                                              
   else                         
   {                                // otherwise add code to its module.
      pATAB->ValidGoal(goalatom);   // error if not valid
      arity = (ARITY) head->pred_arity;
#ifdef BUG_ADD
 DUMP << "Adding Code: " << MODPREDAR(head->mod_ix, goalatom, arity) << NL;
#endif
      pDDB->AddCompiledPredicate(head->mod_ix, goalatom, arity, code_buffer, 
                                 File_Ix);
   }

   return TRUE;

}

//  an object file is a series of records of the form
//
//     [ATOM-TABLE][CODE]
//
//  [ATOM-TABLE] is a series of records of the form
//
//     short char  char ..... null char ... null ..
//     |length||string1|string2|...
//

int  LLoad::rd_atom_table(std::istream *cs)
{                                          // build the atom table
   uintC     posn, length;                 // ray
   //int       atom_index;
   PATOM     a;

   STRptr    p, inbuff_end;
   short     ichar;
   STRptr   ci_atom;
//if (pSTATE->m_vba)
//std::cout << "VBA on for load atom table\n";
//else
//std::cout << "VBA not on yet for load atom table\n";

   length = Large ? fread_uint32(cs) : fread_uint16(cs);
#ifdef BUG_XPL
   DUMP << aS("rd_atom_table: length = ") << length << NL;
   if (IsUnicode)
   {
      int alen;                               // ray
      alen = length / 2;
      DUMP << aS("  wide adjusted length = ") << alen << NL << FLUSH;
   }
#endif
   // to catch the VMS bug
   if (length == 0)
      return -1;
   //   if (IsUnicode)                        // ray
   length = length / 2;                       // get length in characters

   //maxatoms = pATAB->GetMaxAtoms();

// bigdig   atom_index = (is_module) ? 1 : 0;
   //atom_index = 0;
//   atom_index = 1;
   v_global_atoms.clear();
   inbuff_end = loadNameBuf + pSTATE->m_buflen;
//   for(posn = 0; posn < length; atom_index++)
   for(posn = 0; posn < length; )
   {                                        // for all posn to length (4096)
      for(p = loadNameBuf; ; p++)
      {                                     // for all chars up to EOS
         if (p >= inbuff_end)
            pXCPT->Error(longatomS);
//       ichar = ISUNIGETC(f);   // ray
         ichar = fread_int16(cs);
         if (EOF == ichar)
            pXCPT->Error(ioE, aS("Reading Atom Table"));
         ++posn;                            // bump posn here, in case break
         *p = (aCHAR) ichar;                // fill loadNameBuf
         if(*p == EOS)
            break;                          // got atom
      }
      //wprintf(aS("atom  %s\n"),  loadNameBuf); //ray

      //if (atom_index >= maxatoms)
      //   pXCPT->Error(max_load_atomE, maxatoms, atom_index);

#ifdef BUG_XPL
      long filepos = (long)cs->tellg();
      //DUMP << aS("atom ") << atom_index << SP;
      DUMP << aS("name: ") << loadNameBuf << SP;
      DUMP << aS("file offset = ") << filepos << HEXOUTP(filepos);
      DUMP << NL << FLUSH;
#endif
      //pATAB->EnterAtom((STRptr) loadNameBuf, &pglobal_atoms[atom_index]);
      // Atoms are flagged with an initial 8 when they are created when the
      // VBA flag is on.  A kludge to get around the problem that loading
      // .xpl files it isn't known if the atoms are VBA or not until after
      // the fact.
      if (loadNameBuf[0] == 8)
      {
         ci_atom = (STRptr)loadNameBuf;
         ci_atom++;
         //std::cout << "Entering 8 Atom: " << ci_atom << "\n";
         a = pATAB->EnterCIAtom(ci_atom);
      }
      else
         a = pATAB->EnterAtom((STRptr) loadNameBuf);
      v_global_atoms.push_back(a);
   }                                      // end for posn
   return 0;
}

CODEptr LLoad::rd_xpl_pred(std::istream *cs)
{
   uintC          length, dest_length;
   CODEptr        p;
   int            atom_num;          // not a real atom, but the .xpl file num

   CODE_HEADERptr head;
   ARITY          arity;
   MODIX          mod_ix;
   int place;

#ifdef BUG_XPL
   long fileoffset = (long)cs->tellg();
   DUMP << aS("rd_xpl_pred: at offset ") << fileoffset;
   DUMP << HEXOUTP(fileoffset) << NL << FLUSH;
#endif

   //place = ftell(f);
   place = (int)cs->tellg();
   length = LongCode ? fread_uint32(cs) : fread_uint16(cs); // code length
   if (length == 0)
     return(NULL);                        // kludge to catch extra VMS zeroes
   atom_num = fread_int16(cs);             // functor
   arity = (ARITY)fread_uint16(cs);        // arity
   mod_ix = pDDB->getCurrentMIX();

   length -= 4;                           // less the two int16s just read
   if (length >= SrcBufLen)
   {
//      pXCPT->Error(longcodeS, SrcBufLen, length + 10);
      delete SourceBuf;
      SrcBufLen = length + 10;
#ifdef BUG_BUFS
 DUMP << "setting xpl SrcBufLen = " << SrcBufLen << NL;
#endif
      LNEW(SourceBuf, aBYTE[SrcBufLen + 1], aS("load"));
      // The destbuf is about the same size as the srcbuf,
      // although ones intCs and the other bytes, so a factor
      // of 2 should be big enough
      if (DestBufLen < 2 * SrcBufLen)
      {
         delete DestBuf;
         DestBufLen = 2 * SrcBufLen;
#ifdef BUG_BUFS
 DUMP << "setting xpl DestBufLen = " << DestBufLen << NL;
#endif
         LNEW(DestBuf, CODE[DestBufLen], aS("load"));
      }
   }
   //if (length != (int)fread(SourceBuf, 1, length, f))
   //   pXCPT->Error(bufmismatS);
   cs->read((char*)SourceBuf, length);
   EOFCHECK(cs);

#ifdef BUG_XPL
   DUMP << aS("  atom_num = ") << DECHEX(atom_num);
   DUMP << aS("  length = ") << DECHEX(length);
   DUMP << aS("  Loading ") << PREDAR( *(v_global_atoms[atom_num]), arity);
   DUMP << NL << FLUSH;
#endif
   // Get the header information into code
   head = (CODE_HEADERptr ) DestBuf;
   head->d_wam = (cdOP) Owho_am_i;
   head->file_ix = (cdSMINT) File_Ix;
   head->mod_ix = mod_ix;
   head->pred_atom = (cdATOM) v_global_atoms[atom_num];
   head->pred_arity = (cdSMINT) arity;

   // now we have code in src -- map it into dest
   // initialize label counters and go
   Lref = 0;
   Clnum = 0;

   p = DestBuf + (sizeof(CODE_HEADER)/sizeof(CODE));
   p = process_code_buf(p, length);

   resolve_labels();

   //dest_length = (p - DestBuf) * sizeof(CODE);
   //p = (CODEptr ) pDMEM->Alloc(dest_length, aS("Code"));
   dest_length = (uintC)(p - DestBuf);
#ifdef BUG_BUFS
   DUMP << "src " << length << " dest " << dest_length <<
      " ratio " << dest_length/length << NL;
#endif
   LNEW(p, CODE[dest_length], aS("load"));
   Lmemcpy(p, DestBuf, (size_t)(dest_length * sizeof(CODE)));

   fixfix(p);
#ifdef BUG_XPL
   DUMP << aS("  load address ") << p << NL;
   pEXEC->cdDebugCode(p, dest_length);
#endif
   return(p);
}

CODEptr LLoad::rd_plm_pred(std::istream *cs)
{
   //aUINT16        readlength;
   aUINT16        code_length, clause_type;
   uintC          max_src, total_src;

   ARITY         arity;
   MODIX         mod_ix;
   long          init_pos;
   CODEptr       p;
   intCH         atom_num;
   CODE_HEADERptr  head;
   int           dest_length;
   int           a_table_size;
   aBYTE         op;
//   STRptr        clause_name;
   int           clauses;

   //if (EOF == (c = getc(f)))
   //  return(NULL);
   //ungetc(c, f);
   if (!(*cs))
      return NULL;

   if(!loadNameBuf)                          // ray
     return NULL;

   if (rd_atom_table(cs))              // read atom table of 1st clause of proc
     return NULL;                            // check 00 for VMS bug

   init_pos = (long)cs->tellg();                      // So we can regroup

   // compute size of buffer to load all the clauses for the procedure
   code_length = fread_uint16(cs);
   max_src = total_src = code_length;
   clause_type = fread_uint16(cs);
   clauses = 0;
   while (clause_type != 1)                  // not last clause
     {
       // skip N/A, go to end of code segment
       //fseek(f, (long) (4 - PLM_CLHEAD_L + code_length), SEEK_CUR);
       cs->seekg( (long) (4 - PLM_CLHEAD_L + code_length), std::ios::cur);
                                              // skip next atom table
       a_table_size = fread_int16(cs);
       //fseek(f, (long) a_table_size, SEEK_CUR);
       cs->seekg( (long) a_table_size, std::ios::cur);
       code_length = fread_uint16(cs);
       total_src += code_length;
       if (code_length > max_src)
          max_src = code_length;
       clause_type = fread_uint16(cs);
       clauses++;                             // ray (debug)
       if (clauses > MaxClauses)              // mary prevent infinite loop
          pXCPT->Error(labelsE, clauses);
     }

   //fseek(f, init_pos, SEEK_SET);              // back to start of 1st clause
   cs->seekg( init_pos, std::ios::beg);              // back to start of 1st clause

   code_length = fread_uint16(cs);             // get code size for 1st clause
   clause_type = fread_uint16(cs);

   atom_num = fread_int16(cs);

   arity = (ARITY) fread_uint16(cs);
   mod_ix = pDDB->getCurrentMIX();
                                                // now we are at the code  !!
   // have to adjust start?
   //fread(&op, 1, 1, f);                         // to first label of switch
   cs->read((char*)&op, 1);                       // to first label of switch
   if (op == Ono_switch)                        // adjust past switch
     {
       //fseek(f, 6L, SEEK_CUR);                  // skip the remaining labels
       cs->seekg( 6L, std::ios::cur);                // skip the remaining labels
       code_length -= 7;
                                                // now look at try_me_else
       //fread(&op, 1, 1, f);
       cs->read((char*)&op, 1);
       if (op == Ono_try)
         {
           code_length -= 5;
           //fseek(f, 4L, SEEK_CUR);              // skip label & NTV
           cs->seekg( 4L, std::ios::cur);            // skip label & NTV
         }
       else
         //fseek(f, -1L, SEEK_CUR);               // get back to the try_me
         cs->seekg( -1L, std::ios::cur);           // get back to the try_me
     }
   else
     //fseek(f, -1L, SEEK_CUR);                   // get back to first op code
     cs->seekg( -1L, std::ios::cur);               // get back to first op code

   if (max_src >= SrcBufLen)
   {
//     pXCPT->Error(longcodeS, SrcBufLen, code_length + 10);
      delete SourceBuf;
      SrcBufLen = max_src + 10;
#ifdef BUG_BUFS
 DUMP << "setting SrcBufLen = " << SrcBufLen << NL;
#endif
      LNEW(SourceBuf, aBYTE[SrcBufLen + 1], aS("load"));
   }
   // The destbuf is about the same size as the srcbuf,
   // although ones intCs and the other bytes, so a factor
   // of 2 should be big enough
   if (DestBufLen < 2 * total_src)
   {
      delete DestBuf;
      DestBufLen = 2 * total_src;
#ifdef BUG_BUFS
DUMP << "setting DestBufLen = " << DestBufLen << NL;
#endif
      LNEW(DestBuf, CODE[DestBufLen], aS("load"));
   }

    //  now at beginning of valid code.
    //  code_length and code_size are adjusted accordingly.
    //  atom_num and arity all set

                                                // Get header info into code
   head = (CODE_HEADERptr ) DestBuf;
   head->d_wam = (cdOP) Owho_am_i;
   head->file_ix = (cdSMINT) File_Ix;

   head->pred_atom = (cdATOM)v_global_atoms[atom_num];
   head->pred_arity = (cdSMINT) arity;
   head->mod_ix = mod_ix;

   code_length -= PLM_CLHEAD_L;                 // Actual code length

#ifdef BUG_PLM
   DUMP << aS("  atom_num = ") << DECHEX(atom_num);
   DUMP << aS("  length = ") << DECHEX(code_length);
   DUMP << aS("  Loading ") << PREDAR( *(v_global_atoms[atom_num]), arity);
   DUMP << NL << FLUSH;
#endif

   Lref = 0;                              // Init label cntrs and process code
   Clnum = 0;

   p = DestBuf + (sizeof(CODE_HEADER) / sizeof(CODE));
   clauses = 0;
   while (1)
     {                                          // read all the clauses
       //readlength = (int)fread(SourceBuf, 1, code_length, f);
       cs->read((char*)SourceBuf, code_length);
       EOFCHECK(cs);
       //if (code_length != readlength)
       //  pXCPT->Error(bufmismatS);
       p = process_code_buf(p, code_length);
       if (clause_type == 1)       // last clause
         break;                                 // all done
                                                // Else read the next clause in
       rd_atom_table(cs);                        // first its atom table
       code_length  = fread_uint16(cs);
       code_length -= PLM_CLHEAD_L;
       clause_type  = fread_uint16(cs);
       //fseek(f, 4L, SEEK_CUR);                  // skip functor and arity 
       cs->seekg( 4L, std::ios::cur);                  // skip functor and arity 
       clauses++;                             // mary
       if (clauses > MaxClauses)              // mary prevent infinite loop
          pXCPT->Error(labelsE, clauses);
     }

   resolve_labels();
   
   //dest_length = (p - DestBuf) * sizeof(CODE);  // 2
   // p and DestBuf are 2 byte types so diff was halved and had to be doubled

   //p = (CODEptr) pDMEM->Alloc(dest_length, aS("Code"));

   dest_length = (int)(p - DestBuf);

   LNEW(p, CODE[dest_length], aS("load"));
   //aCHAR *note_buffer;
   //note_buffer = new aCHAR[100];
   //Lsprintf(note_buffer, 99, aS("loading %ls"), *(head->pred_atom));
   //LNEW(p, CODE[dest_length], note_buffer);

   Lmemcpy(p, DestBuf, (size_t)(dest_length*sizeof(CODE)));
   fixfix(p);
#ifdef BUG_PLM
   DUMP << aS("  load address ") << p << NL;
   pEXEC->cdDebugCode(p, dest_length);
#endif

   return(p);
}

LBOOL LLoad::rd_plm_ops(std::istream *cs)
{
   //aUINT16       readlength;
   aUINT16       code_length, clause_type;
   ARITY         arity;
   long          init_pos;
   CODEptr       p;
   intCH         atom_num;
   CODE_HEADERptr head;
   //int            dest_length;
   int           a_table_size;
   aBYTE         op;
   LBOOL         is_opdef;

   //if (EOF == (c = getc(f))) 
   //  return(LFALSE);
   //ungetc(c, f);
   if (!(*cs)) return LFALSE;
   
   if (rd_atom_table(cs))            // read atom table of first clause of proc 
     return LFALSE;                          // check 00 for VMS bug 
   
   init_pos = (long)cs->tellg();                      // So we can regroup
   code_length = fread_uint16(cs);  // buffer size required to load all clauses 
   clause_type = fread_uint16(cs);

   while (clause_type != 1)
     {                                       // while not last clause
                             // skip name/arity and get to end of code segment 
       //fseek(f, (long) (4 - PLM_CLHEAD_L + code_length), SEEK_CUR);
       cs->seekg( (long) (4 - PLM_CLHEAD_L + code_length), std::ios::cur);
       a_table_size = fread_int16(cs);        // skip next atom table
       //fseek(f, (long) a_table_size, SEEK_CUR);
       cs->seekg( (long) a_table_size, std::ios::cur);
       code_length = fread_uint16(cs);
       clause_type = fread_uint16(cs);
     }
                                  
   //fseek(f, init_pos, SEEK_SET);   // get back to the beginning of first clause
   cs->seekg( init_pos, std::ios::beg);   // get back to the beginning of first clause
   code_length = fread_uint16(cs);            // get code size for 1st clause 
   clause_type = fread_uint16(cs);
   atom_num = fread_int16(cs);
   arity = (ARITY) fread_uint16(cs);          // now we are at the code  !! 
                                 // See if we have to adjust beginning of code 
   //fread(&op, 1, 1, f);                      // get to first label of switch 
   cs->read((char*)&op, 1);                      // get to first label of switch
   if (op == Ono_switch)                     // adjust past switch 
     {
       //fseek(f, 6L, SEEK_CUR);               // skip the remaining labels 
       cs->seekg( 6L, std::ios::cur);               // skip the remaining labels
       code_length -= 7;       
                                             // now look at try_me_else 
       //fread(&op, 1, 1, f);
       cs->read((char*)&op, 1);
       EOFCHECK(cs);
       if (op == Ono_try)
         {
           code_length -= 5;
           //fseek(f, 4L, SEEK_CUR);           // skip label & NTV 
           cs->seekg( 4L, std::ios::cur);           // skip label & NTV
         } 
       else
         //fseek(f, -1L, SEEK_CUR);            // get back to the try_me 
         cs->seekg( -1L, std::ios::cur);            // get back to the try_me
     } 
   else
     //fseek(f, -1L, SEEK_CUR);                // get back to first op code  
     cs->seekg(-1, std::ios::cur);                // get back to first op code
 
   //   we are at the beginning of valid code, code_length and code_size
   //   are adjusted accordingly. atom_num and arity all set
   // Get the header information into code 

   head = (CODE_HEADERptr ) DestBuf;
   head->d_wam = (cdOP) Owho_am_i;
   head->file_ix = (cdSMINT) File_Ix;
   head->pred_atom =  (cdATOM)v_global_atoms[atom_num];

   head->pred_arity = (cdSMINT) arity;

   is_opdef = head->pred_atom == (cdATOM)pATAB->latentodA ? LTRUE : LFALSE;

   code_length -= PLM_CLHEAD_L;              // Actual code length 

#ifdef BUG_PLM
   DUMP << aS("  atom_num = ") << DECHEX(atom_num);
   DUMP << aS("  length = ") << DECHEX(code_length);
   DUMP << aS("  Loading ") << PREDAR( *(v_global_atoms[atom_num]), arity);
   DUMP << NL << FLUSH;
#endif
  // map code in src to dest 
  // read the clauses 

   if (code_length >= SrcBufLen)
   {
//      pXCPT->Error(longcodeS, SrcBufLen, code_length + 10);
      delete SourceBuf;
      SrcBufLen = code_length + 10;
      LNEW(SourceBuf, aBYTE[SrcBufLen + 1], aS("load")); 
      // The destbuf is about the same size as the srcbuf,
      // although ones intCs and the other bytes, so a factor
      // of 2 should be big enough
      if (DestBufLen < 2 * SrcBufLen)
      {
         delete DestBuf;
         DestBufLen = 2 * SrcBufLen;
         LNEW(DestBuf, CODE[DestBufLen], aS("load"));
      }
   }
                                             // Init label cntrs & process code
   Lref  = 0;

   Clnum = 0;

   p = DestBuf + (sizeof(CODE_HEADER) / sizeof(CODE));
   while (1)
     {
       //readlength = (int)fread(SourceBuf, 1, code_length, f);
       //if (code_length != readlength)
       //  pXCPT->Error(bufmismatS);
       cs->read((char*)SourceBuf, code_length);
       EOFCHECK(cs);
       if (is_opdef)
         p = process_code_buf(p, code_length);
       if (clause_type == 1)                 // last clause 
         break;                              // all done 
       
       rd_atom_table(cs); // Else read the next clause in - first its atom table
       code_length = fread_uint16(cs);
       code_length -= PLM_CLHEAD_L;
       clause_type = fread_uint16(cs);
       //fseek(f, 4L, SEEK_CUR);               // skip functor and arity
       cs->seekg(4, std::ios::cur);                     // skip functor and arity
     }

   if (is_opdef)
      resolve_labels();

   if (is_opdef)
      fixfix(p);
#ifdef xBUG_PLM
   cdDebugCode(p, dest_length / sizeof(CODE));
#endif

   if (is_opdef) 
     {                                       // Execute latent expressions
       TF xB = pEXEC->GetPLMTraceB();
       pEXEC->SetPLMTraceB(FALSE);
       pEXEC->NewProve(DestBuf);
       pEXEC->SetPLMTraceB(xB);
     }

   return LTRUE;
}

void LLoad::resolve_labels(void)
/* Resolve all the references to clauses to jump to here and there.  If the
   first op code was a switch, then the first three references are from
   that switch.  In that case, it all gets a bit confusing for positive
   labels.  A switch reference to a positive label must go to the the try
   at the beginning of the clause, rather than after it as the other 
   references do.  This means subtracting the length of a try_me_else for
   the first clause, or a retry or trust for the others.
      Actually, maybe its only the first one that needs the new offset, because
   the others will only be positive if there is only one clause so you don't
   want to set up choice points. */
{
   intC i;

   long  i32;
   cdSINT label;

#ifdef BUG_LABELS
   if (Lref > 0)
    pDUMP->Write(LString(aS("  i    labref     label   [labelp]   offset\n")));
/*                 ....-....|....-....|....-....|....-....|....-....|  */
#endif
   for (i = 0; i < Lref; i++)
     {
      label = *(cdSINT *) Clabrefs[i];

#ifdef BUG_LABELS
      pDUMP->Write(LString(aS("%5d  %8p  %8d"), 40, i, Clabrefs[i],  label));
      //          errDebugMsg("%5d  %8p  %8d", i, Clabrefs[i], label);
      if (label < 0) 
   //   errDebugMsg("  %8p", Clabels[-label]);
        pDUMP->Write(LString(aS("  %8p"), 40,  Clabels[-label]));
      else 
        pDUMP->Write(LString(aS("          ")));

#endif
      if (label < 0)
        {
          i32 = (long)(Clabels[-label] - Clabrefs[i]);
          //          if (i32 >= 0x7fff || i32 <= -0x7fff)
          //            pXCPT->Error(labjumpE);
          * (cdSINTptr) Clabrefs[i] = i32;
        }

      else if (label > 0)
        {
          i32 = (long)(Ccllabs[label] - Clabrefs[i]);
          //          if (i32 >= 0x7fff || i32 <= -0x7fff)
          //            pXCPT->Error(labjumpE);
          * (cdSINTptr) Clabrefs[i] =  i32;
          if (SwitchB && i < 3 && label == 1)
            * (cdSINTptr) Clabrefs[i] -= (cdOP_L + 2 * (cdSINT_L));
        } 

#ifdef BUG_LABELS
      pDUMP->Write(LString(aS("  %8d\n"), 40,  * (cdSINTptr) Clabrefs[i]));
#endif
   }
}

TERM LLoad::read_xi(aBYTEptr &sp)
{                           // read Xi index from f, set TERM at p to &X[i] 
   int xi = GET_INT16(sp);
   sp += 2;
   // maxvars is hardwired to 0x1000, because the last XVar is
   // used as a temp, so the compiler hardwires 0xfff as XI for temps.
   // so an index of 0xfff is the max, compiler checks for too many
   // vars, so don't really need to here.
   if (xi >= pHXL->GetMaxVars())
      pXCPT->Error(manyvarS);
   else
      return((TERM) pHXL->XVar(xi));

   return(0);                          // a formality 
}

ARITY LLoad::read_arity(aBYTEptr &sp)
{                           // read Xi index from f, set TERM at p to &X[i] 
   int a = GET_INT16(sp);
   sp += 2;
   return a;
}

cdSMINT LLoad::read_yi(aBYTEptr &sp)
{                           // read Xi index from f, set TERM at p to &X[i] 
   int yi = GET_INT16(sp);
   sp += 2;
   return yi;
}

inline long getLongEndian(aBYTEptr p)
{
  return (long) ( ((aINT16) *(p+3) << 24) | 
                  ((aINT16) *(p+2) << 16) |
                  ((aINT16) *(p+1) << 8)  |  
                  (aINT16)  *(p  ) );
}

Cell LLoad::read_const(aBYTEptr &p)
  /*
   **  one byte of tag followed by the value
   **
   **         tag = 1  a short for atoms (2 bytes)
   **         tag = 2  a short for ints (2 bytes)
   **         tag = 3  arbitrary number of single bytes 
   **                  terminated with '\0' for sb string  
   **         tag = 4  a single precision (4 byte) float
   **         tag = 5  a long for ints (4 bytes)
   **         tag = 6  EOS terminated Unicode string
   **         tag = 7  a double precision (8 byte) float
   **         tag = 8  a real
   **         tag = 9  a fixed
   */
{
  aBYTE   tag;
  STRptr  ps;
  intCH   a;
  intCH   i;
  Cell    x;
  //STRptr  pstr;
  union
  {
	 fixedC f;
    long   li;
    double dbl;
    Real   r;
    float  sngl;
  } num;
  char *nump;
  
  tag = *p++;
  
  if (tag == 1)
    {
      a =  GET_INT16(p);
      x.setAtom( v_global_atoms[a]);
      p += 2;
    }
  else if (tag == 2)
    {
      i = GET_INT16(p);
      //pTSVC->putINT(&x, i);
      x.setInt(i);
      p += 2;
    }
  else if (tag == 3)                         // Single-byte char string 
    {                     // A single byte char string -> internal wide string.
      ps = loadNameBuf;
      *ps = (aCHAR)*p++;                     // does the cast work right?
      while( *ps++)
        {
          *ps = (aCHAR)*p++;
          if (ps - loadNameBuf > pSTATE->m_buflen)
            pXCPT->Error(rdbufE);
        }

      //pPSTR->strEnterString((STRptr) loadNameBuf, &pstr);
      //x.setStr( pstr);
      x.setStr(pGCTH->make_string(loadNameBuf));
      x.gc_protect_plm(File_Ix);
      // bigdig not needed? pTSVC->putTHSAVE(&x);       // save thing from gc 
    }
  else if (tag == 4)                         // four byte float 
    {
      //num.li = (long) ( ((aINT16) *(p+3) << 24) | 
      //   ((aINT16) *(p+2) << 16) |
      //   ((aINT16) *(p+1) << 8)  |  
      //   (aINT16) *(p  ) );
     num.li = getLongEndian(p);

      //pTSVC->putFLOAT(&x, num.li);
      //x.setDouble(pGCTH->make_float(num.li));
      x.setSingle(num.sngl);
      //x.gc_protect();
      // bigdig pTSVC->putDBLSAVE(&x);          // save thing from gc (ray) 
      p += 4;
    }
  else if (tag == 5)                         // four byte long integer 
    {
      num.li = getLongEndian(p);

      //pTSVC->putXINT(&x, num.li);
      x.setInt(num.li);
      // bigdig not needed? pTSVC->putTHSAVE(&x);       // save thing from gc 
      p += 4;
    }
  else if (tag == 6)                         // Unicode string 
    {
      ps = loadNameBuf;
      *ps = (aCHAR) GET_INT16(p);
      p += 2;
      while( *ps++)
        {
          *ps = (aCHAR) GET_INT16(p);
          p += 2;
          if (ps - loadNameBuf > pSTATE->m_buflen)
            pXCPT->Error(rdbufE);
        }

      //pPSTR->strEnterString((STRptr) loadNameBuf, &pstr);
      //x.setStr( pstr);
      x.setStr(pGCTH->make_string(loadNameBuf));
      x.gc_protect_plm(File_Ix);
    }
  else if (tag == 7)
    {
      nump = (char *)&num;
#if defined(BIG__ENDIAN) 
      *nump++ = *(p+7);
      *nump++ = *(p+6);
      *nump++ = *(p+5);
      *nump++ = *(p+4);
      *nump++ = *(p+3);
      *nump++ = *(p+2);
      *nump++ = *(p+1);
      *nump++ = *(p);
#else
      *nump++ = *(p);
      *nump++ = *(p+1);
      *nump++ = *(p+2);
      *nump++ = *(p+3);
      *nump++ = *(p+4);
      *nump++ = *(p+5);
      *nump++ = *(p+6);
      *nump++ = *(p+7);
#endif
      //pTSVC->putFLOAT(&x, num.fl);
      x.setDouble(pGCTH->make_float(num.dbl));
      x.gc_protect_plm(File_Ix);
      p += 8;
    }
  else if (tag == 8)         // Real
    {
      aCHAR length = GET_INT16(p);           
      p += 2;
      aCHAR exp    = GET_INT16(p); 
      p += 2;
      aCHAR sign   = GET_INT16(p); 
      p += 2;    
//std::cout << "load real sign = " << sign <<
// ", exp = " << exp << ", length = " << length << NL;
      //num.r = newReal(length, exp, sign);
      LNEW(num.r, LReal(length, (char)sign, exp), aS("read_const"));
      //long *q = (long *)num.r + 1;
      int iq = length-1;
      while(iq >= 0)
        {
          //*q++ = getLongEndian(p);
          (*(num.r))[iq--] = getLongEndian(p);
          p += 4;
        }
//num.r->Dump();
      x.setReal(pGCTH->make_real(num.r));
      delete num.r;  // make_real made a copy
      x.gc_protect_plm(File_Ix);
    }
  else if (tag == 9)                         // fixed
    {
      //num.f.hi =  getLongEndian(p);
      num.f.setFixedHI(getLongEndian(p));
		p += 4;
      //num.f.lo =  getLongEndian(p);
      num.f.setFixedLO(getLongEndian(p));
		p += 4;
      x.setFixed(num.f);
	 }
  else
    pXCPT->Error(badtagS);
  
  return(x);
}

void LLoad::read_xpl_filenames(std::istream *cs)
{
   aCHAR *p;

   p = loadNameBuf;
   *p = fread_int16(cs);  // start the first one
   if (cs->eof())
      return;
   while(*p)
   {
      while(*p++)
      {
         *p = fread_int16(cs);
         if (p - loadNameBuf > pSTATE->m_buflen)
            pXCPT->Error(rdbufE);
      }
      v_short_fnames.push_back(LPathString(loadNameBuf));
      p = loadNameBuf;
      *p = fread_int16(cs);  // see if there's another one
   }

#ifdef LANDFILL
   // just for testing in debugger, to see what we got
   for (size_t i = 0; i < v_short_fnames.size(); i++)
   {
      Lstrcpy(loadNameBuf, (aCHAR*)v_short_fnames[i]);
      continue;
   }
#endif
}

/*
 *** fixup works as follows -- if we have a call or exe we look to
 *** see if code exists for it. If so we change it to call_local or
 *** exe_local and replace the functor by the appropriate pointer.
 *** otherwise we leave as is but chain a fixup structure recording
 *** the fact. When the whole file is loaded in we run down the fixup
 *** chain attempting to fixup again
 */
void LLoad::fixup(CODEptr pcode, TF chain)
{
   //CODEptr             fix_code;
   FIXUPptr            fu;
   PATOM               nameA;
   ARITY               arity;
   MODIX               imod;
   CompiledPredicate  *cp;
   PredicateHead      *pph;

   imod  = (MODIX) * (cdMODIXptr) (pcode + cdOP_L);
   nameA = (PATOM) * (cdATOMptr)  (pcode + cdOP_L + cdMODIX_L);
   arity = (ARITY) * (cdSMINTptr) (pcode + cdOP_L + cdMODIX_L + 2 * cdATOM_L);

#ifdef BUG_FIXUP
 DUMP << "fixup: offset " << (long) (pcode - DestBuf);
 DUMP << SP << ops[(int) * (cdOPptr) pcode];
 DUMP << SP << MODPREDAR(imod, nameA, arity);
 DUMP << " chain " << (int) chain << NL;
#endif

   if (pATAB->callA == nameA)
      return;
   if (pATAB->call_nometaA == nameA)
      return;

   pph = pDDB->getVisiblePredicate(imod, nameA, arity);

   // If its compiled, it can be linked directly, but beware 
	// of meta predicates.  If they're system or user ones,
   // then they must be taken care of in the compiler.  
   // Otherwise, we can't be sure.
   if (pph && pph->IsCompiled() && 
      //(! pph->IsMeta() || imod == SYSTEM_MODULE || imod == USER_MODULE) )
      (! pph->IsMeta() || imod == USER_MODULE) )
   {                                            // code exists
#ifdef BUG_FIXUP
 DUMP << "  fixed" << NL;
#endif
      cp = pph->getCompiledPredicate();
      * (CODEhnd ) (pcode + cdOP_L) = cp->getCode();
      if (Ocall == * (cdOPptr) pcode || Omod_call == * (cdOPptr) pcode)
         * (cdOPptr) pcode = Ocall_direct;
      else
         * (cdOPptr) pcode = Oexec_direct;

      return;
   }
   
   if (!pph && chain) 
   {                                           // no fixup info exists 
#ifdef BUG_FIXUP
 DUMP << "  chained" << NL;
#endif
      //fu = (FIXUPptr) pDMEM->Alloc(sizeof(FIXUP), aS("Fixup"));
      LNEW(fu, FIXUP(), aS("load"));
      if (FixupHead)
         FixupTop -> link = fu;
      else
         FixupHead = fu;

      fu -> link = NULL;
      FixupTop = fu;

      // This is the start of the compiled predicate, or base for fixup.
      // Until code is copied to its final destination we don't know which,
      // so fixfix(), called after predicate is loaded, fills in the fbases.
      fu -> fbase = NULL;
      fu -> foff = (intC)(pcode - DestBuf);
   }
}

void LLoad::fixfix(CODEptr pbase)
{ // The code has been copied and the base pointers in the fixup chain
  // need to be set accordingly
   for(FIXUPptr fu = FixupHead; fu; fu = fu->link)
     if (fu->fbase == NULL)
       fu->fbase = pbase;
}

void LLoad::FixAll()
{
  FIXUPptr     fu;
  CODEptr      pcode;
  
#ifdef BUG_FIXUP
  FILL("FixAll: here's the chain:");
  FIXUPptr fup = FixupHead;
  while(fup)
    {
      DUMP << "base: " << fup -> fbase << "  offset: " << fup -> foff;
      fup = fup->link;
    }
#endif
  
  for(fu = FixupHead; fu; fu = fu->link)
    {
      pcode = fu->fbase + fu->foff;
      fixup(pcode, FALSE);
    }
  while (FixupHead)
    {
      fu = FixupHead;
      FixupHead = (FixupHead) -> link;
      delete fu;
    }
}

int LLoad::fread_int16(std::istream *cs)
{                       // read a 2-byte integer -- if you can't then blow up
  aINT16 i;

#ifdef BUG_FREAD
   long foffset = cs->tellg();
#endif

   //   if (0 == fread((aBYTE *) &i, 2, 1, f))
   //if (0 == fread( &i, 2, 1, f))
   //   pXCPT->Error(eofE);
   cs->read((char*)&i, 2);
   EOFCHECK(cs);
   RevEndian16(i);
#ifdef BUG_FREAD
   pDUMP->Write(LString(aS("fread_int16 read %04X at offset %ld (%lx)\n"),
                        120, i, foffset, foffset));
#endif
   return((int) i);
}

unsigned int LLoad::fread_uint32(std::istream *cs)
{                       // read a 4-byte integer -- if you can't then blow up
  int i;
#ifdef BUG_FREAD
   long foffset = cs->tellg();
#endif

   //   if (0 == fread((aBYTE *) &i, 2, 1, f))
   //if (0 == fread( &i, 4, 1, f))
   //   pXCPT->Error(eofE);
   cs->read((char*) &i, 4);
   EOFCHECK(cs);
   RevEndian32(i);
#ifdef BUG_FREAD
   pDUMP->Write(LString(aS("fread_int32 read %04X at offset %ld (%lx)\n"),
                        120, i, foffset, foffset));
#endif
   return i;
}

unsigned int LLoad::fread_uint16(std::istream *cs)
{                       // read a 2-byte integer -- if you can't then blow up 
  aUINT16 i;                                     // ray
#ifdef BUG_FREAD
   long foffset = cs->tellg();
#endif
   
   //   if (0 == fread((aBYTE *) &i, 2, 1, f))
   //if (0 == fread( &i, 2, 1, f))
   //   pXCPT->Error(eofE);
   cs->read((char*) &i, 2);
   EOFCHECK(cs);
   RevEndian16(i);
#ifdef BUG_FREAD
   pDUMP->Write(LString(aS("fread_uint16 read %04X at offset %ld (%lx)\n"), 
         120, i, foffset, foffset));
#endif
   return((unsigned int) i);
}

CODEptr LLoad::process_code_buf(CODEptr p, int src_length)

{
   aINT32     xx;
   //aBYTE      c;
   aBYTE      *sp;

   CODEptr    fixp = NULL;
   cdOP       op, prevop;
   PATOM      pred, amod;
   intCH      atom_num;
   MODIX      imod;
   ARITY      ar;
   int        i;
   cdSMINT    yi;
   //CLAUSE_BLKptr ci;
   //STRptr     latom_name;
   PredicateHead *pph;

   imod = USER_MODULE;

   sp = SourceBuf;
   if ( Clnum == 0 )  // first code a switch? 
      SwitchB = (cdOP)*sp == Oswitch_on_term ? TRUE : FALSE;
  
   op = 0;
   while( sp < SourceBuf + src_length)
   {
      prevop = op;
      op = (cdOP) *sp++;                       // get next op 
      * (cdOPptr) p = op;                      // to dest
      p += cdOP_L;

      switch(op)
      {    
      case Ono_op:                                // no args 
      case Ofail:
      case Oproceed:
      case Odealloc:
      case Ocut:
      case Ocut64:
      case Otrust_me_else:
      case Ou_var_getlist:
      case Ounify_nil:
         break;
          
      case Oget_nil:                             // Xi 
      case Oget_list:
      case Oput_nil:
      case Oput_list:
      case Ounify_x_var:
      case Ounify_x_val:
         * (TERMptr) p = read_xi(sp);
         p += PTR_L;
         break;
          
      case Oget_x_var:                           // Xi, Xj 
      case Oget_x_val:
      case Oput_x_var:
      case Oput_x_val:
         * (TERMptr) p = read_xi(sp);
         p += PTR_L;
         * (TERMptr) p = read_xi(sp);
         p += PTR_L;
         break;

      case Ounify_y_var:                         // Yi 
      case Ounify_y_val:
         //* (cdSMINTptr) p = (cdSMINT) (*sp++);
         * (cdSMINTptr) p = (cdSMINT) read_yi(sp);
         p += cdSMINT_L;
         break;         

      case Ounify_unsafe:
         //*(cdSMINTptr)p = 
         //      * sp & Y_REG ? (cdSMINT) ((*sp++) | Y_REG) : (cdSMINT) *sp++;
         yi = read_yi(sp);
         *(cdSMINTptr)p = 
               yi & Y_REG ? (yi | Y_REG) : yi;
         p += cdSMINT_L;
         break;

      case Oget_y_var:                           // Yi, Xj 
      case Oget_y_val:
      case Oput_y_var:
      case Oput_y_val:
      case Oput_unsafe:
         //* (cdSMINTptr) p = *sp++;
         * (cdSMINTptr) p = read_yi(sp);
         p += cdSMINT_L;
         * (TERMptr) p = read_xi(sp);
         p += PTR_L;
         break;

      case Oretry:                               // short int 
      case Otrust:
      case Ocutd:
      case Ogoto:
         if (Lref >= MaxClauses) 
            pXCPT->Error(labelsE, Lref); 
         Clabrefs[Lref++] = p;                    // no break
      case Ounify_void:
         * (cdSINTptr) p = GET_INT16(sp);
         p += cdSINT_L;
         sp += 2;
         break;

      case Oalloc:
         * (cdSINTptr) p =  GET_INT16(sp);
         p += cdSINT_L;
         sp += 2;
         break;
          
      // Labels are used in two different ways, depending on whether the 
      // code came from a file or is in memory for execution by pcode.
      // The labels in code files are the generated labels of the 
      // compiler for the switch statements to use at the end of a code 
      // block.  These are not preserved for internal code, but are used
      // to fill in the pointer arrays for resolving references.

      // The cascading sequence of try_me_elses refers to the next 
      // try_me_else, and these references are computed as well in the 
      // arrays.  Note that the pointers to clauses here are at the 
      // beginning of the try_ statements, as opposed to the switch 
      // labels which are at the code following the try_.

      // For each try, a new label op is generated for pcode to examine 
      // to learn what clause number is being executed. 
           
      case Olabel:
         xx = - GET_INT16(sp);                  // get the label number (-ve)
         if (xx >= MaxClauses || xx < 0)
            pXCPT->Error(labelsE, xx);
         sp += 2;
         p -= cdOP_L;                      // back up p, we don't save labels 
         Clabels[xx] = p;                  // and set array to this address 
         break;
          
      case Oretry_me_else:
      case Otrust_me_2_else:
         xx = GET_INT16(sp);
         sp += 2;
         if (xx >= 0)                         // in the clauses 
         {
            Clnum++;
            *(cdSINTptr) Elseptr =  
                  (cdSINT)(p - Elseptr - cdOP_L);       // link to prev else
            Elseptr = p;                      // save this else for next 
            * (cdSINTptr) p = 0;              // will be filled in later
            p += cdSINT_L;
            if (Clnum >= MaxClauses)
               pXCPT->Error(labelsE, Clnum);
            Ccllabs[Clnum] = p;              // for switch refs to this clause 
            if (xx > 0 || op == Otrust_me_2_else)
            {
               * (cdOPptr) p = Olabel;    // add clause label for plm LANDFILL
               p += cdOP_L;
               * (cdSINTptr) p = Clnum;
               p += cdSINT_L;
            }
         }
         else                                  // in the branch table 
         {
            if (Lref >= MaxClauses)
               pXCPT->Error(labelsE, Lref);
            Clabrefs[Lref++] = p;
            * (cdSINTptr) p = xx;
            p += cdSINT_L;
         }
         break;

      case Otry_me_or_else:
      case Otry_me_else:
         xx = GET_INT16(sp);
         sp += 2;
         if (xx > 0)                          // start of the clauses 
         {
            Clnum++;
            Elseptr = p;
            * (cdSINTptr) p = 0;              // will get fixed at next clause 
            p += cdSINT_L; 
            * (cdSINTptr) p = GET_INT16(sp);
            p += cdSINT_L;
            sp += 2;
            if (Clnum >= MaxClauses)
               pXCPT->Error(labelsE, Clnum); 
            Ccllabs[Clnum] = p;      // for switch references to this clause 
            *(cdOPptr) p = Olabel;   // add a clause label for plm LANDFILL
            p += cdOP_L;
            * (cdSINTptr) p = Clnum;
            p += cdSINT_L;
         }
         else                                // in the branch table 
         {
            if (Lref >= MaxClauses)
               pXCPT->Error(labelsE, Lref);
            Clabrefs[Lref++] = p;
            * (cdSINTptr) p = xx;            // will get fixed later 
            p += cdSINT_L;
            * (cdSINTptr) p = GET_INT16(sp);
            p += cdSINT_L;
            sp += 2;
         }
         break;
          
      case Otry:
         if (Lref >= MaxClauses)
            pXCPT->Error(labelsE, Lref);
         Clabrefs[Lref++] = p;
         * (cdSINTptr) p = GET_INT16(sp);
         p += cdSINT_L;
         sp += 2;
         * (cdSINTptr) p = GET_INT16(sp);
         p += cdSINT_L;
         sp += 2;
         break;

      case Oget_con:                        // Constant, Xi 
      case Oput_con:
         * (TERM) p = read_const(sp);
         p += CELL_L;
         * (TERMptr) p = read_xi(sp);
         p += PTR_L;
         break;
          
      case Oexec:
      case Ocall:
      case Omod_exec:
      case Omod_call:
         // save the pointer to the op, which is used by fixup,
         // if necessary, to resolve call/exec later.
         fixp = p - cdOP_L;

         // get module first
         if (op == Omod_call || op == Omod_exec)
         {
            amod = v_global_atoms[(intCH) GET_INT16(sp)];
            imod = pDDB->getModuleIX(amod);
            if (imod == UNDEFINED_MODULE)
//               imod = pDDB->NewModule(*amod);
               imod = pDDB->NewModule(amod);
//               imod = pDDB->NewModule(amod->get_display_name(m_peng));
         }
         else                               // use current module
            imod = UNSPECIFIED_MODULE;

         sp += 2;
         atom_num = (intCH) GET_INT16(sp);
         sp += 2;
         pred = v_global_atoms[atom_num];
         //ar = (ARITY)*(sp);                 // arity 
         //sp += 1;
         ar = read_arity(sp);

         if (imod == UNSPECIFIED_MODULE)
            imod = pDDB->getCurrentMIX();

         * (cdMODIXptr) p = (cdMODIX) imod;
         p += cdMODIX_L;
         * (cdATOMptr) p = (cdATOM) pred;
         p += cdATOM_L;

#ifdef BUG_CALL
 DUMP << "loading: " << ops[op] << SP << MODPREDAR(imod,pred,ar) << NL;
#endif

         // if its either a builtin or extended predicate, set up
         // the escape sequence.
         pph = pDDB->getVisiblePredicate(imod, pred, ar);
         if (pph && pph->IsEscape())
//         if (pDDB->IsEscapePred(imod, pred, ar))
         {
            // If this is a builtin/system predicate, then the
            // call/exec is replaced with an escape instead, cleaning
            // up proceeds and deallocs as appropriate for escape.
#ifdef xBUG_CALL
DUMP << aS("Exec->Escape: prevop = ") << ops[prevop] << NL << FLUSH;
#endif
#ifdef BUG_CALL
 DUMP << "   loading escape: " << MODPREDAR(imod,pred,ar) << NL;
#endif
            if (prevop == Odealloc) 
               fixp -= cdOP_L;
            p = fixp;
            *(cdOPptr)p = (cdOP)Oescape;
            p += cdOP_L;

//            *(cdESCAPEptr)p = pDDB->getEscapePredicate(imod, pred, ar);
            *(cdESCAPEptr)p = pph->getEscapePredicate();
            p += cdESCAPE_L;

            if (prevop == Odealloc)
            {
               *(cdOPptr)p = (cdOP)Odealloc;
               p += cdOP_L;
            }
            //sp += (1);  // other atomnum plus arity 
            if (op == Oexec || op == Omod_exec)
            {
               *(cdOPptr)p = (cdOP)Oproceed;
               p += cdOP_L;
            }
            if (op == Ocall || op == Omod_call) 
               sp+=2;  // skip last arg 
         }
         else
         {

            * (cdATOMptr) p = (cdATOM)pred;
            p += cdATOM_L;
            * (cdSMINTptr) p = ar;
            p += cdSMINT_L;

            // At this point, fixp points to the opcode for
            // this call, which was p at the beginning of this
            // opcode.  fixp is followed by the imod, pred, pred,
            // and arity.
            fixup(fixp, TRUE);
            // now figure third (optional) arg 
            if (op == Ocall || op == Omod_call)                 // short 
            {
               * (cdSINTptr) p = GET_INT16(sp);
               p += cdSINT_L;
               sp += 2;
            }
         } 
         break;

      case Oescape:          // will an escape ever be compiled? check compiler
      case Oget_struc:       // functor, arity, (Xi or short or null)
      case Oput_struc:
         fixp = p - cdOP_L;  // in case we need it for fixup 
         atom_num = (intCH) GET_INT16(sp);
         sp += 2;
         * (cdATOMptr) p = (cdATOM) v_global_atoms[atom_num];
         p += cdATOM_L;
         //* (cdSMINTptr) p = *sp++;                  // arity 
         * (cdSMINTptr) p = read_arity(sp);                  // arity 
         p += cdSMINT_L;
         // now figure 3rd (optional) arg 
         if (op == Oget_struc || op == Oput_struc)  // Xi 
         {
            * (TERMptr) p = read_xi(sp);
            p += PTR_L;
         }
         // else no 3rd arg 
         break;

      case Ounify_con:
         *(TERM)p = read_const(sp);
         p += CELL_L;
         break;

      case Oswitch_on_term:
         // three short ints in backwards order
         // we map these to match what the pcode expects
         for (i = 0; i < 3; i++)
         {
            if (Lref >= MaxClauses)
              pXCPT->Error(labelsE, Lref); 
            Clabrefs[Lref++] = p;
            * (cdSINTptr) p = GET_INT16(sp + 4 - 2*i);
            p += cdSINT_L;
         }
         sp += 6;
         break;
          
      case Oswitch_on_cons:
         // short size, (size xx |CELL|LABEL|)
         xx = * (cdSINTptr) p = (cdSINT) GET_INT16(sp);
         p += cdSINT_L;
         sp += 2;
         while(xx--)
         {       
            * (TERM) p = read_const(sp);
            p += CELL_L;
            if (Lref >= MaxClauses)
               pXCPT->Error(labelsE, Lref);
            Clabrefs[Lref++] = p;
            * (cdSINTptr) p = (cdSINT) GET_INT16(sp);
            p += cdSINT_L;
            sp += 2;
         }
         break;
          
      case Oswitch_on_struc:
      // short size, (size xx |NAME|ARITY|LABEL|)
         *(cdSINTptr)p = xx = GET_INT16(sp);
         p += cdSINT_L;
         sp += 2;
         while(xx--)
         {       
            atom_num = (intCH) GET_INT16(sp);         // functor 
            sp += 2;
            *(cdATOMptr)p = (cdATOM) v_global_atoms[atom_num];
            p += cdATOM_L;

            //*(cdSMINTptr)p = (cdSMINT) *sp++;         // arity 
            *(cdSMINTptr)p = read_arity(sp);         // arity 
            p += cdSMINT_L;

            if (Lref >= MaxClauses)                   // label 
               pXCPT->Error(labelsE, Lref);
            Clabrefs[Lref++] = p;
            *(cdSINTptr)p = (cdSINT) GET_INT16(sp);
            p += cdSINT_L;
            sp += 2;
         }
         break;
          
      default:
         pXCPT->Error(badopE, op, sp - SourceBuf);
      }
#define BUFBUF 20
      if((intC)(p - DestBuf) > (intC)(DestBufLen - (sizeof(CODE_HEADER)/sizeof(CODE)) - BUFBUF))
      {
         int newsize = BUFBUF + (int) ((float)DestBufLen * ((float)src_length / (float)(sp - SourceBuf)));
         pXCPT->Error(destbufE, newsize);
      }
   }
   if (sp != SourceBuf + src_length)                
      pXCPT->Error(badbytecntE);

   return(p);
}

TF LLoad::p_is_loaded(void)
{
   TERM     x;
   aCHAR    fname[_MAX_PATH];
   aCHAR   *fpathname;
   uintC      i;

   x = X(0);
   Lstrcpy(fname, *(x->getAtom()));

   fpathname = Laget_path(fname, aS(".plm"), aS("abin"));
   if (!fpathname)
      fpathname = Laget_path(fname, aS(".xpl"), aS("abin"));
   if (!fpathname)
      pXCPT->Error(fopenE, fname);

   for (i=0; i<MaxFiles; i++)                  // do not load module twice
      if (Load_Files[i].in_use && Lstrcmp(Load_Files[i].fname, fpathname) == 0) 
         break;
   delete fpathname;

   if (i < MaxFiles)
      return TRUE;
   else
      return FALSE;
}

int LLoad::p_unload()
{
   //const DWORD cchPath = 256;
   TERM     x;
   aCHAR    fname[_MAX_PATH];
   //CLAUSE_BLKptr ci, temp;
   //PRED_BLKptr   pi;
   aCHAR   *fpathname = NULL;
   uintC      i;

   x = X(0);
   Lstrcpy(fname, *(x->getAtom()));
      
   // Don't process pathnames because Laget_path tilts slashes
   if (Lstrchr(fname, '/') < 0 && Lstrchr(fname, '\\') < 0)  {
      fpathname = Laget_path(fname, aS(".plm"), aS("abin"));
      if (!fpathname)
         fpathname = Laget_path(fname, aS(".xpl"), aS("abin"));
   }
   if (!fpathname)
   {
      // this is the case where it is probably an unload from a
      // load from memory, the name is the name, but later we
      // want to delete fpathname, so we do this to keep delete
      // happy.
      fpathname = new aCHAR[_MAX_PATH];
      Lstrcpy(fpathname, fname);
   }
    //  pXCPT->Error(fopenE, fname);

   for (i=0; i<MaxFiles; i++)                  // do not load module twice
      if (Load_Files[i].in_use && Lstrcmp(Load_Files[i].fname, fpathname) == 0) 
         break;
   delete fpathname;

   if (i == MaxFiles)
      return TRUE;   // wasn't loaded

   un_ensure_loaded(fname);
   delete Load_Files[i].fname;
   Load_Files[i].fname = NULL;
   Load_Files[i].in_use = false;
   pGCTH->free_plm((short)i);  // free the protected gc objects
   pDDB->UnloadFile(i);
   return TRUE;
}
/*
   maxAtoms = pATAB->GetMaxAtoms();
   for (i = 0; i < maxAtoms; i++)                 // find clauses withis fileix
     if(pi = pATAB->PredHead(i))
      {                                            // pred blk exists
         if(!(ci = pi->pclause))
            continue;                              // no clause blks here
   
         while(ci->fileix == fileix)
         {                                         // first clause blk is ours
            ci = ci->clink;                        // next clause blk
            pDMEM->Free((DDptr)(pi->pclause));     // free this clause blk
            pi->pclause = ci;                      // unthread it
            if(!ci)
            break;                                 // no more clause blks
         }
         if(!ci)
            continue;                              // no more clause blks
                                                   // there are more ...
         while(ci->clink)                          // but this is not ours
            if(ci->clink->fileix == fileix)
            {                                      // next cls block is ours
               temp = ci->clink->clink;  
               pDMEM->Free((DDptr)(ci->clink));    // free next clause block
               ci->clink = temp;                   // unthread it
               ci->info &= (~extended);
            }
            else
               ci = ci->clink;                     // next cls block not ours
      }
   return TRUE;
   */

int LLoad::p_loadZfile()
{
//   std::cout << "loading a file\n";
//   return p_loadZmemoryZtest();
   TERM x;

   x = X(0);
   if (! x->IsAtom())
       pXCPT->Error(sysargE);
   return load_file( *(x->getAtom()) );
}


int LLoad::load_file(STRptr fname1)
{
   LPathString fname(fname1);
   fname.TiltSlashes();

   int rc = load_file2(fname);

   size_t i;
   LPathString elname;
   bool b_same;

   while (! v_ensure_loaded.empty())
   {
      elname = v_ensure_loaded.back();
      v_ensure_loaded.pop_back();
      b_same = false;
      for (i=0; i < v_short_fnames.size(); i++)
      {
         if (is_same_fname(elname, v_short_fnames[i]))
         {
            b_same = true;
            break;
         }
      }
      if (b_same == false)
      {
         rc = load_file2(elname);
      }
   }

   return rc;
}

int LLoad::load_file2(STRptr fname)
{
   int err;

   if (b_loading)
      pXCPT->Error(loadloadE, fname);

   // Note two mechanisms here, one added after the other. ensure_loaded
   // uses short file names, disregards path in checking sameness, whereas
   // unload, is_loaded, etc. all use the full path.  So two arrays of
   // loaded files.  Seemed to make sense that ensure_loaded should be
   // just name, due to issues with moving a file from one place to another.
   v_short_fnames.push_back(LPathString(fname));

   aCHAR *fpathname = Laget_path(fname, aS(".plm"), aS("abin"));
   if (!fpathname)
      fpathname = Laget_path(fname, aS(".xpl"), aS("abin"));
   if (!fpathname)
      pXCPT->Error(fopenE, fname);

   Lstrncpy((STRptr)LoadFile, fpathname, _MAX_PATH-1);
   int len = (int)Lstrlen(LoadFile)+1;
   char *filename;
   LNEW(filename, char[len*2], aS("temporary"));
   wcstombs(filename, LoadFile, len*2);

   std::ifstream code_stream;
   code_stream.open(filename,
         std::ios::in | std::ios::binary);
   //code_stream.open(filename, std::ios::in | std::ios::binary);
   //code_stream.open(filename, std::ios::in);

   try
   {
      err = Load(&code_stream);
   }
   catch(LExcept EX)
   {
      code_stream.close();
      delete filename;
      delete fpathname;
      throw EX;
   }
   code_stream.close();
   delete filename;
   delete fpathname;
   if (err == TRUE)
      return TRUE;
   else
      return FALSE;
}

int LLoad::load_from_memory(STRptr xplname, int code_length, unsigned char* code)
{
#if defined(SOLARIS) && defined(GNU)
  // sstream not available for this config it appears
  STUB(aS("load from memory"));
  return NOTOK;
#else
   int i;
   int err;

   Lstrcpy((STRptr)LoadFile, xplname);

//This code appears to output 0x7F instead of 0xFF
//#ifdef BUG_PLM
//   DUMP << aS("load from mem = ") <<std::hex<<__toascii(code[0])<<" "<<__toascii(code[1])<<" "<<__toascii(code[2])<<" "<<__toascii(code[3])<<" "<<__toascii(code[4])<<" "<<std::dec<<NL<<FLUSH;
//#endif

   std::string scode(code_length, '0');
   for (i=0; i<code_length; i++)
      scode[i] = code[i];

   //delete code;
   std::istringstream code_stream(scode,
         std::ios::in | std::ios::binary);

//   std::istringstream *code_stream;
//   code_stream = new std::istringstream(scode, std::ios::in | std::ios::binary);

   err = Load(&code_stream);

   if (err == TRUE)
      return OK;
   else
      return NOTOK;
#endif  // not available for sun/gcc
}

int LLoad::p_loadZmemoryZtest()
{
#if defined(SOLARIS) && defined(GNU)
  STUB(aS("test load from memory"));
  return FALSE;
#else
   int err;
   TERM x;

   x = X(0);
   if (! x->IsAtom())
       pXCPT->Error(sysargE);
   Lstrcpy((STRptr)LoadFile, *(x->getAtom()));
   int len = (int)Lstrlen(LoadFile)+1;
   char *filename;
   LNEW(filename, char[len*2], aS("load"));
   wcstombs(filename, LoadFile, len*2);

   char *code;
   LNEW(code, char[1000000], aS("load"));
   //std::string code;
   std::ifstream in;
   // Need to check amzi/abin path as well,
   // make a copy of lenv asys_fopen for opening streams
   in.open(filename, std::ios::in | std::ios::binary);
   int i = 0;
   while (in)
      in.get(code[i++]);
   //code[i] = EOF;
   int code_length = i;
   in.close();

   std::string scode(code_length, '0');
   for (i=0; i<code_length; i++)
      scode[i] = code[i];

   delete code;
   delete filename;
   std::istringstream code_stream(scode,
         std::ios::in | std::ios::binary);

//   std::istringstream *code_stream;
//   code_stream = new std::istringstream(scode, std::ios::in | std::ios::binary);
//   std::cout << "loading from memory\n";
   err = Load(&code_stream);
   if (err == TRUE)
      return TRUE;
   else
      return FALSE;
#endif
}

int LLoad::p_loadZmemory()
// loadFromMemory(atom name, int length, addr location)
{
#if defined(SOLARIS) && defined(GNU)
  // sstream not available for this config it appears
  STUB(aS("load from memory"));
  return FALSE;
#else
   int err;
   TERM x0, x1, x2;
   int code_length;
   char *code;
   int i;

   x0 = X(0);
   if (! x0->IsAtom())
       pXCPT->Error(instanceE, aS("arg1 must be an atom"));
   x1 = X(1);
   if (! x1->IsInt())
      pXCPT->Error(instanceE, aS("arg2 must be an integer"));
   x2 = X(2);
//   if (! x2->IsPtr())
//      pXCPT->Error(instanceE, aS("arg3 must be a pointer"));

   Lstrcpy((STRptr)LoadFile, *(x0->getAtom()));
   code_length = x1->getInt();
   code = (char*)(x2->getPtr());

   std::string scode(code_length, '0');
   for (i=0; i<code_length; i++)
      scode[i] = code[i];

   //delete code;
   std::istringstream code_stream(scode,
         std::ios::in | std::ios::binary);

//   std::istringstream *code_stream;
//   code_stream = new std::istringstream(scode, std::ios::in | std::ios::binary);

   err = Load(&code_stream);
   if (err == TRUE)
      return TRUE;
   else
      return FALSE;
#endif  // not available for sun/gcc
}

int LLoad::p_loadZops()
{
   int err;
   TERM x;

   x = X(0);
   if (! x->IsAtom())
       pXCPT->Error(sysargE);

   aCHAR *fpathname = Laget_path(*(x->getAtom()),
         aS(".plm"), aS("abin"));
   if (!fpathname)
      pXCPT->Error(fopenE, (STRptr)*(x->getAtom()));

   Lstrncpy((STRptr)LoadFile, fpathname, 255);
   int len = (int)Lstrlen(LoadFile)+1;
   char *filename;
   LNEW(filename, char[len*2], aS("temporary"));
   wcstombs(filename, LoadFile, len*2);

   std::ifstream code_stream;
   // Need to check amzi/abin path as well,
   // make a copy of lenv asys_fopen for opening streams
   code_stream.open(filename, std::ios::in | std::ios::binary);

   try { err = LoadOps(&code_stream); }
   catch( LExcept EX )
   {
      delete filename;
      delete fpathname;
      code_stream.close();
      throw EX;
   }
   delete filename;
   delete fpathname;
   code_stream.close();
   if (err == TRUE)
      return TRUE;
   else
      return FALSE;
}

bool LLoad::is_same_fname(LPathString f1, LPathString f2)
{
   LString fname1 = f1.GetFileName();
   LString fname2 = f2.GetFileName();
   if ( fname1 != fname2 )
      return false;
   f1.AddExt(aS(".plm"));
   f2.AddExt(aS(".plm"));
   LString ext1 = f1.GetExt();
   LString ext2 = f2.GetExt();
   if ( ext1 != ext2 )
      return false;
   return true;
}

// Note two mechanisms here, one added after the other. ensure_loaded
// uses short file names, disregards path in checking sameness, whereas
// unload, is_loaded, etc. all use the full path.  So two arrays of
// loaded files.  Seemed to make sense that ensure_loaded should be
// just name, due to issues with moving a file from one place to another.

TF LLoad::p_ensure_loaded()
{
   //TERM     x;
   //aCHAR    fname[_MAX_PATH];
   size_t      i;
   LPathString fname;

   //x = X(0);
   fname = *(X(0)->getAtom());
   fname.TiltSlashes();
   
   //Lstrcpy(fname, *(x->getAtom()));

   for (i=0; i<v_short_fnames.size(); i++)
   {
      if (is_same_fname(fname, (LPathString)v_short_fnames[i]))
         return TRUE;
   }

   if (! b_loading)
      load_file(fname);
   else
      v_ensure_loaded.push_back(fname);

   return TRUE;
}

void LLoad::un_ensure_loaded(aCHAR *fn)
{
   LPathString fname(fn);
   fname.TiltSlashes();
   
   std::vector<LPathString>::iterator vi = v_short_fnames.begin();

   while (vi != v_short_fnames.end())
   {
      if (is_same_fname(fname, *vi))
      {
         v_short_fnames.erase(vi);
         break;
      }
      vi++;
   }

   return;
}



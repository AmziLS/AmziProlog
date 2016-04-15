/***************************************************************************\
*
* hxl.cpp -- Heap, X registers and Local
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: hxl.cpp,v $
* Revision 1.8  2006/05/07 03:32:29  mary
* Mary changes.
*
* Revision 1.7  2006/03/08 23:38:59  dennis
* fixed LNEW bug, MS uses bad_alloc now...
*
* Revision 1.6  2005/08/09 17:55:29  dennis
* debugging changes
*
* Revision 1.5  2005/08/04 21:05:39  dennis
* sync logging
*
* Revision 1.4  2005/02/21 17:40:08  dennis
* vba addition
*
* Revision 1.3  2004/01/15 20:29:47  dennis
* fixed nasty unifycopy gc bug
*
* Revision 1.2  2003/12/10 22:29:04  dennis
* built dll with copy all feature for the dynamic db for now
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.24  2003/06/23 17:39:16  dennis
* fixed heapgc/error recovery bug
*
* Revision 1.23  2002/12/06 17:37:36  dennis
* a6-3-2, finally got rid of mscw tagging of heap cells to maintain
* state of dynamic clause backtracking, put backtracking info in
* the choice point structure instead.  much cleaner.  retract can now
* just analyze the control stack to see if a retract is safe.  heapgc
* no longer has to worry about those wierd cells.
*
* Revision 1.22  2002/12/02 18:25:12  dennis
* Converted to Visual Studio .NET.
*
* Revision 1.21  2002/11/26 16:50:40  dennis
* thanksgiving updates
*
* Revision 1.20  2002/09/27 00:27:23  dennis
* fix another retract pulls the rug out from goal bug
*
* Revision 1.19  2002/06/19 04:04:39  dennis
* alib missing exports added, fixed gc/0
*
* Revision 1.18  2002/06/02 03:50:56  dennis
* all the XStr forms of logic server calls call strterm and grow the
* heap, so made new ExecProve and CallProve that to the strterm inside
* so that the heap can rolled back to before the Exec/Call.  Important
* that this be done in the Prove, so that if heapgc is encountered,
* the new heap is used for the rollback.
*
* Revision 1.17  2002/05/20 04:34:11  dennis
* final changes
*
* Revision 1.16  2002/05/15 16:59:08  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.15  2002/02/13 03:19:59  dennis
* reals changed to have a LReal class, moved to file of same name,
* math functions moved out of termsvc and into lmath.cpp/h, eval rewritten
* to reflect various options for numbers, lexcept modified so anyone, even
* non-engine objects, can throw LExcept objects.
*
* Revision 1.14  2002/02/04 17:20:59  dennis
* New lreal wrapper class on reals, start of number options:
* decimals: real/float, floats: single/double.  Created lmath.h/cpp
* but nothing in them yet.  Work to be done on sorting out mixed mode
* arithmetic for different options.  Also - casts removed, as they
* were causing problems.
*
* Revision 1.13  2002/01/28 06:29:19  dennis
* changes for parsing numbers, handling different options
*
* Revision 1.12  2002/01/06 20:31:27  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.11  2001/12/06 16:43:20  dennis
* fixed heapgc bug and allowed for unlimited vars in copyterm
*
* Revision 1.10  2001/11/09 02:28:10  dennis
* Finished, I hope, sorted and indexed predicates. Needed to rethink
* how mscws worked, given the new itertors for moving from clause to
* clause.
*
* Revision 1.9  2001/10/13 02:58:13  dennis
* see/tell bugs, used to close function streams
*
* Revision 1.8  2001/07/10 16:51:31  dennis
* added more memory leak checking tools, all clean so far
*
* Revision 1.7  2001/06/27 15:15:10  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
* Revision 1.6  2001/04/02 21:50:13  dennis
* got debugger working again
*
* Revision 1.5  2001/04/01 15:54:52  ray
* Modified compiler and loader for fixed data.
*
* Revision 1.4  2001/03/13 20:05:18  dennis
* added meta$call to try and isolate meta calls.
*
* Revision 1.3  2001/02/27 21:09:13  ray
* removed ? from properNames
* implemented integer/1 for reals
* repaired -ve reals error
*
* Revision 1.2  2001/02/05 03:11:43  dennis
* Fixed nasty heap GC bug, other minor fixes
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.16  2000/10/24 14:00:45  ray
* Added atom_chars.
* Performed cosmetic surgery on termsvc.
*
* Revision 1.15  2000/09/25 02:11:19  dennis
* first version of modules working, runs the modular version of
* duck world.  still needs import and export.  release 6.1.1
*
* Revision 1.14  2000/09/02 02:10:19  dennis
* new version of get$db replacing dbrefgen for getting clauses
* from dynamic database
*
* Revision 1.13  2000/08/26 00:32:05  dennis
* Merged with ray's changes for new numbers and ISO standard features.
* Added new AtomTable, Dynamic Database and operators based on STL
* maps as basis for ISO modules.
*
* Revision 1.12  2000/08/14 02:05:37  ray
* Added Real class.
* No division yet.
* No error reporting yet.
*
* Revision 1.10  2000/05/14 03:52:33  dennis
* a5-1-8 replaced ddbatom with atomdb and one 'user' module
*
* Revision 1.9  2000/03/31 08:51:37  dennis
* Small bug fixes for Linux, still not running on Linux
*
*
* 05/19/99 Ray zeroed Heap after alloc check in Init
* 05/19/99 Ray zeroed out new pmask for GC
* 03/29/99 Ray commentd out IBMPC action for LINUX
* 01/26/99 Ray added nonref check for structures in mark_
* 10/98    Ray simplified heapGETN()
\***************************************************************************/

/**
The Heap, X registers and Local stack are a single contiguous
array of cells.  They need to be contiguous because of optimizations
in the WAM that make decisions about cells based on relative
address.
** See line 616 - Ray
*/

#include "inc.h"
#include "pch.h"

#ifdef LANDFILL
// BUG_HEAP might cause GPF
#define noBUG_HEAP

#ifdef BUG_HEAP
#define noBUG_H
#define noBUG_X  5   // number of X cells to display
#define noBUG_L
#define noBUG_HGC
#define noBUG_HGC_SUMMARY
#define noBUG_HINIT
#endif
#endif

HXL::HXL(LEngine *peng)
{
   m_peng = peng;
   Heap = NULL;
}

HXL::~HXL()
{
   delete[] Heap;
}

/* Heap, X and Local are alloc'd in contiguous memory so simple address
   compares can be used to decide where a term is located.
   IMPORTANT:  We MUST allocate with Heap < Local --
   then var <-> var bindings is simply bind-to-lower address */

void HXL::Init(uintC heapsize, uintC localsize, uintC xvars, int hbumpperc)
{
   uintC    numCells;

   HeapLim  = heapsize;
   LocalLim = localsize;
   MaxVars  = xvars;

   numCells = HeapLim + LocalLim + MaxVars + 8;

   //Heap = LNEW(Cell[numCells], aS("heap"));
   //if (Heap == NULL)
   //   pXCPT->Error(outofmemE, aS("Heap + Local"));
#ifdef BUG_HINIT
   DUMP << "  HXL LNEW for this many cells: " << numCells << NL << FLUSH;
   try
   {
      Heap = new Cell[numCells];
   }
   catch(...)
   {
      DUMP << "  HXL failed to allocate" << NL << FLUSH;
   }
#else

/*
try
   {
      Heap = new Cell[numCells];
   }
   catch(...)
   {
      std::cout << "heap allocation error\n";
      pXCPT->Error(outofmemE, aS("Heap + Local"));
   }
*/
   LNEW(Heap, Cell[numCells], aS("heap"));
//   LNEWX(Heap, Cell[numCells]);
#endif

#ifdef BUG_HINIT
   DUMP << "  HXL LNEW succeeded " << NL << FLUSH;
#endif

   if ( (intC)Heap % sizeof(intC) )  // Cells must be on 4-byte boundary
      pXCPT->Error(alignE, aS("Heap"));

   for(unsigned int i = 0; i < numCells; i++)               // ray
      //Heap[i] = 0;
      Heap[i].setUnused();

   X = Heap + HeapLim + 8;

   H = Heap + 1;                    // offsets to H[0] are uninitialised !!
   HeapTop = Heap + HeapLim;        // TOS for Heap

   L = Local = X + MaxVars;
   LocalTop = L + LocalLim;

   HeapHW = H;
   LocalHW = L;

   if (hbumpperc > 25) 
     hbumpperc = 25;
   else 
     if (hbumpperc < 0) 
       hbumpperc = 2;
   HeapBumper = Heap + (intC)(HeapLim*(100 - hbumpperc)/100);
   HeapBumpPerc = hbumpperc;

   return;
}

TERM HXL::StrToCharList(STRptr c)
{
   TERM t, u;
   intC i=0;

   u = t = heapGET();
   //pTSVC->putVAL( (t), ((long) (t + 1)) | (long) listT );
   t->setList(t+1);
   while(*c)
   {
      // bigdig heapPUSH( (consT | charS) | pTSVC->ILong((CELL)(*c)) );
      //heapPUSH( cellCHAR((CELL)(*c)) );
      heapGET()->setChar(*c);
      ++c;
      if (++i > pSTATE->m_buflen)
         pXCPT->Error(stroverS);
      t = heapGET();
      //pTSVC->putVAL( (t), ((long) (t + 1)) | (long) listT );
      t->setList(t+1);
   }
   heapPOP();
   // bigdig heapPUSH(consT | atomS |  pTSVC->ILong(nilA));
   //heapPUSH(cellATOM(nilA));
   heapGET()->setAtom(pATAB->nilA);
   return(u);
}


fixedC HXL::ListToFixed(TERM List)
{
  TERM x;
  fixedC result;
  result.init_hilo(0,0);
  char sign = '+';

  //result.hi = 0;
  //result.lo = 0;

  if (List->IsList())
    {
		List = List->getTerm();
		x = (List)->dref();
		if(x->IsAtom())
		  if( x->getAtom() == pATAB->minusA)
			 {
				sign = '-';
				List++;
				List = List->getTerm();
				x = (List)->dref();
			 }
		  else
			 pXCPT->Error(typeE, aS("head of arg 2 must be '-' or integer"));

		if (x->IsInt())
		  {
			 //result.hi = (intC)x->getInt();
          result.setHI(x->getInt());
			 if(sign == '-')
				//result.hi |= 0x80000000;
            result.setSign(sign);
		  }
		else
			 pXCPT->Error(typeE, aS("arg 2"));
		List++;
		List = List->getTerm();
		x = (List)->dref();
		if(!(x->IsAtom() &&  x->getAtom() == pATAB->periodA))
		  pXCPT->Error(typeE, aS("arg 2 expected '.'"));

		List++;
		List = List->getTerm();
		x = (List)->dref();
		if (x->IsInt())
		  //result.lo = (intC)x->getInt();
        result.setLO(x->getInt());
		else
		  pXCPT->Error(typeE, aS("arg 2 must be list of integers"));
		return result;
	 }
  else
	 pXCPT->Error(typeE, aS("must be list"));
  return result;
}

TERM HXL::FixedToList(fixedC f)
{
   TERM t, u;
   intC i=0;
	PATOM pointAnum = pATAB->EnterAtom(aS("."));
	PATOM signAnum  = pATAB->EnterAtom(aS("-"));

   u = t = heapGET();
   t->setList(t+1);

	//if(f.hi & 0x80000000)
   if (f.isNeg())
	  {
		 heapGET()->setAtom(signAnum);
		 t = heapGET();
		 t->setList(t+1);
	  }
	//heapGET()->setInt(f.hi & 0x7fffffff);
   heapGET()->setInt(f.getHI());
	t = heapGET();
	t->setList(t+1);
	heapGET()->setAtom(pointAnum);
	t = heapGET();
	t->setList(t+1);
	heapGET()->setInt(f.getLO());
   heapGET()->setAtom(pATAB->nilA);
   return(u);
}

TERM HXL::StrToAtomList(STRptr c)
{
  TERM t, tList;
  intC i=0;
  aCHAR a[2];
  PATOM p;

  a[1] = 0;
  tList = t = heapGET();
  t->setList(t+1);
  while(*c)
	 {
		a[0] = *c;
		p = pATAB->EnterAtom(a);
      heapGET()->setAtom(p);
      ++c;
      if (++i > pSTATE->m_buflen)
		  pXCPT->Error(stroverS);
      t = heapGET();
      t->setList(t+1);
	 }
  heapPOP();
  heapGET()->setAtom(pATAB->nilA);
  return(tList);
}

int HXL::CharListToStr(TERM t, STRptr c)
{            //  map list term t to buffer c -- make sure c is big enough 
  TERM x;
  
  if (t->IsList())
    {
      while(TRUE)
        {
          t = (TERM) t->getTerm();
          x = (t)->dref();
          if (x->IsChar())
            *c++ = (aCHAR)x->getChar();
          else if (x->IsInt())
            *c++ = (aCHAR)x->getInt();
          else
            pXCPT->Error(typeE, aS("list must be of character codes"));
          t++;
          t = (t)->dref();
          if (t->IsAtom())
            if (t->getAtom() == pATAB->nilA)
              break;
            else
              pXCPT->Error(typeE, aS("list must be of character codes"));
          else if (t->IsList())
            continue;
          pXCPT->Error(typeE, aS("list must be of character codes"));
        }
      *c = EOS;
      return(TRUE);
   }
   pXCPT->Error(typeE, aS("must be a character code list"));

   return(0);                                      // a formality 
}

int HXL::AtomListToStr(TERM t, STRptr c)
{            //  map list term t to buffer c -- make sure c is big enough 
  TERM x;
  
  if (t->IsList())
    {
      while(TRUE)
        {
          t = (TERM) t->getTerm();
          x = (t)->dref();
          if (x->IsAtom())
            *c++ = **(x->getAtom());
          else
            pXCPT->Error(typeE, aS("list must be of atomic chars"));
          t++;
          t = (t)->dref();
          if (t->IsAtom())
            if (t->getAtom() == pATAB->nilA)
              break;
            else
              pXCPT->Error(typeE, aS("list must be of atomic chars"));
          else if (t->IsList())
            continue;
          pXCPT->Error(typeE, aS("list must be of character codes"));
        }
      *c = EOS;
      return TRUE;
   }
   pXCPT->Error(typeE, aS("must be a character code list"));

   return 0;                                      // a formality 
}

Cell *HXL::heapError(int n)
{
   //H -= n;
   //HeapHW -= n;
   pXCPT->Error(heapE);
   return NULL;
}

Cell *HXL::localError()
{
   pXCPT->Error(localE);
   return NULL;
}

void HXL::GC(ARITY arity)
{
   HeapGC hgc(this); 

   //hgc.SetEO(m_eo);
   hgc.GC(arity);
}

Cell *HXL::SaveX()
{
   Cell *XI;
   LNEW(XI, Cell[MaxVars], aS("heap"));
   //if (XI == NULL)
   //   pXCPT->Error(outofmemE, aS("Copy of X Variables"));
   CopyFromX(XI, MaxVars);
   return XI;
}

void HXL::RestoreX(Cell *XI)
{
   CopyToX(XI, MaxVars);
   delete[] XI;
}

LString HXL::cellname(TERM t)
{
   if (t->getTerm() == NULL)
      return LString(aS("UNBOUND"));
   else if (t->getTerm() >= Local && t->getTerm() < LocalTop)
      return LString(aS("L%d"), 50, (int)(t->getTerm() - Local));
      //errDebugMsg("L[%d]", t->getTerm() - Local);
   else if (t->getTerm() >= Heap && t->getTerm() < HeapTop)
      return LString(pSTATE->m_properNames ? aS("?H%d") : aS("H%d"), 
							50, (int)(t->getTerm() - Heap));
      //errDebugMsg("H[%d]", t->getTerm() - Heap);
   else if (t->getTerm() >= X && t->getTerm() < Local)
      return LString(aS("X%d"), 50, (int)(t->getTerm() - X));
      //errDebugMsg("X[%d]", t->getTerm() - X);
   else
      return LString(aS("Non-stack term %08lx"), 128, *(intCptr)t);
}

void HXL::DumpX(ARITY a)
{
#ifdef LANDFILL
   int i;
   TERM t;

   for (i=0, t=X; i<(int)a; i++, t++)
   {
      DUMP << "  x" << i << " = ";
      pTSVC->Dump(t);
      DUMP << NL;
   }
#endif
}

void HXL::Dump()
{
#if defined(BUG_H) || defined(BUG_X) || defined(BUG_L)
   //TERM t;
   //int  i;
#endif
/*
#ifdef BUG_H
   DUMP << "----- Heap -----" << NL;
   for (t = Heap + 1, i=1; t <= H; t++, i++)
   {
      DUMP << "  H[" << i << "] = ";
      pTSVC->Dump(t);
      DUMP << NL;
   }
   DUMP << "----- -----" << NL << FLUSH;
#endif

#ifdef BUG_X
   DUMP << "----- X -----" << NL;
   for (i=0, t=X; i<BUG_X; i++, t++)
   {
      DUMP << "  X[" << i << "] = ";
      pTSVC->Dump(t);
      DUMP << NL;
   }
   DUMP << "----- -----" << NL << FLUSH;
#endif

#ifdef BUG_L
   DUMP << "----- L -----" << NL;
   for (i=0, t=Local; t<=L; i++, t++)
   {
      DUMP << "  L[" << i << "] = ";
      pTSVC->Dump(t);
      DUMP << NL;
   }
   DUMP << "----- -----" << NL << FLUSH;
*/  
  /* buggy, some terms cdcdcdcd = crash
     
     pDUMP->Write( aS("\n----- Local -----\n") );
     
     for (t = Local, i=1; t <= L; t++, i++)
     {
     pDUMP->Write( LString( aS("  L[%d] = "), 40, i) );
     pTSVC->Dump(m_eo, t);
     pDUMP->Write( NL );
     }
     */
//#endif
}

#ifdef LANDFILL
void HXL::DumpCellName(TERM t)
{
   if (t >= Local && t < LocalTop)
      DUMP << "L[" << (int)(t-Local) << "]";
   else if (t >= Heap && t < HeapTop)
      DUMP << "H[" << (int)(t-Heap) << "]";
   else if (t >= X && t < Local)
      DUMP << "X[" << (int)(t-X) << "]";
   else
      DUMP << "DDB";
}
#endif

TF HXL::p_dumpZhxl()
{
  Dump();
  return TRUE;
}

//-----------------------------------------
// HeapGC implementation
//

// ----- Public functions -----

HeapGC::HeapGC(HXL *phxl)
{
  /* Counter used by stashing predicates to indicate how many
     stashers are holding up gc.  When its zero its ok to gc. */
  
  //heapGC_OK = 0;
  
  //GCheapB = FALSE;
  
  m_phxl = phxl;
  m_peng = phxl->m_peng;
  /* Percentage of heap used as buffer at end, gc is triggered
     when H is within g_heapbump of top. */
  
  
#ifdef xBUG_GC
  for (ith=0; ith<NTERMS; ith++)
    termhist[ith] = 0;
  ith = 0;
#endif
  
  return;
}

void HeapGC::GC(ARITY arity)
{ // garbage collect the heap stack. have just entered a call or exec /arity
  intC     i, masksize;

#ifdef BUG_HGC_SUMMARY
  TERM HTop1 = pHXL->HTop();
  //DUMP << "heapgc" << NL << FLUSH;
  FILL("---> HeapGC::GC(" << arity << ")");
#endif

#ifdef BUG_HGC
//  heapDump();
//  codeDumpStack();
  DUMP << "********************************************" << NL;
  DUMP << "             HeapGC::GC" << NL;
  DUMP << "********************************************" << NL;
  DUMP << NL << "Control Stack Before:" << NL;
  pCNTL->Dump();
  DUMP << NL;
  DUMP << NL << "MSCW Chain Before:" << NL;
  //pDDB->mscw_heap_walk();
  DUMP << NL;
#endif

//printf("gc\n");
  
#ifdef xBUG_GC
  intC      i;
  errDebugMsg("\n\nheapGC  arity(%d)\n", arity);
  if (arity > 20) 
    pXCPT->Error(aS("hpgc.c arity error"));
#endif
  
  if (m_phxl->H >= m_phxl->HeapTop) 
    pXCPT->Error(heapoverS);

  //if (heapGC_OK > 0) return;     // tmstash.c functions might prevent gc 
  
#ifdef FULL_GC
  Hbegin = m_phxl->Heap + 1;
#else                     // cowardly collect from last choice point only
  if (B != NULL)
    Hbegin = B -> HBc;
  else return;
#endif
  
  //pXCPT->Error(gcheapW,
  //   (intC)((HTERM) m_phxl->H - (HTERM) m_phxl->Heap));
  
#ifdef xxBUG_GC
  errDebugMsg("\nGC goal %s, L[%ld], arity %u, B.Tagc %lp\n",
         Cur_goal, (L-Local), arity, (B->Tagc));
  /* debug_state(arity);*/
#endif
  //GCheapB = FALSE;
  
  /* A cell is not collectable if
     
     it is in a structure whose functor cell is not collectable
     it is in a list whose list cell is not collectable
     it is referenced by an Xi (0 < i < arity of call)
     it is referenced by a Yi
     it is referenced by B->Tagc
     
     We garbage collect the heap by marking all those not collectable cells
     in the heap ABOVE B -> HBc (This means we don't have to bother about 
     Xis in any choicepoints or any cells referenced in the trail (since
     cells in the heap are trailed only if they are before B -> HBc).
     
     FULLGC goes beyond this, collecting choice point Xi's and trail
     as well.
     
     Note that memSysAlloc zeroes the allocated memory, we count
     on this. Forgot that when we removed memSysAlloc!
     */
  
  masksize = (intC) ((HTERM) m_phxl->H - (HTERM) Hbegin + 1);
  if (masksize == 0) 
    return;
  
  //pm = pmask = (MASKptr)
  //      memSysAlloc((size_t) masksize * sizeof(intC), "Heap GC Mask");
  LNEW(pmask, uintC[masksize], aS("heap"));
  pm = pmask;
  for(i = 0; i < masksize; i++)               // ray
    pmask[i] = 0;

  //if (NULL == pm)
  //  pXCPT->Error(outofmemE, aS("HeapGC Shadow"));
  
  mark(pmask, arity);     // mark all reachable cells in pmask
  
#ifdef xxBUG_GC
  errDebugMsg("\n---- the mask ----\n");
  for (i = 0; i < masksize; i++)
    {
      if (i%10 == 0) 
        errDebugMsg("\n%04d: ",i);
      if (i%5 == 0) 
        errDebugMsg(" ");
      errDebugMsg("%4d ",* (pmask + i));
    }
#endif
  
  freecompact(pmask, masksize, arity);     // compress the heap 
  
  //memSysFree((aBYTEptr)pmask);
  //  delete[] pmask;                     // let ~HeapGC do it. ray
  delete pm;

#ifdef xBUG_GC
  errDebugMsg("\ngc_doheap should be finished ok\n");
  debug_state(arity);
#endif
  
  //GCheapB = FALSE;
  
#ifdef xBUG_HGC
  DUMP << NL << "Control Stack After:" << NL;
  pCNTL->Dump();
  DUMP << NL;
  DUMP << NL << "MSCW Chain After:" << NL;
  pDDB->mscw_heap_walk();
  DUMP << NL;
#endif

/*
#ifdef BUG_HGC_SUMMARY
  TERM HTop2 = pHXL->HTop();
  int dif = HTop1 - HTop2;
  BUGOUT << "***** heapgc *****\n";
  BUGOUT << "   #cells = " << dif << "   new top = " << HTop2 << NL;
  BUGOUT << "***** *****\n";
  DUMP << FLUSH;
#endif
*/

#ifdef BUG_HGC_SUMMARY
  TERM HTop2 = pHXL->HTop();
  int dif = HTop1 - HTop2;
  //DUMP << "   #cells = " << dif << NL;
  //DUMP << FLUSH;
  FILL("   #cells = " << dif);
  std::cout << NL << NL << "   Heap GC recovered " << dif << NL << NL << FLUSH;
#endif

   if (pHXL->HeapPastBumper())
      pXCPT->Error(heapgcE);        // if we didn't recover enough, throw error 

  return;
}

// ----- Private Functions -----

#ifdef xBUG_GC
void HeapGC::validate_term(HTERM t)
{
  int ithx;
  
  if (hpDebugTermLoc(t)) 
    return;
  
  /*
    print_cell(t);
    ithx = ith;
    for (ith=ithx-1; ith==ithx; ith--)
    {
    if (ith<0) ith=NTERMS-1;
    print_cell(termhist[ith]);
    }
    pXCPT->Error(aS("HPGC - Bad term"));
    */
}
#endif

// mark loops through the local, X registers, old choice point Xs,
// and the trail looking for references to the heap that will be marked 
void HeapGC::mark(MASKptr tpm, ARITY arity)
{
#ifdef FULL_GC
  CHOICE_POINTptr   Bt;
  int               i;
  TERM               Xt;
  TRAIL_CELLptr      tc;
#elif defined(xBUG_GC)
  int i;
#endif
  HTERM               t;
  
  for(t = (HTERM) m_phxl->Local; t < (HTERM) m_phxl->L; ++t)  
    {
#ifdef xxBUG_GC
      i = (int)((HTERM)t - (HTERM)Local);
      if (i%1 == 0) 
        errDebugMsg("\n");
      errDebugMsg("L[%d] ",i);
#endif

  //      if((unsigned long *)*t != t)               // ray
        mark_(t, tpm, 0);
    }
  
  for(t = (HTERM) m_phxl->X; t < ((HTERM) m_phxl->X) + arity; ++t) 
    {
#ifdef xxBUG_GC
      i = (int)((HTERM)t - (HTERM)X);
      if (i%1 == 0) 
        errDebugMsg("\n");
      errDebugMsg("X[%d] ",i);
#endif
      mark_(t, tpm, 0);
    }
  
#ifndef FULL_GC
  if (B->Tagc)
    {
      t = (HTERM) B->Tagc;
      mark_(t, tpm, 0);
    }
  
#else
  /* Walk the choice point stack, looking for references into the
     heap.  These include the Xi's, a tag if present, and the
     saved heap pointer.  The Xi's may not be initialized, but
     at worst this will just cause unneeded heap cells to be saved.
     The tag points to an atom.  Bt->HBc is a little trickier, it
     doesn't actually care whats on the heap cell because it will
     be the new top of the heap.  But it is important that its pointer
     be adjusted as the stack moves down. Thus the special pmREF. */
  
  for (Bt = pCNTL->BTop(); Bt != NULL; Bt = Bt->Bc)
    {
#ifdef xxBUG_GC
      errDebugMsg("\nB[%d]", (int)((STACK_ELMptr)Bt-Stack));
      /* cdPrintChoice(Bt);*/
#endif
      if (ON_HEAP(Bt->HBc))
        pmREF(Bt->HBc);
      else
        pXCPT->Error(badrefS);
      Xt = (TERM) (Bt + 1);
      for (i = 0; i < Bt->NTVc; i++)
        {
#ifdef xxBUG_GC
          if (i%1 == 0) 
            errDebugMsg("\n");
          errDebugMsg("BX[%d] ", i);
#endif
          mark_((HTERM) (Xt+i), tpm, 0);
        }
      if (Bt->Tagc)
        {
          t = (HTERM) Bt->Tagc;
          mark_(t, tpm, 0);
        }
    }
  
  //tc = &Trail[TR];
  tc = pCNTL->trailTopCell();
  for (i = (int)(pCNTL->TRTop())-1; i >= 0; i--)
    {
#ifdef xxBUG_GC
      if (i%1 == 0) 
        errDebugMsg("\n");
      errDebugMsg("T[%d] ", i);
#endif
      t = (HTERM) ((--tc)->reset_cell);
      mark_(t, tpm, 0);
    }
  /* Finally, make sure all the special use heap cells
     have been accounted for.  These are the two cells
     used by dbref$gen to remember the database cells
     on backtracking, and the cell used by sub$string
     to remember the index position on backtracking. */
#ifdef xBUG_HGC
 DynamicClause *pdc;
 DynamicPredicate *pdp;
 PredicateHead *xpph;
 PATOM xpred;
 ARITY xar;
#endif

  for (t=(HTERM)Hbegin; t<=(HTERM)m_phxl->H; t++)
    {
      /*
      if (t->IsMSCW())
        {
#ifdef xBUG_HGC
 DUMP << "HGC Marking MSCW: " << t;
 pdc = t->getMSCWDBRef();
 pdp = pdc->getDynamicPredicate();
 xpph = pdp->getPredicateHead();
 xpred = xpph->getName();
 xar = xpph->getArity();
 DUMP << SP << PREDAR(xpred,xar) << NL;
#endif
          pmMARK(t);
//          pmMARK(t+1);
        }
        */
      if (t->IsRHeap())
        pmMARK(t);
    }
                                            // end of new code for whole thing 
#endif
  
  return;
}



// mark_ takes a reference to the heap and marks it, and then follows
// any chains of reference and marks them as well 
void HeapGC::mark_(HTERM t, MASKptr tpm, int depth)
{                                                  // mark recursively from t 
  int      iar, iar1, depth1;
  HTERM    t1, t2;

  depth1 = 1+depth;

loop:
#ifdef xxBUG_GC
  /* errDebugMsg("\nmark_ t = %lp, *t = %08lx\n", t, (intC) *t);*/
  errDebugMsg2("\nmark_ %lp: ", "\nmark_ %p: ", t);
  VALID_TERM(t);
  print_cell(t);
#endif
  
#ifdef xBUG_GC
  termhist[ith++] = t;
  if (ith >= NTERMS) 
    ith=0;
#endif
  
  if (t == NULL)
    {
      /*      errDebugMsg("\nHPGC: NULL term\n");*/
      return;
    }
  //if (*t == 0)
  if (t->IsUnused())  // bigdig  is this equivalent?
    {
      /*      errDebugMsg("\nHPGC: NULL cell\n");*/
      return;
    }

  if (t->IsUnused())
    return;
  
  if (ON_NEWHEAP(t))
    {
      if (pmMARKED(t)) 
        return;
      pmMARK(t);
    }
                                                     // ray
  if ((TERM)t->IsUnbound())
    return;
    
  if (t->IsConst()) 
    return;
  
  if (t->IsList())                              // list
    {
#ifdef xxBUG_GC
      errDebugMsg("list");
#endif
      t = t->getTerm();
      
      if (! ON_HEAP(t))                               // Make sure we are OK 
        return;

        mark_(t, tpm, depth1);
      ++t;
      goto loop;
    }

  else if (t->IsRef())                           // reference
    {
#ifdef xxBUG_GC
      errDebugMsg("ref");
#endif
      t = t->getTerm();
      goto loop;
    }       

  else if (t->IsStruct())                         // structure
    {
      //      if((*t & CELL_TAG) != 3)                   // 3 means constant
      //        return;                                    // ray
      TERM t0 = t;

      t2 = t1 =  t->getTerm();               // get value (without tag)

#ifdef xxBUG_GC
      errDebugMsg("struct %Fs/%d", *(t1->getAtom()), 
                  t1->getArity());
#endif
      
      if (! ON_HEAP(t1))                              // Make sure we are OK 
        return;
      
      if(t1->IsConst()) 
        t = t1;
      else
        {                                           // bad term - ray
          t++;
          goto loop;
        }
      
      if (ON_NEWHEAP(t1)) 
        pmMARK(t1);

      iar1 = iar = (int)t1->getArity();
      while (iar-- > 0)
        mark_(++t2, tpm, depth1);
      return;
    }
#ifdef xBUG_GC
  errDebugMsg("\n  t = %p, *t = %08lx", t, (intC) *t);
#endif
  pXCPT->Error(hbadtermS);
  return;
}

/*
**  tpm is an array of ints - one element for each element of Heap
**  we now begin to rellocate all those marked elements of Heap 
**  (for which the corresponding entry in tpm is non-zero).
**
**  We make two passes up the Heap stack - first we go through from
**  HB to H, compressing the stack by moving marked items down the stack
**  ignoring unmarked items. For each rellocated cell Ci -> Cj, 
**  prellocate[i] <- j.
**
**  Then we walk the compacted stack, looking for cells with a heap reference
**  in them (listT, refT, strucT), we check to see if Ci is j > 0, if it is
**  we create the new address and insert it into the cell
**
**  Next we walk down the local stack to see if any cells reference rellocated
**  Heap items and we change the reference -- finally we do the same on the 
*** X registers
**
**
*/
void HeapGC::freecompact(MASKptr tpm, uintC masksize, ARITY arity)
{
#ifdef FULL_GC
  CHOICE_POINTptr   Bt;
  TERM              Xt;
  TRAIL_CELLptr     tc;
#endif
  intC              i;
  HTERM             NewH;
  HTERM             t;

  NewH =  (HTERM) (Hbegin);                     // bottom of compacted heap 
  //DynamicClauseIterator *pdci;

#ifdef BUG_HGC
 DynamicClause *pdc;
 DynamicPredicate *pdp;
 PredicateHead *xpph;
 PATOM xpred;
 ARITY xar;
#endif
  
  for(i = 0; i < (intC) masksize; ++i)    // Now rellocate heap above Hbegin
    {
      // if a cell is marked as being in use, relocate it to the
      // top of the new heap, increment the new heap, and store
      // in the mask array the offset from here to where it was
      // relocated. if it was just a B->HBc reference, set adjustment
      // to point to next cell, but don't move anything.
      
      if (tpm[i])
        if (tpm[i] && MARKFLAG)
          {
            *NewH = * ((Hbegin) + i);
            tpm[i] = (intC) ((HTERM)NewH - (HTERM)(Hbegin));
            ++NewH;
          }
        else
          tpm[i] = (intC) (1 + (HTERM)NewH - (HTERM)(Hbegin));
    }
  //if ((intC) ((HTERM) NewH - (HTERM) Heap)  > (intC) (HeapLim - g_heapbump))

  //if (NewH > m_phxl->HeapBumper)
  //  pXCPT->Error(heapgcE);        // if we didn't recover anything, give up 
  
  // find all refs into the (new) heap - changing the pointer if necessary
  
  for(t = m_phxl->Heap; t < NewH; ++t)   // must start at Heap for references 
    change_reference(t, tpm);
  
  for(t = m_phxl->Local; t < (HTERM) m_phxl->L; ++t)
    change_reference(t, tpm);
  
  for(t = m_phxl->X; t < ((HTERM) m_phxl->X) + arity; ++t)
    change_reference(t, tpm);
  
#ifndef FULL_GC
  if (B->Tagc) 
    { 
      t = (TERM) B->Tagc;
      change_reference((TERM) &t, tpm);
      B->Tagc = (TERM) t;
    }
  
#else
  /* try to collect the whole thing */
  
  
  /* Will this work? does the change_reference really happen
     for Bt->HBc?  and Bt->Tagc? it doesn't look like the new
     heap pointer gets put back in Bt-HBc for example.
     It must be working for the Xt's, but check anyway. */
  
  for (Bt = pCNTL->BTop(); Bt != NULL; Bt = Bt->Bc)
    {
#ifdef BUG_HGC
 DUMP << "Old Bt->HBc = " << Bt->HBc << NL;
#endif
      //t = (HTERM)(Bt->HBc);
      Bt->HBc = (Hbegin) + tpm[(intC)((HTERM) Bt->HBc - (HTERM) (Hbegin))];
      //change_reference((TERM) &t, tpm);
      //Bt->HBc = (TERM)t;
#ifdef BUG_HGC
 DUMP << "New Bt->HBc = " << Bt->HBc << NL;
#endif
      Xt = (TERM) (Bt + 1);
      for (i = 0; i < Bt->NTVc; i++)
      {
#ifdef BUG_HGC
 DUMP << "  Old Xt[" << i << "] *= " << *(Xt+i) << NL;
#endif
        change_reference(Xt+i, tpm);
#ifdef BUG_HGC
 DUMP << "  New Xt[" << i << "] *= " << *(Xt+i) << NL;
#endif
      }
      if (Bt->Tagc)
        {
          //t = Bt->Tagc;
          Bt->Tagc = 
				(Hbegin) + tpm[(intC)((HTERM) Bt->Tagc - (HTERM) (Hbegin))];
          //change_reference((TERM) &t, tpm);
          //Bt->Tagc = (TERM) t;
        }
    }
  
  //tc = &Trail[TR];
  tc = pCNTL->trailTopCell();
  for (i = pCNTL->TRTop()-1; i >= 0; i--)
    {
      t = (--tc)->reset_cell;
      if (ON_NEWHEAP(t))
         t = (Hbegin) + tpm[(intC)((HTERM) t - (HTERM) (Hbegin))];
         //change_reference((TERM) &t, tpm);
      tc->reset_cell = (TERM) t;
    }
  
  /* end of new code for whole thing */
#endif
  
  //pXCPT->Error(gcheap2W, 
  //   (intC)((HTERM) m_phxl->H - NewH));
  
  m_phxl->H = ((HTERM) NewH - (HTERM) m_phxl->Heap) + m_phxl->Heap;
  
  // note !!! cannot set this before since in change_reference 
  // we want ON_NEWHEAP() to refer to the OLD collectable stack
}


void HeapGC::change_reference(HTERM t, MASKptr tpm)
{
  TERM  newt, tm;
//  int tag;

  if (! t->IsConst())                         // has no pointer into heap
    {
      tm = t->getTerm();   
      if (ON_NEWHEAP(tm))                          // e.g. not unbound ref say
        {
          newt = (Hbegin) + tpm[(intC)((HTERM) tm - (HTERM) (Hbegin))];
          //pTSVC->putVAL( (t), (pTSVC->LValue(t) & CELL_TAG) | (intC) newt );
          //tag = t->getType();
          t->setJustTerm(newt);
          //t->setType(tag);
        }
    }
}

//------------------------------------------
// HeapDebug implementation
//

#ifdef xBUG_HEAP

void HeapDebug::prin_cell(TERM tin, int n_dent, int n_levels)
{
  int i=0;
  int ar=0;
  int del_dent=2;
  DynamicClause*   pdb=NULL;
  TERM t;
  PATOM satom;
  
  t = tin;
  
  //   if (n_dent > 20) n_dent = del_dent;
  
  errDebugMsg("%*s", n_dent, "");
  
  //   if (t)
  //   {
  //      errDebugMsg("%08lx: ", (intC)*t);
  //      hpDebugTermLoc(t);
  //   }
  //   else
  //   {
  //      errDebugMsg("Null term\n");
  //      return;
  //   }
  
  if (!t)
    {
      errDebugMsg("Null term\n");
      return;
    }
  
  switch(t->getType())
    {
        case atomS:
          i = t->getAtom();
          ar = t->getArity();
          if (i >= 0 && i < eMaxAtoms)            
            errDebugMsg2("ATOM %Fs/%d ", "ATOM %s/%i ",
                         *(i), ar);
          else
            errDebugMsg("ATOM - bad atom %0*lx", PP_SIZE, *(intCptr)t);
          
          PADDR(tin);
          break;
          
        case intS:   errDebugMsg("INT %d ", t->getInt());
          PADDR(tin);
          break;
          
        /*
        case dbrefS:
          errDebugMsg("DBREF ");
          PADDR(tin);
          if (n_levels)
            {
              pdb = t->getDBRef();
              prin_cell(&(pdb->dbcode), n_dent+del_dent, n_levels-1);
            }
          break;
          */
          
        case singleS:   errDebugMsg("REAL %f ", getDouble(t));
          PADDR(tin);
          break;
          
        case longS:   errDebugMsg("LONG %ld ", pTSVC->getXINT(t));
          PADDR(tin);
          break;
          
        /*
        case mscwS:
          i = t->getAtom();
          ar = t->getArity();
          if (i >= 0 && i < eMaxAtoms)            
            errDebugMsg2("MSCW %Fs [%d] ", "MSCW %s [%i] ",
                         *(i), ar);
          else
            errDebugMsg("MSCW - bad atom %0*lx", PP_SIZE, *(intCptr)t);
          
          PADDR(tin);
          break;
          */
          
        case strS:   errDebugMsg("STRING %s ", pTSVC->getSTR(t));
          PADDR(tin);
          break;
          
        default:    errDebugMsg("GARBAGE CONSTANT TYPE ");
          PADDR(tin);
      
    case strucT:
      errDebugMsg("STRUCT ");
      PADDR(tin);
      t = t->getTerm();
      prin_cell(t, n_dent+del_dent, 0);
      if (n_levels)
        {
          ar = (int)t->getArity();
          satom = t->getAtom();
          while (ar-- > 0)
            {
              //               errDebugMsg2("\n  %Fs[%d]: ", "\n  %s[%d]: ",
              //                     *(satom), i++);
              prin_cell(++t, n_dent+del_dent, n_levels-1);
            }
        }
      break;
      
    case refT:
      if (t != t->getTerm())
        {
          errDebugMsg("REF ");
          PADDR(tin);
          if (n_levels)
            {
              t = t->getTerm();
              //              errDebugMsg("\n  -R-> ");
              prin_cell(t, n_dent+del_dent, n_levels-1);
            }
        }
      else
        {
          errDebugMsg("UNBOUND ");
          PADDR(tin);
        }
      break;
      
    case listT:
      errDebugMsg("LIST ");
      PADDR(tin);
      if (n_levels)
        {
          t = t->getTerm();
          prin_cell(t, n_dent+del_dent, n_levels-1);
          prin_cell(++t, n_dent+del_dent, n_levels-1);
        }
      break;
      
    default:
      errDebugMsg("GARBAGE CELL TYPE ");
      PADDR(tin);
    }
  return;   
}


void HeapDebug::print_cell(TERM t, int n_levels)
{
  prin_cell(t, 0, n_levels);
  return;
}

void HeapDebug::debug_state(ARITY arity)
{
  int i;
  
  errDebugMsg("\n--- STATE DUMP ---\n");
  
  for(i = 0; i < (int)arity; ++i)
    {
      errDebugMsg("\nX[%d]  ", i);
      print_cell(&X[i], 0);
    }
  
  errDebugMsg("\n\nL %d   H %d   E %d   B %d   TR %d\n",
              (int)(L - Local), (int)(H - Heap), 
              E ? (int)((Cell*) E - Stack) : -1,
              B ? (int)((Cell*) B - Stack) : -1, 
              TR);
  errDebugMsg("\n");
  /*   debug_heap(Heap+1, (int)(H - Heap)); */
  debug_local(Local, (int)(L - Local));
  cdDebugChoice();
  cdDebugTrail();
  errDebugMsg("\n----- HEAP -----");
  memDebugDump((aBYTEptr)Heap, (int)(4*(H-Heap)), 2);
  errDebugMsg("\n------------------------------------------\n");
}


void HeapDebug::debug_heap(TERM fromT, int length)
{
  TERM t;
  
  errDebugMsg("---- HEAP ----- ");
  
  for(t = fromT; t < fromT + length; ++t)
    {
      errDebugMsg("\nH[%d] ", (int) (t - Heap));
      print_cell(t, 0);
    }
  errDebugMsg("\n");
}

void HeapDebug::heapDump()
{
  TERM t;
  FILE*  dumpfile;
  
  dumpfile = logfile;
  
  //   dumpfile = fopen("dump", "a");
  fprintf(dumpfile, "\n----- Heap Dump -----\n\n");
  
  for (t=Heap+1; t<H; t++)
    {
      fprintf(dumpfile, "H[%05d] %08x: ", t-Heap, *t);
      print_cell(t, 0);
    }
  
  fprintf(dumpfile, "\n\n----- End of Heap -----\n\n");
  
  //   fclose(dumpfile);
  return;
}

void HeapDebug::debug_local(TERM fromT, int length)
{
  TERM t;
  
  errDebugMsg("---- LOCAL ----- ");
  
  for(t = fromT; t < fromT + length; ++t)
    {
      errDebugMsg("\nL[%d] %0*lx ", (int) (t - Local), PP_SIZE, (long) t);
      print_cell(t, 0);
    }
  errDebugMsg("\n");
}

TF HeapDebug::hpDebugTermLoc(TERM t)
{
  TF ret = TRUE;
  if (t == NULL)
    {
      errDebugMsg("NULL");
      ret=FALSE;
    }
  else if (t >= Local && t < LocalTop)
    errDebugMsg("L[%d]", t - Local);
  else if (t >= Heap && t < HeapTop)
    errDebugMsg("H[%d]", t - Heap);
  else if (t >= X && t < Local)
    errDebugMsg("X[%d]", t - X);
  else if (t >= Stack && t < StackTop)
    errDebugMsg("B[%d]", t - Stack);
  else
    {
      errDebugMsg2("?{%Fp}", "?{%p}", t);
      ret=FALSE;
    }
  return ret;
}

#endif // LANDFILL




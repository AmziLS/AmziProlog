/*****************************************************************\
*
* gcthing.cpp - gc for odd things, like strings and gigadigits
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: gcthing.cpp,v $
* Revision 1.6  2006/05/07 03:32:29  mary
* Mary changes.
*
* Revision 1.5  2005/08/09 17:55:29  dennis
* debugging changes
*
* Revision 1.4  2005/08/04 21:05:39  dennis
* sync logging
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
* Revision 1.17  2003/09/11 02:07:44  dennis
* fixed memory leak problem with dynamic iterators
*
* Revision 1.16  2002/07/04 16:20:25  dennis
* support academic registration
*
* Revision 1.15  2002/06/23 20:01:29  dennis
* fixed some gc issues
*
* Revision 1.14  2002/06/19 04:04:39  dennis
* alib missing exports added, fixed gc/0
*
* Revision 1.13  2002/05/15 16:59:08  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.12  2002/02/13 03:19:59  dennis
* reals changed to have a LReal class, moved to file of same name,
* math functions moved out of termsvc and into lmath.cpp/h, eval rewritten
* to reflect various options for numbers, lexcept modified so anyone, even
* non-engine objects, can throw LExcept objects.
*
* Revision 1.11  2002/02/04 17:20:59  dennis
* New lreal wrapper class on reals, start of number options:
* decimals: real/float, floats: single/double.  Created lmath.h/cpp
* but nothing in them yet.  Work to be done on sorting out mixed mode
* arithmetic for different options.  Also - casts removed, as they
* were causing problems.
*
* Revision 1.10  2002/01/28 06:29:19  dennis
* changes for parsing numbers, handling different options
*
* Revision 1.9  2002/01/27 22:52:05  ray
* Corrections to power and fixed
*
* Revision 1.8  2002/01/20 20:48:05  ray
* revised real divide, printReal
*
* Revision 1.7  2002/01/06 20:31:27  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.6  2001/10/13 02:58:13  dennis
* see/tell bugs, used to close function streams
*
* Revision 1.5  2001/10/05 17:07:01  ray
* Added real things. Fixed some bugs. Stopped adding '.' in prep
*
* Revision 1.4  2001/08/02 18:51:00  dennis
* merge of new streams complete
*
* Revision 1.3  2001/07/21 00:39:46  dennis
* added garbage collector for strings and things
*
* Revision 1.2  2001/07/10 16:51:31  dennis
* added more memory leak checking tools, all clean so far
*
* Revision 1.1  2001/06/27 15:15:10  dennis
* miscellaneous changes and bug fixes, work with leak detection
*
*
\*****************************************************************/

#include "inc.h"
#include "pch.h"

#ifdef LANDFILL
#define noBUG_GCREAL
// BUG_GC_SUMMARY might cause GPF
#define noBUG_GC_SUMMARY
#define noBUG_GCTH
#endif

#ifdef BUG_GCREAL
int realcount = 1;
#endif

GCThings::~GCThings()
{
   GCThing *t = first;
   GCThing *t2;
   while (t)
   {
      t2 = t->get_next();
      delete t;
      t = t2;
   }
}

#ifdef LANDFILL
void GCThing::Dump()
{
   LEngine *m_peng = g_peng;
   DUMP << "gcthing: " << this << " b_heap = " << b_heap << " dbuse = " << dbuse << NL << FLUSH;
//      " what_am_i = " << what_am_i() << NL << FLUSH;
   DUMP << "         " << " next = " << next << NL << FLUSH;
}
#endif

GCString::GCString(LEngine *m_peng, STRptr ps)
{
   LNEW(s, LString(ps), aS("gc_object"));
}

GCString::~GCString()
{
   delete s;
}

GCReal::GCReal(Real rr)
{
   LNEWX(r, LReal(*rr));
#ifdef BUG_GCREAL
   id = realcount++;
   std::cout << ">>> GCReal " << id << NL;
   r->Dump();
#endif
}

GCReal::~GCReal()
{
#ifdef BUG_GCREAL
   std::cout << "<<< ~GCReal " << id << NL;
#endif
   delete r;
}

GCDBIter::~GCDBIter()
{
#ifdef BUG_GCTH
   *g_dump << "deleting gc_dbiter: " << pdci << NL << FLUSH;
#endif
   delete pdci;
}

GCStash::~GCStash()
{
   delete st;
}

GCString *GCThings::make_string(STRptr s)
{
   GCString *gs;
   LNEW(gs, GCString(m_peng, s), aS("gc_object"));
   add_thing(gs);
   return gs;
}

GCDouble *GCThings::make_float(double d)
{
   GCDouble *gf;
   LNEW(gf, GCDouble(d), aS("gc_object"));
   add_thing(gf);
   return gf;
}

GCReal *GCThings::make_real(Real r)
{
   GCReal *gr;
   LNEW(gr, GCReal(r), aS("gc_object"));
   add_thing(gr);
   return gr;
}

GCDBIter *GCThings::make_dbiter(DynamicClauseIterator *i)
{
   GCDBIter *gi;
   LNEW(gi, GCDBIter(i), aS("gc_object"));
   add_thing(gi);
#ifdef BUG_GCTH
   DUMP << "added gc_dbiter: " << gi << NL;
#endif
   return gi;
}

GCStash *GCThings::make_stash(Stash *st)
{
   GCStash *gst;
   LNEW(gst, GCStash(st), aS("gc_object"));
   add_thing(gst);
   return gst;
}

void GCThings::add_thing(GCThing *g)
{
   if (first)
   {
      last->set_next(g);
      last = g;
      g->set_next(NULL);
   }
   else
   {
      first = g;
      last = g;
      g->set_next(NULL);
   }
   if ( gcthingfreq && (gccount++ > gcthingfreq) )
   {
      gc_things();
      gccount = 0;
   }
#ifdef BUG_GC_SUMMARY
//   std::cout << '+' << gccount;
#endif
}

// called when a .plm is unloaded, so we free the static things
void GCThings::free_plm(short file_ix)
{
   GCThing *th;
   for (th = first; th; th = th->get_next())
   {
      if (th->get_plm() == file_ix)
         th->set_plm(-1);
   }
}

void GCThings::gc_things()
{
   TERM t, Xt;
   int i;
   CHOICE_POINTptr Bt;

#ifdef BUG_GCTH
 DUMP << NL << NL << "*** Starting gc_things ***" << NL << FLUSH;
#endif

#ifdef BUG_GC_SUMMARY
   int recovered = 0;
   int left = 0;
 DUMP << "gcthing " << NL << FLUSH;
#endif

   // Walk the Heap, XVars, Local, and ChoicePoint stack
   // looking for cell references that are GCThings, and
   // mark those things as in use.

   //for (t=pHXL->GetHeap(); t<=pHXL->HTop(); t++)
   for (t=pHXL->GetHeap(); t<pHXL->HTop(); t++)
   {
      if (t->IsGCThing())
      {
         t->getGCThing()->setHeapUse();
#ifdef xBUG_GCTH
         if (t->IsMSCW())
            DUMP << "protecting mscw: " << t->getGCThing() << NL;
#endif
      }
   }

   //for (i=0, t=pHXL->XVar(0); i<=pHXL->GetMaxVars(); t++, i++)
   for (i=0, t=pHXL->XVar(0); i<pHXL->GetMaxVars(); t++, i++)
   {
      if (t->IsGCThing())
         t->getGCThing()->setHeapUse();
   }

   for (t=pHXL->GetLocal(); t<pHXL->LTop(); t++)
   {
      if (t->IsGCThing())
         t->getGCThing()->setHeapUse();
   }

   for (Bt = pCNTL->BTop(); Bt != NULL; Bt = Bt->Bc)
   {
      if (Bt->gc_pdci)
         Bt->gc_pdci->setHeapUse();
      Xt = (TERM) (Bt + 1);
      for (i = 0; i < Bt->NTVc; i++)
      {
         t = Xt + i;
         if (t->IsGCThing())
            t->getGCThing()->setHeapUse();
      }
   }

   // Walk the GCThing chain, freeing that which can be freed.
   // We always keep the first one for simplicity, and save
   // the last in case it is about to be protected.

   GCThing *th = first;
   GCThing *th2;
   if (th)
      th2 = th->get_next();

   while (th2)
   {
#ifdef BUG_GCTH
 DUMP << "considering: " << th2 << NL << FLUSH;
#endif
      if (th2->inUse())
      {
#ifdef BUG_GC_SUMMARY
 left++;
#endif
#ifdef BUG_GCTH
  DUMP << "inuse: ";
  th2->Dump();
  DUMP << FLUSH;
#endif
         th = th2;
         th2 = th->get_next();
      }
      else
      {
#ifdef BUG_GCTH
  DUMP << "deleting: ";
  th2->Dump();
  DUMP << FLUSH;
#endif
         // if this is the last one, save it and be done
         if (th2->get_next())
         {
            th->set_next( th2->get_next() );
            delete th2;
#ifdef BUG_GC_SUMMARY
 recovered++;
#endif
            th2 = th->get_next();
         }
         else
            th2 = NULL;
      }
      th->clrHeapUse();  // clear for next time around
   }
#ifdef BUG_GC_SUMMARY
 DUMP << "   recovered: " << recovered << "  left: " << left << NL << FLUSH;
 std::cout << NL << NL << "   GCThing recovered: " << recovered << "  left: " << left << NL << NL << FLUSH;
#endif
}

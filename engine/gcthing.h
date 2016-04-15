/*****************************************************************\
*
* gcthing.h - gc for odd things, like strings and gigadigits
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: gcthing.h,v $
* Revision 1.2  2004/01/15 20:29:47  dennis
* fixed nasty unifycopy gc bug
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.12  2002/07/04 16:20:25  dennis
* support academic registration
*
* Revision 1.11  2002/06/23 20:01:29  dennis
* fixed some gc issues
*
* Revision 1.10  2002/05/15 16:59:08  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.9  2002/02/13 03:19:59  dennis
* reals changed to have a LReal class, moved to file of same name,
* math functions moved out of termsvc and into lmath.cpp/h, eval rewritten
* to reflect various options for numbers, lexcept modified so anyone, even
* non-engine objects, can throw LExcept objects.
*
* Revision 1.8  2002/02/04 17:20:59  dennis
* New lreal wrapper class on reals, start of number options:
* decimals: real/float, floats: single/double.  Created lmath.h/cpp
* but nothing in them yet.  Work to be done on sorting out mixed mode
* arithmetic for different options.  Also - casts removed, as they
* were causing problems.
*
* Revision 1.7  2002/01/28 06:29:19  dennis
* changes for parsing numbers, handling different options
*
* Revision 1.6  2002/01/06 20:31:27  dennis
* put in new memory leak LNEW that reports on lines of code, had
* to take out std:: classes from engine and replace with pointers
* to classes to get it to work, due to various news and deletes in
* the stl class libraries.
*
* Revision 1.5  2001/10/13 02:58:13  dennis
* see/tell bugs, used to close function streams
*
* Revision 1.4  2001/10/05 17:07:01  ray
* Added real things. Fixed some bugs. Stopped adding '.' in prep
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

//typedef GD *Real;                             // defined here in case no reals

class GCThing
{
private:
   GCThing *next;
   int      dbuse;
   // static constants are owned by the .plm that loaded them,
   // static_file is the file_ix from the loader, as each compiled
   // file creates new gcthings, there shouldn't be any conflict here.
   short    plm;
   bool     b_heap;
public:
   GCThing()
   { plm = -1; b_heap = false; dbuse = 0; }
   virtual ~GCThing() {};
   void set_plm(short file_ix)
   { LASSERT( ((file_ix >= 0 && plm < 0) || file_ix < 0), aS("two owners of static gc object"));
     plm = file_ix; }
   short get_plm()
   { return plm; }
   GCThing *get_next()
   { return next; }
   void set_next(GCThing *gc)
   { next = gc; }
   void setHeapUse()
   { b_heap = true; }
   void clrHeapUse()
   { b_heap = false; }
   bool inUse()
   { return (plm >= 0 || b_heap || dbuse); }
   void inc_dbuse()
   { // over 100, keep forever
      //if (! b_protect)
      //{
      //   dbuse++;
      //   if (dbuse > 55) b_protect = true;
      //}
      if (dbuse < 100)
         dbuse++;
   }
   void dec_dbuse()
   { // over 100, keep forever
      //if (! b_protect && dbuse > 0) dbuse--;
      if (dbuse < 100 && dbuse > 0) dbuse--;
   }
//   virtual LString what_am_i() = 0;
#ifdef LANDFILL
   virtual void Dump();
#endif
};

class GCString : public GCThing
{
public:
   LString *s;
   //GCString(LString *ss)
   //{ s = ss; }
   GCString(LEngine *m_peng, STRptr ps);
   ~GCString();
//   LString what_am_i()
//   { return LString(aS("string: ")) + *s; }
};

class GCDouble : public GCThing
{
public:
   double d;
   GCDouble(double dd)
   { d = dd; }
   ~GCDouble() {};
   int operator==(const GCDouble &dd)
   { return d == dd.d; }
//   LString what_am_i()
//   { return LString(aS("double")); }
};

class GCReal : public GCThing
{
public:
   Real r;
#ifdef LANDFILL
   int id;
#endif
   GCReal(Real rr);
   ~GCReal();
   int operator==(const GCReal &rr)
   { return r == rr.r; }
//   LString what_am_i()
//   { return LString(aS("real")); }
};

class GCDBIter : public GCThing
{
public:
   DynamicClauseIterator *pdci;
   //GCString(LString *ss)
   //{ s = ss; }
   GCDBIter(DynamicClauseIterator *i)
   { pdci = i; }
   ~GCDBIter();
//   LString what_am_i()
//   { return LString(aS("dbiter")); }
};

class GCStash : public GCThing
{
public:
   Stash *st;
   GCStash(Stash *s)
   { st = s; }
   ~GCStash();
//   LString what_am_i()
//   { return LString(aS("stash")); }
};

class GCThings
{
private:
   GCThing *first;
   GCThing *last;
   LEngine *m_peng;
   int      gcthingfreq;
   int      gccount;
public:
   GCThings(LEngine *peng)
   { m_peng = peng; first = NULL; last = NULL;}
   ~GCThings();
   void Init(int fr)
   { gcthingfreq = fr; gccount = 0; }
   GCString *make_string(STRptr);
   GCDouble *make_float(double);
   GCReal *make_real(Real);
   GCDBIter *make_dbiter(DynamicClauseIterator*);
   GCStash *make_stash(Stash*);
   void add_thing(GCThing *gc);
   void free_plm(short file_ix);
   void gc_things();
};

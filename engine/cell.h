//-*-C++-*-
/*****************************************************************\
*
* cell.h - the basic Prolog cell
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
*
\******************************************************************/

#ifndef CELL_H
#define CELL_H

class Cell;

typedef short MODIX;                   // make sure length right for Cell size
typedef aBYTE CLAUSE_NUMBER;

  // recorded type -------------  (0x80 denotes not fixed)
  // reported type ----         |
  //                   |        |                
  //                -------  -------- 
enum Tag { 
  refT = 1,      // 0x1   1  0x81 129
  strucT,        // 0x2   2  0x82 130
  listT,         // 0x3   3  0x83 131
  atomS,         // 0x4   4  0x84 132
  //dbrefS,        // 0x5   5  0x85 133
  strS,          // 0x6   6  0x86 134
  rheapS,        // 0x7   7  0x87 135
  stashS,        // 0x8   8  0x88 136
  unusedT,       // 0x9   9  0x89 137
  //mscwS,         // 0xa  10  0x8a 138
  //mscwinuseS,    // 0xb  11  0x8b 139
  ptrS,          // 0xc  12  0x8c 140
  // all higher reported tags are numbers 
  intS = 16,     // 0x10 16  0x90 144         // 0x10 denotes an int
  charS,         // 0x11 17  0x91 145         // charS is an annotated int
  fixedS = 0x20, // 0x20 32  0x0    0         // 0x80 (recorded)flags not fixed
  realS,         // 0x21 33  0xa1 161
  // floating point types
  singleS,       // 0x22 34  0xa2 162
  doubleS        // 0x23 35  0xa3 163
};

//class LString;
class GCReal;
class GCString;
class GCDouble;
class GCThing;
//class LDouble;
class GCDBIter;
class GCStash;
class DynamicClauseIterator;
class Stash;
//class GD;

/* Cell content.
 *
 * Cell holds a disparity of types up to size double.
 * By the natural laws of C++ it should be a base class with derived types.
 * However, that would require a v-table in the base class, raising the size
 * of instances to three words, and since there will be many instances that
 * is considered too extravagent, and it would also lead to Cells of different 
 * sizes, which is not convenient. So it is defined as a union of size two 
 * words. Unions cannot have classes containing constructors or overloaded 
 * operators as members, so Real is just a typedef for GD *.
 * The second word of the cell is normally the descriptor and the first word
 * the value. If the value needs to be larger than one word then it is 
 * replaced by a reference. 
 *
 * FixedC is an exception, because it consists of two gigadigits each of 
 * which has a spare bit in the most significant place. Therefore it can
 * occupy both words of the cell and the spare bit that goes in the
 * descriptor field (the least significant gigadigit) is used as a type 
 * indicator, 0 meaning fixedC and 1 otherwise. 
 * To avoid endian problems, the descriptor has no subfields and descriptor 
 * data are extracted by shifting and masking.
 */

/* Coercion.
 * Cells, RANDS_ and RANDS are chameleon classes which can hold various types,
 * identified by a type field. For these cases coercion operators are defined
 * to extract a particular content type, and avoid necessity for a 'get'
 * primitive. The advantage is brevity, and in many cases the C compiler
 * will automatically invoke the coercion to suit the destination without 
 * explicit code at all.
 * These operators do not modify the contents of their argument but simply 
 * extract it, regardless of whether it is the valid kind. Therefore it is
 * necessary to preface it with a type test, if there is any doubt.
 *
 * On the other hand, simple types such as integer fixedC and Real may have
 * coercion operators defined which return a different type of equivalent 
 * value. No predicate preface is necessary because they cannot be invoked
 * on the wrong type.
 */

//typedef GD *Real;                             // defined here in case no reals

class Cell
{
public:

  union
  {
      TERM      t;
      PATOM     a;
      intC      i;
      float     f;
      GCReal   *gcr;
      GCString *s;
      GCDouble  *gcd;
      //GCDBIter *db;
      GCStash  *gst;
      GCThing  *gc;
      void     *p;
  };
  uintC      descr;

  // Cell descr.   
  // Bit 31 is 0 for fixedC; so then other bits are overlaid and are not descr.
  // //  ------ ---------------- ------
  // // | type | arity or clnum | imod |
  // //  ------ ---------------- ------
  // //  31     23               15   0

  // switching to larger arity, smaller imod 50/50 4095 for each
  //  ------ ------ ----------------
  // | type | imod | arity or clnum |
  //  ------ ------ ----------------
  //  31     23    11              0
  /*
  MODIX  imod;

  union {
     aBYTE  arity;
     CLAUSE_NUMBER clnum;
  };
  aBYTE  type;
  */

public:
  Cell()                          // no longer any type, arity or imod fields
  { i = 0; descr = 0; setType(unusedT); }

  // note that these also cause casting problems, like can't << fixedC because
  // it thinks it can make a Cell from a fixedC.  aaarghhh.  fix sometime.
  Cell(long n)
  { i = n; setImod(0); setArity(0); setType(intS); }
  Cell(GCDouble *);
  Cell(GCReal *);
  Cell(PATOM aa)
  { setType(atomS); setArity(0);  a = aa; setImod(0); }
  Cell(fixedC f)
  { i = f.getFixedHI(); descr = f.getFixedLO(); }

  ~Cell() {};

  int operator==(Cell &c) ;
  friend Lostream & operator<<( Lostream &, Cell& );

  // Casts make sense for numbers, not sure for other
  // quantities.  No, the problem is error checking,
  // different callers have different needs, and the
  // cast implies that cell can be cast to something.
/*
  //operator PATOM();                         // cast cell to PATOM
  //operator aCHAR();                         // cast cell to aCHAR
  operator intC();                          // cast cell to long
  operator double();                        // cast cell to double
  operator float();                         // cast cell to float
  operator fixedC();                        // cast cell to fixed
//#ifdef REALS
  operator Real();                          // cast cell to Real
//#endif
*/
   void setType(Tag type)                    // convoluted type access
   { 
      int t = (type | 0x80) << 24;
      descr = type == fixedS ? descr & 0x7fffffff : (descr & ~0xff000000) | t;
   }

   Tag getType()
   { return descr & 0x80000000 ? (Tag)((descr >> 24) & 0x7f) : fixedS; }

   // Is
		  
   BOOL IsConst()                           // Does not contain a heap pointer
   {   
       int type = getType(); 
       return !(type == refT || type == strucT || type == listT);
   }
   BOOL IsGCThing()
   //{   return (getType() == strS || getType() == doubleS ||
   //            getType() == mscwS || getType() == realS ||
   //            getType() == mscwinuseS || getType() == stashS); }
   {   return (getType() == strS || getType() == doubleS ||
               getType() == realS || getType() == stashS); }
   BOOL IsAtom()
   {   return (getType() == atomS); }
   BOOL IsInt()
   {   return (getType() == intS) || (getType() == charS); }
   BOOL IsChar()
   {   return (getType() == charS); }
   BOOL IsSingle()
   {   return (getType() == singleS); }
   BOOL IsDouble()
   {   return (getType() == doubleS); }
   BOOL IsScientific()
   {   return (IsSingle() || IsDouble()); }
   BOOL IsReal()
   {   return (getType() == realS); }
   BOOL IsFixed()
   {   return (getType() == fixedS); }
   BOOL IsFixedOrReal()
   {   return (IsFixed() || IsReal()); }
   //BOOL IsDBRef()
   //{   return (getType() == dbrefS); }
   //BOOL IsMSCW()
   //{   return (getType() == mscwS); }
   //BOOL IsMscw()
   //{   
	//  int type = getType(); 
	//  return (type == mscwS || type == mscwinuseS); 
	//}
   //BOOL IsInUseMSCW()
   //{   return (getType() == mscwinuseS); }
   BOOL IsRHeap()
   {   return (getType() == rheapS); }
   BOOL IsStr()
   {   return (getType() == strS); }
   BOOL IsList()
   {   return (getType() == listT); }
   BOOL IsRef()
   {   return (getType() == refT); }
   BOOL IsVar()
   {   return (getType() == refT); }
   BOOL IsUnbound()
   {   return (t == this && getType() == refT); }
   BOOL IsRefPtr()
   {   return (getType() == refT && t != this); }
   BOOL IsPtr()
   {   return (getType() == ptrS); }
   BOOL IsStruct()
   {   return (getType() == strucT); }
   BOOL IsUnused()
   {   return (getType() == unusedT); }
   BOOL IsNumber()
	{ return getType() >= intS; }
   BOOL IsFixedNeg()
   { return descr & 0x80000000; }
   BOOL IsStash()
   {   return (getType() == stashS); }
   void setImod(int N)
   {   descr = (descr & ~0x00fff000) | (N << 12);}
   void setArity( ARITY N)
   {   descr = (descr & ~0x0fff) | N;}
   void setUnbound()
   {   setType(refT);  t = this; }
   //void setMSCWClauseNumber(uintCH cl)  // use arity for clause number
   //{   setArity(cl); }
   //void setMSCW(GCDBIter *dbref, MODIX i, CLAUSE_NUMBER cl)
   //{   setType(mscwS); db = dbref; setImod(i); setArity(cl); }
   //void setInUseMSCW() // assumes already was set as mscw
   //{   setType(mscwinuseS); }

   void setList(TERM tt)
   {   setType(listT);  t = tt; }
   void setSingle(float ff)
   {   f = ff; setType(singleS); }
   void setDouble(GCDouble *dd)
   {   gcd = dd; setType(doubleS); }   
   void setAtom(PATOM aa)
   {   setType(atomS); setArity(0);  a = aa; }
   void setInt(intC ii)
   {   setType(intS);  i = ii; }
   void setFixed(fixedC ff);
  //   { i = ff.hi; descr = ff.lo; }
   void setReal(GCReal *rr)
   {   setType(realS); gcr = rr; }
   void setChar(intC ii)   // uintCH??  or intC would work as it's i in either case?  was intCH
   {   setType(charS);   i = ii; }
//   void setStr(STRptr ss)
//   {   setType(strS);  s = ss == aS("") ? NULL : new LString(ss); }
//   void setStr(STRptr ss)
//   {   setType(strS);  s = new GCString(new LString(ss)); }
   void setStr(GCString *ss)
   {   setType(strS);  s = ss; }
//   void setLString(LString *ss)
//   {   setType(strS);  s = ss; }
   void setString(GCString *ss)
   {   setType(strS); s = ss; }
   void setTerm(TERM tt)
   {   setType(refT);  t = tt; }
   void setJustTerm(TERM tt)
   {   t = tt; }
   void setStruct(TERM tt)
   {   setType(strucT);  t = tt; }
   void setFA(PATOM f, int ar)
   {   setType(atomS); setArity(ar);  a = f; }
   void setStash(GCStash *tt)
   {   setType(stashS);  gst = tt; }
   //void setDBRef(GCDBIter *ddb)
   //{   setType(dbrefS); db = ddb; }
   void setRHeapI(long ii)
   {   setType(rheapS); i = ii; }
   void setUnused()
   {   setType(unusedT); i = 0; }
   void setModix(short n)
   {   setImod(n); }
   void setPtr(void *pp)
   {   setType(ptrS); p = pp; }
   void setDescr(uintC d)
   {   descr = d; }

   aCHAR getChar()                        // gets aCHAR. Use getInt() for chars
   {   return (aCHAR)i; }
   intC getInt()
   {   return i; }
   TERM getRef()
   {   return t; }
   TERM getTerm()
   {   return t; }
   PATOM getAtom()
   {   return a; }
   //int getArity()
   int getImod()
   {   return (descr >> 12) & 0x0fff; }
   //MODIX getImod()
   MODIX getArity()
   {   return (MODIX)(descr & 0x0fff);}
   MODIX getModix()
   {   return getImod(); }
   TERM getListHead()
   {   return t; }
   GCThing *getGCThing()
   {   return gc; } 
   //DynamicClauseIterator *getDBRef();
   //{   return db; }
   Real getReal();
   Real getRealCopy();
   double getDouble();
   float getSingle()
   {   return f; }
   fixedC getFixed();
   STRptr getStr();
   LString *getLString();
   Stash *getStash();
   intC getRHeapI()    // changed from long
   {   return i; }
   //CLAUSE_NUMBER getMSCWClauseNumber()
   //{   return (CLAUSE_NUMBER)getArity(); }
   //DynamicClauseIterator *getMSCWDBRef();
   //{   return db; }
   void *getPtr()
   {   return p; }

   double forceDouble();
   int forceInt();

   TF isNullary()
   { return getArity() == 0; }
   TF isUnary()
   { return getArity() == 1; }
   TF isBinary()
   { return getArity() == 2; }

  void gc_protect_plm(short);

   // Term functions  
   // These understand the nature of a term as a collection of cells.  
   // Some of these functions are here rather than in TermSvcs 
   // for convenience of the DDB structures.

   //  dereference - the key to logical variables. 
   //  A reference points to another term, and eventually the term is 
   //  a real thing (not a reference) or else it points to itself;
   //  which denotes an unbound variable.
   TERM dref()
   {
      TERM t = this;
      while(t->IsRefPtr())
         t = t->getRef();
      return t;
   }

   int term_size();
   int _term_size(int);
};

#endif  // CELL_H













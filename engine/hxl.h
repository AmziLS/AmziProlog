//-*-C++-*-
/****************************************************************************

hxl.h -- Heap, X registers and Local

Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.


****************************************************************************/

#ifndef HXL_H
#define HXL_H

typedef TERM HTERM;

typedef uintCptr  MASKptr;

#if defined(P64)
#define Cell_SHIFT 3
#else
#define Cell_SHIFT 2
#endif

#define FULL_GC
//#define NO_GC

class HeapGC;

class HXL
{
   friend class HeapGC;
#ifdef LANDFILL
   friend class HeapDebug;
#endif

private:
  LEngine *m_peng;
  
  TERM   Heap;
  TERM   HeapTop;
  
  TERM   Local;
  TERM   LocalTop;
  
  TERM   H;
  TERM   L;
  TERM   X;
  
  TERM   HeapHW;
  TERM   LocalHW;
  
  uintC   HeapLim;
  uintC   LocalLim;
  intC    MaxVars;
  
  TERM HeapBumper;                            // When to start GC
  int  HeapBumpPerc;                          // percentage of heap bumper
  
public:
  HXL(LEngine *peng);
  ~HXL();
  
  void Init(uintC heapsize, uintC localsize, uintC xvars, int hbumpperc);
  void GC(ARITY arity);
  
  void Reset()
  {   H = Heap + 1; L = Local; }
  fixedC ListToFixed(TERM);
  TERM FixedToList(fixedC);
  TERM StrToCharList(STRptr);
  TERM StrToAtomList(STRptr);
  int  CharListToStr(TERM, STRptr);
  int  AtomListToStr(TERM, STRptr);
  
  TERM heapError(int n);                // HeapError lets the macros below compile.
  
  intC GetMaxVars()
  {   return MaxVars; }
  
  TERM HTop() 
  { return H; }
  TERM GetHeap() 
  { return Heap; }
  void SetHTop(TERM t) 
  { H = t; }
  void SetHVal(Cell c) 
  {*H = c; }
  uintC GetHeapHW() 
  { return (uintC)(101*(HeapHW - Heap)/(100-HeapBumpPerc)); }
  void ResetHeapHW() 
  { HeapHW = H; }
  TERM heapGET()
  { return H < HeapHW ? H++ : (H < HeapTop ? (HeapHW = H++) : heapError(1)); }
  //{ return H++; }
  void heapINC()
  {  H < HeapHW ? H++ : ( H < HeapTop ? (HeapHW = H++) : heapError(1) ); }
  //{ H++; }
  //void heapQINC()
  //{   H++; }
  TERM heapGETN(int n)
  {
	 TERM Hin = H;
	 H += n;
	 return (H < HeapHW ) || ((HeapHW = H) < HeapTop ) ? Hin :
		(heapError(n));
   }
   //{ H+=n; return H-n; }
   void heapPUSH(Cell v)
   {   *(heapGET()) = v; }
   TERM heapPushRef()
     // bigdig   {   pTSVC->putVAL(H, (Cell)H); return heapGET(); }
   {   H->setUnbound(); return heapGET(); }
   TERM heapPOP()
   {   return ( H-- ); }
   intC heapSIZE()
   {   return (intC)HeapLim; }
   intC heapUSE()
   {   return (intC)(H - Heap); }
   BOOL HeapPastBumper()
   {   return H > HeapBumper; }
   TERM HVar(int n)
   {   return &Heap[n]; }
   BOOL OnHeap(TERM t)
   {   return t >= Heap && t < H; }
   BOOL OnHXL(TERM t)
   {   return t >= Heap && t < L; }

   TERM XVar(int n)
   {   LASSERT((n <= MaxVars), "maxvar oops"); return &X[n]; }
   Cell XVal(int n)
   {   LASSERT((n <= MaxVars), "maxvar oops"); return X[n]; }
   TERM GetX() { return X; }
   void SetXVal(int n, Cell c)
   {   LASSERT((n <= MaxVars), "maxvar oops"); X[n] = c; }
   void SetXRef(int n, Cell *t)
   {   LASSERT((n <= MaxVars), "maxvar oops"); X[n].setTerm(t); }
   void CopyToX(TERM t, int ncells)
   {   Lmemcpy(X, t, ncells * sizeof(Cell)); }
   void CopyFromX(TERM t, int ncells)
   {   Lmemcpy(t, X, ncells * sizeof(Cell)); }

   TERM SaveX();
   void RestoreX(Cell *XI);

   TERM localError();
   intC localSIZE()
   {   return (intC)LocalLim; }
   intC localUSE()
   {   return (intC)(L - Local); }
   BOOL OnLocal(TERM t)
   {   return t >= Local && t < L; }
   TERM LVar(int n)
   {   return &L[n]; }
   void SetLTop(TERM newL)
   {   newL <= LocalTop ? L = newL : localError(); }
   void localINC(int inc)
   { 
	  if ( (L += inc) >= LocalHW ) 
		 if ( (LocalHW = L) >= LocalTop ) 
			localError(); 
	}
TERM LTop() 
{ return L; }
   TERM GetLocal() { return Local; }
   uintC GetLocalHW() { return (uintC)(LocalHW+5-Local); }
   void ResetLocalHW() { LocalHW = L; }

   // Returns a string such as H[23] given a term.
   LString cellname(TERM t);

   // Debugging method and predicate
   void Dump();
   void DumpX(ARITY a);
   TF p_dumpZhxl();
#ifdef LANDFILL
   void DumpCellName(TERM t);
#endif
};


#define MARKFLAG  1
#define REFFLAG   2
#ifdef xBUG_GC
#define NTERMS   10
#endif


class HeapGC
{
private:
   //int   heapGC_OK;
   //TF    GCheapB;
   intC  g_heapbump;     // safety bumper on heap, when to call heapGC

   TERM  Hbegin;         // heap cell from which gc begins
   HXL   *m_phxl;
   LEngine *m_peng;

   MASKptr    pmask;
   MASKptr    pm;

#ifdef xBUG_GC
   TERM  termhist[NTERMS];
   int   ith, ithx;
#endif

public:
   HeapGC(HXL *phxl);
   // this line caused crashes for some reason, but don't
   // think the delete is necessary...
//   ~HeapGC() { delete[] pmask; }            //  ray xxx
   ~HeapGC() {};

   void  GC(ARITY);

private:
   // These used to be the macros.
   BOOL  ON_NEWHEAP(TERM t)
   {   return (HTERM)Hbegin <= (HTERM)(t) && (HTERM)(t) <= (HTERM)m_phxl->H; }
   BOOL  ON_HEAP(TERM t)
   {return (HTERM)m_phxl->Heap<= (HTERM)(t) && (HTERM)(t)<= (HTERM)m_phxl->H;}

   void  pmMARK(TERM t)
   {   pm[(HTERM)(t)-(HTERM)Hbegin] |= MARKFLAG; }
   void  pmREF(TERM t)
   {   pm[(HTERM)(t)-(HTERM)Hbegin] |= REFFLAG; }
   BOOL  pmMARKED(TERM t)
   {   return pm[(HTERM)(t)-(HTERM)Hbegin] & MARKFLAG; } // ray (&&)

#ifdef xBUG_GC
   void VALID_TERM(TERM t)
   {   if (!ON_HEAP(t)) validate_term(t); }
#else
   void VALID_TERM(TERM t) {};
#endif

   void  mark(MASKptr, ARITY);
   void  mark_(HTERM, MASKptr, int);
   void  freecompact(MASKptr, uintC, ARITY);
   void  change_reference(HTERM, MASKptr);
#ifdef xBUG_GC
   void  validate_term(HTERM);
#endif
};

#ifdef BUG_HEAP
class HeapDebug
{
public:
   void  print_cell(TERM, int);
   void  debug_state(ARITY);
   void  debug_heap(TERM, int);
   void  debug_local(TERM, int);
   TF    hpDebugTermLoc(TERM);
   void  heapDump(void);
   void  _offset(TERM);  //used by plmprint_cell - shouldn't need two
private:
   void  PADDR(TERM t)
   {
      hpDebugTermLoc(t);
      //errDebugMsg(" = %08lx ", (intC)*(t));
      //errDebugMsg("\n");
   }

};
#endif // LANDFILL

#endif //HXL_H



/**************************************************************************\
*
* lmath.h -- Math routines
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
\**************************************************************************/

#ifndef LMATH_H
#define LMATH_H

void floatfields(longlong *llp, char *sign, int *exp, longlong *mant);

class RAND 
{                                           // Used by arithmetic functions.
private:
  union
  {
	 TERM     a_list;                          // for real numbers
	 double   a_double; 
    float    a_single;
	 intC     a_int;                           // intC is a system long
	 fixedC   a_fixed;
	 Real     a_real;                          // adumbrated type real
  } ;
  Tag kind;

public:
  RAND()
  { kind = unusedT; }
  // all a tricky business, reals are pointers, someone 'new'd
  // it, and gcth makes a fresh copy, so we can safely delete.
  // so, if all the reals are in rands, and all the rands are
  // on the heap, then this destructor should ensure there are
  // no real memory leaks.  you think?
  ~RAND()
  { if (kind == realS) delete a_real; }

  void clear()
  { if (kind == realS) delete a_real; }

  void setList(TERM l) { clear(); a_list = l; kind = listT; }
  void setDouble(double d) { clear(); a_double = d; kind = doubleS; }
  void setSingle(float s) { clear(); a_single = s; kind = singleS; }
  void setInt(int i) { clear(); a_int = i; kind = intS; }
  void setFixed(fixedC f) { clear(); a_fixed = f; kind = fixedS; }
  void setReal(Real r) { clear(); a_real = r; kind = realS; }

  Tag getKind() { return kind; }
  bool isSingle() { return kind == singleS; }
  bool isDouble() { return kind == doubleS; }
  bool isInt() { return kind == intS; }
  bool isFixed() { return kind == fixedS; }
  bool isReal() { return kind == realS; }
  bool isDecimal() { return isReal() || isFixed(); }
  bool isFloat() { return isSingle() || isDouble(); }

  TERM getList() { return a_list; }
  double getDouble() { return a_double; }
  float  getSingle() { return a_single; }
  intC   getInt()    { return a_int; }
  fixedC getFixed()  { return a_fixed; }
  Real   getReal()   { return a_real; }

  // casts seem to cause problems and maybe at least one
  // bug with the double casts, so back to more conservative approach
   double toDouble() const;
   float  toSingle() const;
   fixedC toFixed() const;
   Real   toReal() const;
   intC   toInt() const;

   Tag classify(const RAND &c) const;         // classify rand types
   void makeCell(LEngine *m_peng, Cell &c);
#ifdef LANDFILL
   void Dump(LEngine *);
#endif
};

typedef RAND *RANDptr;

class LMath
{
private:
   LEngine   *m_peng;

private:
   clock_t e_cpuclock(void);
   double  e_cputime(void);
   double  e_random(void);

public:
   LMath(LEngine *peng);
   void Init();

   void EvalTop(TERM, RAND&);
   void Eval(TERM, RAND&);
   void EvalExp(TERM, RAND&);
	void Eval(RAND&, RAND&, PATOM , RAND&);
   Tag promote(RAND&, RAND&);
   Tag demote(RAND&, RAND&);
   void evalAtom( PATOM a, RAND &value );
   void evalInt1(RAND&, PATOM, RAND&);
   void evalInt2(RAND&, RAND&, PATOM, RAND&);
   void evalFixed1(RAND&, PATOM, RAND&);
   void evalFixed2(RAND&, RAND&, PATOM , RAND&);
   void evalReal1(RAND&, PATOM, RAND&);
   void evalReal2(RAND&, RAND&, PATOM, RAND&);
   void evalSingle1(RAND&, PATOM, RAND&);
   void evalSingle2(RAND&, RAND&, PATOM, RAND&);
   void evalDouble1(RAND&, PATOM, RAND&);
   void evalDouble2(RAND&, RAND&, PATOM, RAND&);

   void intPow(RAND& rand1, RAND& rand2, RAND& value);
   //void RealModu(Real, intC, RAND&);   
   //void RealModu(Real, Real, RAND&);   

	int signum(long i)
	  {   return i < 0 ? -1 : (i > 0 ? 1 : 0);}
	float signum(float f)
	  {   return (float)(f < 0 ? -1.0 : (f > 0 ? 1.0 : 0));}
	double signum(double f)
	  {   return f < 0 ? -1.0 : (f > 0 ? 1.0 : 0);}
   int signum(fixedC n)
   { return (n.getHI() == 0) && (n.getLO() == 0) ? 0 :
          (n.isNeg() ? -1 : 1); }
	double intpartnum(double fval)
	  { return signum(fval) * (double)floor(fabs(fval)); }
	double fracpartnum(double fval)
	  { return fval - intpartnum(fval); }

  void sqrtReal(LReal &, LReal &);
  TERM  realToList(Real);
  TERM  gigitsList(Real);

  // Predicates

  TF p_is(void);
  TF p_numeq(void);
  TF p_almost_equal(void);
  TF p_gt(void);
  TF p_lt(void);
  TF p_arith_plus(void);

   // Number Types
   TF p_integer(void);
   //TF p_long(void);
   //TF p_short(void);
   TF p_fixed_real(void);
   TF p_long_real(void);
   TF p_real(void);
   TF p_single(void);
   TF p_double(void);
   TF p_float(void);

      TF p_divrem(char);

      TF p_newReal(void);
   TF p_truncate(void);

   TF p_is_integer(void);
   TF p_is_odd(void);
   TF p_fraction(void);

   TF p_nth(void);

   TF p_int_real(void);
   TF p_float_real(void);
   TF p_real_list(void);
   TF p_real_components(void);
    // Primes
  TF p_makePrimes();                   // makePrimes(Lo, Hi, Primes)
  TF p_Primes();                       // get prime array length 
  TF p_nthPrime();                     // get indexed element 
  TF locatePrime();                    // get index of element 
  TF p_deletePrimes();

};
#endif


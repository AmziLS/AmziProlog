//-*-C++-*-

#ifndef FIXED_H
#define FIXED_H

enum signs_ {bothPos_, Aneg_, Bneg_, bothNeg_};

class RAND;

//struct FIXEDC;
//typedef struct FIXEDC fixedC;
class fixedC;

fixedC floatToFixed(double d);
bool isGigit(aINT32 i);
bool isAbsGigit(aINT32 i);


// A special type of real, with just two gigits, so it can
// be squished into a cell for fast Prolog processing.  The sign
// is the high order bit of the 'hi' gigit.
// NOTE that the 'hi' gigit cannot be negative, that is, when the
// sign bit is set negative, hi is still in positive format.

class fixedC
{
private:
  Gigit  hi;
  Gigit  lo;
  
public:
   friend Lostream &operator<<( Lostream &os, fixedC &f );

   //fixedC()
   //{ hi = 0; lo = 0; }
   //fixedC(aINT32 h, aINT32 l)  // called from cell or people who know hi format
   //{ hi = h; lo = l; }
   //fixedC(char s, aINT32 h, aINT32 l)
   //{ setHI(h); setLO(l); setSign(s); }
   void init_hilo(aINT32 h, aINT32 l)
   { hi = h; lo = l; }
   void init_shilo(char s, aINT32 h, aINT32 l)
   { setHI(h); setLO(l); setSign(s); }

   Gigit getHI()
   { return hi & 0x7fffffff; }
   Gigit getLO()
   { return lo; }

   void setHI(aINT32 h)
   { LASSERT((isGigit(h)), aS("assigning non-gigit fixed HI")); hi = h; }
   void setLO(aINT32 l)
   { LASSERT((isGigit(l)), aS("assigning non-gigit fixed LO")); lo = l; }

   // take a standard signed hi, and create proper gigit real with correct sign
   void setSignedHI(aINT32 h)
   { if (h < 0) { hi = -h; setSign('-'); }
     else hi = h;
     LASSERT(isGigit(getHI()), aS("set signed non-gigit HI")); }

   // these return the actuals, for copying into cells, not the
   // gigits
   aINT32 getFixedHI()
   { return hi; }
   aINT32 getFixedLO()
   { return lo; }

   void setFixedHI(aINT32 h)
   { hi = h; }
   void setFixedLO(aINT32 l)
   { lo = l; }


  void add(fixedC &, const char, RAND &);
  void mult(fixedC &, const char, RAND &);

  //  RAND operator+(const FIXEDC &n) const
  //	 {RAND result; return add(n, '+', RAND); }

  char getSign() const
	 { return hi & 0x80000000 ? '-' : '+'; }

  void setSign(char c) 
	 { 
		if(c == '-')
		  hi |= 0x80000000;
		else
		  hi &= ~0x80000000;
	 }

  void setNeg()
  { hi |= 0x80000000; }
  void setPos()
  { hi &= 0x7fffffff; }

  void negate()
  { if (isNeg()) setPos(); else setNeg(); }

  bool isZero() const
  { return ((hi & 0x70000000) == 0) && lo == 0; }

  bool isPos() const
  { return (hi &0x80000000) == 0; }

  bool isNeg() const
  { return (hi &0x80000000) == 0x80000000; }

  bool isInteger() const
  { return lo == 0; }

  fixedC abs() const
  { fixedC result = *this; result.hi &= 0x7fffffff; return result; }

  void abs()
  { hi &= 0x7fffffff; }

  aINT32 floor()
  { return isZero() ? 0 : (hi < 0 ? -(hi & 0x7fffffff) - 1 : hi);}

  aINT32 ceil()
  { return isZero() ? 0 : (hi < 0 ? -(hi & 0x7fffffff) : hi + 1);}

  aINT32 round()
  {
	 aINT32 hi = getHI() & 0x7fffffff;
	 if(isPos())
		return getLO() >= 500000000 ? hi + 1 : hi;
	 else
		return -(getLO() >= 500000000 ? hi + 1 : hi);
}

  double toDouble(void) const;
  aINT32 toInt() const
  { return hi; }

  signs_ signs(const fixedC &B, const char opSign) const
  {
	 if(opSign == '-')
		return getSign() == '+' ? (B.getSign() == '+' ? Bneg_ : bothPos_) :
		(B.getSign() == '+' ? bothNeg_ : Aneg_) ;
	 else
		return getSign() == '+' ? (B.getSign() == '+' ? bothPos_ : Bneg_) :
		(B.getSign() == '+' ? Aneg_ : bothNeg_) ;
  }	
	 
  char compare(const fixedC &) const; 
  char compareAbs(const fixedC &) const;

  //operator double();

  //fixedC &operator=(fixedC &f)
  //{ hi = f.hi; lo = f.lo; return *this; }
  bool operator==(fixedC &f)
  { if (f.hi == hi && f.lo == lo) return true;
    else return false; }

  bool operator==(aINT32 i)
  { if (i > 0 && hi > 0 && getHI() == i && getLO() == 0) return true;
    else if (i < 0 && hi < 0 && getHI() == -i && getLO() == 0) return true;
    else return false; }

  fixedC operator-()
  {
	 fixedC result;
	 result.hi = (hi & 0x7fffffff) | (~hi & 0x80000000); 
	 result.lo = lo; 
	 return result; 
  }

#ifdef LANDFILL
  void Dump();
  void Dump(LEngine *m_peng);
#endif

};

#endif

























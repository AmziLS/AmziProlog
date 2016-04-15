/**************************************************************************\
* 
* lreal.h -- Amzi! Real Numbers, by Ray Reeves
* 
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
*
\***************************************************************************/

#ifndef LREAL_H
#define LREAL_H

// make this a cfg/prolog_flag some day
#define MAX_REAL 100000

void floatToReal(double d, LReal &result);
Real listToReal(TERM list);
TF almostEqual(LReal &r1, LReal &r2);

//enum errors_ {numberCast, numberDomain, indexError, sizeError, zeroDivide, badArg, badOp};  // ray
enum combo_ { sum_, diff_, prod_, div_};   // for makeReal

extern const int fullgig;
extern const int halfgig;
//extern const LReal realMinusOne;

//#define realZero LReal((Gigit)0)
//#define realOne LReal((Gigit)0)

extern const LReal realZero;
extern const LReal realOne;
extern const LReal realTwo;

//class RealException
//{
//public:
//   aCHAR* msg;
//   RealException(aCHAR *m)
//   { msg = m; }
//};

class LReal
{
private:
  Gigit *m_gigits;                   // gigit array
   aINT32  m_offset;                  // offset of array from alloc space
   aINT32  m_length;                  // number of active gigits
   aINT32  m_exponent;
   aINT32  m_array_length;            // how much room we have
   char   m_sign;

public:

   friend Lostream& operator<<( Lostream &os, LReal &lr );
   friend TF almostEqual(LReal&, LReal&);

   virtual ~LReal()
 	{ 
	  if(m_gigits != 0)
		 {
			Gigit *p = m_gigits - m_offset; 
			//			std::cout << "p " << p <<'\n';
			delete[] p;
		 }
	}

   LReal();
   LReal( const LReal &lr );
   LReal(Gigit i );
   LReal(longlong ll);
   LReal(double d);
   LReal(fixedC f);
   LReal(aINT32 length, char sign, aINT32 exponent);
   LReal(fixedC, aINT32);
   LReal(aCHAR* s);
#if defined(UNIX) && defined(P64)
   LReal(long l);
#endif

   void Init(aINT32 length, char sign, aINT32 exponent);
   void Init(LReal&, LReal&, combo_);
   void Init(LReal &A, LReal &B, combo_ c, int delta);
   void Init(double d);

   float toSingle();
   double toDouble();
   fixedC toFixed();
   aINT32 toInt();
   void toString(aCHAR *s, int len);
   bool isIntegerable();

   LReal& operator=( const LReal & );
   Gigit &operator[]( int i )
   { LASSERT(inRange(i), aS("LReal::operator[]")); return m_gigits[i]; }
   bool operator==( const LReal & );
   bool operator==( fixedC & );
   bool operator==( aINT32 );
   bool operator<( const LReal &r )
   { return compare(r) == '<' ? true : false; }
   bool operator>( const LReal &r )
   { return compare(r) == '>' ? true : false; }
   bool operator<=( const LReal &r )
   { char s = compare(r); return (s == '<' || s == '=') ? true : false; }
   bool operator>=( const LReal &r )
   { char s = compare(r); return (s == '>' || s == '=') ? true : false; }
   bool operator<( int n )
   { return compare(n) == '<' ? true : false; }
   bool operator>( int n )
   { return compare(n) == '>' ? true : false; }
   bool operator<=( int n )
   { char s = compare(n); return (s == '<' || s == '=') ? true : false; }
   bool operator>=( int n )
   { char s = compare(n); return (s == '>' || s == '=') ? true : false; }
   LReal operator*( long n)
   { LReal r; mult(n,r); return r; }
   LReal operator*( LReal& lr)
   { LReal r; mult(lr,r); return r; }
   LReal operator+(LReal &n)
   { LReal r; add(n, '+', r); return r; }
   LReal operator+(aINT32 n)
   { LReal r; add(n, '+', r); return r; }
   LReal operator-(LReal &n)
   { LReal r; add(n, '-', r); return r; }
   LReal operator-(aINT32 n)
   { LReal r; add(n, '-', r); return r; }
   LReal operator-()
   { LReal r(*this); r.negate(); return r; }
   LReal operator>>(int n)
   { LReal r; rshift(n, r); return r; }
   LReal operator<<(int n)
   { LReal r; lshift(n, r); return r; }

   // gets

   int getLen() const
   { return m_length; }
   int getExp() const
   { return m_exponent; }
   int msExp() const             // most significant Col
   { return m_exponent + m_length - 1; }
   int lsExp() const            // least significant Col
   { return m_exponent; }
  aINT32 * refCol(const int Col) const         // address of Col
  { return (aINT32*)(m_gigits + Col - m_exponent);  }
   char getSign() const
   { return m_sign; }
   // getGd range 1->length  (not 0->length-1) - also note
   // that out of range represents exponent or the like, so
   // a return value of 0 is appropriate.
   Gigit getGd(int i) const
   { return i < 1 || i > getLen() ? 0 : m_gigits[i-1]; }
   aINT32 getGd0(int i) const               // including descr
   { return i < 0 || i > getLen() ? 0 : m_gigits[0];}

   // sets

   void setSign(char s)
   { m_sign = s; }
   void setExp(int e)
   { m_exponent = e; }
   void setLen(int l)
   { LASSERT(inRange(l), aS("LReal::setLen")); m_length = l; }
   void setOffset(int o)
   { 
	  LASSERT(inRange(o), aS("LReal::setOffset")); 
	  m_offset = o; 
	  m_gigits += o;
	}
   void setMaxOffset()
	  {m_offset = m_array_length - (m_length); m_gigits += m_offset; }
   void setGd(int i, Gigit val)
   { LASSERT(inRange(i), aS("LReal::setGd")); m_gigits[i-1] = val; }
   void setNeg()
   { setSign('-'); }
   void setPos()
   { setSign('+'); }
   void setGigits(Gigit *gs)
   { delete m_gigits; m_gigits = gs; }
	void nullify()
	  { for(aINT32 *p = (aINT32*)(m_gigits + m_length - 1); p >= (aINT32*)m_gigits; p--) *p = 0;}

   // modifies

   void incLen()
   { LASSERT(inRange(m_length+1), aS("LReal::incLen")); m_length++; }
   void incLen(int i)
   { LASSERT(inRange(m_length+i), aS("LReal::incLen")); m_length+=i; }

   void decLen()
   { LASSERT(inRange(m_length-1), aS("LReal::decLen")); m_length--; }
   void decLen(int i)
   { LASSERT(inRange(m_length-i), aS("LReal::decLen")); m_length-=i; }

   void incExp()
   { m_exponent++; }
   void incExp(int e)
   { m_exponent+= e; }
   void decExp()
   { m_exponent--; }
   void decExp(int e)
   { m_exponent-= e; }

   // Exp refers to the math column number:  // ... |2|1|0(units)|-1|-2| ...
   // ex.  111111111222222222333333333.444444444555555555 =
   //      [5s, 4s, 3s, 2s, 1s]
   //  cols -2  -1   0   1   2
   // getCol(Col) gets the gigadigit at Col. Zero when out of range.
   Gigit getCol(int Col) const       // digit at arithmetic column
   { return getGd(1 + Col - lsExp());  }
   int unitsIx()  const                  // virtual index of Col 0
   { return 1 - lsExp(); }                

  aINT32 *lsa() const
  { return (aINT32*)m_gigits;}
  aINT32 *msa() const
  { return (aINT32*)(m_gigits + m_length - 1);}
   Gigit msg()                        // most significant gigadigit
   //{ return (this + getLen())->g;}
   { return m_gigits[m_length - 1]; }
   aINT32 msg1()                       // 2nd most significant gigadigit
   //{ return getLen() < 2 ? 0 : (this + getLen() - 1)->g;}
   { return getLen() < 2 ? 0 : m_gigits[m_length - 2]; }
   aINT32 msg2()                       // 3rd most significant gigadigit
   //{ return getLen() < 3 ? 0 : (this + getLen() - 2)->g;}
   { return getLen() < 3 ? 0 : m_gigits[m_length - 3]; }

   int signum()                       // assumes normalized
   { return getSign() == '-' ? -1 : (isZero() ? 0 : 1); }

   // tests
   bool isFixable()
   { return (lsExp() == -1 && getLen() < 3) || 
      (lsExp() == 0 && getLen() == 1); }
   bool isPos() const
   { return m_sign == '+'; }
   bool isNeg() const
   { return m_sign == '-'; }
   bool isUnitary() const
   { return (m_length == 1 && m_exponent == 0); }
   bool isZero() const
	  {                                    // valid for non-normalized numbers
	  for(int i = 0; i < m_length; i++)
		 if(m_gigits[i] != 0) 
			return false; 
	  return true; 
	}
   bool isOne() const
   { return isUnitary() && getSign() == '+' && m_gigits[0] == 1; }
   bool isOdd() const
   { return getCol(0) & 1; }             // meaningless if not integer
   bool isInteger() const
   { return lsExp() >= 0; }

   bool inRange(int i) const
   { return i >= 0 && i <= m_array_length; }


   // functions

   char compare(long n);
   char compare(const LReal &B);
   char compareAbs(const LReal &B);
   void powerR(longlong x, long n, LReal &); // fast exponentiation continued
   void pow(int, int, int, LReal &);
   void mult(long, LReal &result);
   //Real mult(LReal &);
   void mult(LReal &b, LReal &result);
   void intDiv(LReal &Bin, LReal &Remp, int delta, LReal &result);
   void div(LReal &Divisor, int delta, LReal &result);
   void div(LReal &Divisor, int delta, bool intDivFlag, LReal &result);
   void divisionLoop(LReal &B, int delta, bool intDivFlag, LReal &result);
   void add(long, char, LReal &result);
   void add(LReal &, char, LReal &result);
   void rshift(int, LReal &result) const;
   void rshift32(int) const;
   void lshift(int, LReal &result) const;
   void lshift32(int) const;
   void floor(LReal &result);
   void ceil(LReal &result);
	void round(LReal &);
   void absReal(LReal &result); 
   void rxor(LReal &B, LReal &result);
   void divU(LReal &r2, aINT32 delta, LReal &result);
   void divS(LReal &r2, aINT32 delta, LReal &result);
   void modS(LReal &r2, aINT32 mod, aINT32 delta, LReal &result);
   void modS(LReal &r2, LReal &mod, aINT32 delta, LReal &result);
   void modU(LReal &r2, aINT32 mod, aINT32 delta, LReal &result);
   void modU(LReal &r2, LReal &mod, aINT32 delta, LReal &result);
   void modulus(aINT32 mod, aINT32 delta, LReal &result);
   void truncate(long epsilon, LReal &);
   void truncate(long epsilon);

   Real maxReal(LReal &n)  
   {  Real result = compare(n) == '<' ? &n : this;
      return result; }

   Real minReal(LReal &n) 
   {  Real result = compare(n) == '>' ? &n : this;
      return result; }

   // utilities

   //double realToFloat();
	void prepend(Gigit);
   void trimToLen(int, LReal &result);
	void trimToEpsilon(int);
   void trimToPrecision(int);
   void normalize();
   void negate()                              // mutates 
   { if(getSign() == '+') setSign('-'); else setSign('+'); }
   signs_ signs(LReal &B, char opSign);
   void intPart(LReal &);

//#ifdef LANDFILL
   void Dump();
   void Dump(LEngine *m_peng);
//#endif
	void showReal(char *);

};

#endif //LREAL_H

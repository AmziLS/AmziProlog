/**************************************************************************\
* 
* lreal.cpp -- Amzi! Real Numbers, by Ray Reeves
* 
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
\***************************************************************************/

#include "inc.h"
#include "pch.h"

#ifdef LANDFILL
#define noBUG_REAL
#define noBUG_REAL_MATH
#define noBUG_DIV
#define noBUG_FLOAT
#endif

const int fullgig = 1000000000;
const int halfgig = 500000000;
//const LReal realMinusOne((Gigit)-1);
const LReal realZero((Gigit)0);
const LReal realOne((Gigit)1);
const LReal realTwo((Gigit)2);


void floatToReal(double d, LReal &result)
{
  //GD realTwo[]  = {0x7e000001, 2};
  //Real newTwo;
  //LNEWX(newTwo, LReal((Gigit)2));
  longlong *llp = (longlong *)&d;
  longlong significand;
#ifdef BUG_FLOAT
long *kludge;
std::cout.precision(18);
std::cout << "===>>> floatToReal  d = " << d << HEXOUTP(d) << NL;
kludge = (long*)llp;
std::cout << " hex d = " << HEXOUT(kludge[1]) << HEXOUT(kludge[0]) << NL;
#endif
  int exponent;
  char sign;
  if(d == 0)
  {
    //LNEWX(r, LReal((Gigit)0));
    //return r;
     result = realZero;
     return;
  }
  floatfields(llp, &sign, &exponent, &significand);
#ifdef BUG_FLOAT
 double def = pow(2, exponent-51);
 double sig = (double)significand;
 double dagain = def * sig;
 std::cout << "  def = " << def << NL;
 std::cout << "  sig = " << sig << NL;
 std::cout << "  dagain = " << dagain << NL;
#endif
  //Real expfactor = newTwo->pow(exponent-51, delta);
  LReal two(realTwo);
  //LReal two((Gigit)2);
  LReal expfactor;                             // not a fraction
#ifdef BUG_FLOAT
  std::cout << "two" << NL;
  two.Dump();
  std::cout << "expfactor" << NL;
  expfactor.Dump();
#endif
  two.pow(exponent-51, 2, -2, expfactor);      // hardwire delta and epsilon
  //LNEWX(r, LReal(significand));
  LReal r(significand);
  if (sign == ' ') sign = '+';
  r.setSign(sign);
#ifdef BUG_FLOAT
std::cout << "  sign = " << sign << NL;
std::cout << "  exponent = " << exponent << HEXOUTP(exponent) << NL;
//std::cout << "  significand = " << HEXOUTP(significand) << NL;
kludge = (long*)&significand;
std::cout << "  significand = " << HEXOUT(kludge[1]) << HEXOUT(kludge[0]) << NL;
std::cout << "  expfactor:" << NL;
expfactor.Dump();
std::cout << "  r before multiplying by exponent:" << NL;
r.Dump();
#endif
  //(r->mult(*expfactor))->assignTo(r);
  //r = r->mult(*expfactor);
  //r.mult(expfactor, r);
  r = r * expfactor;
#ifdef BUG_FLOAT
std::cout << "  r after exponent applied:" << NL;
r.Dump();
#endif
  //result = r->trimToLen(3);      // floats are <4 gigits
  // we now have 19 digits of precision, need to throw
  // away 3 which are meaningless
  r.trimToPrecision(15);
#ifdef BUG_FLOAT
std::cout << "  r after precision applied:" << NL;
r.Dump();
#endif
  result = r;
  return;
}

TF almostEqual(LReal &r1, LReal &r2)
{
   if (r1.getSign() != r2.getSign())
      return FALSE;
   if ((r1.getExp() + r1.getLen()) != (r2.getExp()+r2.getLen()))
      return FALSE;
   int i,j;
   int ii, jj;

   for (i = r1.getLen()-1, j = r2.getLen()-1; (i >= 0 && j >= 0); i--, j--)
   {
      if (i>0 && j>0)
      {
         if (r1[i] != r2[j])
            return FALSE;
      }
      else
      {
         // assume trailing zeroes are not significant
         for (ii=r1[i], jj=r2[j]; (ii > 0 && jj > 0); ii/=10, jj/=10)
         {
            if ( ii%10 == 0 || jj%10 == 0 )
               continue;
            else
               if (ii-jj > 1 || jj-ii > 1)
                  return FALSE;
               else
                  return TRUE;
         }
      }
   }
   return TRUE;
}

Real listToReal(TERM list)
{
  TERM List = list;
  int length, pointix = 0;
  TERM t;
  char sign = aS('+');

  for (length = 0; List->IsList(); List = (++List)->dref())
	 {                                        // get length, sign & point
		List = List->getTerm();
		t = (List)->dref();
		if(t->IsAtom())
		  if(length == 0)
			 if(Lstrcmp(*(t->getAtom()), aS("-")) == 0)
			 //			 if(t->getAtom() == pATAB->minusA)
				sign = aS('-');                  // negative sign
			 else
				throw LExcept(number_domainE, (STRptr)*(t->getAtom()));  // numberDomain;                     // unknown atom
		  else
			 if(Lstrcmp(*(t->getAtom()), aS(".")) == 0)
				//			 if(t->getAtom() == pATAB->periodA)
				pointix = length;
			 else
				throw LExcept(number_domainE, (STRptr)*(t->getAtom()));                     // unknown atom
		else                                   // not an atom
		  if (t->IsInt())                      // each element must be int 
			 length++;
		  else
			 throw LExcept(number_domainE, aS("non integer")); 
	 }
  int exp = pointix ? pointix - length : 0;
  //Real r = ::newReal(length, exp, sign);
  Real r;
  LNEWX(r, LReal(length, sign, exp));
  List = list;
  for(int i = length;  List->IsList(); List = (++List)->dref())
	 {
		List = List->getTerm();
		t = (List)->dref();
		if (t->IsInt())
		  {
			 Gigit gigit = (aINT32)(t->getInt());
			 if(gigit < 0)
          {
            delete r;  // don't need it anymore
				throw LExcept(number_domainE, aS("negative gigit"));
          }
			 r->setGd(i--, gigit);              // transfer gigadigit
		  }
	 }
  r->normalize();
  return r;
}

int significantDigits(Gigit g)
{
   int i, ans;
   for (i = 100000000, ans = 9; (i > 0 && g/i == 0); i /= 10, ans--)
      ;
   return ans;
}

void dpdivide(aINT32 hi, aINT32 lo, aINT32 divisor, aINT32 *q, aINT32 *rem)
{
  longlong dividend = ((longlong)hi * fullgig) + lo;
  *q = (aINT32)dividend/divisor;
  *rem = (aINT32)dividend % divisor;
}

aINT32 gdAdd(aINT32 &carry, aINT32 a, aINT32 b)  // no Reals involved
{     // given carry in and 2 gigadigits, return sum & carry out mod 1 gig
  Gigit result = a + b + carry;
  carry = 0;
  if(result >= fullgig) 
	 {
		carry = 1;
		result -= fullgig;
	 }
  else
	 if(result < 0) 
		{
		  carry = -1;
		  result += fullgig;
		}
  return result;
}

Lostream& operator<<( Lostream &os, LReal &lr )
{
   aCHAR buf[512];
   lr.toString(buf,511);
   os << "Real " << buf << NL;
   os << "sign: " << lr.m_sign << "  exp: " << lr.m_exponent <<
           "  len: " << lr.m_length << "  array_length: " << lr.m_array_length << NL;
   os << "[ ";
   char f = std::cout.fill('0');
   int w = (int)std::cout.width(9);
   for (int i = lr.getLen()-1; i >= 0; i--)
   {
      os << std::setfill('0') << std::setw(9) << lr.m_gigits[i] << SP;
   }
   os.fill(f);
   os.width(w);
   os << "]" << NL;
   return os;
}

//----------------------------------------------------
// Constructors
//

LReal::LReal()
{
   m_offset = 0;
   m_length = 0;
   m_exponent = 0;
   m_array_length = 0;
   m_sign = '+';
   m_gigits = NULL;
}

LReal::LReal(aINT32 length, char sign, aINT32 exponent)
{
  m_offset = 0;
   m_length = length;
   m_exponent = exponent;
   m_array_length = length;
   m_sign = sign;
   LNEWX(m_gigits, Gigit[m_array_length]);
   memset(m_gigits, 0, m_array_length * sizeof(Gigit));
	//	std::cout << "new lse " << m_gigits << '\n';
}

LReal::LReal( const LReal &lr )
{
  m_offset = 0;                        // do not propogate offset
   m_exponent = lr.m_exponent;
   m_array_length = m_length = lr.m_length;
   m_sign = lr.m_sign;
   LNEWX(m_gigits, Gigit[m_length]);
   memcpy(m_gigits, lr.m_gigits, m_length * sizeof(Gigit));
   //for (int i=0; i < m_array_length; i++)
   //   m_gigits[i] = lr.m_gigits[i];
	//	std::cout << "new copy " << m_gigits << '\n';
}

// promote a long (maybe >= gigit)
LReal::LReal(Gigit i)
{
  //aINT32 Q, R, gigadigit = abs(i);
   aINT32 abs_i = abs(i);
   m_offset = 0;
   m_length = m_array_length = abs_i >= fullgig ? 2 : 1;

   LNEWX(m_gigits, Gigit[m_array_length]);
   m_sign = i < 0 ? '-' : '+';
   m_exponent = 0;
   if(m_length == 2)
   {
      aINT32 Q, R;
      dpdivide(0, i, fullgig, &Q, &R);
      m_gigits[1] = Q;
      m_gigits[0] = R;
   }
   else 
      m_gigits[0] = abs_i;
   normalize();
}

LReal::LReal(double d)
{
  m_offset = 0;
   m_gigits = NULL;   // must initialize as = will delete what was there
   LReal r;
   floatToReal(d, r);
   *this = r;
   //m_sign = r->m_sign;
   //m_exponent = r->m_exponent;
   //m_length = r->m_length;
   //m_array_length = r->m_array_length;
   //LNEWX(m_gigits, Gigit[m_array_length]);
   //memcpy(m_gigits, r->m_gigits, m_array_length * sizeof(Gigit));
   ////for (int i=0; i<m_array_length; i++)
   ////   m_gigits[i] = r->m_gigits[i];
   //delete r;
   //normalize();
}

void LReal::Init(double d)
{
   //LReal r;
   floatToReal(d, *this);
   //*this = r;
   //m_sign = r->m_sign;
   //m_exponent = r->m_exponent;
   //m_length = r->m_length;
   //m_array_length = r->m_array_length;
   //LNEWX(m_gigits, Gigit[m_array_length]);
   //memcpy(m_gigits, r->m_gigits, m_array_length * sizeof(Gigit));
   ////for (int i=0; i<m_array_length; i++)
   ////   m_gigits[i] = r->m_gigits[i];
   //delete r;
   //normalize();
}

LReal::LReal(fixedC f)
{                                         // real from an overflowed fixed
  //Real result = new GD[3];
  //m_sign = f.hi & 0x80000000 ? '-' : '+' ;
  m_offset = 0;
   m_sign = f.getSign();
  m_array_length = 3;
  m_length = 2;
  m_exponent = -1;
  LNEWX(m_gigits, Gigit[m_array_length]);
  setGd(1, f.getLO());
  //setGd(2, f.hi & 0x7fffffff);
  setGd(2, f.getHI());
  normalize();
}

LReal::LReal(longlong l)
{                                         // promote a longlong
    longlong absl = l < 0 ? - l : l;
    aINT32 R = absl % fullgig;                    // remainder
    longlong Q = absl/fullgig;                  // quotient
    int length = Q > fullgig ? 3 : (Q > 0 ? 2 : 1);
    int size = length + 1;
    
    //Real result = new GD[size];
    m_offset = 0;
    m_array_length = size;
    m_length = length;
    m_sign = l < 0 ? '-' : '+';
    m_exponent = 0;
    LNEWX(m_gigits, Gigit[m_array_length]);
    //result->setDescr(length, 0, l < 0 ? '-' : '+');
    
    switch(m_length)
    {                                     // set quotient
        case 3:                               // Q is more than 1 gig
            setGd(3, (Gigit)(Q/fullgig));
            setGd(2, Q % fullgig);
            break;
        case 2:
            setGd(2, (Gigit)Q);
            break;
    }
    setGd(1, R);                    // set remainder
    normalize();
}

#if defined(UNIX) && defined(P64)
// Is this correct?  It was added to stop GNU from complaining in 64-bit mode
// but I'm not sure if this constructor is correct for a single long.
LReal::LReal(long ll)
{
    long long l = (long long) ll;
    // promote a longlong
    longlong absl = l < 0 ? - l : l;
    aINT32 R = absl % fullgig;                    // remainder
    longlong Q = absl/fullgig;                  // quotient
    int length = Q > fullgig ? 3 : (Q > 0 ? 2 : 1);
    int size = length + 1;
    
    //Real result = new GD[size];
    m_offset = 0;
    m_array_length = size;
    m_length = length;
    m_sign = l < 0 ? '-' : '+';
    m_exponent = 0;
    LNEWX(m_gigits, Gigit[m_array_length]);
    //result->setDescr(length, 0, l < 0 ? '-' : '+');
    
    switch(m_length)
    {                                     // set quotient
        case 3:                               // Q is more than 1 gig
            setGd(3, (Gigit)(Q/fullgig));
            setGd(2, Q % fullgig);
            break;
        case 2:
            setGd(2, (Gigit)Q);
            break;
    }
    setGd(1, R);                    // set remainder
    normalize();
}
#endif

LReal::LReal(fixedC f, aINT32 carry)
{ // real from an overflowed fixed
  //LNEWX(m_rep, LRealRep(3, carry < 0 ? '-' : '+', -1));
  m_offset = 0;
   m_length = m_array_length = 3;
   m_sign = carry < 0 ? '-' : '+';
   m_exponent = -1;
   LNEWX(m_gigits, Gigit[m_array_length]);
  //setGd(1, f.lo);
  //setGd(2, f.hi);  // was this a bug for negatives?
  setGd(1, f.getLO());
  setGd(2, f.getHI());
  setGd(3, 1);
  normalize();
}

void LReal::Init(LReal &A, LReal &B, combo_ c)
{                                         // make room for result of arith op
  int length, msExp, lsExp, exp;
  //Real result;
  
  switch(c)
	 {
	 case sum_:
		msExp = max(A.msExp(), B.msExp());
		lsExp  = min(A.lsExp(),  B.lsExp());
		length = 2 + msExp - lsExp;         // # of gigadigits + carry space
		exp    = min(A.lsExp(), B.lsExp());
		break;
	 case prod_:
		length = A.getLen() + B.getLen();   // max # of gigadigits
		exp    = A.lsExp() + B.lsExp();     // do exponent right here
		break;
	 case div_:
	 default:                              // RealException(aS("bad combo"));
      throw LExcept(internalE, aS("bad combo for LReal")); 
		//return NULL;
	 }
  if(length > MAX_REAL)
	 //pXCPT->Error(indexE, aS("real"), length);
    throw LExcept(number_sizeE, aS("real"));
  int size = 1 + length;
  //result = new GD[size];                  // length + descr
  //LNEWX(result, GD[size]);                  // length + descr
  //if (result == NULL)
  //   throw RealException(aS("out of memory"));
  //result->setDescr(length, exp, '+');
  //for(int i = 1; i <= length; i++)
  //	 result->setGd(i, 0);
  //LNEWX(m_rep, LRealRep(size, '+', exp));
  Gigit *p = m_gigits - m_offset; 
  delete p;
  m_offset = 0;
  m_length = length;
  m_array_length = size;
  m_sign = '+';
  m_exponent = exp;
  LNEWX(m_gigits, Gigit[m_array_length]);
  memset(m_gigits, 0, m_array_length * sizeof(Gigit));
}

void LReal::Init(LReal &A, LReal &B, combo_ c, int delta)
{ // for divides, we need to know the delta as well
  int length, exp;
  int numLen, denomLen;
  //Real result;
  
  switch(c)
	 {
	 case div_:
		numLen   = A.getLen();
		denomLen = B.getLen();
		//delta = abs(pSTATE->m_delta);
		length = (numLen > denomLen ? numLen : denomLen) + delta;
		exp = A.lsExp() - B.lsExp();
		break;
	 case sum_:
	 case prod_:
	 default:
      throw LExcept(internalE, aS("bad combo in LReal"));
		//return NULL;
	 }
  if(length > MAX_REAL)
	 //pXCPT->Error(indexE, aS("real"), length);
    throw LExcept(number_sizeE, aS("real"));
  int size = 1 + length;
  //result = new GD[size];                  // length + descr

  //LNEWX(m_rep, LRealRep(length, '+', exp));
  m_offset = 0;
  m_length = length;
  m_array_length = size;
  m_sign = '+';
  m_exponent = exp;
  delete m_gigits;
  LNEWX(m_gigits, Gigit[m_array_length]);
  memset(m_gigits, 0, m_array_length * sizeof(Gigit));
  //LNEWX(result, GD[size]);                  // length + descr
  //if (result == NULL)
  //   throw RealException(aS("out of memory"));
  //result->setDescr(length, exp, '+');
  //for(int i = 1; i <= length; i++)
//	 result->setGd(i, 0);
//  return result;
}

LReal::LReal(aCHAR *string)
{
   aCHAR *s = string;
   aCHAR *start;
   aCHAR *end;
   //int sign;
   int intLen, size, fracLen;
   //Real r;
   int exp; 
   int explicit_exp;
   int last_gigit_length;
   //long *p;

   // bump past the sign, remembering its there for when
   // we create a real.
   if (*s == aS('-'))
   {
      m_sign = '-';  // an int
      s++;
   }
   else
      m_sign = '+';

   start = s;
   intLen = 1;
   while ((*++s >= aS('0') && *s <= aS('9'))) intLen++;
   size = (intLen % 9) ? 1 + intLen/9 : intLen/9;  // size of real int part

   fracLen = 0;
   if (*s == aS('.'))
      while ((*++s >= aS('0') && *s <= aS('9'))) fracLen++;
   end = s;

   if (fracLen > 0)
   {
      exp = - ((fracLen % 9) ? 1 + fracLen/9 : fracLen/9);
      size -= exp;
   }
   else
      exp = 0;

   //r = ::newReal(size, exp, sign);
   //LNEWX(r, LReal(size, sign, exp));
  m_offset = 0;
   m_length = m_array_length = size;
   LNEWX(m_gigits, Gigit[m_array_length]);

   explicit_exp = 0;
   if (*s == aS('r') || *s == aS('R') ||
       *s == aS('g') || *s == aS('G') )
   {
      s++;
      if ( (*s >= aS('0') && *s <= aS('9')) ||
           *s == aS('-') || *s == aS('+') )
         explicit_exp = Latol(s);
   }

   // we can now set the full exponent
   m_exponent = exp + explicit_exp;

   // trim whatever's there, for the backward walk through
   // 'the number'.
   *end = EOS;

   // and now the digits
   //p = (long*)(r+1);
   int gi = 0;
   if (fracLen > 0)
   {
      last_gigit_length = fracLen % 9;
      if (last_gigit_length > 0)  // special handling for last gigit
      {
         end -= last_gigit_length;
         m_gigits[gi] = (Gigit)Latol(end);   //*p = Latol(end);
         
//std::cout << end << " --> " << *p << NL;
         for(int i=9; i > last_gigit_length; i--)
            m_gigits[gi] *= 10;   //*p *= 10;
//std::cout << end << " --shifted--> " << *p << NL;
         gi++;  //p++;

         fracLen -=last_gigit_length;
         *end = EOS;
      }

      while (fracLen > 0)
      {
         end -= 9;
         m_gigits[gi] = Latol(end);  //*p = Latol(end);
//std::cout << end << " --> " << *p << NL;
         gi++; //p++;
         fracLen -= 9;
         *end = EOS;  // cut off processed gigit
      }
      end--;  // should be just after point, back up to it
      *end = EOS;
   }

   // just after the integer part now
   while (intLen > 0)
   {
      intLen -= 9;
      end -= 9;
      if (intLen < 0)
         end += (-intLen);
      m_gigits[gi] = Latol(end);  // *p = Latol(end);
//std::cout << end << " --> " << *p << NL;
      gi++;  // p++;
      *end = EOS;
   }

   normalize();
}

void LReal::Init(aINT32 length, char sign, aINT32 exponent)
{
   m_offset = 0;
   m_length = length;
   m_array_length = m_length;
   m_sign = sign;
   m_exponent = exponent;
   delete [] m_gigits;
   LNEWX(m_gigits, Gigit[m_array_length]);
   memset(m_gigits, 0, m_array_length * sizeof(Gigit));
}

//-----------------------------------------
// operators
//
LReal& LReal::operator=( const LReal &lr )
{
	Gigit *p = m_gigits - m_offset; 
	delete p;
	m_offset = 0;
   m_length = m_array_length = lr.m_length;
   m_exponent = lr.m_exponent;
   m_sign = lr.m_sign;
   LNEWX(m_gigits, Gigit[m_length]);
   memcpy(m_gigits, lr.m_gigits, m_length * sizeof(Gigit));
	//   delete m_gigits;
	//   LNEWX(m_gigits, Gigit[m_array_length]);
	//   memcpy(m_gigits, lr.m_gigits, m_array_length * sizeof(Gigit));
   //for (int i=0; i < m_array_length; i++)
   //   m_gigits[i] = lr.m_gigits[i];
   return *this;
}

bool LReal::operator==( const LReal &lr )
{
   if (m_length != lr.m_length)
      return false;
   if (m_exponent != lr.m_exponent)
      return false;
   if (m_sign != lr.m_sign)
      return false;
   if (memcmp(m_gigits, lr.m_gigits, m_length * sizeof(Gigit)))
      return false;

   //for (int i=0; i < m_length; i++)
   //   if (m_gigits[i] != lr.m_gigits[i])
   //      return false;

   return true;
}

bool LReal::operator==( fixedC &f )
{
#ifdef BUG_REAL
 std::cout << "---------------------------------------------" << NL;
 std::cout << "operator==(Real,Fixed)" << NL << SP2;
 Dump();
 std::cout << NL << SP2;
 f.Dump();
 std::cout << NL << "---------------------------------------------" << NL;
#endif
   if (m_length > 2)
      return false;
   if (m_sign != f.getSign())
      return false;
   if (m_length == 2 && m_exponent == -1 &&
       m_gigits[0] == f.getLO() && m_gigits[1] == f.getHI())
       return true;
   if (m_length == 1 && m_exponent == -1 &&
       m_gigits[0] == f.getLO() && f.getHI() == 0)
       return true;
   if (m_length == 1 && m_exponent == 0 &&
       m_gigits[0] == f.getHI() && f.getLO() == 0)
       return true;

   return false;
}

bool LReal::operator==( aINT32 i )
{
   if (! isInteger())
      return false;

   if (i > 0 && i < 1000000000)
   {
      if (m_exponent == 0 && m_length == 1 &&
          m_sign == '+' && m_gigits[0] == i)
         return true;
      else
         return false;
   }

   if (i < 0 && i > -1000000000)
   {
      if (m_exponent == 0 && m_length == 1 &&
          m_sign == '-' && m_gigits[0] == -i)
         return true;
      else
         return false;
   }

   if (i < 0 && m_sign == '+')
      return false;
   if (i >= 0 && m_sign == '-')
      return false;
   if (m_exponent > 1)
      return false;
   if (m_length > 2)
      return false;

   int ii = (i < 0) ? -i : i;

   if (m_length == 2 && m_exponent == 0 &&
       m_gigits[1] == ii/fullgig && m_gigits[0] == ii % fullgig)
      return true;

   if (m_length == 1 && m_exponent == 1 &&
       m_gigits[1] == ii/fullgig && ii % fullgig == 0)
       return true;

   return false;
}

double LReal::toDouble()
{
   if(isZero())
      return (double)0;
   //Real rtrim = getLen() > 2 ? trimToLen(3) : this;
   LReal rtrim;
   if (getLen() > 2)
      trimToLen(3, rtrim);
   else
      rtrim = *this;
   double expfactor = ::pow(fullgig, (double)rtrim.lsExp());
   int len = rtrim.getLen();
   double acc = 0;
   for(int i = len; i > 0; i--)
      acc = fullgig*acc + rtrim.getGd(i);
   if(rtrim.getSign() == '-')
      acc = -acc;
   return acc*expfactor;
}

float LReal::toSingle()
{
   return (float)toDouble();
}

fixedC LReal::toFixed()
{
#ifdef BUG_REAL
   std::cout << "real->fixedC coerce" << SP2;
   Dump();
#endif
  fixedC result;
  int len = getLen();
  int exp = lsExp();

  if(exp == -1)
	 if(len < 3)
		 {
			//result.lo = getGd(1);
			//result.hi = len == 2 ? getGd(2) : 0;
         result.setLO(getGd(1));
         result.setHI( len == 2 ? getGd(2) : 0 );
		 }
	 else
		throw LExcept(number_castE, aS("to fixed"));
  else
	 if(exp == 0 && len == 1)
		{                                         // in integer really
		  //result.lo = 0;
		  //result.hi = getGd(1);
       result.setLO(0);
       result.setHI(getGd(1));
		}
	 else
		throw LExcept(number_castE, aS("to fixed"));
  if(getSign() == '-')
	 result.setNeg();
#ifdef BUG_REAL
  result.Dump();
#endif
  return result;
}

bool LReal::isIntegerable()
{
   bool rc;
   if (getLen() + lsExp() > 2)
      return false;
   if (lsExp() < 0)
      return false;
   if (getLen() + lsExp() == 1)
      return true;
   Real r;
   LNEWX(r, LReal((long)0x7fffffff));
   if (compareAbs(*r) == '<')
      rc = true;
   else
      rc = false;
   delete r;
   return rc;
}

aINT32 LReal::toInt()
{                                 // expects this to be a real integer already
  //GD realBase[] = {0x7e000002, 0, 1};

  if(getLen() + lsExp() > 2)
	 return 0;                     // behave as floats do

  long result = m_gigits[0];  //(this + 1)->g;
  switch(getLen())
	 {
	 case 1:
		return isPos()? result : -result;
	 case 2:
		if(compare(1000000000) == '>')
		  return result + m_gigits[1]*fullgig;  //((this + 2)->g)*gig;
	 default:
		throw LExcept(number_castE, aS("to integer"));
		return 0;
	 }
}

void LReal::toString(aCHAR *result, int max_length)
{
   int i, exp, gd;
   Gigit g;
   bool leading_zero;
   aCHAR *s;
   int extra_zeros = 0;
   if (lsExp() > 0)
      extra_zeros = lsExp();
   if (lsExp() < 0 && -lsExp() - getLen() > 0)
      extra_zeros = -lsExp() - getLen();
   int slen = extra_zeros*9 + m_length*9 + 3;   // digits, sign, point and EOS
   if (slen >= max_length)
      throw LExcept(number_sizeE, aS("write format of real"));  //sizeError;

   s = result;
   if (isNeg())
      *s++ = aS('-');
   int intpart_gigits = getLen() + lsExp();

   // if negative or zero, no int part, just a fractional
   // part, maybe needing some leading 0 gigits.
   if (intpart_gigits <= 0)
   {
      *s++ = aS('0');
      *s++ = aS('.');
      while (intpart_gigits++ < 0)
      {
         for (i=0; i<9; i++) *s++ = aS('0');
      }
      intpart_gigits--; // decrement so it just plunges for next part
   }
   for (int gi = m_length-1; gi >= 0; gi--)
   {
      g = m_gigits[gi];
      // if its the first int part gigit, supress leading zeros
      if (gi == m_length-1 && intpart_gigits > 0)
      {
         leading_zero = true;
         for (i=0, exp=100000000; i<9; i++, exp /= 10)
         {
            gd = g / exp;
            if (gd > 0 || i == 8)   // last zero means it was zero
            {
               *s++ = aS('0') + gd;
               leading_zero = false;
            }
            else if (! leading_zero)
               *s++ = aS('0');
            g = g % exp;
         }
      }
      else
      {
         for (i=0, exp=100000000; i<9; i++, exp /= 10)
         {
            gd = g / exp;
            *s++ = aS('0') + gd;
            g = g % exp;
         }
      }
      if (--intpart_gigits == 0)
         *s++ = aS('.');
   }

   // might need to trim trailing fractional zeros
   if (intpart_gigits <= 0)
   {
      while (*(s-1) == aS('0'))
         s--;
   }

   // was a big integer, needs some more zeros
   if (intpart_gigits > 0)
   {
      while (intpart_gigits-- > 0)
      {
         for (i=0; i<9; i++) *s++ = aS('0');
      }
      *s++ = aS('.');
   }

   // don't end with a solitary point
   if (*(s-1) == aS('.'))
      *s++ = aS('0');

   *s = EOS;
}


//-------------------------------
// functions
//

char LReal::compare(long i)
{
   Real r;
   LNEWX(r, LReal(i));
   char result = compare(*r);
   delete r;
   return result;
}

char LReal::compare(const LReal &B) 
{                                         // comparison of real numbers
  if(isPos() && B.isNeg() )               // by virtue of sign
	 return '>';
  if(isNeg() && B.isPos())
	 return '<';

  char result = compareAbs(B);            // both same sign
  return result == '=' ? '=' : (isPos() ? result :
										 (result == '<' ? '>' : '<'));
}

char LReal::compareAbs(const LReal &B) 
{                                         // compare reals, ignore signs)
  if(isZero())
	 {
		if(B.isZero())
		  return '=';
		return B.isNeg() ? '>' : '<';
	 }
  if(B.isZero())                          // A is not zero
	 return isNeg() ? '<' : '>';
  if(msExp() > B.msExp())                 // by virtue of significance
	 return '>';
  if(msExp() < B.msExp())
	 return '<';

  int lowestCol = lsExp() < B.lsExp() ? lsExp() : B.lsExp();
		for(int i = msExp(); i >= lowestCol; i--)
		  {                         // same significance, maybe different lengths
			 if(getCol(i) < B.getCol(i))
				return '<';
			 else
			 if(getCol(i) > B.getCol(i))
				  return '>';
		  }
    return '=';
}

void LReal::powerR(longlong x, long n, LReal &result) // fast exponentiation continued
{                                         // only used to continue powerZ
   LASSERT((&result != this), aS("real self assignment"));
  //Real v = ::newReal(x);       
  //Real t = newReal();                     // acc
   //Real v, t;
   //LNEWX(v, LReal(x));
   //t = this;
   LReal v(x);
   LReal t(*this);

  for(int u = n; u; u >>= 1)
	 {
		if(u & 1)
		  //(*t * *v)->assignTo(v);
        //v = *t * *v;
        //v.mult(t,v);
        v = t * v;
		//(*t * *t)->assignTo(t);
      //t = *t * *t;
      t = t * t;    //t.mult(t,t);
	 }
  //return v;
  result = v;
  return;
}

void LReal::pow(int n, int delta, int epsilon, LReal &result) 
{   // fast exponentiation
   // can't share on return, must be new object going out,
   // because gc might destroy it.
   LASSERT((&result != this), aS("real self assignment"));
#ifdef BUG_REAL_MATH
std::cout << "===>> LReal::pow" << NL;
std::cout << "LReal::pow " << n << "  delta = " << delta << NL;
Dump();
#endif

  if(n == 0)
  {
     result = realOne;
     return;
  }
  else if(n == 1)
  {
     result = *this;
     return;
  }
  else if(isZero())
  {
     result = realZero;
  }
  else
	 {                     // length can explode, so trim fractions to epsilon
     LReal v((Gigit)1);
     LReal t(*this);
	  LReal t1, v1;
     for(int u = abs(n); u; u >>= 1)
	    {
		   if(u & 1)
			  {
				 v = t*v;
	 			 v.trimToEpsilon(epsilon);
			  }
  			t = t*t;
	  		t.trimToEpsilon(epsilon);
	    }
     if (n < 0)
     {
        t = realOne;
        t.div(v, delta, false, result);
     }
     else
        result = v;
  }
#ifdef BUG_REAL_MATH
std::cout << "result = " << NL;
result->Dump();
std::cout << "<<=== LReal::pow" << NL;
#endif
  return;
}

void LReal::mult(long B, LReal &result)
{
   LASSERT((&result != this), aS("real self assignment"));
   LReal rb((B));
   mult(rb, result);
   return;
  //LNEWX(Real RB, LReal(B));
  //Real result = mult(*RB);
  //return result;
}

/*
Real LReal::mult(LReal &B) 
{
  if(B.isZero() || isZero() || getLen() == 0)
	 return realZero;
  if(B.compare(*realOne) == '=')
	 return this;               // copy constructor
  if(this->compare(*realOne) == '=')
		//return B.newReal();                 // copy constructor
    return &B;                      // copy constructor

  Real C;
  //Real C = ::newReal(*this, B, prod_);         // result is new object
  LNEWX(C, LReal(*this, B, prod_));
  long *pA = &(m_gigits[0]);  //(long *)this + 1;
  long *pB = &(B.m_gigits[0]);  //(long *)&B + 1; 
  long *pC = &(C->m_gigits[0]);  //(long *)C + 1;
  long *startB = pB;
  long *startC = pC;
  long *pCC = pC;
  long *endA  = pA + getLen();
  long *endB  = pB + B.getLen();
  long *endC  = pC + C->getLen();
  longlong product;
  long carry;
	 
  while(pC < endC)
	 *pC++ = 0;
  
  for(pC = startC; pA < endA; pA++, pCC++)
	 {
		carry = 0;
		
		for(pC = pCC, pB = startB; pB < endB; pB++, pC++)
		  {
			 product = (longlong)(*pA)*(longlong)(*pB) + *pC + carry;
			 *pC = product % fullgig;
			 carry = product / fullgig;
		  }
		*pC = carry;
	 }
  if(getSign() != B.getSign())
	 C->setSign('-');
  C->normalize();
  return C;
}
*/

void LReal::mult(LReal &B, LReal &C) 
{
  LASSERT((&C != this && &C != &B), aS("real self assignment"));
#ifdef BUG_REAL_MATH
std::cout << "===>> LReal::mult" << NL;
std::cout << "LReal::mult " << NL;
std::cout << "this: " << NL;
Dump();
std::cout << "times that: " << NL;
B.Dump();
#endif

  if(B.isZero() || isZero() || getLen() == 0)
     C = realZero;
  else if (B == realOne)
     C = *this;
  else if (*this == realOne)
     C = B;
  else
  {
      C.Init(*this, B, prod_);
		aINT32 *pA = m_gigits;
		aINT32 *pB = B.m_gigits;
		aINT32 *pC = C.m_gigits;
		aINT32 *msaA = msa();
		aINT32 *msaB = B.msa();
		longlong product;
		aINT32 carry;
	    
     while(pC <= C.msa())
	    *pC++ = 0;  
  
     for(aINT32 *startC = C.m_gigits; pA <= msaA; pA++, startC++)
	    {
		   carry = 0;		   
		   for(pB = B.m_gigits, pC = startC; pB <= msaB; pB++, pC++)
		     {
			    product = (longlong)(*pA)*(*pB) + *pC + carry;
			    *pC = product % fullgig;
			    carry = (aINT32)(product / fullgig);
		     }
		   *pC = carry;
	    }
     if(getSign() != B.getSign())
	    C.setSign('-');
     C.normalize();
     //result = C;
     //return C;
  }
#ifdef BUG_REAL_MATH
std::cout << "result = " << NL;
result.Dump();
std::cout << "<<=== LReal:Mult" << NL;
#endif
   return;
}

void showRqBq(LReal, LReal, aINT32, int, char *);

void LReal::intDiv(LReal &Bin, LReal &Remp, int delta, LReal &result)
{         
   LASSERT((&result != this && &result != &Bin && &result != &Remp), 
			  aS("real self assignment"));
                                           // return A (this) intDiv B
  LReal Q;

  div(Bin, 0, true, Q);

  LReal Qint;
  Q.intPart(Qint);
  LReal temp = Qint * Bin;       // gcc needs this extra step for some reason
  Remp = *this - temp; 
  result = Qint;
  return;
}

void LReal::div(LReal &Divisor, int delta, LReal &result) 
{                                   
   LASSERT((&result != this), aS("real self assignment"));
  div(Divisor, delta, false, result);
  return;
}

void LReal::div(LReal &Divisor, int delta, bool intDivFlag, 
					 LReal &result) 
{                          // this is Dividend, result = Q = Dividend/Divisor
   LASSERT((&result != this && &result != &Divisor), 
			  aS("real self assignment"));
#if defined(BUG_REAL_MATH) || defined(BUG_DIV)
std::cout << "===>> LReal::div" << NL;
std::cout << "LReal::div " << "  delta = " << delta << NL;
std::cout << "this: " << NL;
Dump();
std::cout << "divisor: " << NL;
Divisor.Dump();
#endif
  //Real realZero = ::newReal(0L);
  //Real realOne  = ::newReal(1L);
  LReal Q;

  if(Divisor.isZero())
     throw LExcept(zero_divideE);  //zeroDivide;

  if(isZero())
	 Q = realZero;
  else if (*this == Divisor)
	 Q = realOne;
  else
  {  
     aINT32 B0 = Divisor.msg();
     long D = (Divisor.getLen() == 1 || B0 >= 500000000) ? 1 : 
	  //    long D = B0 >= 500000000 ? 1 : 
	 (B0 >= 300000000 ? 2 : (B0 >= 200000000 ? 3 : (B0 >= 100000000 ? 5 :
								1 + 500000000/B0 )));
     //const Real DB = Divisor.mult(D);     
     //const Real DA = this->mult(D);     // make DA conform. may increase len
     LReal DB;
     LReal DA;
     Divisor.mult(D, DB);
     mult(D, DA);
     DA.setSign('+');
     DB.setSign('+');

#ifdef BUG_DIV
	  std::cout << "calling divisionLoop DA/DB, delta = " <<
		 delta << " intdiv " << intDivFlag << NL;
	  DA.Dump();
	  DB.Dump();
#endif
     //Q = DA->divisionLoop(DB, delta, intDivFlag); // get Q
     DA.divisionLoop(DB, delta, intDivFlag, Q); // get Q
	  //delete DA;
	  //delete DB;
	  
     Q.setSign(getSign() == Divisor.getSign() ? '+' : '-');
  }
#if defined(BUG_REAL_MATH) || defined(BUG_DIV)
  std::cout << "result = " << NL;
  Q.Dump();
  std::cout << "<<=== LReal::div" << NL;
#endif
  result = Q;
  return;                                
}
/* Calculate Q is A/B. 
 * If length of B is greater than 1 then A (this) and B are D - multiplied 
 * in order to help estimate q. R is initially A.
 * The quotient gigit q is estimated from the leading one or two gigits of R,
 * and may have to be decremented by 1 or 2.
 * At each iteration, R is replaced by the difference between R and qB, and 
 * q is prepended to Q. The prepend operation is performed 'in situ' and it 
 * increases the length of Q is by one. 
 * The quotient gigit index qExp is maintained to:
 *   correct the exponent of the answer
 *   correct the exponent of qB at each iteration
 *   as a stop criterion for integer divide.
 */
void LReal::divisionLoop(LReal &B, int delta, bool intDivFlag, LReal &Q)
{                                         // Q = A/B (D multiplied). this is A
  //   LASSERT((&Q != this && &Q != &B), aS("real self assignment"));
#if defined(BUG_REAL_MATH) || defined(BUG_DIV)
std::cout << "===>> LReal::divisionLoop" << NL;
std::cout << "LReal::divisionLoop " << "  delta = " << delta << NL;
std::cout << "this: " << NL;
Dump();
std::cout << "divisor: " << NL;
B->Dump();
#endif
// showReal("A"); 
// B.showReal("B"); 
  const aINT32 B0 = B.msg();                 // msg of denominator B
  const int lenB = B.getLen();             // length of denominator B
  const int expB = B.lsExp();              // least exp of denominator B
  const int msExpB = B.msExp();            // most exp of denominator B
  const int	lenA = getLen();               // length of numerator A
  const int	expA = lsExp();                // exp of numerator A
  const int msExpA = msExp();              // most exp of numerator A

  int maxQ = (lenA > lenB ? lenA : lenB) + delta;
  int expQ = expA - expB;
  Q.Init(maxQ, '+', expQ);                 // quotient register
  Q.setLen(0);                             // set length zero to start
  Q.setMaxOffset();                        // prepare to prepend

  longlong llR, llRB0;                     // long long accumulators
  aINT32 R0;                                 // 1st gigit of remainder
  bool carry;
  LReal qB;                                // qB register
  LReal R = *this;                         // remainder register, init A
  LReal diff;
  LReal qB2;
  int zeros = 0;                           // zeros between significant gigits
  R0 = R.msg();
  aINT32 q, saveq;                           // quotient gigit
  int qExp = msExpA - msExpB;              // nominal exponent of q msg
  int saveqExp, lastqExp = qExp + 1 ;      // setup for leading zero
  bool  qmsg = true;
  int i, shift;
  while(true)
	 {                                      // long division loop
		R0 = R.msg();
		llR = (longlong)R0*fullgig + R.msg1(); // try two leading gigits
		llRB0 = llR/B0;
		carry = llRB0 < fullgig;             
		saveq = q = carry ? (long)llRB0 : R0/B0; // initial q
		qB = B*q;                            // initial qB
		qB.incExp(qExp);
		shift = qB.msExp() - R.msExp();
		if(shift)
		  {                                  // align R msExp and qB msExp
			 qExp -= shift;
			 qB.decExp(shift);
		  }
		//showRqBq(R, qB, q, qExp, " before correction");	
		if(qmsg)
		  lastqExp = qExp+1;

		diff = R - qB;

		 //diff.showReal("R - qB");
		 //diff.showReal(" diff");
		saveq = q;
		saveqExp = qExp;
		if(diff.isNeg())
		  {
			  for(i = 0; i < 2 ; i++)		 
				 {                             // qB too big
					if(--q == 0)                // reduce q and check for zero
					  {
						 qExp--;
						 q = 999999999;
					  }
					qB2 = B*(q);                // qB2 is qB with 1 lower q
					qB2.incExp(qExp);
					diff = R - qB2;
					//diff.showReal(" diff1");
					if(diff.isPos())
					  break;
				 }
			  if(i < 2)
				 {                             // it worked! excess q was <= 2
					//std::cout << " decrementing by " << i+1 <<'\n';
					qB = qB2;                   // revise qB
					diff = R - qB;              // revise diff
				 }
			  else
				 {                             // too bad, q still too big
					q = saveq;                  // restore q
					qExp = saveqExp - 1;        // decrement original exponent
					qB.decExp();                // decrement qBs exponent
					diff = R - qB;              // revise diff
				 }
			}
		 //showRqBq(R, qB, q, qExp, " final qB");

		R = diff;                            // next remainder

		zeros = qmsg ? 0 : lastqExp - (1 + qExp);
		while(zeros-- > 0 && Q.getLen() < maxQ)
		  Q.prepend(0);                      // insert zeros
		lastqExp = qExp;

		Q.prepend(q);                        // prepend the q digit

		qmsg = false;                        // no longer msg of q
#ifdef BUG_DIV
		std::cout << "testing for another go around, Q and R = " << NL;
		Q->Dump();
		R->Dump();
#endif
		if(Q.getLen() >= maxQ)
		  break;                             // finished because no more wanted
		if(intDivFlag && qExp < 0)
		  break;                             // finished because integer divide
		if(R.isZero())
		  break;                             // finished because no remainder

		qExp--;
	 }                                      // end divide loop
  Q.setExp(qExp);
  Q.normalize();

#if defined(BUG_REAL_MATH) || defined(BUG_DIV)
  std::cout << "result = " << NL;
  Q.Dump();
  std::cout << "<<=== LReal::divisionLoop" << NL;
#endif
}


void LReal::add(long B, char Bsign, LReal &result)
{
   LASSERT((&result != this), aS("real self assignment"));
  //Real RB = ::newReal(B);
  //Real RB;
  //LNEWX(RB, LReal(B));
  //Real result = add(*RB, Bsign);

  LReal RB(B);
  add(RB, Bsign, result);
  return;
  //delete [] RB;
  //  delete[] RB;
  //delete RB;
  //return result;
}


void LReal::add(LReal &B, char opSign, LReal &sum)
{                                              // return Real = this + B
   LASSERT((&sum != this && &sum != &B), aS("real self assignment"));
  aINT32 carry = 0;
  //Real result = ::newReal(*this, B, sum_);     // has space for carry
  //Real result;
  //LNEWX(result, LReal(*this, B, sum_));
  sum.Init(*this, B, sum_);
//Dump();
//B.Dump();
  int col, msExp = sum.msExp() - 1;        // remember, result is oversize
  aINT32 *p = &(sum.m_gigits[0]);  // (aINT32 *)(result + 1);

  switch(signs(B, opSign))
	 {
	 case bothPos_:
		for(col = sum.lsExp(); col <= msExp; col++)
		  *p++ = gdAdd(carry, getCol(col), B.getCol(col));
		break;
	 case Aneg_:
		if(compareAbs(B) == '>')
		  {
			 for(col = sum.lsExp(); col <= msExp; col++)
				*p++ = gdAdd(carry, getCol(col), -B.getCol(col));
			 sum.setSign('-');
		  }
		else
		  for(col = sum.lsExp(); col <= msExp; col++)
			 *p++ = gdAdd(carry, -getCol(col), B.getCol(col));
		break;
	 case Bneg_:
		if(compareAbs(B) == '<')
		  {
			 for(col = sum.lsExp(); col <= msExp; col++)
				*p++ = gdAdd(carry, -getCol(col), B.getCol(col));
			 sum.setSign('-');
		  }
		else
		  for(col = sum.lsExp(); col <= msExp; col++)
			 *p++ = gdAdd(carry, getCol(col), -B.getCol(col));
		break;
	 case bothNeg_:
		for(col = sum.lsExp(); col <= msExp; col++)
		  *p++ = gdAdd(carry, getCol(col), B.getCol(col));
		sum.setSign('-');
		break;
	 }
  
  sum.setGd(sum.getLen(), carry);
  sum.normalize();
//result->Dump();
  //result = sum;
  return;
}

void LReal::floor(LReal &result)
{
  if(isInteger())
	 result = *this;
  else
	 {
		intPart(result);
      if (isNeg())
		   result = result - 1;
	 }
}

void LReal::ceil(LReal &result)
{
  if(isInteger())
	 result = *this;
  else
	 {
		intPart(result);
      if (! isNeg())
		   result = result + 1;
	 }
}

void LReal::round(LReal &result)
{
  if(isInteger())
	 result = *this;
  else
	 if(msExp() < -1)
		result = realZero;
	 else
		if(msExp() == -1)
		  {
			 result = msg() >= 500000000 ? realOne : realZero;
			 if(getSign() == '-' && msg() >= 500000000)
				result.setSign('-');
		  }
		else
		  {
			 intPart(result);
			 if(getCol(-1) >= 500000000)
				result = getSign() == '+' ? result + 1 : result - 1;
		  }
}

void LReal::absReal(LReal &result) 
{ 
  //Real result;
  //LNEWX(result, LReal(*this));
   result = *this;
  if(getSign() == '-') 
	 result.negate();                     // bump sign
  return;
}

void LReal::rxor(LReal &B, LReal &result)
{                                              // return Real = this xor B
   LASSERT((&result != this && &B != &result), aS("real self assignment"));
  //Real result = this->newReal();     
  //Real result;
  //LNEWX(result, LReal(*this));
  // LReal r(*this);
   result = *this;
  //aINT32 *p = (aINT32 *)(result + 1);
  aINT32 *p = (aINT32 *)result.m_gigits[0];

  for(int col = lsExp(); col <= msExp(); col++)
		  *p++ = getCol(col) ^ B.getCol(col);
  return;
}

void LReal::divU(LReal &r2, aINT32 delta, LReal &result)
{

      if (r2.isZero())
        throw LExcept(zero_divideE);
      
      //LReal halfDivisor = r2.absReal() >> 1;
      LReal halfDivisor;
      r2.absReal(halfDivisor);
      halfDivisor = halfDivisor >> 1;
      LReal quotient;
      LReal remainder;
      intDiv(r2, remainder, delta, quotient);

      if(remainder.isZero())
         result = quotient;   /// is this necessary?
        //value.setReal( quotient );
      if(r2 < 0)
      {
        if(remainder.getSign() == '+')
          //(*quotient + 1)->assignTo(quotient);
          quotient = quotient + 1;
      }
      else
          //(*quotient - 1)->assignTo(quotient);
          quotient = quotient - 1;
      //value.setReal( quotient );
      result = quotient;
 }

void LReal::divS(LReal &r2, aINT32 delta, LReal &result)
{
      if (r2.isZero())
        throw LExcept(zero_divideE);

      bool negmod, oddmod;
      negmod = r2 < 0;
      oddmod = r2.isOdd();
      LReal halfDivisor;
      //halfDivisor = *(r2->absReal()) >> 1;      // always +ve
      r2.absReal(halfDivisor);
      halfDivisor = halfDivisor >> 1;
      LReal quotient;
      LReal remainder;
      intDiv(r2, remainder, delta, quotient);
      char oflo = remainder.compare(halfDivisor);
      halfDivisor.setNeg();
      char uflo = remainder.compare(halfDivisor);
      if(negmod)
        {
          if(oflo == '>') 
            //((*quotient) - 1)->assignTo(quotient);
            quotient = quotient - 1;
          if(oddmod)                         
            if(uflo == '<') 
              //((*quotient) + 1)->assignTo(quotient);
              quotient = quotient + 1;
            else;
          else
            if(uflo == '<') 
              //((*quotient) + 1)->assignTo(quotient);
              quotient = quotient + 1;
        }
      else 
        {
          if(uflo == '<') 
            //((*quotient) - 1)->assignTo(quotient);
            quotient = quotient - 1;
          if(oddmod)                         
            if(oflo == '>') 
              //((*quotient) + 1)->assignTo(quotient);
              quotient = quotient + 1;
            else;
          else
            if(oflo == '>' || oflo == '=') 
              //((*quotient) + 1)->assignTo(quotient);
              quotient = quotient + 1;
        }
      //value.setReal( quotient );
      result = quotient;
}

void LReal::modS(LReal &r2, aINT32 modulus, aINT32 delta, LReal &result)
{
   LReal mod(modulus);
   modS(r2, mod, delta, result);
   return;
}

void LReal::modS(LReal &r2, LReal &modulus, aINT32 delta, LReal &result)
{

      if (r2.isZero())
        throw LExcept(zero_divideE);
      
      bool negmod, oddmod;
      negmod = r2 < 0;
      oddmod = r2.isOdd();
      LReal absmod;
      r2.absReal(absmod);
      LReal halfDivisor = absmod >> 1;
      LReal quotient, remainder;
      intDiv(r2, remainder, delta, quotient);
      char oflo = remainder.compare(halfDivisor);
      halfDivisor.setNeg();
      char uflo = remainder.compare(halfDivisor);
      if(negmod)
        {
          if(oflo == '>') 
            //((*remainder) - (*absmod))->assignTo(remainder);
            remainder = remainder - absmod;
          if(oddmod)                         
            if(uflo == '<') 
              //(*remainder + (*absmod))->assignTo(remainder);
              remainder = remainder + absmod;
            else;
          else
            if(uflo == '<' || uflo == '=') 
              //(*remainder + (*absmod))->assignTo(remainder);
              remainder = remainder + absmod;
        }
      else 
        {
          if(uflo == '<') 
            //(*remainder + (*absmod))->assignTo(remainder);
            remainder = remainder + absmod;
          if(oddmod)                         
            if(oflo == '>') 
              //(*remainder - (*absmod))->assignTo(remainder);
              remainder = remainder - absmod;
            else;
          else
            if(oflo == '>' || oflo == '=') 
              //(*remainder - (*absmod))->assignTo(remainder);
              remainder = remainder - absmod;
        }
      result = remainder;
      //value.setReal( remainder );
}

void LReal::modU(LReal &r2, aINT32 modulus, aINT32 delta, LReal &result)
{
   LReal mod(modulus);
   modU(r2, mod, delta, result);
}

void LReal::modU(LReal &r2, LReal &modulus, aINT32 delta, LReal &result)
{
  LReal remainder;

  if (modulus == 0)
     throw LExcept(zero_divideE);

  LReal quotient;
  intDiv(modulus, remainder, delta, quotient);
      if(remainder.isNeg())
         //(*remainder + *modulus)->assignTo(remainder);
         remainder = remainder + modulus;
      //value.setReal( remainder );
      result = remainder;
}

void LReal::modulus(aINT32 modulus, aINT32 delta, LReal &result)
{
  LReal remainder;

  if (modulus == 0)
     throw LExcept(zero_divideE);

  LReal quotient;
  LReal realmod(modulus);
  intDiv(realmod, remainder, delta, quotient);
      if(remainder.isNeg())
         //(*remainder + *modulus)->assignTo(remainder);
         remainder = remainder + modulus;
      //value.setReal( remainder );
      result = remainder;
}

void LReal::truncate(long epsilon)
{  // in situ. cuts length if exponent < epsilon 
  if (epsilon == 0)
     return;
  int fat = epsilon - lsExp();
  if(fat > 0)
	 {
		setOffset(fat);
		m_length-= fat;
		m_exponent+= fat;
	 }
}

void LReal::truncate(long epsilon, LReal &r)
{ // currently, cuts length if exponent < epsilon 
  if (epsilon == 0)
     return;
  r = LReal(*this);
  //int fat = pSTATE->m_epsilon - lsExp();
  int fat = epsilon - lsExp();
  if(fat > 0)
	 {
		r.setOffset(fat);
		r.m_length-= fat;
		r.m_exponent+= fat;
	 }
  //  trimToLen(newLen < 2 ? 2 : newLen, result); // don't trim length below 2
}

//--------------------------------------------
// utilities
//

/*
double LReal::realToFloat()
{
  LReal rtrim;
  if(isZero())
    return (double)0;
  if(getLen() > 2)
	 trimToLen(3, rtrim);
  else 
	 rtrim = *this;
  double expfactor = ::pow(fullgig, rtrim.lsExp());
  int len = rtrim.getLen();
  double acc = 0;
  for(int i = len; i > 0; i--)
    acc = fullgig*acc + rtrim.getGd(i);
  if(rtrim.getSign() == '-')
    acc = -acc;
  return acc*expfactor;
}
*/

void LReal::prepend(Gigit n)
{                                         // only if m_offset > 0
  if(m_offset > 0) 
	 {
		*(--m_gigits) = n; 
		m_offset--;
		m_length++;
	 }
  else
	 throw LExcept(number_sizeE, aS("real"));
}

void LReal:: trimToEpsilon(int epsilon)
{    // trim in situ
  if (epsilon == 0)
     return;

  if(lsExp() >= epsilon)
	 return;
  int fat = epsilon - lsExp();
  if(m_length <= fat)
	 {
		m_length = 1;
		m_gigits[0] = 0;
		m_exponent = 0;
		return;
	 }
  m_offset = fat;
  m_length -= fat;
  m_exponent += fat;
  m_gigits += fat;
  return;
}
 
void LReal::trimToLen(int newLen, LReal &result)
{                                         // trim fat off length
   if (newLen < 1)
   {
      result = realZero;
      return;
   }

   int fat = getLen() - newLen;
   if(fat < 1)
   {
      result = *this;
      return;
   }

   //LNEWX(result, LReal(newLen, getSign(), lsExp()+fat));
   result.Init(newLen, getSign(), lsExp() + fat); // exp will increase

   Gigit *p = result.lsa();               // first new gigit to be filled in
   Gigit *q = m_gigits + fat - 1;         // the highest cut gigit
   int carry = *q++ < halfgig ? 0 : 1;    // initial roundoff
   while(newLen--)
   {
      *p = *q++ + carry;                  // copy gigits
      carry = *p < fullgig ? 0 : 1;
      if(carry)
         *p -= fullgig;
      p++;
   }
   if(carry)
   {                                      // roundoff caused oflo! rare
      *p = carry;
      result.incLen();                    // contraction allows space for this
   }
   result.normalize();                    // ?? must be normal already
   return;
}


void LReal::trimToPrecision(int dec_precision)
{
   // trim to required decimal precision, in place,
   // changing this real.
   aINT32 i,j,excess,keyg;
   aINT32 sd = 0;
   aINT32 factor = 1;
   aINT32 carry = 0;
   longlong x;

   keyg = 0;
   for (i = getLen() - 1; i >= 0; i--)
   {
      if (sd >= dec_precision)
      {
         m_gigits[i] = 0;
         continue;
      }

      if (i == getLen()-1)
         sd += significantDigits(m_gigits[i]);
      else
         sd += 9;

      if (sd <= dec_precision)
         continue;
      else
      {
         excess = sd - dec_precision;
         for (j=0, factor=1; j<excess; j++, factor *= 10)
            ;
         carry = m_gigits[i] % factor;
         keyg = i;
         m_gigits[i] = (m_gigits[i]/factor) * factor;
      }
   }
   // now see if we can round appropriatly
   if (carry == 0 && factor == 1 && keyg > 0) 
   {                                     // need to look at preceding gigit
      if (m_gigits[keyg-1] > halfgig)
         carry = 1;
   }
   if (carry > factor/2)
   {
      for(i=keyg; i<getLen() && factor>0; i++)
      {
         x = m_gigits[i] + factor;
         m_gigits[i] = x % fullgig;
         if (x >= fullgig)
            factor = (aINT32)(x/fullgig);
         else
            factor = 0;
      }
   }
   normalize();
}

void LReal::normalize()
{                                         // in situ normalization
   Gigit *p;

   for(p = msa(); *p == 0 && m_length > 1; p--)
	  m_length--;                          // set m_length for true msa

   if(getLen() == 1 && *p == 0)
   {                                      // zero is special case
      m_exponent = 0;
      m_sign = '+';
      return;                             
   }

	//	Gigit *msa = msa();
	while(*m_gigits == 0 && m_gigits < msa())
	  {
		 m_gigits++;
		 m_offset++;
		 m_length--;
		 m_exponent++;
	  }
	/*
   int trailZeros = 0;
   p = m_gigits;
	q = p - 1;
	while(*(++q) == 0)
	  trailZeros++;                       // count trailing trailZeros
   if(trailZeros)
   {                                     // trim trailing trailZeros
	  while(q <= msa())
		  *p++ = *q++;                     // move gigit forward
      m_exponent+= trailZeros;
      m_length-= trailZeros;
   }
	*/
}   

inline signs_ LReal::signs(LReal &B, char opSign)
{
   if(opSign == '-')
      return getSign() == '+' ? (B.getSign() == '+' ? Bneg_ : bothPos_) :
            (B.getSign() == '+' ? bothNeg_ : Aneg_) ;
   else
      return getSign() == '+' ? (B.getSign() == '+' ? bothPos_ : Bneg_) :
            (B.getSign() == '+' ? Aneg_ : bothNeg_) ;
}									 

void LReal::intPart(LReal &result)
{
  //Real realZero = ::newReal(0L);
  int exp = lsExp();
  int len = getLen();
  int size = len + exp;
  if(size < 1)
  {
     result = realZero;
	 //return realZero;
  }// no int part
  else if(exp >= 0)
  {
     result = *this;
	 //return this;                            // no fracpart
  }
  else
  {
      // exp is -ve, size is >= 1
      //Real result;
      //LNEWX(result, LReal(size, getSign(), 0));
      LReal r(size, getSign(), 0);
      //= ::newReal(size, 0, getSign());       // just long enough for int part
      Gigit *p = &(r.m_gigits[0]);        //(aINT32 *)result + 1;
      Gigit *q = &(m_gigits[getLen() - size]); //(aINT32 *)this + 1 + len - size;
      for(int i = 0; i < size; i++)
         *p++ = *q++;
      result = r;
  }
  return;
}

/*
 * Shifting Reals has the same semantics as shifting integers, ie: 
 * multiplying or dividing by 2**shift. 
 * However, with integers there are no carries, but shifting Reals to the 
 * right entails adding weighted carry bits and shifting left entails 
 * subtracting gigs. Right shift is up to twice as fast as dividing but
 * left shift is only slightly faster than multplying.
 * The carries completely garble the bit pattern, so 'shift' is a euphemism.
 */
void LReal::rshift(int n, LReal &result) const
{
  if(n < 0)
  {
     lshift(-n, result);
	 //return lshift(-n);
  }
  else if( m_exponent < 0)
  {
     result = realZero;
	 //return realZero;                       // don't do fractions
  }
  else
  {

     //LNEWX(Real result, LReal(m_length + m_exponent, m_sign, 0)); // denormalize
     result.Init(m_length + m_exponent, m_sign, 0);

     aINT32 *p = result.msa();
     aINT32 *q = msa();
     while(q >= lsa())
	    *p-- = *q--;
     while(p > result.lsa())
	    *p-- = 0;

     while(n > 32)
	    {
		   result.rshift32(32);
		   n -= 32;
	    }
     result.rshift32(n);
     result.normalize();
  }
  return;
}

void LReal::rshift32(int n) const
{
  aUINT32 carry = 0;
  int i, maskn = n == 32 ? 0xffffffff : (1 << n) - 1;
  longlong acc = 0;                        // accumulator for carry bit weights
  longlong bw;
  longlong bitWeight = (longlong)500000000 << 32;
  for(aUINT32 *p = (aUINT32 *)msa(); p >= (aUINT32 *)lsa(); p--)
	 {                                      // over the length of result
		if(carry > 0)
		  for(i = n - 1, bw = bitWeight; i >= 0; i--, bw >>= 1)
			 {
				if((carry >> i) & 1)
				  acc += bw;                   // sum carry bit weights
			 }
		carry = *p & maskn;
		if(n == 32)
		  {                                  // uSoft compiler error if n == 32!
			 *p >>= 16;                       // shift result
			 *p >>= 16;                       // shift result
		  }
		else
		  *p >>= n;                          // shift result
		*p += (acc >> 32);                   // add the carry
	 }
}	 

void LReal::lshift(int n, LReal &result) const
{
  if(n < 0)
  {
     rshift(-n, result);
	 //return rshift(-n);
  }
  else if( m_exponent < 0)
  {
     result = realZero;
	 //return realZero;                       // don't do fractions
  }
  else
  {
     //LNEWX(Real result, LReal(m_length + 1 + n/32 , m_sign, m_exponent)); 
     LReal r(m_length + 1 + n/32 , m_sign, m_exponent);

     aINT32 *p = r.lsa();
     aINT32 *q = lsa();
     while(q <= msa())
	    *p++ = *q++;
     while(p > r.msa())
	    *p++ = 0;

     while(n > 32)
	    {
		   r.lshift32(32);
		   n -= 32;
	    }
     r.lshift32(n);

     r.normalize();
     result = r;
  }
  return;
}

void LReal::lshift32(int n) const
{        
  aUINT32 carryin = 0, carryout;
  for(aINT32 *p = lsa(); p <= msa(); p++, carryin = carryout)
	 {                                      // over the length of result
		carryout = 0;
		for(int i = 0; i < n; i++)
		  {                                  // shift 1 bit at a time
			 *p <<= 1;
			 carryout <<= 1;
			 if(*p >= ::fullgig)
				{                              // insert carry bit
				  carryout |= 1;
				  *p -= ::fullgig;
				}
		  }                                  // finished shifting
		*p |= carryin;                       // add carryin
	 }
}	 

//#ifdef LANDFILL
void LReal::Dump()
{
   std::cout << "Real sign: " << m_sign << "  exp: " << m_exponent <<
           "  len: " << m_length << "  array_length: " << m_array_length << NL;
   std::cout << "[ ";
   for (int i = getLen()-1; i >= 0; i--)
      std::cout << m_gigits[i] << SP;
   std::cout << "]" << NL;
}
//#endif

void LReal::showReal(char *tag)
{
  int i, j, len = getLen();
  std::cout << getSign() << ' ';
  //  printf("%s: %c", tag, getSign());
  for(i = len, j = 0; i > 0; i--, j++)
	 {
		if((i != len) && ((j % 8) == 0))
		  std::cout << '\n';
		std::cout.width(9);
		std::cout.fill('0');
		std::cout << getGd(i) << ' ';
		//		  printf("\n\t");
		//		printf(" %09d", getGd(i));
	 }
  //  printf("g%d\n", lsExp());
  std::cout << 'g' << lsExp() << " length " << getLen() << tag << '\n';
}

void showRqBq(LReal R, LReal qB, aINT32 q, int qExp, char *tag)
{
  int i, j;
  int RmsExp = R.msExp(), RlsExp = R.lsExp();
  int qBmsExp = qB.msExp(), qBlsExp = qB.lsExp();
  aINT32 * p;

  std::cout << " q " << q << " qExp " << qExp << tag << '\n';

  std::cout << R.getSign() << ' ';
  p = R.msa();
  for(i = RmsExp, j = 0; i >= RlsExp; p--, i--, j++)
	 {
		if((i != RmsExp) && ((j % 8) == 0))
		  std::cout << '\n';
		std::cout.width(9);
		std::cout.fill('0');
		std::cout << *p;
		if(i == 0)
		  std::cout << '.';
		else
		  std::cout << ' ';
	 }
  std::cout << 'g' << R.lsExp() << " R" << '\n';

  std::cout << qB.getSign() << ' ';
  p = qB.msa();
  for(i = qBmsExp, j = 0; i >= qBlsExp; i--, j++)
	 {
		if((i != qBmsExp) && ((j % 8) == 0))
		  std::cout << '\n';
		std::cout.width(9);
		std::cout.fill('0');
		std::cout << *p--;
		if(i == 0)
		  std::cout << '.';
		else
		  std::cout << ' ';
	 }
  std::cout << 'g' << qB.lsExp() << " qB" << '\n';
}


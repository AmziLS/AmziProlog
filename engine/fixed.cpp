#include "inc.h"
#include <math.h>

aINT32 gdAdd(aINT32 &, aINT32, aINT32);         // no Reals involved
const aINT32 gig = 1000000000;

bool isGigit(aINT32 i)
{
   return (i >= 0 && i < 1000000000);
}

bool isAbsGigit(aINT32 i)
{
   return (i > -1000000000 && i < 1000000000);
}


fixedC floatToFixed(double d)
{
   LReal r;
   floatToReal(d, r);
   return r.toFixed();
}

Lostream &operator<<( Lostream &os, fixedC &f )
{
   os << "fixedC: " << (char)f.getSign() << f.getHI() << '.' << f.getLO() << NL;
   os << "  sign: " << (char)f.getSign() << SP2;
   os << "hi: " << f.getHI() << "  lo: " << f.getLO() << NL;
   return os;
}

//fixedC::operator double()
//{ return toFloat(); }

double fixedC::toDouble(void) const
{
  double acc = (hi & 0x7fffffff) + lo*1e-9;
  if(hi < 0)
    acc = -acc;
  return acc;
}

void fixedC::add(fixedC &B, const char opSign, RAND &value) 
{                                              // value = this + B
  //uaINT32 Ahi = hi   & 0x7fffffff;               // suppress signs
  //uaINT32 Bhi = B.hi & 0x7fffffff;            
  aINT32 carry = 0;
  fixedC f;
  Real   r;

  switch(signs(B, opSign))
	 {
	 case bothPos_:
		  f.lo = gdAdd(carry, getLO(), B.getLO());
		  f.hi = gdAdd(carry, getHI(), B.getHI());
		break;
	 case Aneg_:
		if(compareAbs(B) == '>')
		  {
			 f.lo = gdAdd(carry, getLO(), -B.getLO());
			 f.hi = gdAdd(carry, getHI(), -B.getHI());
			 f.setNeg();
		  }
		else
		  {
			 f.lo = gdAdd(carry, -getLO(), B.getLO());
			 f.hi = gdAdd(carry, -getHI(), B.getHI());
		  }
		break;
	 case Bneg_:
		if(compareAbs(B) == '<')
		  {
			 //f.lo = gdAdd(carry, -hi, B.hi);
			 //f.hi = gdAdd(carry, -lo, B.lo);
			 f.lo = gdAdd(carry, -getLO(), B.getLO());
			 f.hi = gdAdd(carry, -getHI(), B.getHI());
			 f.setNeg();
		  }
		else
		  {
			 f.lo = gdAdd(carry, getLO(), -B.getLO());
			 f.hi = gdAdd(carry, getHI(), -B.getHI());
		  }
		break;
	 case bothNeg_:
			 f.lo = gdAdd(carry, getLO(), B.getLO());
			 f.hi = gdAdd(carry, getHI(), B.getHI());
			 f.setNeg();
		break;
	 }
  if(carry)                                 // sum overflowed
  {
//	 value.a_real = newReal(value.a_fixed, carry);
    LNEWX(r, LReal(f, carry));
    value.setReal( r );
  }
  else
     value.setFixed( f );

  return;
}
void gigPlusEq(aINT32 &gA, aINT32 gB, aINT32 & carry)
{
  gA += gB;
  carry = gA >= gig ? 1 : 0;
  if(carry)
	 gA-= gig;
}

void gigProd(aINT32 gA, aINT32 gB, aINT32 & carry, aINT32 &prod)
{
  longlong acc = (longlong)gA*gB;
  prod = acc % gig;
  carry = (aINT32)(acc/gig);

}

void fixedC::mult(fixedC &B, const char opSign, RAND &value) 
{                                           // value = this * B
   fixedC prod;
	aINT32 sumCarry1, sumCarry2, prodCarry, underflow, temp;
	char prodSign = getSign() == B.getSign() ? '+' : '-';

	abs();
	B.abs();
	gigProd((aINT32)B.lo, (aINT32)lo, (aINT32 &)prod.lo, (aINT32 &)underflow);
	if(underflow != 0)
	  goto really;                           // underflow
	gigProd(B.lo, (aINT32)hi, (aINT32 &)prod.hi, (aINT32 &)prodCarry);   // prod.hi gets prod carry
	gigPlusEq((aINT32 &)prod.lo, (aINT32)prodCarry, (aINT32 &)sumCarry1); // prod.lo+= temp, sumCarry1  
	if(sumCarry1 != 0)
	  {
		 gigPlusEq((aINT32 &)prod.hi, (aINT32)sumCarry1, (aINT32 &)sumCarry2);  // prod.hi+= sumCarry, t
		 if(sumCarry2 != 0)
			goto really;
	  }
	// prod.lo is done
	gigProd((aINT32)B.hi, (aINT32)lo, (aINT32 &)prodCarry, (aINT32 &)temp);      // 
	gigPlusEq((aINT32 &)prod.hi, (aINT32)prodCarry, (aINT32 &)sumCarry2);
	if(sumCarry2 != 0)
	  goto really;                          // overflow
	gigPlusEq((aINT32 &)prod.lo, (aINT32)temp, (aINT32 &)sumCarry2);
	if(sumCarry2 != 0)
	  {
		 gigPlusEq((aINT32 &)prod.hi, (aINT32)sumCarry2, (aINT32 &)temp);
		 if(temp != 0)
			goto really;                       // overflow
	  }
	gigProd((aINT32)B.hi, (aINT32)hi, (aINT32 &)prodCarry, (aINT32 &)temp);
	if(prodCarry != 0)
	  goto really;                           // overflow
	gigPlusEq((aINT32 &)prod.hi, (aINT32)temp, (aINT32 &)sumCarry1);
	if(sumCarry1 != 0)
	  goto really;                           // overflow

	prod.setSign(prodSign);
	value.setFixed( prod );
	return;

 really:
	LReal r1(*this);
	LReal r2(B);
   Real r;
	LNEWX(r, LReal());
	r1.mult(r2, *r);
	if(prodSign == '-')
	  r->setSign('-');
	value.setReal( r );
	//delete r1;
	//delete r2;
	return;
}

char fixedC::compare(const fixedC &B) const 
{                                         // comparison of fixedC numbers
  if(isPos() && B.isNeg() )               // by virtue of sign
	 return '>';
  if(isNeg() && B.isPos())
	 return '<';

  char result = compareAbs(B);            // both same sign
  return result == '=' ? '=' : (isPos() ? result :
										 (result == '<' ? '>' : '<'));
}

char fixedC::compareAbs(const fixedC &Bin) const 
{                                             // compare fixedC, ignore signs)
  fixedC A = this->abs();
  fixedC B = Bin.abs();

  if(A.hi < B.hi)
	 return '<';
  if(A.hi > B.hi)
	 return '>';
  if(A.lo < B.lo)
	 return '<';
  if(A.lo > B.lo)
	 return '>';
  return '=';
}

#ifdef LANDFILL
void fixedC::Dump()
{
   std::cout << *this;
}
void fixedC::Dump(LEngine *m_peng)
{
   DUMP << *this;
}
#endif








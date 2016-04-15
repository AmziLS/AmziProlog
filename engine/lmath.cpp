/**************************************************************************\
*
* lmath.cpp -- Math routines
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: lmath.cpp,v $
* Revision 1.5  2006/01/03 18:50:32  dennis
* ray's fix, bigdig
*
* Revision 1.4  2005/02/09 17:35:39  dennis
* ?var added
*
* Revision 1.3  2004/04/12 18:09:02  dennis
* Allowed multiple lsx_loads to quietly continue
*
* Revision 1.2  2004/04/09 22:32:38  mary
* Updated to Visual Studio .Net 2003.
*
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.31  2002/12/06 18:31:14  dennis
* modifications for building on HPUX
*
* Revision 1.30  2002/12/06 17:37:36  dennis
* a6-3-2, finally got rid of mscw tagging of heap cells to maintain
* state of dynamic clause backtracking, put backtracking info in
* the choice point structure instead.  much cleaner.  retract can now
* just analyze the control stack to see if a retract is safe.  heapgc
* no longer has to worry about those wierd cells.
*
* Revision 1.29  2002/11/04 04:03:25  dennis
* changed name of ERA to ERAPQ to make gcc happy, don't know why
* that worked
*
* Revision 1.28  2002/09/05 23:05:37  dennis
* Added CheckProfessional to main.cpp, so the version can be checked
* by looking in unlock.xpl in case it didn't come in from the loaded
* .xpl.
*
* Revision 1.27  2002/08/12 16:36:16  dennis
* a6-2-11 changes
*
* Revision 1.26  2002/06/19 04:04:39  dennis
* alib missing exports added, fixed gc/0
*
* Revision 1.25  2002/06/13 00:06:18  dennis
* updated makefiles for Sun GCC, gs3
*
* Revision 1.24  2002/06/09 03:07:57  dennis
* added locking
*
* Revision 1.23  2002/06/02 04:15:42  dennis
* updated makefiles
*
* Revision 1.22  2002/06/02 03:50:56  dennis
* all the XStr forms of logic server calls call strterm and grow the
* heap, so made new ExecProve and CallProve that to the strterm inside
* so that the heap can rolled back to before the Exec/Call.  Important
* that this be done in the Prove, so that if heapgc is encountered,
* the new heap is used for the rollback.
*
* Revision 1.21  2002/05/20 04:34:11  dennis
* final changes
*
* Revision 1.20  2002/05/15 16:59:08  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.19  2002/05/01 19:35:49  ray
* added is_integer to fixed type (lmath.cpp and fixed.h)
*
* Revision 1.18  2002/05/01 02:26:33  dennis
* minor fixes
*
* Revision 1.17  2002/04/17 19:31:16  ray
* Extended q_cf to convert periodic cfs to rational
*
* Revision 1.16  2002/04/09 17:31:18  ray
* corrected error in divide and mod arith
*
* Revision 1.15  2002/03/31 19:43:36  ray
* added round for all types rewrote floor and ceil for reals
*
* Revision 1.14  2002/03/16 00:19:45  ray
* repaired sqrt
*
* Revision 1.13  2002/03/13 15:05:13  ray
* repaired sqrt. repaired fixed mult with -ve rands
*
* Revision 1.12  2002/03/10 22:18:58  dennis
* fixed some small bugs, updated documentation
*
* Revision 1.11  2002/03/04 03:46:12  dennis
* fixed bug in sorted predicates
*
* Revision 1.10  2002/03/01 22:06:09  dennis
* more documentation for 6.2, also timings added to apitrace functions
* execstr and friends.
*
* Revision 1.9  2002/02/28 16:46:56  ray
* rewrote divide
*
* Revision 1.8  2002/02/22 21:24:56  ray
* repairs to fixed mult and divide
*
* Revision 1.6  2002/02/19 04:11:39  dennis
* changed reals to use pass by reference, eliminating almost all needs
* for new and delete, seems to have eliminated most all leaks due to math,
* put in memcpy, memmove etc. for copying gigit arrays around.
*
* Revision 1.5  2002/02/15 02:28:16  dennis
* unicode I/O working again
*
* Revision 1.4  2002/02/14 16:57:27  ray
* replaced divide and shift routines. Corrected LReal constructor calls
*
* Revision 1.3  2002/02/13 19:56:57  dennis
* fixed short bug, floatToReal, and real ** 1
*
* Revision 1.2  2002/02/13 03:20:00  dennis
* reals changed to have a LReal class, moved to file of same name,
* math functions moved out of termsvc and into lmath.cpp/h, eval rewritten
* to re1780flect various options for numbers, lexcept modified so anyone, even
* non-engine objects, can throw LExcept objects.
*
* Revision 1.1  2002/02/04 17:20:59  dennis
* New lreal wrapper class on reals, start of number options:
* decimals: real/float, floats: single/double.  Created lmath.h/cpp
* but nothing in them yet.  Work to be done on sorting out mixed mode
* arithmetic for different options.  Also - casts removed, as they
* were causing problems.
*
*
\**************************************************************************/

#include <math.h>
#include "inc.h"
#include "pch.h"
#include "era.h"

#ifdef LANDFILL
#define noBUG_EVAL
#endif

const double  e = 2.718281828459045;
const double pi = 3.141592653589793;
const double degtorad = (2 * pi) / 360;
const double radtodeg = 360 / (2 * pi);

std::vector<intC> prime;                 // this is the extensible prime array

class PQITEM
{
public:
  PQITEM(intC x, intC y)
  {p = x; pn = y;}
  ~PQITEM() {}

  bool operator<(const PQITEM &item2) const
  { return item2.pn < pn; }

  intC pn;
  intC p;
};

//------------------------------------------------------------
// Some tricky stuff for something or other
//

class PZ
{
public:
  PZ(longlong a, longlong m, long e)
  { acc = a; x = m; n = e;}
  ~PZ() {}

  longlong x;
  longlong acc;
  long n;
};

long powerZ(long x, long n)
{                                          // x**n
  longlong acc = 1;                        // accumulator
  longlong accn;                           // acc at start of loop          
  longlong llx = x;                        // squares accumulator
  longlong xn = 1;                         // square x at start of loop

  if(n == 0)
	 return 1;
  if(n == 1)
	 return x;
  if(x == 0)
	 return 0;
  for(; n; n >>= 1, accn = acc, xn = llx)
	 {
		if(llx > fullgig || llx < -fullgig)
		  throw(PZ(xn, accn, n));
		if(n & 1)
		  {
			 acc *= llx;
			 if(acc > fullgig || acc < -fullgig)
				throw(PZ(xn, accn, n));
		  }
		llx *= llx;
	 }
  return (long)accn;
}

long powerZm(long x, long n, long modulus)
{                                          // x**n
  longlong acc = 1;                        // accumulator
  longlong llx = x;                        // squares accumulator

  if(n == 0)
	 return 1;
  if(n == 1)
	 return x%modulus;
  if(x == 0)
	 return 0;
  for(; n; n >>= 1)
	 {
		if(n & 1)
		  {
			 acc = acc*llx%modulus;
			 if(acc < 0)
				acc += modulus;
		  }
		llx = llx*llx%modulus;
		if(llx < 0)
		  llx += modulus;
	 }
  return (long)acc;
}

TF adjustmod(char discrim, aINT32 modulus, aINT32 &q, aINT32 &rem)
{
  switch(discrim)
    {
    case 'u':
      if(modulus < 0)
        return FALSE;
      if(rem < 0) 
        {
          rem += modulus;
          q -= 1;
        }
      break;
    case 's':
      if(modulus < 0)
        return FALSE;
      if(rem < -(modulus >> 1))
        {
          rem += modulus;
          q -= 1;
        }
      else
        if(rem >= (modulus >> 1))
          {
            rem -= modulus;
            q += 1;
          }
      break;
    default:
      break;
    } 
  return TRUE;
}

TF adjustmod(char discrim, LReal &modulus, LReal &q, LReal &rem)
{
  switch(discrim)
    {
    case 'u':
      if(modulus.isNeg())
        return FALSE;
      if(rem.isNeg()) 
        {
          rem = rem + modulus;
          q = q - 1;
        }
      break;
    case 's':
      if(modulus.isNeg())
        return FALSE;
      if(rem < - (modulus >> 1))
        {
          rem = rem + modulus;
          q = q - 1;
        }
      else
        if(rem >= (modulus >> 1))
          {
            rem = rem - modulus;
            q = q + 1;
          }
      break;
    default:
      break;
    } 
  return TRUE;
}

void euclid1(uintC A0, uintC A1, uintC T0, uintC T1, uintC &G, uintC &T)
{                                            // G is gcd(M, A)
  if(A1 == 0)
    {
      G = A0;
      T = T0;
    }
  else
    euclid1(A1, A0 % A1, T1, T0 - (A0/A1)*T1, G, T);
}

uintC inverse(uintC M, uintC I)
{                               // multiplicative inverse of A modu M. M prime.
  uintC G, T;

  euclid1(M, I, 0, 1, G, T);                 // G is gcd(M, A)
    if((G % M) != 1)                         //  G is a unit or fail
      throw LExcept(number_domainE, aS("inverse"));
  return T % M;
}

static int lo, hi;
static char nolofactors[] = {
1,0,0,0,0,0,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,
1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,1,0,0,0,0,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,0,
1,0,0,1,0,1,0,0,1,0,0,1,1,0,0,0,0,1,1,0,1,1,0,0,0,0,0,1,0,0,0,0,0,1,0,1,1,0,1,
0,0,1,1,0,0,0,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,0,0,0,0,0,0,1,0,1,
1,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,0,1,0,1,0,0,1,
1,0,0,1,0,1,0,0,0,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,
0,0,0,0,0,1,0,0,0,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,1,0,0,
1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,0,
0,0,1,1,0,0,0,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,
1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,0,1,0,1,0,0,0,1,0,0,1,0,1,0,0,0,1,0,1,0,0,1,
0,0,0,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,
1,0,1,0,0,0,0,0,0,1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,0,0,0,1,0,0,0,0,1,0,0,0,
1,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,1,
0,0,1,1,0,0,0,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,0,0,0,1,0,0,1,0,1,
1,0,1,0,0,1,0,0,1,0,0,0,1,0,1,1,0,0,1,0,0,0,0,1,0,0,0,1,0,1,1,0,0,1,0,1,0,0,0,
1,0,0,0,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,0,0,1,
1,0,0,0,0,1,0,0,0,0,0,1,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,0,0,0,1,1,0,0,
1,0,0,1,0,0,0,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,0,0,0,0,1,0,1,
0,0,1,1,0,0,0,0,0,1,0,1,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,0,0,0,0,1,0,0,0,0,1,
1,0,1,1,0,1,0,0,1,1,0,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,0,1,0,1,0,0,1,
0,0,0,1,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,
0,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,0,0,0,1,1,0,0,
1,0,1,1,0,0,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,0,
0,0,0,1,0,0,0,0,1,0,0,1,1,0,1,0,0,1,1,0,0,0,0,0,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,
1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,0,0,1,0,0,1,
1,0,0,1,0,1,0,0,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,
1,0,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,0,0,1,0,0,0,
0,0,1,1,0,1,0,0,0,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,0,0,0,1,0,0,1,0,1,
0,0,0,0,0,0,0,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,0,
1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,1,0,1,0,0,1,
1,0,0,1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,
1,0,1,0,0,0,0,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,0,1,0,0,1,0,1,1,0,0,0,0,1,1,0,0,
0,0,1,1,0,1,0,0,0,0,0,0,1,0,0,0,0,1,1,0,1,1,0,1,0,0,1,1,0,0,1,0,0,0,0,1,0,0,1,
0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,0,
0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,0,0,0,1,
1,0,0,0,0,1,0,0,1,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,0,0,0,0,0,1,1,0,0,1,0,1,1,0,0,
1,0,1,0,0,1,0,0,0,0,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,0,
1,0,0,0,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,0,1,0,1,0,0,1,
0,0,1,1,0,0,0,0,0,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,0,0,0,0,0,0,1,0,1,
1,0,0,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0,0,1,
1,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,1,0,1,
0,0,1,0,0,1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,0,0,0,0,1,0,0,
1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,0,
0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,0,0,0,0,0,1,0,1,
1,0,0,1,0,0,0,0,1,0,0,0,1,0,1,1,0,1,1,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,1,0,0,0,
1,0,0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,
1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,0,1,0,0,0,0,1,0,0,0,
1,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,0,0,0,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,1,
0,0,1,0,0,0,0,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,1,0,0,0,1,0,0,0,0,1,
1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,0,
1,0,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,
1,0,0,0,0,1,0,0,0,0,0,1,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,0,0,0,0,0,1,1,0,0,
1,0,1,1,0,0,0,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,0,0,0,0,1,0,0,1,0,0,0,0,1,1,0,1,
0,0,1,1,0,0,0,0,0,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,1,0,1,1,0,0,0,0,0,1,0,0,0,0,1,
0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,0,0,1,0,0,1,
0,0,0,1,0,0,0,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,0,
1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,0,0,0,1,0,0,0,0,0,1,1,0,1,1,0,0,0,0,1,1,0,0,
1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,0,0,0,0,0,0,1,0,0,1,0,1,1,0,1,
0,0,0,1,0,0,0,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,
1,0,1,0,0,0,0,0,1,1,0,0,0,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,0,0,1,0,0,1,
1,0,0,1,0,1,0,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,1,0,0,0,0,0,0,1,0,1,0,0,1,
1,0,0,0,0,1,0,0,0,1,0,0,1,0,1,1,0,1,0,0,1,1,0,0,0,0,1,1,0,1,1,0,0,0,0,0,1,0,0,
0,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,0,1,0,1,
0,0,1,0,0,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1,1,0,1,0,0,0,1,0,0,1,0,0,
1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,0,0,1,
0,0,0,1,0,1,0,0,1,0,0,0,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,0,0,1,1,0,1,
1,0,1,0,0,0,0,0,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,
1,0,1,1,0,1,0,0,0,0,0,1,1,0,0,0,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,0,0,1,
0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,0,1,0,1,0,0,0,1,0,0,1,0,1,
0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,0,0,1,0,0,0,0,1,
1,0,0,0,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,0,0,0,
1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,0,0,0,0,0,1,1,0,0,
1,0,0,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,0,1,0,0,1,0,1,
0,0,1,1,0,0,0,0,1,1,0,1,0,0,1,0,0,1,0,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0,0,0,1,0,1,
1,0,1,1,0,1,0,0,0,1,0,0,0,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,0,1,0,0,1,0,1,0,0,1,
1,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,
0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,0,0,0,0,1,0,0,
1,0,1,0,0,0,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,0,
0,0,1,1,0,0,0,0,1,1,0,1,1,0,1,0,0,1,1,0,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,
1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,0,1,0,1,0,0,1,
1,0,0,1,0,0,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,
1,0,1,0,0,1,0,0,0,1,0,1,1,0,0,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,0,0,0,1,0,0,0,
1,0,1,1,0,1,0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,0,0,0,1,0,1,1,0,1,
0,0,0,1,0,0,0,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,1,
1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0,1,0,0,0,
1,0,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,0,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,
1,0,0,0,0,1,0,0,0,0,0,1,1,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,0,1,0,0,0,0,0,1,1,0,0,
0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,1,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,0,0,0,1,1,0,1,
0,0,1,1,0,0,0,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,0,0,0,0,1,0,0,0,0,0,
1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,0,0,1,
0,0,0,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,0,1,0,0,1,0,1,
1,0,1,0,0,1,0,0,0,0,0,1,1,0,1,1,0,0,0,0,0,1,0,0,0,0,1,1,0,1,1,0,0,0,0,1,1,0,0,
1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,0,0,1,
0,0,0,1,0,0,0,0,0,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,
1,0,0,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,1,0,0,1,0,0,0,1,0,1,1,0,0,0,0,0,0,0,1,
1,0,0,1,0,1,0,0,0,1,0,1,0,0,1,0,0,0,0,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,
1,0,1,0,0,1,0,0,0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,
0,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,0,1,0,1,
0,0,1,0,0,0,0,0,1,1,0,0,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,1,0,0,
1,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,0,0,0,
1,0,0,1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,1,0,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,
1,0,1,0,0,0,0,0,0,1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,0,0,0,1,1,0,0,
1,0,1,1,0,1,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,0,0,1,
0,0,1,0,0,0,0,0,1,1,0,0,1,0,0,0,0,1,0,0,0,1,0,1,1,0,1,1,0,1,0,0,0,1,0,0,0,0,1,
0,0,1,1,0,1,0,0,0,1,0,0,1,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,0,0,0,1,0,0,0,0,1,
1,0,0,0,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,0,
1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,0,1,0,0,0,0,1,1,0,0,
1,0,0,1,0,1,0,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,0,1,0,1,1,0,1,
0,0,1,1,0,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,0,1,0,1,0,0,0,0,0,0,1,0,1,
0,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,0,0,0,1,0,0,1,
1,0,0,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,
0,0,1,0,0,1,0,0,0,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,0,0,0,0,1,0,0,
1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,0,0,0,1,0,1,1,0,1,0,0,0,1,0,0,1,0,0,1,0,1,1,0,0,
0,0,1,1,0,0,0,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,
1,0,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,0,1,0,1,0,0,1,
1,0,0,1,0,0,0,0,0,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,0,0,1,0,0,0,0,0,1,1,0,1,
1,0,0,0,0,1,0,0,0,1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,
1,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,1,
0,0,1,1,0,0,0,0,1,1,0,1,0,0,0,0,0,1,1,0,0,0,0,0,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,
1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,0,1,0,1,0,0,0,
0,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,0,1,0,0,1,0,1,1,0,1,0,0,1,1,0,0,0,0,1,1,0,1,
1,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,0,0,1,0,0,0,0,0,1,1,0,0,
1,0,1,1,0,0,0,0,1,0,0,1,0,0,0,1,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1,1,0,1,
0,0,1,1,0,0,0,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1,
1,0,1,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,0,0,0,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,0,0,1,
0,0,0,0,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,0,0,1,
1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,0,0,0,0,0,1,1,0,0,
1,0,0,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0,1,
0,0,0,1,0,0,0,0,1,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,0,0,0,0,1,0,0,1,0,1,
1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,1,0,0,0,0,1,0,0,1,
1,0,0,1,0,1,0,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,
0,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,
0,0,1,1,0,0,0,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,
0,0,1,0,0,0,0,0,1,1,0,1,1,0,1,0,0,0,1,0,0,0,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,0,
1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,0,0,1,
1,0,0,1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,
1,0,1,0,0,0,0,0,0,1,0,1,0,0,0,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,0,0,0,1,0,0,0,
1,0,1,1,0,1,0,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,1,0,0,1,1,0,0,0,0,0,1,0,1,0,0,1,
0,0,0,1,0,0,0,0,1,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,1,
0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,0,1,0,0,0,0,1,
1,0,0,0,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,
1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,0,0,0,0,1,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,0,
0,0,0,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,1,1,0,1,
0,0,1,1,0,0,0,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,
1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,0,1,0,1,0,0,1,
1,0,0,1,0,1,0,0,1,1,0,0,0,0,1,0,0,0,0,0,1,1,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,0,1,
0,0,1,0,0,1,0,0,0,0,0,1,1,0,0,1,0,1,0,0,0,0,0,0,1,0,1,1,0,1,1,0,0,0,0,0,1,0,0,
1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,0,0,0,
0,0,1,1,0,0,0,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,
1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,1,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0,1,
1,0,0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,0,0,0,1,1,0,1,1,0,0,0,0,0,1,0,0,0,0,1,1,0,1,
1,0,1,0,0,1,0,0,0,1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,0,0,0,1,0,0,0,
1,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,1,
0,0,1,1,0,0,0,0,1,1,0,0,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,1,0,0,0,0,0,0,1,0,1,
1,0,1,0,0,0,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,0,0,1,1,0,0,1,0,1,0,0,0,
1,0,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,
1,0,0,0,0,1,0,0,0,0,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,0,0,0,0,0,1,1,0,0,
1,0,1,1,0,0,0,0,1,0,0,1,0,0,0,1,0,1,1,0,0,1,0,1,0,0,0,1,0,0,1,0,0,0,0,1,1,0,1,
0,0,1,0,0,0,0,0,0,1,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,0,0,0,1,0,0,0,0,1,
1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,0,0,0,1,0,1,0,0,1,
0,0,0,1,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,
1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,1,0,0,0,0,1,1,0,0,
1,0,1,1,0,1,0,0,1,0,0,1,0,0,0,1,0,1,1,0,0,1,0,0,0,0,1,0,0,0,1,0,0,1,0,1,1,0,1,
0,0,0,1,0,0,0,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,0,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,
0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,0,0,1,0,0,1,
1,0,0,1,0,0,0,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,0,
1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,
0,0,1,1,0,1,0,0,1,0,0,1,1,0,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,1,0,1,
0,0,1,0,0,0,0,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,0,1,0,0,1,0,0,
1,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,0,0,1,
1,0,0,1,0,1,0,0,0,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,
1,0,0,0,0,0,0,0,0,1,0,1,0,0,1,1,0,1,0,0,1,1,0,0,0,0,0,1,0,1,1,0,0,0,0,0,1,0,0,
1,0,1,1,0,1,0,0,0,0,0,1,1,0,0,0,0,1,0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,1,0,0,1,
0,0,1,1,0,0,0,0,1,1,0,0,0,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,
0,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,0,1,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,0,0,0,1,
0,0,0,0,0,1,0,0,1,1,0,0,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,0,0,1,1,0,0,
1,0,1,0,0,0,0,0,0,1,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,0,
1,0,0,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,0,0,0,1,1,0,0,0,0,0,1,0,1,1,0,1,
0,0,1,1,0,0,0,0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,1,0,0,0,0,0,0,1,0,1,
1,0,1,1,0,1,0,0,0,0,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,0,1,0,0,1,0,0,1,0,1,0,0,1,
1,0,0,0,0,1,0,0,1,1,0,0,0,0,0,1,0,0,0,0,1,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,0,0,1,
0,0,1,0,0,1,0,0,0,1,0,1,1,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,0,0,0,0,0,0,1,0,0,
1,0,0,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,0,1,0,0,1,0,0,
0,0,1,1,0,0,0,0,1,1,0,1,1,0,1,0,0,1,0,0,0,1,0,1,1,0,1,0,0,0,0,0,0,1,0,0,1,0,1,
1,0,0,1,0,1,0,0,1,0,0,0,0,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,0,1,0,1,0,0,1,
1,0,0,1,0,0,0,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,
0,0,1,0,0,1,0,0,0,1,0,0,1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,0,0,0,0,1,0,0,0,
1,0,1,1,0,0,0,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,0,1,1,0,0,
0,0,1,1,0,0,0,0,1,1,0,1,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,1,0,0,0,1,0,0,1,0,1,
1,0,1,0,0,1,0,0,1,1,0,0,1,0,1,0,0,0,1,0,1,0,0,1,0,0,0,1,0,1,1,0,0,1,0,1,0,0,0,
1,0,0,1,0,1,0,0,1,0,0,1,0,0,1,1,0,0,1,0,0,1,0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,
1,0,0,0,0,1,0,0,0,0,0,1,1,0,0,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,0,0,0,1,0,0,0,
1,0,1,1,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1};

int nolos = sizeof(nolofactors);


#if ! ( (defined(SOLARIS) || defined(HPUX) || defined(BSD) || defined(LINUX) ) && defined(GNU) )
void floatfields(longlong *llp, char *sign, int *exp, longlong *mant)
{                                            // analyse prolog float (double)
  *sign = *llp < 0 ? '-' : ' ';
  *exp = ((*llp >> 52) & 0x7ff) - 0x400;
  *mant = (*llp & 0xfffffffffffff) | 0x10000000000000;
}
#else  // solaris gnu has problems with the longlong constants
void floatfields(longlong *llp, char *sign, int *exp, longlong *mant)
{                                            // analyse prolog float (double)
  *sign = *llp < 0 ? '-' : ' ';
  *exp = ((*llp >> 52) & 0x7ff) - 0x400;
  *mant = (*llp & 0xfffffffffffffLL) | 0x10000000000000LL;
}
#endif


//---------------------------------------------------------------
// RAND functions
//

double RAND::toDouble() const
//RAND::operator double()                      // coerce rand to double
//  { return kind == intS ? (double)(a_int) : (kind == doubleS ? a_double : 0); }
{
   switch(kind)
   {
   case intS:
      return (double)a_int;
      break;
   case singleS:
      return (double)a_single;
      break;
   case doubleS:
      return a_double;
      break;
   case realS:
      return a_real->toDouble();
      break;
   case fixedS:
      return a_fixed.toDouble();
      break;
   default:
      throw LExcept(number_castE, aS("to double"));
   }
}

float RAND::toSingle() const
{
   return (float)toDouble();
}

Real RAND::toReal() const
// NOTE that caller is responsible for disposing of new real
{   // coerce rand to real
   Real r;
   switch(kind)
   {
   case realS:
      LNEWX(r, LReal(*a_real)); // make copy in case receiver deletes
      return r;
   case intS:
      LNEWX(r, LReal(a_int));
      return r;
      //return newReal(a_int);
   case fixedS:
      LNEWX(r, LReal(a_fixed));
      return r;
      //return newReal(a_fixed);
   case singleS:
      LNEWX(r, LReal());
      floatToReal((double)a_single, *r);
      return r;
   case doubleS:
      LNEWX(r, LReal());
      floatToReal(a_double, *r);
      return r;
   case listT:
      return listToReal(a_list);
   default:
      throw LExcept(number_castE, aS("to real"));
   }
}
//#endif

intC RAND::toInt() const
//RAND::operator Real()
{   // coerce rand to real
   switch(kind)
   {
   case intS:
      return a_int;
   case realS:
      return a_real->toInt();
      //return newReal(a_int);
   case fixedS:
      return a_fixed.toInt();
      //return newReal(a_fixed);
   case doubleS:  
      return (long)(a_double);
   case singleS:
      return (long) (a_single);
   default:
      throw LExcept(number_castE, aS("to integer"));
   }
}

fixedC RAND::toFixed() const
//RAND::operator fixedC()
{                                          // coerce rand to fixed
	 fixedC result;
	 //char sign;

	 switch(kind)
      {
		case fixedS:
		  //result.hi = this->a_fixed.hi;
		  //result.lo = this->a_fixed.lo;
        result = this->a_fixed;
		  break;
		case realS:
		  if(a_real->isFixable())
           result = a_real->toFixed();  // already have cast
        else
			 //{
			//	result.hi = a_real->getGd(2);
			//	result.lo = a_real->getGd(1);
			//	sign = a_real->getSign();
			//	if(sign == '-')
			//	  result.setNeg();
			//	break;
			 //}
		    throw LExcept(number_castE, aS("to fixed"));
		  break;
      case singleS:
         return floatToFixed(a_single);
      case doubleS:
         return floatToFixed(a_double);
      case intS:
		case charS:
         if (isAbsGigit(a_int))
         {
            result.setSignedHI(a_int);
            result.setLO(0);
         }
		  //sign = a_int < 0 ? '-' : '+';
		  //result.hi = a_int < 0 ? -a_int : a_int;
		  //result.lo = 0;
		  //if(result.hi < 1000000000)
		//	 {                                  // valid fixedC
		//		if(sign == '-')
		//		  result.setNeg();
				break;
      default:
        throw LExcept(number_castE, aS("to fixed"));
		  break;
      }
	 return result;
}

void RAND::makeCell(LEngine *m_peng, Cell &c)
{
   // Fill the provided cell, c, with the proper
   // arithmetic value.
   switch(kind)
   {
   case intS:
      c.setInt(a_int);
      return;
   case fixedS:
      c.setFixed(a_fixed);
      return;
   case realS:
      GCReal *rr;
      rr = pGCTH->make_real(a_real);
      c.setReal(rr);
      return;
   case singleS:
      c.setSingle(a_single);
      return;
   case doubleS:
      GCDouble *dd;
      dd = pGCTH->make_float(a_double);
      c.setDouble(dd);
      return;
   default:
      pXCPT->Error(internalE, aS("Bad RAND type"));
   }
}

#ifdef LANDFILL
void RAND::Dump(LEngine *m_peng)
{
   // Fill the provided cell, c, with the proper
   // arithmetic value.
   fixedC f;

   switch(kind)
   {
   case intS:
      DUMP << "integer " << getInt();
      return;
   case fixedS:
      f = getFixed();
      DUMP << "fixed " << f;
      //DUMP << "fixed " << getFixed();
      return;
   case realS:
      DUMP << "real " << getReal() << SP2 << *getReal();
      return;
   case singleS:
      DUMP << "single " << getSingle();
      return;
   case doubleS:
      DUMP << "double " << getDouble();
      return;
   default:
      DUMP << "Bad Kind" << NL;
      pXCPT->Error(internalE, aS("Bad RAND type"));
   }
   DUMP << NL;
}
#endif
//---------------------------------------------------------------
// LMath
//

LMath::LMath(LEngine *peng)
{
   m_peng = peng;
}

void LMath::Init()
{
}

// random atom, floating point number between 0 and 1.  
// Because it can actually be 1.0, it is not very useful for 
// generating integer ranges, so we filter out the 1.0 cases.  

double LMath::e_random()
{
   double r;
   r =  rand() / (double) RAND_MAX;
   while (r > 0.999999)
      r =  rand() / (double) RAND_MAX;
   return r;
}

double LMath::e_cputime()
{                                  // cputime atom, floating point cpu seconds 
// MS doesn't support clock() for DLLs in the 16-bit world.
#if defined(P16) && defined(LIB_DLL)
   time_t now;
   time(&now);
   return (double)difftime(now,StartTime);
#else
   return (double) clock() / (double) CLOCKS_PER_SEC;
#endif
}

clock_t LMath::e_cpuclock()
{                                  // cputime atom, floating point cpu seconds 
   return clock();
}

//---------------------------------------------------------------
// Evaluation of mathematical expressions
//

// Philosophy of references vs. pointers: when we want the
// the subroutine to fill in a value of an object, we can
// either pass a pointer or the object into a reference.  Only
// difference is . or -> in subroutine.  Arbitrary decision,
// hmmm, lets go with newer C++ reference style...

// EvalTop is top entry point for evaluation, and makes final
// adjustments for default float type, etc.  Eval is the
// recursive call.

// promote is the function that decides what type two numbers
// of different types should be.  answer depends on whether the
// system is in decimals real or float.  As to floats, all work
// done in doubles until final answer comes back through EvalTop

// for the various operators, each type gets a shot, and then
// defers to another more general type.  ints defer to reals
// or doubles, depending on decimals.  fixed defer to reals.
// reals defer to doubles, and convert answer back.  doubles
// throw an error if no such operator.

void LMath::EvalTop(TERM t, RAND &value)
{ // returns RAND containing value of the expression under term
#ifdef BUG_EVAL
 DUMP << "===>>> EvalTop" << NL;
 pTSVC->DumpWrite(t);
 DUMP << NL;
 pTSVC->Dump(t);
 DUMP << NL;
#endif
   t = t->dref();

   try
   {
      Eval(t, value);

      // fixed the final answer, if necessary, reals
      // to fixed, floats to single if need be.
      if (value.isReal() && value.getReal()->isFixable())
      {
         value.setFixed( value.getReal()->toFixed() );
      }
      else if (value.isDouble() && pSTATE->m_floats == single_)
      {
         value.setSingle( (float)value.getDouble() );
      }
   }
   catch(LExcept &ex)
   {
      if (ex.GetType() == ARITH)
      {
         LString msg(aS(" in term: "));
         aCHAR tstr[256];
         pWRIT->termWriteString(t, tstr, 255, true);
         msg = msg + tstr;
         ex.AddToMsg(msg);
      }
      throw ex;
   }
#ifdef BUG_EVAL
 DUMP << "returned value = ";
 value.Dump(m_peng);
 DUMP << NL << "<<<=== EvalTop" << NL;
#endif
   return;
}

void LMath::Eval(TERM term, RAND &value)
{ // returns RAND containing value of the expression under term
#ifdef BUG_EVAL
 DUMP << "===>>> Eval/2 " << NL;
 pTSVC->DumpWrite(term);
 //pTSVC->Dump(term);
 DUMP << NL;
#endif

   term = term->dref();

   switch(term->getType())
   {
   case intS:
   case charS:
      value.setInt( term->getInt() );
      break;
   case singleS:
      value.setSingle( term->getSingle() );
      break;
   case doubleS:
      value.setDouble( term->getDouble() );
      break;
   case fixedS:
		value.setFixed( term->getFixed() );
		break;
   case realS:
      // a tricky business - gcthing will gc the real in the
      // term, so don't let RAND delete it, get copy instead.
      value.setReal( term->getRealCopy() );
      break;
   case atomS:
      //evalatom = term->getAtom();
      evalAtom( term->getAtom(), value );
      break;
   case listT:
      // reals can be represented as lists of gigits
      value.setReal( listToReal(term) );    
      break;
   case strucT:         // the term is an expression
      EvalExp(term, value);
      break;
   default:
      throw LExcept(number_domainE, aS("evaluating expression"));
   }
#ifdef BUG_EVAL
 DUMP << "returned value = ";
 value.Dump(m_peng);
 DUMP << NL << "<<<=== Eval/2" << NL;
#endif
}

void LMath::EvalExp(TERM term, RAND & value)
{ // a structure, arithmetic operator with args
   RAND         rand1, rand2;  
   PATOM        rator;
   //long         i1;
   //double       d1;
   //Real         r1;
   //fixedC       f1;
   TF           isBinary;

#ifdef BUG_EVAL
 DUMP << "===>>> EvalExp " << NL;
 pTSVC->DumpWrite(term);
 //pTSVC->Dump(term);
 DUMP << NL;
#endif
  term = term->dref();
  term = term->getTerm();  // get to functor
  rator = term->getAtom();   // get operator
  
  isBinary = term->isBinary();
  
  Eval(++term, rand1);                 // evaluate the first arg 

  if(isBinary)
    {
      Eval(++term, rand2);                // evaluate 2nd arg
      Eval(rand1, rand2, rator, value);    // rands are resolved
      return;
	 }

  switch(rand1.getKind())
    {
    case intS:
      evalInt1(rand1, rator, value);
      break;
    case fixedS:
      evalFixed1(rand1, rator, value);
      break;
    case singleS:
      //rand1.setDouble( (double)rand1.getSingle() );
      evalSingle1(rand1, rator, value);
      break;
    case doubleS:
      evalDouble1(rand1, rator, value);
      break;
    case realS:
		evalReal1(rand1, rator, value);
      break;
    default:
      throw LExcept(number_castE, aS("in expression"));
    }
#ifdef BUG_EVAL
 DUMP << "returned value = ";
 value.Dump(m_peng);
 DUMP << NL << "<<<=== EvalExp" << NL;
#endif
}

void LMath::Eval(RAND &rand1, RAND &rand2, PATOM rator, RAND &value)
{  // named arithmetic operation on two terms
#ifdef BUG_EVAL
 DUMP << "===>>> Eval/4 " << NL;
 DUMP << "rator = " << (STRptr)*rator << NL;
 DUMP << "rand1 = ";
 rand1.Dump(m_peng);
 DUMP << NL << "rand2 = ";
 rand2.Dump(m_peng);
 DUMP << NL;
#endif
   long modulus = pSTATE->m_modulo;

   // will change the value of one or the other
   // rands, if necessary, to bring both to the
   // correct type
   Tag randType = promote(rand1, rand2);

   switch(randType)
   {
   case intS:
      evalInt2(rand1, rand2, rator, value);
      return;
   case fixedS:
		evalFixed2(rand1, rand2, rator, value);
		return;
   case singleS:
      evalSingle2(rand1, rand2, rator, value);
      return;
   case doubleS:
      evalDouble2(rand1, rand2, rator, value);
      return;
   case realS:   
       evalReal2(rand1, rand2, rator, value);
       LASSERT((value.isReal()), aS("evalReal2 returned non-real"));
       if(modulus > 0)
       {
          Real v;
          LNEWX(v, LReal());
          value.getReal()->modulus(modulus, pSTATE->m_delta, *v);
          value.setReal(v);
          //if (value.isReal())
          //  RealModu(value.getReal(), modulus, value);
       }
       return;
   default:
      throw LExcept(number_castE, aS("evaluating expression"));
      return;
   }
#ifdef BUG_EVAL
 DUMP << "returned value = ";
 value.Dump(m_peng);
 DUMP << NL << "<<<=== Eval/4" << NL;
#endif
}

Tag LMath::promote(RAND &a, RAND &b)
{
   // will change one of the rands, so
   // both the same, if necessary
   if (a.getKind() == b.getKind())
   {
      //if (a.isSingle())
      //{
      //   a.setDouble( a.toDouble() );
      //   b.setDouble( b.toDouble() );
      //   return doubleS;
      //}
      //else
         return a.getKind();
   }

   Real r;

   if (pSTATE->m_decimals == real_)
   {
      if (a.isReal())
      {
         b.setReal( b.toReal() );
         return realS;
      }
      else if (b.isReal())
      {
         a.setReal( a.toReal() );
         return realS;
      }
      else if (a.isFixed())
      {
         r = b.toReal();
         if (r->isFixable())
         {
            b.setFixed( r->toFixed() );
            delete r;
            return fixedS;
         }
         else
         {
            a.setReal( a.toReal() );
            b.setReal( r );
            return realS;
         }
      }
      else if (b.isFixed())
      {
         r = a.toReal();
         if (r->isFixable())
         {
            a.setFixed( r->toFixed() );
            delete r;
            return fixedS;
         }
         else
         {
            b.setReal( b.toReal() );
            a.setReal( r );
            return realS;
         }
      }
      // one must be a float by this point, so make
      // them both double for arithmetic, main Eval
      // will set back to single if necessary
      else 
      {
         a.setDouble( a.toDouble() );
         b.setDouble( b.toDouble() );
         return doubleS;
      }
   }
   else  // m_decimals == float_
   {
      if (a.isDouble() || b.isDouble())
      {
         a.setDouble( a.toDouble() );
         b.setDouble( b.toDouble() );
         return doubleS;
      }
      else if (a.isSingle() || b.isSingle())
      {
         a.setSingle( a.toSingle() );
         b.setSingle( b.toSingle() );
         return singleS;
      }
      else  // not both ints, so make them reals
      {
         a.setReal( a.toReal() );
         b.setReal( b.toReal() );
         return realS;
      }
   }
}

Tag LMath::demote(RAND &a, RAND &b)
{
   // for almost equals, converts both to the
   // least significant of the two
   if (a.getKind() == b.getKind())
   {
      return a.getKind();
   }

   if (a.isInt() || b.isInt())
   {
      a.setInt( a.toInt() );
      b.setInt( b.toInt() );
      return intS;
   }
   else if (a.isSingle() || b.isSingle())
   {
      a.setSingle( a.toSingle() );
      b.setSingle( b.toSingle() );
      return singleS;
   }
   else if (a.isDouble() || b.isDouble())
   {
      a.setDouble( a.toDouble() );
      b.setDouble( b.toDouble() );
      return doubleS;
   }
   else
   {
      a.setReal( a.toReal() );
      b.setReal( b.toReal() );
      return realS;
   }
}

void LMath::evalAtom( PATOM a, RAND &value )
{
   if (a == pATAB->eA)
      value.setDouble( e );
   else if (a == pATAB->piA)
      value.setDouble( pi );
   else if (a == pATAB->degtoradA)
      value.setDouble( degtorad );
   else if (a == pATAB->radtodegA)
      value.setDouble( radtodeg );
   else if (a == pATAB->cpuclockA)
      value.setInt( e_cpuclock() );
   else if (a == pATAB->cputimeA)
      value.setDouble( e_cputime() );
   else if (a == pATAB->randomA)
      value.setDouble( e_random() );
   else if (a == pATAB->infA)
      value.setDouble( HUGE_VAL );
   else
      //throw badArg;
      throw LExcept(arithargE, (STRptr)*a);

   if (pSTATE->m_decimals == real_)
   {
      value.setReal( value.toReal() );
   }

   return;
}

void LMath::evalInt1(RAND& rand1, PATOM rator, RAND& value)
{
   // uses integer functions where available, defers
   // to floats if not found.
#ifdef BUG_EVAL
 DUMP << "EVAL evalInt1: ";
 DUMP << rator << SP << rand1.getInt();
 DUMP << NL;
#endif

   long i1 = rand1.getInt();

   if (rator == pATAB->floatA) 
      value.setDouble( (double)i1 );
   else if (rator == pATAB->plusA)
      value.setInt( i1 );
   else if (rator == pATAB->minusA)
      value.setInt( -i1 );
   else if (rator == pATAB->notA)    // bitwise '\' operator
      value.setInt( ~i1 );
   else if (rator == pATAB->integerA)
      value.setInt( i1 );
   else if (rator == pATAB->absA)
      value.setInt( abs(i1) );
   else if (rator == pATAB->floorA)
      value.setInt( i1 );
   else if (rator == pATAB->ceilA)
      value.setInt( i1 );
   else if (rator == pATAB->roundA)
      value.setInt( i1 );
   else if (rator == pATAB->signA)
      value.setInt( signum(i1) );
   else // see if real or float can handle it
   {
      if (pSTATE->m_decimals == real_)
      {
         rand1.setReal( rand1.toReal() );
         evalReal1(rand1, rator, value);
      }
      else  // decimals float_
      {
         rand1.setDouble( rand1.toDouble() );
         evalDouble1(rand1, rator, value);
      }
   }
   return;
}

// A negative modulus has no meaning and shouldn't be allowed.
// However, C calls % the modulus operator even though it is defined
// as the remainder after division. The Prolog standard properly 
// distinguishes 'rem' from 'mod' and allows negative modulus to produce
// an 'undefined' error.
void LMath::evalInt2(RAND& rand1, RAND& rand2, PATOM rator, RAND& value)
{
   aINT32 quotient = 0, halfdivisor, absmod, remainder;
   aINT32 residue, modulus = pSTATE->m_modulo;
   ldiv_t divtemp;
   char discrim;
   bool oddmod;    
   Real r;

   intC i1 = rand1.getInt();
   intC i2 = rand2.getInt();

   if (rator == pATAB->minusA || rator == pATAB->plusA)
   {
	  longlong sum = rator == pATAB->minusA ? 
		 (longlong)i1 - i2 : (longlong)i1 + i2;
      if(modulus == 0)
      {
         if(sum < 0x7fffffff && sum > -0x7fffffff)
            value.setInt( (intC)sum );
         else
         {  // int overflowed, promote to real
            //value.a_real = newReal(sum);
            if (pSTATE->m_decimals == real_)
            {
               LNEWX(r, LReal(sum));
               value.setReal(r);
            }
            else  // let float handle it
            {
               rand1.setDouble( rand1.toDouble() );
               rand2.setDouble( rand2.toDouble() );
               evalDouble2(rand1, rand2, rator, value);
            }
         }
      }
      else  // different modulus
      {
         discrim = modulus > 0 ? 'u' : 's';
         residue = sum % modulus;
         adjustmod(discrim, modulus, quotient, residue);
         value.setInt( residue );
      }
   }
   else if (rator == pATAB->timesA)
   {
      if(modulus == 0)
      {
         longlong prod = (longlong)i1*i2;
         if(prod < 0x7fffffff && prod > -0x7fffffff)
            value.setInt( (intC)prod );
         else
         {  // int overflowed, promote to real
               //value.a_real = newReal(sum);
            if (pSTATE->m_decimals == real_)
            {
               LNEWX(r, LReal(prod));
               value.setReal(r);
            }
            else  // let float handle it
            {
               rand1.setDouble( rand1.toDouble() );
               rand2.setDouble( rand2.toDouble() );
               evalDouble2(rand1, rand2, rator, value);
            }
         }
      }
      else  // other modulus
      {
         discrim = modulus > 0 ? 'u' : 's';
         residue = ((longlong)i1*(longlong)i2) % modulus;
         adjustmod(discrim, modulus, quotient, residue);
         value.setInt( residue );
      }
   }

   else if (rator == pATAB->divA)
   {                                      // div (not an int result)
      if (i2 == 0)
         throw LExcept(zero_divideE);

      if (pSTATE->m_decimals == real_)
       {
          rand1.setReal( rand1.toReal() );
          rand2.setReal( rand2.toReal() );
          evalReal2(rand1, rand2, rator, value);
       }
      else // decimals float_
      {
         rand1.setDouble( rand1.toDouble() );
         rand2.setDouble( rand2.toDouble() );
         evalDouble2(rand1, rand2, rator, value);
      }
   }

   else if (rator == pATAB->divdivA)
   {                                       // int div
		modulus = pSTATE->m_modulo;
		if(modulus > 0)
      {
			 longlong prod = 
				(longlong)i1*inverse(modulus, i2);  // divdiv repacement in Zm
			 value.setInt( prod % modulus );
      }
      else
      {
         switch(pSTATE->m_rounded)
         {
           case TO_ZERO:
             if (i2 == 0)
                throw LExcept(zero_divideE);
             value.setInt( i1/i2 );
             break;
           case TO_FLOOR:
             goto divuA;
           case TO_NEAREST:
             goto divsA;
           default:
             pXCPT->Error(internalE, aS("bad state for rounded"));
         }
      }
   }

  else if (rator == pATAB->remA)
    {                                       // rem
      switch(pSTATE->m_rounded)
        {
        case TO_ZERO:
          if (i2 == 0)
             throw LExcept(zero_divideE);
          value.setInt( i1 % i2 );
          break;
        case TO_FLOOR:
          goto moduA;
        case TO_NEAREST:
          goto modsA;
        }
    }
  else if (rator == pATAB->divsA)
    {                                        // divsA
    divsA:
      if (i2 <= 0)                           // negative mod not allowed
        throw LExcept(zero_divideE);
      
      oddmod = (i2 & 1) == 1;
      absmod = abs(i2);
      halfdivisor = absmod/2;
      divtemp = ldiv(i1, i2);                // ldiv is division of longs
      quotient = divtemp.quot;               // quotient
      if(divtemp.rem < -halfdivisor) 
        quotient--;
      if(oddmod)                         
        if(divtemp.rem > halfdivisor) 
          quotient++;
        else;
      else
        if(divtemp.rem >= halfdivisor) 
          quotient++;
      value.setInt( quotient );
    }
      
    else if (rator == pATAB->modsA)
      {                                      // modsA
      modsA:
        if (i2 <= 0)                         // negative mod not allowed
          throw LExcept(zero_divideE);
        
      divtemp = ldiv(i1, i2);
      remainder = divtemp.rem;
      adjustmod('s', i2, quotient, remainder);
      value.setInt( remainder );
    }
        
    else if (rator == pATAB->divuA)  // Division property: a= m*q+r  0 =< r < m
      {                                      // divuA
    divuA:
        if (i2 <= 0)                         // negative mod not allowed
          throw LExcept(zero_divideE);
      
      divtemp = ldiv(i1, i2);
      quotient = divtemp.quot;
      if(divtemp.rem < 0)
        quotient -= 1;
      value.setInt( quotient );
    }

    else if (rator == pATAB->moduA || rator == pATAB->modA )
      {                                      // modA or moduA
    moduA:
        if (i2 <= 0)                         // negative mod not allowed
          throw LExcept(zero_divideE);
      
      divtemp = ldiv(i1, i2);
      remainder = divtemp.rem;
      adjustmod('u', i2, quotient, remainder);
      value.setInt( remainder );
    }

    else if (rator == pATAB->powerA)
       intPow(rand1, rand2, value);
    else if (rator == pATAB->maxA)
        value.setInt( max(i1, i2) );
    else if (rator == pATAB->minA)
        value.setInt( min(i1, i2) );
    else if (rator == pATAB->bitandA)
        value.setInt( i1 & i2 );
    else if (rator == pATAB->bitorA)
        value.setInt( i1 | i2 );
    else if (rator == pATAB->xorA)  
        value.setInt( i1 ^ i2 );
    else if (rator == pATAB->lshiftA)
        value.setInt( i1 << i2 );
    else if (rator == pATAB->rshiftA)
        value.setInt( i1 >> i2 );
    else
    {
       if (pSTATE->m_decimals == real_)
       {
          rand1.setReal( rand1.toReal() );
          rand2.setReal( rand2.toReal() );
          evalReal2(rand1, rand2, rator, value);
       }
       else  // decimals float_
       {
          rand1.setDouble( rand1.toDouble() );
          rand2.setDouble( rand2.toDouble() );
          evalDouble2(rand1, rand2, rator, value);
       }
    }

   return;
}

void LMath::evalFixed1(RAND& rand1, PATOM rator, RAND& value)
{
   // try the various fixed functions, defer to float
   // if can't handle it
#ifdef BUG_EVAL
 DUMP << "EVAL evalInt1: ";
 DUMP << rator << SP << rand1.getInt();
 DUMP << NL;
#endif

   fixedC f1 = rand1.getFixed();

   if (rator == pATAB->plusA)        // give me back my switch!
      value.setFixed( f1 );
   else if (rator == pATAB->minusA)
   {
      f1.negate();
      value.setFixed( f1 );
   }
   else if (rator == pATAB->absA)
   {
      f1.setPos();
      value.setFixed( f1 );
   }

   else if (rator == pATAB->floorA)
      value.setInt( f1.floor() );
   else if (rator == pATAB->ceilA)
      value.setInt( f1.ceil() );
   else if (rator == pATAB->roundA)
      value.setInt( f1.round() );
   else if (rator == pATAB->signA)
      value.setInt( (intC)signum(f1) );
   else if (rator == pATAB->floatA)
      value.setDouble( f1.toDouble() );
   else if (rator == pATAB->realA)
   {
      Real r;
      LNEWX(r, LReal(f1));
      value.setReal( r );
   }
   else
   {
      rand1.setReal( rand1.toReal() );
      evalReal1(rand1, rator, value);
   }

   return;
}

void LMath::evalFixed2(RAND & rand1, RAND & rand2, PATOM rator, RAND & value)
{            // evaluate dyadic expressions on the real domain, return to value
  intC modulus = pSTATE->m_modulo;
  fixedC f1 = rand1.getFixed();
  fixedC f2 = rand2.getFixed();

  if (rator == pATAB->minusA)
		f1.add(f2, '-', value);
  else if (rator == pATAB->plusA)
		f1.add(f2, '+', value);
  else if (rator == pATAB->timesA)
		f1.mult(f2, '+', value);
  else
    { // promote to real for rest
       rand1.setReal( rand1.toReal() );
       rand2.setReal( rand2.toReal() );
       evalReal2(rand1, rand2, rator, value);
	 }

  return;
}

void LMath::evalReal1(RAND& rand1, PATOM rator, RAND& value)
{                            // evaluate monadic expressions on the Real domain
#ifdef BUG_EVAL
  DUMP << "===>>> evalReal1 " << NL;
  DUMP << rator << SP << *(rand1.getReal());
  DUMP << NL;
#endif

  Real r = rand1.getReal();

  // non real returns
  if(rator == pATAB->integerA)
  {
     LReal ri;
     r->intPart(ri);
     value.setInt( ri.toInt() );
  }
  else if(rator == pATAB->floatA)
		value.setDouble( r->toDouble() );
  else
  {
     // real returns;
     Real v;
     LNEWX(v, LReal());

     if (rator == pATAB->minusA)
        *v = - *r;
     else if (rator == pATAB->absA)
        r->absReal(*v);
     else if (rator == pATAB->floorA)
       r->floor(*v);
     else if (rator == pATAB->ceilA)
       r->ceil(*v);
     else if (rator == pATAB->roundA)
       r->round(*v);
     else if (rator == pATAB->sqrtA)
	    sqrtReal(*r, *v);
     else
	    {
        rand1.setDouble( r->toDouble() );
        evalDouble1(rand1, rator, value);
        v->Init( value.toDouble() );
        //LNEWX(r, LReal(value.toDouble()));
        //value.setReal( r );
	    }

     value.setReal(v);
  }

#ifdef BUG_EVAL
 DUMP << "returned value = ";
 value.Dump(m_peng);
 DUMP << NL << "<<<=== EvalReal" << NL;
#endif
  return;
}

void LMath::evalReal2(RAND& rand1, RAND& rand2, PATOM rator, RAND& value)
{                            // evaluate dyadic expressions on the Real domain
  //Real remainder, quotient, absmod, halfDivisor;
  //bool negmod, oddmod;
  intC modulus = pSTATE->m_modulo;

  Real v;
  LNEWX(v, LReal());

#ifdef BUG_EVAL
  DUMP << "===>>> evalReal2 " << NL;
  DUMP << "rator = " << rator << NL;
  DUMP << "rand1 = " << *rand1.getReal() << NL;
  DUMP << "rand2 = " << *rand2.getReal() << NL;
#endif
  Real r1 = rand1.getReal();
  Real r2 = rand2.getReal();
  int i2;

  try {

  if (rator == pATAB->xorA)
    r1->rxor(*r2, *v);
  else if (rator == pATAB->plusA)
    r1->add(*r2, '+', *v);
  else if (rator == pATAB->minusA)
    r1->add(*r2, '-', *v); 
  else if (rator == pATAB->timesA)
    r1->mult(*r2, *v);
  else if (rator == pATAB->divA)
    r1->div(*r2, pSTATE->m_delta, *v);
  else if (rator == pATAB->divdivA)
    {                                           // int divide

      LReal remainder;
      switch(pSTATE->m_rounded)
        {
        case TO_ZERO:
          if (r2->isZero())
             throw LExcept(zero_divideE);
          r1->intDiv(*r2, remainder, pSTATE->m_delta, *v); // quotient
		    break;                               // ray
        case TO_FLOOR:
           r1->divU(*r2, pSTATE->m_delta, *v);
           break;
          //goto divuA;
        case TO_NEAREST:
          //goto divsA;
           r1->divS(*r2, pSTATE->m_delta, *v);
           break;
        default:
           pXCPT->Error(internalE, aS("unknown rounded value"));
        }
    }
  else if (rator == pATAB->remA)
    {                                            // rem
      LReal q;
      switch(pSTATE->m_rounded)
        {
        case TO_ZERO:
          if (r2->isZero())
            throw LExcept(zero_divideE);
          r1->intDiv(*r2, *v, pSTATE->m_delta, q); 
          //value.setReal( remainder );                      // remainder
          break;
        case TO_FLOOR:
           r1->modU(*r2, modulus, pSTATE->m_delta, *v);
           break;
          //goto moduA;
        case TO_NEAREST:
           r1->modS(*r2, modulus, pSTATE->m_delta, *v);
           break;
          //goto modsA;
        default:
           pXCPT->Error(internalE, aS("unknown rounded value"));
        }
    }
  else if (rator == pATAB->divsA)
    {                                           // divs
      r1->divS(*r2, pSTATE->m_delta, *v);

    }
  
  else if (rator == pATAB->modsA)
    {                                        // mods
       r1->modS(*r2, modulus, pSTATE->m_delta, *v);
    }
  
  else if (rator == pATAB->divuA)  // Division property: a= m*q+r  0 =< r < m
    {                                        //divu
        r1->divU(*r2, pSTATE->m_delta, *v);
   }
  
  else if (rator == pATAB->moduA || rator == pATAB->modA)
    {                                        // mod or modu
      r1->modU(*r2, modulus, pSTATE->m_delta, *v);
    }
  else if (rator == pATAB->maxA)
		value.setReal( r1->maxReal(*r2) );     // max
  else if (rator == pATAB->minA)
		value.setReal( r1->minReal(*r2) );     // min
   else if (rator == pATAB->lshiftA)
   {
      i2 = rand2.toInt();
      *v = *r1 << i2;
      //value.setReal( *r1 << i2 );
   }
   else if (rator == pATAB->rshiftA)
   {
      i2 = rand2.toInt();
      *v = *r1 >> i2;
      //value.setReal( *r1 >> i2 );
   }
   else if(rator == pATAB->powerA && rand2.getReal()->isIntegerable())
   {
      i2 = rand2.toInt();
      r1->pow(i2, pSTATE->m_delta, pSTATE->m_epsilon, *v);
      //value.setReal( r1->pow(i2, pSTATE->m_delta) );
   }
	else
   {
      rand1.setDouble( r1->toDouble() );
      rand2.setDouble( r2->toDouble() );
      evalDouble2(rand1, rand2, rator, value);
      //LNEWX(Real r, LReal(value.getDouble()));
      v->Init(value.getDouble());
      //value.setReal( r );
   }

   value.setReal( v );

#ifdef BUG_EVAL
 DUMP << "returned value = ";
 value.Dump(m_peng);
 DUMP << NL << "<<<=== evalReal2" << NL;
#endif
   return;

   }
   catch (LExcept x)
   {
      delete v;
      throw x;
   }
}

void LMath::evalSingle1(RAND& rand1, PATOM rator, RAND& value)
{
#ifdef BUG_EVAL
 DUMP << "EVAL evalDouble1: ";
 DUMP << rator << SP << rand1.getDouble();
 DUMP << NL;
#endif

  float d1 = rand1.getSingle();

  if (rator == pATAB->floatA)
    value.setSingle( d1 );
  else if (rator == pATAB->plusA)
    value.setSingle( d1 );
  else if (rator == pATAB->minusA)
    value.setSingle( -d1 );
  else if (rator == pATAB->absA)
    value.setSingle( fabs(d1) );
  else if (rator == pATAB->signA)
    value.setSingle( signum(d1) );
  else if (rator == pATAB->lnA || rator == pATAB->logA)
    value.setSingle( log(d1) );
  else if (rator == pATAB->log10A)
    value.setSingle( log10(d1) );
  else if (rator == pATAB->expA)         
    value.setSingle( exp(d1) );
  else if (rator == pATAB->sqrtA)
    value.setSingle( sqrt(d1) );
  else if (rator == pATAB->sinA)
    value.setSingle( sin(d1) );
  else if (rator == pATAB->cosA)
    value.setSingle( cos(d1) );
  else if (rator == pATAB->tanA)
    value.setSingle( tan(d1) );
  else if (rator == pATAB->asinA)
    value.setSingle( asin(d1) );
  else if (rator == pATAB->acosA)
    value.setSingle( acos(d1) );
  else if (rator == pATAB->atanA)
    value.setSingle( atan(d1) );
  else if (rator == pATAB->integerA)
    value.setInt( (intC)(d1) );
  else if (rator == pATAB->floorA)
    value.setInt( (intC)floor(d1) );
  else if (rator == pATAB->ceilA)
    value.setInt( (intC)ceil(d1) );
  else if (rator == pATAB->roundA)
    value.setInt( (intC)floor(d1 + 0.5) );
  else if (rator == pATAB->realA)
  {
     Real r;
     LNEWX(r, LReal(d1));
     value.setReal( r );
  }
  else
    throw LExcept(arithopE, (STRptr)*rator);

  return;
}

void LMath::evalSingle2(RAND& rand1, RAND& rand2, PATOM rator, RAND &value)
{     // evaluate dyadic expressions on the single domain
#ifdef BUG_EVAL
 DUMP << "EVAL evalDouble2: ";
 DUMP << rand2.getDouble() << SP << rator << SP << rand2.getDouble();
 DUMP << NL;
#endif

   float d1 = rand1.getSingle();
   float d2 = rand2.getSingle();

    if (rator == pATAB->plusA)
      value.setSingle( d1 + d2 );
    else if (rator == pATAB->minusA)
      value.setSingle( d1 - d2 ); 
    else if (rator == pATAB->timesA)
      value.setSingle( d1*d2 );
    else if (rator == pATAB->divA)
      value.setSingle( d1/d2 );
    else if (rator == pATAB->powerA)
      value.setSingle( pow(d1, d2) );
    else if (rator == pATAB->maxA)
      value.setSingle( max(d1, d2) );
    else if (rator == pATAB->minA)
      value.setSingle( min(d1, d2) );
    else if (rator == pATAB->divdivA)
    {
      if (d2 == 0)
        throw LExcept(zero_divideE);
      value.setSingle( floor(d1/d2) );
    }
    else
        throw LExcept(arithopE, (STRptr)*rator);

	 return;
}



void LMath::evalDouble1(RAND& rand1, PATOM rator, RAND& value)
{
#ifdef BUG_EVAL
 DUMP << "EVAL evalDouble1: ";
 DUMP << rator << SP << rand1.getDouble();
 DUMP << NL;
#endif

  double d1 = rand1.getDouble();

  if (rator == pATAB->floatA)
    value.setDouble( d1 );
  else if (rator == pATAB->plusA)
    value.setDouble( d1 );
  else if (rator == pATAB->minusA)
    value.setDouble( -d1 );
  else if (rator == pATAB->absA)
    value.setDouble( fabs(d1) );
  else if (rator == pATAB->signA)
    value.setDouble( signum(d1) );
  else if (rator == pATAB->lnA || rator == pATAB->logA)
    value.setDouble( log(d1) );
  else if (rator == pATAB->log10A)
    value.setDouble( log10(d1) );
  else if (rator == pATAB->expA)         
    value.setDouble( exp(d1) );
  else if (rator == pATAB->sqrtA)
    value.setDouble( sqrt(d1) );
  else if (rator == pATAB->sinA)
    value.setDouble( sin(d1) );
  else if (rator == pATAB->cosA)
    value.setDouble( cos(d1) );
  else if (rator == pATAB->tanA)
    value.setDouble( tan(d1) );
  else if (rator == pATAB->asinA)
    value.setDouble( asin(d1) );
  else if (rator == pATAB->acosA)
    value.setDouble( acos(d1) );
  else if (rator == pATAB->atanA)
    value.setDouble( atan(d1) );
  else if (rator == pATAB->integerA)
    value.setInt( (intC)(d1) );
  else if (rator == pATAB->floorA)
    value.setInt( (intC)floor(d1) );
  else if (rator == pATAB->ceilA)
    value.setInt( (intC)ceil(d1) );
  else if (rator == pATAB->roundA)
    value.setInt( (intC)floor(d1 + 0.5) );
  else if (rator == pATAB->realA)
  {
     Real r;
     LNEWX(r, LReal(d1));
     value.setReal( r );
  }
  else
    throw LExcept(arithopE, (STRptr)*rator);

  return;
}

void LMath::evalDouble2(RAND& rand1, RAND& rand2, PATOM rator, RAND &value)
{     // evaluate dyadic expressions on the double domain
#ifdef BUG_EVAL
 DUMP << "EVAL evalDouble2: ";
 DUMP << rand2.getDouble() << SP << rator << SP << rand2.getDouble();
 DUMP << NL;
#endif

   double d1 = rand1.getDouble();
   double d2 = rand2.getDouble();

    if (rator == pATAB->plusA)
      value.setDouble( d1 + d2 );
    else if (rator == pATAB->minusA)
      value.setDouble( d1 - d2 ); 
    else if (rator == pATAB->timesA)
      value.setDouble( d1*d2 );
    else if (rator == pATAB->divA)
      value.setDouble( d1/d2 );
    else if (rator == pATAB->powerA)
      value.setDouble( pow(d1, d2) );
    else if (rator == pATAB->maxA)
      value.setDouble( max(d1, d2) );
    else if (rator == pATAB->minA)
      value.setDouble( min(d1, d2) );
    else if (rator == pATAB->divdivA)
    {
      if (d2 == 0)
        throw LExcept(zero_divideE);
      value.setDouble( floor(d1/d2) );
    }
    else
        throw LExcept(arithopE, (STRptr)*rator);

	 return;
}

void LMath::intPow(RAND& rand1, RAND& rand2, RAND& value)
{
   intC modulus = pSTATE->m_modulo;
   intC i1 = rand1.getInt();
   intC i2 = rand2.getInt();
   //Real r1;

   if(i2 == 0)
   {
      value.setInt( 1 );
      return;
   }
   if(i2 == 1)
   {
      value.setInt( i1 );
      return;
   }

   if(modulus > 0)
   {
      long absi2 = i2 < 0 ? -i2 : i2;
      value.setInt( powerZm(i1, absi2, modulus) );  // i1**i2, both int
      if(i2 < 0)                     
         value.setInt( inverse(modulus, value.getInt()) );
      return;
   }

	//   if (i2 < 0 || i1 < 0)  
   if (i2 < 0)  // going to have to promote anyway, so do it now
   {
      if (pSTATE->m_decimals == real_)
      {
         rand1.setReal( rand1.toReal() );
         rand2.setReal( rand2.toReal() );
         evalReal2(rand1, rand2, pATAB->powerA, value);
         if (value.getReal()->isIntegerable())
            value.setInt( value.getReal()->toInt() );
      }
      else  // decimals float
      {
         rand1.setDouble( rand1.toDouble() );
         rand2.setDouble( rand2.toDouble() );
         evalDouble2(rand1, rand2, pATAB->powerA, value);
      }
      return;
   }

   try
   {
      value.setInt( powerZ(i1, i2) );  // i1**absi2, both int
      // powerZ succeeded (would have thrown PZ otherwise)
      return;
   }
   catch(PZ pZ)
   {  // powerZ overflowed
      if (pSTATE->m_decimals == real_)
      {
         // continue where we left off with reals
         //LNEW(r1, LReal(pZ.acc), aS("Eval"));          // continue with reals
         LReal r1(pZ.acc);
         Real v;
         LNEWX(v, LReal());
         r1.powerR(pZ.x, pZ.n, *v);
         value.setReal( v );
         //value.setReal( r1->powerR(pZ.x, pZ.n) );
         //delete r1;
      }
      else  // decimals float
      {
         rand1.setDouble( rand1.toDouble() );
         rand2.setDouble( rand2.toDouble() );
         evalDouble2(rand1, rand2, pATAB->powerA, value);
      }
      return;
   }
}

void LMath::sqrtReal(LReal &r, LReal &result)
{                 // sqrt(N): guess(n+1) = (guess(n) + N/guess(n))/2 (Newton)
  if(r.isNeg())
    pXCPT->Error(arithargE, aS("sqrt"));
  LReal cnvrgnt(r);
  LReal r2, lastCnvrgnt, fraction;
  LReal two((Gigit)2);
  double d;

  r.trimToLen(2, r2);                        // let float take first guess
  d = r2.toDouble();
  d = sqrt(d);
  floatToReal(d, cnvrgnt);
  int epsilon = pSTATE->m_epsilon;
  if (epsilon == 0)   // 0 means turn it off for pow, but we need something here
     epsilon = -4;
  epsilon += r.lsExp();
  LReal Epsilon(1L);
  Epsilon.setExp(epsilon);

  do
	 {
		lastCnvrgnt = cnvrgnt;
		r.div(lastCnvrgnt, pSTATE->m_delta, fraction);
		(lastCnvrgnt + fraction).div(two, pSTATE->m_delta, cnvrgnt);
	 }  while((cnvrgnt - lastCnvrgnt).compareAbs(Epsilon) == '>');
  cnvrgnt.truncate(epsilon);
  result = cnvrgnt;
}
/*
void LMath::RealModu(Real r1, intC modulus, RAND& value)
{                                        // evaluate modu on the Real domain
  if (modulus == 0)
	 throw LExcept(zero_divideE);

  //Real r2 = newReal(modulus);
  Real r2;
  LNEW(r2, LReal(modulus), aS("RealModu"));
  RealModu(r1, r2, value);
  intC IntRem = value.getReal()->toInt();
  value.setInt( IntRem );
}

void LMath::RealModu(Real r1, Real modulus, RAND& value)
{                                        // evaluate modu on the Real domain
  Real remainder;

  if (modulus == 0)
     throw LExcept(zero_divideE);

  Real quotient = r1->intDiv(*modulus, remainder, pSTATE->m_delta);
      if(remainder->isNeg())
         //(*remainder + *modulus)->assignTo(remainder);
         remainder = (*remainder + *modulus);
      value.setReal( remainder );
}
*/

//-----------------------------------------------------------
// Predicates
//

TF LMath::p_is()
{
  RAND rand;
  Cell x0;

#ifdef BUG_EVAL
  DUMP << "EVAL is: ";
  pTSVC->Dump(X(1));
  DUMP << NL;
#endif
  //if ( Eval(pHXL->XVar(1), rand) )
  //  x0.setAtom(pATAB->arithEA);
  EvalTop(pHXL->XVar(1), rand);
     //else 
  rand.makeCell(m_peng, x0);

  return(pTSVC->UnifyConst(pHXL->XVar(0), &x0));
}

TF LMath::p_arith_plus()
{// Stops heap build up for sums, used only by compiler. Not a user's built-in 
  RAND    rand1, rand2;
  double  f;
  Cell    x0;
  //Real    r;

  
  //if(Eval(pHXL->XVar(1), rand1) || Eval(pHXL->XVar(2), rand2))
  //  x0.setAtom(pATAB->arithEA);
  EvalTop(X(1), rand1);
  EvalTop(X(2), rand2);
  //else 
  //  {
  if (rand1.isInt() && rand2.isInt())
     x0.setInt( rand1.getInt() + rand2.getInt() );
  else
  {
     f = rand1.toDouble() + rand2.toDouble();
     x0.setDouble(pGCTH->make_float(f));
  }

  return(pTSVC->UnifyConst(pHXL->XVar(0), &x0));
}    

TF LMath::p_numeq() 
{
  RAND  rand1, rand2;  

  EvalTop(pHXL->XVar(0), rand1);
  EvalTop(pHXL->XVar(1), rand2);

  if(rand1.isReal()  || rand2.isReal())
  {
     Real r1 = rand1.toReal();
     Real r2 = rand2.toReal();
     TF tf = (*r1 == *r2);
     delete r1;
     delete r2;
     return tf;
     //return (rand1.toReal())->compare((*rand2.toReal())) == '=';
  }
  else
     return rand1.toDouble() == rand2.toDouble();
}

TF LMath::p_almost_equal() 
{
   RAND  rand1, rand2;  

   EvalTop(X(0), rand1);
   EvalTop(X(1), rand2);

   Tag tag = demote(rand1, rand2);

   if (tag == doubleS)
   {
      double d1 = rand1.getDouble();
      double d2 = rand2.getDouble();
      longlong *pll;
      char s1,s2;
      int exp1,exp2;
      longlong mant1,mant2;
      pll = (longlong*)&d1;
      floatfields(pll, &s1, &exp1, &mant1);
      pll = (longlong*)&d2;
      floatfields(pll, &s2, &exp2, &mant2);
      if (s1 != s2)
         return FALSE;
      if (exp1 != exp2)
         return FALSE;
      int dif = (int)(mant1 - mant2);
      if ( dif > 10 || dif < -10 )
         return FALSE;
      return TRUE;
   }
   else if (tag == singleS)
   {
      double d1 = (double)rand1.getSingle();
      double d2 = (double)rand2.getSingle();
      longlong *pll;
      char s1,s2;
      int exp1,exp2;
      longlong mant1,mant2;
      pll = (longlong*)&d1;
      floatfields(pll, &s1, &exp1, &mant1);
      pll = (longlong*)&d2;
      floatfields(pll, &s2, &exp2, &mant2);
      if (s1 != s2)
         return FALSE;
      if (exp1 != exp2)
         return FALSE;
      if ( (mant1 - mant2) > 10e8 || (mant1 - mant2) < -10e8 )
         return FALSE;
      return TRUE;
   }
   else if (tag == intS)
   {
      return rand1.getInt() == rand2.getInt();
   }
   else if (tag == fixedS)
   {
      return almostEqual(*rand1.toReal(), *rand2.toReal());
   }
   else if (tag == realS)
   {
      return almostEqual(*rand1.getReal(), *rand2.getReal());
   }
   else
   {
      return FALSE;
   }
}

TF LMath::p_gt()
{
  RAND  rand1, rand2;  
  
  EvalTop(pHXL->XVar(0), rand1);
  EvalTop(pHXL->XVar(1), rand2);
  
  if(rand1.getKind() == realS  || rand2.getKind() == realS)
  {
     Real r1 = rand1.toReal();
     Real r2 = rand2.toReal();
     TF tf = ( r1->compare(*r2) == '>' );
     delete r1;
     delete r2;
     return tf;
  }
  else
     return (rand1.toDouble() > rand2.toDouble());
}

TF LMath::p_lt()
{
  RAND  rand1, rand2;  
  
  EvalTop(pHXL->XVar(0), rand1);
  EvalTop(pHXL->XVar(1), rand2);
  
  if(rand1.getKind() == realS  || rand2.getKind() == realS)
  {
     Real r1 = rand1.toReal();
     Real r2 = rand2.toReal();
     TF tf = ( r1->compare(*r2) == '<' );
     delete r1;
     delete r2;
     return tf;
  }
  else
     return (rand1.toDouble() < rand2.toDouble());
}

TF LMath::p_is_integer()
{  // is_integer()
   TERM t = X(0);
   if(t->IsInt())
     return TRUE;
//#ifdef REALS
   if(t->IsReal())
     return (t->getReal())->isInteger();
//#endif
   if(t->IsFixed())
     return (t->getFixed()).isInteger();

   if(t->IsDouble())
      return (t->getDouble() == floor(t->getDouble()));

   if(t->IsSingle())
      return (t->getSingle() == floor(t->getSingle()));

   return FALSE;
}

TF LMath::p_is_odd()
{                                            // integer()
   TERM t = pHXL->XVar(0)->dref();
   if(t->IsInt())
     return (t->getInt() & 1) == 1;
//#ifdef REALS
   if(t->IsReal())
     return (t->getReal())->isOdd();
//#endif
   return FALSE;
}
// end predicates


TF LMath::p_newReal()
{                                            // newReal(Exp, Value, Real)
   TERM t0  = pHXL->XVar(0)->dref();         // exp
   TERM t1  = pHXL->XVar(1)->dref();         // value
   TERM t2  = pHXL->heapGETN(1);             // new real
   intC value = t1->getInt();
   //Real r = ::newReal(1, (intC)*t0, value < 0 ? '-' : '+');
   Real r;
   LNEWX(r, LReal(1, value < 0 ? '-' : '+', t0->getInt() ));
   r->setGd(1, value < 0 ? -value : value);
   t2->setReal(pGCTH->make_real(r));
   return pTSVC->Unify(t2, pHXL->XVar(2));    
}

TF LMath::p_truncate()
{                          
   TERM t0  = pHXL->XVar(0)->dref();
   if(!(t0->IsReal()))
     return FALSE;
   TERM t1  = pHXL->heapGETN(1);             // 

   Real r0 = t0->getReal();
   //Real r1 = r0->truncate(pSTATE->m_epsilon);
   //t1->setReal(r1);
   //Real r1 = r0->truncate(pSTATE->m_epsilon);
   LReal r1;
   r0->truncate(pSTATE->m_epsilon, r1);
   t1->setReal(pGCTH->make_real(&r1));
   return pTSVC->Unify(t1, pHXL->XVar(1));    
}
   

TF LMath::p_nth()
{                  
   TERM t0 = pHXL->XVar(0)->dref();
   TERM t1 = pHXL->XVar(1)->dref();
   TERM t2 = pHXL->heapGETN(1);             
   intC n  = t0->getInt();
//#ifdef REALS
   if(t1->IsReal())
     {                                       // nth(r, n, G) G is from nth Col
       if(n < 0)
         return FALSE;
       Real r  = t1->getReal();                   // get the real
       if(n > r->getLen())
         return FALSE;
       t2->setInt(r->getGd(n));             // set result type
       return pTSVC->Unify(t2, pHXL->XVar(2));    
     }
//#endif
   if(t1->IsList())
     for (int i = 1; i <= n && t1->IsList(); i++, t1 = (++t1)->dref())
       {                      // nth(n, list, Element) Element is nth element
         t1 = t1->getTerm();
         if(i == n)
           {
             t2->setTerm(t1);              
             return pTSVC->Unify(t2, pHXL->XVar(2));    
           }
       }
   return FALSE;
}

TF LMath::p_fraction()
{                                               // integer()
   TERM t = pHXL->XVar(0)->dref();
//#ifdef REALS
   if(t->IsReal())
     {
       Real r = t->getReal();
       return r->lsExp() < 0;
     }
//#endif
   double d;
   if(t->IsScientific())
     {
      if (t->IsDouble())
        d = t->getDouble();
      else
         d = (double)t->getSingle();
       return floor(d) != ceil(d);
     }
   return FALSE;
}


TF LMath::p_divrem(char discrim)
{                                         // divrem(N, D, Q, R) N = Q*D + R  
  TERM t0 = X(0);  //pHXL->XVar(0)->dref();
  TERM t1 = X(1);  //pHXL->XVar(1)->dref();
  TERM t2 = pHXL->heapGETN(1);             
  TERM t3 = pHXL->heapGETN(1);             
  ldiv_t divtemp;
  
  if(t0->IsInt())
    {
      intC i0 = t0->getInt();
      if(t1->IsInt())
        {                                      // both int
          aINT32 i1 = t1->getInt();
          divtemp = ldiv(i0, i1);
          aINT32 i2 = divtemp.quot;
          aINT32 i3 = divtemp.rem;
          if(!adjustmod(discrim, i1, i2, i3))
            return FALSE;
          t2->setInt(i2);
          t3->setInt(i3);
        }
      else
        if(t1->IsReal())
          {
            LReal r3;
            //Real r0 = newReal(i0);
            LReal r0(i0);
            //LNEWX(r0, LReal(i0));
            Real r1 = t1->getReal();
            //t2->setReal(r0->intDiv(*r1, r3, pSTATE->m_delta));
            //t3->setReal(r3);
            LReal r2;
            r0.intDiv(*r1, r3, pSTATE->m_delta, r2);
            t2->setReal(pGCTH->make_real(&r2));
            t3->setReal(pGCTH->make_real(&r3));
//            delete r0;
//            delete r1;
          }
        else
          return FALSE;
    }
  else
    if(t0->IsReal())
      {
        Real r0 = t0->getReal();
        LReal r3;
        if(t1->IsReal())
          {                                           // both Real
            Real r1 = t1->getReal();
            //t2->setReal(r0->intDiv(*r1, r3, pSTATE->m_delta));
            //t3->setReal(r3);
            LReal r2;
            r0->intDiv(*r1, r3, pSTATE->m_delta, r2);
            t2->setReal(pGCTH->make_real(&r2));
            t3->setReal(pGCTH->make_real(&r3));
//            delete r0;
//            delete r1;
          }
        else
          if(t1->IsInt())
            {
              LReal r3;
              LReal r1(t1->getInt());
              //LNEWX(Real r1, LReal(t1->getInt()));
              Real r0 = t0->getReal();
              //t2->setReal(r0->intDiv(*r1, r3, pSTATE->m_delta));
              //t3->setReal(r3);
              LReal r2;
              r0->intDiv(r1, r3, pSTATE->m_delta, r2);
              t2->setReal(pGCTH->make_real(&r2));
              t3->setReal(pGCTH->make_real(&r3));
//            delete r0;
//              delete r1;
            }
          else
            return FALSE;
      }
    else
      return FALSE;

  return pTSVC->Unify(pHXL->XVar(2), t2) &&  pTSVC->Unify(pHXL->XVar(3), t3);
}

TF LMath::p_integer()
{  return X(0)->IsInt(); }  // gets chars as well

//TF LMath::p_long()
//{  return X(0)->IsInt(); }

//TF LMath::p_short() // nothing is short
//{  return FALSE;}

TF LMath::p_fixed_real()
{  return X(0)->IsFixed(); }

TF LMath::p_long_real()
{  return X(0)->IsReal(); }

TF LMath::p_real()
{  return X(0)->IsFixedOrReal(); }

TF LMath::p_single()
{ return X(0)->IsSingle();}

TF LMath::p_double()
{ return X(0)->IsDouble();}

TF LMath::p_float()
{ return X(0)->IsScientific();}

TF LMath::p_int_real()
{                             // create a Real array from a long, or vice versa
  Real r;
  TERM t = pHXL->heapGET();

  TERM x = (pHXL->XVar(0))->dref();
  if (x->IsInt())
    {
      //r = newReal((intC)*x);
      LNEWX(r, LReal(x->getInt()));
      t->setReal(pGCTH->make_real(r));
      return(pTSVC->Unify(t, pHXL->XVar(1)));
    }
  TERM y = (pHXL->XVar(1))->dref();
  if (y->IsReal())
    {
      r = y->getReal();
      t->setInt(r->toInt());
      return(pTSVC->Unify(t, pHXL->XVar(0)));
    }
  return FALSE;
}

TF LMath::p_float_real()
{                            // create a Real array from a float, or vice versa
  LReal r;
  double d;
  Cell c;
  TERM t = pHXL->heapGET();                       // result

  TERM t0 = X(0);
  if (t0->IsScientific())
    {
      d = t0->forceDouble();
      floatToReal(d,r);
      //t = new Cell(r);
      c.setReal(pGCTH->make_real(&r));
      return(pTSVC->Unify(&c, pHXL->XVar(1)));
    }

  TERM t1 = X(1);
  if (t1->IsReal())
    {
      r = *(t1->getReal());
      d = r.toDouble();
      t->setDouble(pGCTH->make_float(d));
      return(pTSVC->Unify(t, pHXL->XVar(0)));
    }

  return FALSE;
}

TF LMath::p_real_list()
{                                 // create a Real array from Real Prolog list
  TERM  t, x;
  Real r;

  x = (pHXL->XVar(1))->dref();
  if (x->IsList())
    try
      {
        r = listToReal(x);        
        t = pHXL->heapGET();
        t->setReal(pGCTH->make_real(r));
        return(pTSVC->Unify(t, pHXL->XVar(0)));
      }
  catch (bool b)
    {return b;}

  x = (pHXL->XVar(0))->dref();
  if (x->IsReal())
    {                                             // Real to list
      r = x->getReal();
      return(pTSVC->Unify(realToList(r), pHXL->XVar(1)));
    }
  return FALSE;
}

TF LMath::p_real_components()
// real_components(REAL, SIGN, EXPONENT, LENGTH, GIGIT_LIST)
{
   Real r;
   Cell c;
   TERM tr = X(0);

   if (tr->IsVar())
   {
      STUB(aS("can't construct reals from real_components yet"));
      return FALSE;
   }
   else if (tr->IsReal())
   {
      r = tr->getReal();
      if (r->isPos())
         c.setAtom(pATAB->plusA);
      else
         c.setAtom(pATAB->minusA);
      if ( FALSE == pTSVC->Unify(&c, X(1)) )
         return FALSE;
      if ( FALSE == pTSVC->UnifyInt(r->lsExp(), X(2)) )
         return FALSE;
      if ( FALSE == pTSVC->UnifyInt(r->getLen(), X(3)) )
         return FALSE;
      if ( FALSE == pTSVC->Unify(gigitsList(r), X(4)) )
         return FALSE;
      return TRUE;
   }
   else
      return FALSE;
}

TERM LMath::gigitsList(Real r)
// makes a list reflecting internals, good for compiler to
// generate code
{
  TERM  t, list;
  int i;
  int len = r->getLen();
  list = pHXL->heapGETN(2*len + 1);
  t = list+1;
  list->setList(t);
  for (i = len-1; i >= 0; i--)
  {
     t++->setInt((*r)[i]);
     t->setList(t+1);
     t++;
  }
  t--;
  t->setAtom(pATAB->nilA);
  return list;
}

TERM LMath::realToList(Real r)
// makes a list with a gigit point, padding for exponent
{
  TERM  head, tail, arg1;
  PATOM pointAnum = pATAB->EnterAtom(aS("."));
  PATOM signAnum  = pATAB->EnterAtom(aS("-"));

  int msExp = r->msExp();                        // needed only for listlen
  int lsExp = r->lsExp();

  if(msExp < 0)                                  // include trailing zeroes
    msExp = -1;
  if(lsExp > 0)                                  // include leading zeroes
    lsExp = 0;
  
  int listlen = r->isNeg() + 1 + msExp - lsExp;
  if(msExp < 0)
    listlen += 2;                                // "0." ...
  else
    if(lsExp < 0)
      listlen += 1;                              // ... '.' ...
  
  arg1 = pHXL->heapGETN(2*listlen + 3);          // space for a list
  head = arg1 - 1;                               // set up for 1st bump
  tail = head + 1;                               // set up for 1st bump
  arg1->setList(arg1 + 1);                       // list starts at arg1+1  
  if(r->isNeg())
    {                                            // insert '-'
      head += 2;                                 // bump
      tail += 2;
      head->setAtom(signAnum);                   // insert sign
      tail->setList(head + 2);                   // append
      if(r->msExp() < 0)
        {                 
          head += 2;                             // bump
          tail += 2;
          head->setInt(0);                       // insert leading zero
          tail->setList(head + 2);               // append
        }
    }
  int col = msExp;
  while( col >= lsExp)
    {
      head += 2;                                 // bump
      tail += 2;         
      if(col == -1)
        {
          if(msExp < 0)
            {
              head->setInt(0);
              tail->setList(head + 2);           // append
              head+= 2;                          // bump
              tail+= 2;
            }
          head->setAtom(pointAnum);              // insert the giga point
          tail->setList(head + 2);               // append
          head+= 2;                              // bump
          tail+= 2;
        }
      head->setInt(r->getCol(col--));            // insert gigadigit
      tail->setList(head + 2);                   // append
    }
  tail->setAtom(pATAB->nilA);                    // terminate list
  return arg1;
}

TF LMath::p_makePrimes()  // makePrimes(+Lo, +Hi, -Primes, -LastPrime)
{                                               // Eratosthenes sieve
  intC lowest; 
  TERM t0 =(pHXL->XVar(0))->dref();
  TERM t1 =(pHXL->XVar(1))->dref();

/**************************************\
* The pre-filter zaps 3, 5, 7, 11 & 13 *
* 17 is first prime in pq.             *
\**************************************/

  lo = (t0->getType() == doubleS ? (intC)(t0->getDouble()) :
        (t0->getType() & intS ? t0->getInt() : -1));  // get lowest 
  if(lo < 0)
    return false;
  hi = (t1->getType() == doubleS ? (intC)(t1->getDouble()) :
        (t1->getType() & intS ? t1->getInt() : -1));  // get highest 
  if(hi < 0)
    return false;

  if(hi < 11)
    hi = 11;

  intC factorLimit = (intC)floor(sqrt((double)hi));

  int primeHeapSize = (int)(1.25 * hi/log((double)hi));  // from prime number theorem

  if(primeHeapSize < 10)
    primeHeapSize = 10;

  ERAPQ *era;                                      // instantiate priority queue
  LNEW(era, ERAPQ(factorLimit << 1, m_peng), aS("miscellaneous"));  

  era->push(34, 289);                // prime 17 factor just to prime the heap

  prime.clear();

  if(lo <= 2)                                    // put in primes from lo to 13
  prime.push_back(2);
  if(lo <= 3)
  prime.push_back(3);
  if(lo <= 5)
  prime.push_back(5);
  if(lo <= 7)
  prime.push_back(7);
  if(lo <= 11)
  prime.push_back(11);
  if(lo <= 13)
  prime.push_back(13);

  /*
   * All putative primes must be tested against the top of the heap,
   * but only primes =< factorLimit should be entered in to the heap.
   */
  int i, delta;
  intC pp;
  for(pp = 17, i = 8, delta = 1;                 // pp: putative prime
      pp < hi; 
      pp += 2, i+= delta)                        // 17 is first pp to try 
    {
      if (i == -1) 
        {                                        // must do 0 twice
          i++;
          delta = 1;
        }
      if (i == nolos)
        delta = -1;                              // symmetric about this point
      
      if (nolofactors[i])
        {                                        // only if no low factors
			 era->update(pp);
          lowest = era->top();                   // nearest non-prime
          if(!lowest)
            break;                               // heap empty!
          if(lowest == pp)
            continue;                            // pp is not prime
                                                 // pp is a new prime 
          if(pp <= factorLimit) 
              era->push(pp << 1, pp*pp);         // insert in the heap 

          if(pp >= lo)                           // only save if pp > lo 
              prime.push_back(pp);               // insert pp in prime array 
        }                                        // end nolofactors
    }                                            // end for

  TERM t2 = pHXL->heapGET();
  t2->setInt((intC)prime.size());
  TERM t3 = pHXL->heapGET();
  t3->setInt(prime[prime.size() - 1]);
  delete era;
  return pTSVC->Unify(pHXL->XVar(2), t2) && pTSVC->Unify(pHXL->XVar(3), t3);
}

TF LMath::p_deletePrimes()
{  prime.empty(); return true;}

TF LMath::p_Primes()                      // get prime vector length
{  
  TERM t0 = pHXL->heapGET();
  t0->setInt((intC)prime.size());
  return pTSVC->Unify(pHXL->XVar(0), t0);
}

//int biSearch(intC Brodie, intC *prime, int start, int stop)
int biSearch(intC Brodie, std::vector<intC> prime, int start, int stop)
{
  if(stop < start)
    return FALSE;
  int half = (start + stop)/2;
  intC Half = prime[half];
  if (Brodie == Half)
    return half;
  if( Brodie < Half)
    return biSearch(Brodie, prime, start, half - 1);
  else
    return biSearch(Brodie, prime, half + 1, stop);
}

TF LMath::locatePrime()                   // get index of element 
{  
  TERM t1 =(pHXL->XVar(1))->dref();
  if(!(t1->getType() & intS))
    return false;
  intC Brodie = t1->getInt();
  int size = (int)prime.size();
  if((Brodie < 0) || (Brodie > prime[size-1]))
    return false;
  int index = biSearch(Brodie, prime, 1, size);
  if(index == 0)
    return false;
  TERM t0 = pHXL->heapGET();
  t0->setInt(index);
  return pTSVC->Unify(pHXL->XVar(0), t0);
}

TF LMath::p_nthPrime()                 // get indexed element, 1-origin 
{  
  TERM t0 =(pHXL->XVar(0))->dref();
  if(!(t0->getType() & intS))
    return locatePrime();
  TERM t1 = pHXL->heapGET();

  int index = t0->getInt();
  if(index > (int)prime.size() || index < 1)
    {
      pXCPT->Error(indexE, aS("prime"), index);
      return false;
    }
  t1->setInt(prime[index - 1]);
  return pTSVC->Unify(pHXL->XVar(1), t1);
}

/**************************************************************\
* There is a global array of factors (PQELEMENTs) and a global *
* priority queue whose elements are pointers to factors.       *
\**************************************************************/

ERAPQ::ERAPQ(const int size, LEngine * peng)
{ 
  m_peng = peng;
  length = 0; 
  free = 1;
  maxSize = size;
  LNEW(pq, PQELEMENT *[size], aS("miscellaneous"));   // the element ptr array
  LNEW(factor, PQELEMENT[size], aS("miscellaneous")); // the element array
  //if(!pq || !factor)
  //  {
  //    pXCPT->Error(outofmemE, aS("Allocating prime factors"));
  //  }
}

void ERAPQ::Insert(PQELEMENT *element)
{
  if(length >= maxSize)
    {
      pXCPT->Error(heapofloE, aS("sieve"));
      return;
    }

  pq[++length] = element;                      // insert element 
 
  SiftUpPn();                                  // elevate last to proper place
}

intC ERAPQ::pop() 
{
  intC result;

  if(heapEmpty())
    {
      pXCPT->Error(heapufloE, aS("sieve"));
      return(0);
    }

  result = pq[1]->p;                           // top of the heap

  pq[1] =  length == 1 ? 0 : pq[length];       // put last 1st
  if(pq[1])
    SiftDownPn();                              // sink 1st to proper place
  length--;
  return result;
}

inline bool ERAPQ::enforce(intC X, intC Y)
{ 
  if(pq[X]->pn <= pq[Y]->pn)
    return true;
  PQELEMENT *temp = pq[Y]; 
  pq[Y] = pq[X]; 
  pq[X] = temp;
  return false;
}

inline void ERAPQ::SiftUpPn() 
{
  register intC k, k2;
  
  for(k = length; k > 1; k = k2) 
    { 
      k2 = k >> 1; 
      enforce(k2, k);
    } 
}

inline void ERAPQ::SiftDownPn() 
{      
  register uintC c, k;
  
  for(k = 1, c = 2; c <= length; c = k << 1) 
    { 
      if((c+1) <= length) 
        if(pq[c+1]->pn < pq[c]->pn)
          c++; 
      if(enforce(k, c))
        break;
      k = c;   
    }
}

void ERAPQ::push(intC prime, intC primen)
{ 
  factor[free].p  = prime;
  factor[free].pn = primen;
  Insert(&factor[free++]); 
}

void ERAPQ::update(intC pp)
{
  for(PQELEMENT *lowest = pq[1]; lowest->pn < pp; lowest = pq[1])
	 if(lowest->pn > hi)                         // update
		pop();                                    // finished with this prime 
	 else
		{
		  lowest->update();
		  SiftDownPn();                           // sink new val to proper place
		}
}
  
void ERAPQ::dumpERAPQ()
{
  for(uintC j = 1; j <= length; j++)
    printf("%d: %d %d\n", j, pq[j]->p, pq[j]->pn);
}

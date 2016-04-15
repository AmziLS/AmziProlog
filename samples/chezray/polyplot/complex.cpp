
#include "amzi.h"
#include "complex.h"

double absvalue, theta;
int    angle;

COMPLEX COMPLEX::cpower(int n) 
{
  COMPLEX temp1, temp2;
  int n2;
  
  switch(n)	
    {
    case 0: return COMPLEX(1.0, 0);
    case 1: return *this;
    case 2: return *this * *this;
    case 3: return *this * *this * *this;
    default:
      n2 = n >> 1;
      temp1 = cpower(n2);
      temp2 = temp1 * temp1;
      return n == (n2 << 1) ? temp2 : *this * temp2;
    }
}
/* 
 * Classical efficient algorithm for evaluating a polynomial.
 * a + bz +cz^2 ...  =  a +z(b + z(c + ...
 */
COMPLEX COMPLEX::polyapply(double poly[], int degree)
{
COMPLEX z;
  if(degree == 0)
    return poly[0];
 z = *this * polyapply(&poly[1], degree-1) + poly[0];
  return z;
}




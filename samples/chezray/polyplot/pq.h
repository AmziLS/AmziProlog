#include <string.h>
#include <math.h>
#include "amzi.h"
#include "complex.h"

#define RELIEF     1
#define CARTESIAN  2
#define DISTANCE   3
#define MANDELBROT 4

class POLAR
{
public:
  POLAR(double m = 0.0, double a = 0.0): modulus(m), angle(a) {}
  POLAR(COMPLEX z)
    {
      modulus = sqrt(z.rpart()*z.rpart() + z.ipart()*z.ipart());
      angle   = atan(z.ipart()/z.rpart());
    }
  void setMod(double m)
    { modulus = m;}
  void setAngle(double a)
    { angle = a;}
  double getMod() const
    { return modulus;}
  double getAngle() const
    { return angle; }
  COMPLEX cartesian()
    { return COMPLEX(modulus* cos(angle), modulus* sin(angle)); }
private:
  double modulus;
  double angle;
};
/*
 * Class pq contains data and methods for a particular polynomial quotient,
 * as prescribed by prolog.
 */
class PQ
{
public:
  PQ &operator= (const PQ &pq)                // equals operator
    {
      Scale       = pq.Scale;
      numDegree   = pq.numDegree;
      denomDegree = pq.denomDegree;
      for(int i = 0; i< 40; i++)
	{
	  num[i]   = pq.num[i]; 
	  denom[i] = pq.denom[i];
	}
      strcpy(header, pq.header);
      return *this;
    }

  void setAngle(int a) 
    { Angle = a; }
  void setView(int d) 
    { View = d; }
  int getAngle() const
    { return Angle; }
  int getView() const
    { return View; }
  void getHeader(char * h) const
    { strcpy(h, header);}
  COMPLEX appliedto(COMPLEX);                   // this pq applied to a z value
  int displaypq(ENGid);

private:
  int    View;                                  // View display is wanted
  int    Angle;                                 // increments of grad lines
  double Scale;                                 // weight of poly quotient
  int    numDegree;                             // numerator degree
  int    denomDegree;                           // denominator degree
  double num[100];                              // numerator
  double denom[100];                            // denominator
  char   header[80];                            // window legend
};


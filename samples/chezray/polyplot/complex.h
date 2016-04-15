//-*-C++-*-
/* complex.h */
#ifndef COMPLEX_H
#define COMPLEX_H

#include <math.h>

const  double pi = 4*atan(1.0);

class COMPLEX
{
public:
  COMPLEX(double r = 0.0, double i = 0.0) : re(r), im(i) {}
  COMPLEX(const COMPLEX &c) : re(c.re), im(c.im) {}
  COMPLEX &operator= (const COMPLEX &c)
  {
    re = c.re;
    im = c.im;
    return *this;
  }
  COMPLEX operator+(const COMPLEX &c) const
  {
    return COMPLEX(re + c.re, im + c.im);
  } 
  COMPLEX operator+=(const COMPLEX &c) 
  {
    re += c.re;
    im += c.im;
    return *this;
  } 
  friend COMPLEX operator+(double d, const COMPLEX &c) 
  { return c + COMPLEX(d); }
  friend COMPLEX operator+(int i, const COMPLEX &c) 
  { return c + COMPLEX(i); }
  COMPLEX operator-(const COMPLEX &c) const
  { return COMPLEX(re - c.re, im - c.im); } 
  friend COMPLEX operator-(double d, const COMPLEX &c) 
  { return c - COMPLEX(d); }
  friend COMPLEX operator-(int i, const COMPLEX &c) 
  { return c - COMPLEX(i); }
  COMPLEX operator*(const COMPLEX &c) const
  { return COMPLEX(re*c.re - im*c.im, re*c.im + c.re*im); } 
  friend COMPLEX operator*(double d, const COMPLEX &c) 
  { return c * COMPLEX(d); }
  friend COMPLEX operator*(int i, const COMPLEX &c) 
  { return c * COMPLEX(i); }
  COMPLEX operator/(const COMPLEX &c) const
  {
    double norm = c.normOf();
    return COMPLEX((re*c.re + im*c.im)/norm, (c.re*im - re*c.im)/norm);
  } 
  COMPLEX cis(double theta)
  { return COMPLEX(cos(theta), sin(theta)); }
  COMPLEX cpower(int);
  COMPLEX polyapply(double [], int);
  friend COMPLEX cpower(const COMPLEX &, int);
  double rpart() const
  { return(re);}
  double ipart() const
  { return(im);}
  double normOf() const
  { return(re*re + im*im);}
  void setRe(double d)
  { re = d;}
  void setIm(double d)
  { im = d;}

private:
  double re;
  double im;  
};
#endif COMPLEX_H






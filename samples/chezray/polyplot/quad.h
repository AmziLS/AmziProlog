//-*-C++-*-


/*        quad.h  */

#define WIDTH   800
#define HEIGHT  600

#include <windef.h>
#include "complex.h"
/*
 * Class Quad contains data and methods for displaying a polynomial quotient.
 */
class QUAD
{
public:
  QUAD(void);
  QUAD(const QUAD &Q):
  deltaAngle(Q.deltaAngle),
  Epsilon(Q.Epsilon),
  Quantum(Q.Quantum),
  ColorMax(Q.ColorMax),
  XOrg(Q.XOrg),
  YOrg(Q.YOrg),
  Scale(Q.Scale),
  FXOffset(Q.FXOffset),  
  FYOffset(Q.FYOffset),
  Width(Q.Width),
  Height(Q.Height),
  Left(Q.Left),
  Right(Q.Right),
  Top(Q.Top),
  Bottom(Q.Bottom),
  xAxis(Q.xAxis),
  yAxis(Q.yAxis),
  Axes(Q.Axes),
  UnitC(Q.UnitC) {}

  // ~QUAD();
  BOOL CALLBACK dlgProc(HWND, UINT, WPARAM , LPARAM );
  //  void tesselate(HWND, int);
  void tesselate(HDC, int);
  void setFunction();
  int  solve(int, int);
  void start(HWND);
  int  rootFinder(POINT);
  void init(UINT n);
  void initLut(void);
  void cross(COMPLEX, char);
  int  const getScale(void) { return Scale;}
  int  const getColorMax(void) { return ColorMax;}
  double const getEpsilon(void) { return Epsilon;}
  COLORREF const getLut(int i) { return lut[i];}
  void setDelta(int angle) {deltaAngle = angle;}
  int  getDelta() { return deltaAngle;}
  int  getGoal() { return Goal;}
  int  getSound() { return Sound;}
  POINT const origin(void)
  {
    POINT point;
    
    point.x = (WIDTH>>1)  - (long)(FXOffset*Scale);
    point.y = (HEIGHT>>1) + (long)(FYOffset*Scale);
    return point;
  }
  void setAxes(void)
  {
    xAxis  = (int)(YOrg+Scale*FYOffset);
    yAxis  = (int)(XOrg+Scale*FXOffset);
  };
  
  COMPLEX  screen2model(POINT point)
  {
    COMPLEX z;
    
    z.setRe(((double)(point.x-XOrg))/Scale+FXOffset);
    z.setIm(((double)(YOrg-point.y))/Scale+FYOffset);
    return z;
  }
  COMPLEX  screen2model(POINTS point)
  {
    COMPLEX z;
    
    z.setRe(((double)(point.x-XOrg))/Scale+FXOffset);
    z.setIm(((double)(YOrg-point.y))/Scale+FYOffset);
    return z;
  }
  POINT model2screen(COMPLEX z)
  {
    POINT s;

    s.x = (int)(XOrg+ Scale* (z.rpart()- FXOffset));
    s.y = (int)(YOrg- Scale* (z.ipart()+ FYOffset));
    return s;
  }
  
private:
  int    Goal;
  int    deltaAngle;
  double Epsilon;
  int	 Quantum;
  int	 ColorMax;
  int    XOrg;
  int    YOrg;
  int	 Scale;
  double FXOffset;      // X coord at center of window
  double FYOffset;      // Y coord at center of window
  int    Width;
  int    Height;
  int	 Left;
  int	 Right;
  int	 Top;
  int    Bottom;
  int    xAxis;
  int    yAxis;
  int    Axes;
  int    UnitC;
  int    Text;
  int    Sound;

  COLORREF lut[16];
  COLORREF *lutp;
  COLORREF background;
};

class PALOMINE
{
public:
  PALOMINE();

private:
  WORD palVersion;
  WORD palNumEntries;
  int  palPalEntry[16];
};   

enum{X, Y};

#include <windows.h>
#include <commdlg.h>    // includes common dialog functionality
#include <dlgs.h>       // includes common dialog template defines
#include <stdio.h>
#include "complex.h"
#include "quad.h"

#define NW 0
#define NE 1
#define SE 2
#define SW 3

HBRUSH paintBrush[32];
COLORREF black = 0xffffff;

int tiles, pixels;
HGDIOBJ last1, last2, last3;

extern int angle;
extern QUAD Q;
extern int colorFunction(COMPLEX);

void QUAD::tesselate(HDC hDC, int quadCode)
{                                                /* color tile or quad it */
  int color, refcolor, n;
  int width, height, oddWidth, oddHeight, w2, h2;
  int shift, depth, code, quadcode;
  int i, j, top, left;
  POINT point, center, axes;
  COMPLEX z;

  quadcode = quadCode;
  depth = 0;
  while(quadcode>>=2)
    depth++;

  top    = 0;
  left   = 0;
  width  = WIDTH;
  height = HEIGHT;

  for(shift = depth-1; shift>=0; shift--)
    {
      code = (quadCode >> (2*shift))&3;
      oddWidth  = width&1;
      oddHeight = height&1;
      width  >>= 1;
      height >>= 1;
      switch(code)                   /* nw:0  ne:1  se:2  sw:3  fencepost:4 */
         {
	case NW: 
	  break;
	case NE:
	  left += width;
	  if(oddWidth)
	    width++;
	  break;
	case SE:
	  top  += height;
	  left += width;
	  if(oddHeight)
	    height++;
	  if(oddWidth)
	    width++;
	  break;
	case SW:
	  top  += height;
	  if(oddHeight)
	    height++;
	  break;
	      }
    }

  axes = Q.origin();
  
  if((height<Quantum) || (width<Quantum))
    {
      for(i = 0, point.x = left; i < width; i++, point.x++)
        for(j = 0, point.y = top; j < (height+1); j++, point.y++)
          {
	    z = screen2model(point);
	    //	    z.setRe(((double)(point.x-Q.XOrg))/Q.Scale+Q.FXOffset);
	    //z.setIm(((double)(Q.YOrg-point.y))/Q.Scale+Q.FYOffset);

       n = colorFunction(z);
	    pixels++;
	    if(n == 16)
	      n = 15;
	    color = *(Q.lutp+n);
	    SetPixel(hDC, point.x, point.y, color);
	    if(Q.FYOffset == 0)
	      SetPixel(hDC, point.x, HEIGHT-point.y, color);
          }
      return;
    }
  
  w2  = width/2;
  h2  = height/2;
  center.x  = left + w2;
  center.y  = top  + h2;
  
  z.setRe(((double)(center.x-Q.XOrg))/Q.Scale+Q.FXOffset);
  z.setIm(((double)(Q.YOrg-center.y))/Q.Scale+Q.FYOffset);
  refcolor = colorFunction(z);                             // center color
  pixels++;
                                                // check corners 
  point.x = left;
  point.y = top;
  z.setRe(((double)(point.x-Q.XOrg))/Q.Scale+Q.FXOffset);
  z.setIm(((double)(Q.YOrg-point.y))/Q.Scale+Q.FYOffset);
  color = colorFunction(z);
  pixels++;
  if(color != refcolor)     
    goto recurse;

  point.y = top + height;
  z.setIm(((double)(Q.YOrg-point.y))/Q.Scale+Q.FYOffset);
  color = colorFunction(z);
  pixels++;
  if(color != refcolor)    
    goto recurse;

  point.x = left + width;
  point.y = top + height;
  z.setRe(((double)(point.x-Q.XOrg))/Q.Scale+Q.FXOffset);
  z.setIm(((double)(Q.YOrg-point.y))/Q.Scale+Q.FYOffset);
  color = colorFunction(z);
  pixels++;
  if(color != refcolor)    
    goto recurse;

  point.y = top;
  z.setIm(((double)(Q.YOrg-point.y))/Q.Scale+Q.FYOffset);
  color = colorFunction(z);
  pixels++;
  if(color != refcolor)    
    goto recurse;

  point.x = left;                               // left side
  z.setRe(((double)(point.x-Q.XOrg))/Q.Scale+Q.FXOffset);
  for(i = 0; i <= height; i++)                  
    {
      point.y = top + i;
      z.setIm(((double)(Q.YOrg-point.y))/Q.Scale+Q.FYOffset);
      color = colorFunction(z);
      pixels++;
      if(color != refcolor)    
        goto recurse;
    }
  point.x = left + width;                       // right side
  z.setRe(((double)(point.x-Q.XOrg))/Q.Scale+Q.FXOffset);
  for(i = 0; i <= height; i++)                  
    {
      point.y = top + i;
      z.setIm(((double)(Q.YOrg-point.y))/Q.Scale+Q.FYOffset);
      color = colorFunction(z);
      pixels++;
      if(color != refcolor)    
        goto recurse;
    }
  point.y = top;                                // top side
  z.setIm(((double)(Q.YOrg-point.y))/Q.Scale+Q.FYOffset);
  for(i = 1; i < width; i++)
    {
      point.x = left + i;
      z.setRe(((double)(point.x-Q.XOrg))/Q.Scale+Q.FXOffset);
      color = colorFunction(z);
      pixels++;
      if(color != refcolor)     
        goto recurse;
    }
  point.y = top + height;                       // bottom side
  z.setIm(((double)(Q.YOrg-point.y))/Q.Scale+Q.FYOffset);
  for(i = 1; i < width; i++)
      {
      point.x = left + i;
      z.setRe(((double)(point.x-Q.XOrg))/Q.Scale+Q.FXOffset);
      color = colorFunction(z);
      pixels++;
      if(color != refcolor)    
        goto recurse;
    }
                                                // perimeter all one color 
  last1 = SelectObject(hDC, paintBrush[refcolor]);
  tiles++;
  PatBlt(hDC, left, top, width, height, PATCOPY);
  if(Q.FYOffset == 0)                           // mirror
    PatBlt(hDC, left, HEIGHT-(top + height), width, height, PATCOPY); 
   
  return;

 recurse:            
  tesselate(hDC, (quadCode << 2)|0);                        /* nw */
  tesselate(hDC, (quadCode << 2)|1);                        /* ne */
  tesselate(hDC, (quadCode << 2)|2);                        /* se */
  tesselate(hDC, (quadCode << 2)|3);                        /* sw */
}

int normOf(POINT point)
{
  return point.x*point.x + point.y*point.y;
}

void QUAD::initLut(void)
{
lutp = lut;
paintBrush[ 0] = CreateSolidBrush(lut[0]);
paintBrush[ 1] = CreateSolidBrush(lut[1]);
paintBrush[ 2] = CreateSolidBrush(lut[2]);
paintBrush[ 3] = CreateSolidBrush(lut[3]);
paintBrush[ 4] = CreateSolidBrush(lut[4]);
paintBrush[ 5] = CreateSolidBrush(lut[5]);
paintBrush[ 6] = CreateSolidBrush(lut[6]);
paintBrush[ 7] = CreateSolidBrush(lut[7]);
paintBrush[ 8] = CreateSolidBrush(lut[8]);
paintBrush[ 9] = CreateSolidBrush(lut[9]);
paintBrush[10] = CreateSolidBrush(lut[10]);
paintBrush[11] = CreateSolidBrush(lut[11]);
paintBrush[12] = CreateSolidBrush(lut[12]);
paintBrush[13] = CreateSolidBrush(lut[13]);
paintBrush[14] = CreateSolidBrush(lut[14]);
paintBrush[15] = CreateSolidBrush(lut[15]);
paintBrush[16] = (HBRUSH)GetStockObject(WHITE_BRUSH);
}












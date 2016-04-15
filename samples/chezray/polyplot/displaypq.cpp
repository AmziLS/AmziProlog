    /**************************************************************\
    *                                                              *
    * displaypq.cpp 
    *                                                              *
    * Copyright (c) 1997 by Ray Reeves.  All Rights Reserved.      *
    *                                                              *
    \**************************************************************/

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <math.h>
#include "pq.h"
#include "quad.h"
#include <Amzi.h>

#define SHMEMSIZE sizeof(PQ)
#define BACKGROUND 15
#define HIGHCUTOFF 11
#define CONTOUR     3
#define GRADIENT    8
#define BLACK       0

extern PRED_INIT arrayPreds[];

LPVOID lpvMem = NULL;                    // addr of shared memory

int relief(COMPLEX, double, double);
int cartesian(COMPLEX, double, double);

QUAD * Qp;
PQ pq;
POLAR pw;
POINT orbit[16];
char buf[80];

/* DLL Entry and Exit Points */
/* ------------------------- */

#if defined(_WIN32)
extern "C" BOOL WINAPI DllMain(HINSTANCE hinstDLL, 
				DWORD fdwReason, 
				LPVOID lpReserved)
{                                                    // DLL EntryPoint function
  HANDLE hMapObject = NULL;
  BOOL fInit, fIgnore;

  switch(fdwReason)
    {
    case DLL_PROCESS_ATTACH:
      hMapObject = CreateFileMapping((HANDLE) 0xFFFFFFFF,
				     NULL,
				     PAGE_READWRITE,
				     0,
				     SHMEMSIZE,
				     "dllmemfilemap");
      if(hMapObject == NULL)
	return FALSE;

      fInit = (GetLastError() != ERROR_ALREADY_EXISTS);
      lpvMem = MapViewOfFile(hMapObject, FILE_MAP_WRITE, 0, 0, 0);

      if(lpvMem == NULL)
	return FALSE;

      if(fInit)
	memset(lpvMem, '\0', SHMEMSIZE);     // 1st process. init shared mem   
      else
	pq = *(PQ *)lpvMem;                  // not 1st. unload shared mem
      break;
    case DLL_THREAD_ATTACH:
      break;
    case DLL_THREAD_DETACH:
      break;
    case DLL_PROCESS_DETACH:
      fIgnore = UnmapViewOfFile(lpvMem);
      fIgnore = CloseHandle(hMapObject);
      break;
    default:
      break;
    }
  return TRUE;
  UNREFERENCED_PARAMETER(hinstDLL);
  UNREFERENCED_PARAMETER(lpReserved);
}
#else
int _pascal LibMain( HANDLE hInstance, WORD wDataSeg, WORD wHeapSize, 
		    LPSTR lpszCmdLine)
{                                                 
  return 1;
}

int _pascal _export WEP(int param)
{ 
  return 1;
}
#endif

TF EXPFUNC  p_setpq(ENGid);                        // set pq parms from prolog

PRED_INIT arrayPreds[] = 
{
  {"initpq",    3, p_setpq},                       // the only prolog entry
  {NULL, 0, NULL}   
};

/* Required LSX Initialization Function */
/* ------------------------------------ */

#ifdef _WIN32
extern "C"__declspec(dllexport) RC EXPFUNC InitPreds(ENGid eid, void* p)
#else
RC EXPFUNC InitPreds(ENGid eid, void* p)
#endif
{
  RC rc;
  char buf[80];
  
  rc = lsInitPreds(eid, arrayPreds);
  if (rc)
    {
      wsprintf(buf, "Error #%d Loading pq Predicates", rc);
      MessageBox(NULL, buf, "displaypq Error", MB_OK);
    }
  else
    MessageBox(NULL, "pq Predicates Loaded", "LARGE Info", MB_OK);
  return 0;
}

__declspec(dllexport)POINT *getOrbit(void)
  {
    return orbit;
  }

__declspec(dllexport)void passQ(QUAD *Q)
{
  Qp = Q;
}
__declspec(dllexport)void pqInit(int a, int d)
{
  ((PQ *)lpvMem)->setAngle(a);
  ((PQ *)lpvMem)->setView(d);
}

__declspec(dllexport) void pqHeader(char *h)
{
  ((PQ *)lpvMem)->getHeader(h);
}

__declspec(dllexport)int colorFunction(COMPLEX zin)
{
  COMPLEX z, w, delta;
  double msq, epsilon, eps2;
  int n, view;
  
  z = zin;
  view = ((PQ *)lpvMem)->getView();
  switch(view)
    {
    case MANDELBROT:
      orbit[0] = Qp->model2screen(z);
      orbit[15].x = orbit[15].y = 0;
      for(n = 0; n < 100; n++)
	{
	  if((n > 0) && (n < 15))
	    {
	      orbit[n] = Qp->model2screen(z);
	      orbit[n+1].x = orbit[n+1].y = 0;
	    }
	  z = pq.appliedto(z)+zin;
	  if(z.normOf() > 4)
	    return 1 + n % 12;
	}
     return 0;
    case DISTANCE:
      orbit[0] = Qp->model2screen(z);
      w = Qp->screen2model(orbit[0]);
      for(n = 0; n < Qp->getColorMax(); n++)
	{
	  if(n > 0)
	    orbit[n] = Qp->model2screen(z);
	  
	  w = ((PQ *)lpvMem)->appliedto(z);                      // solve 
	  delta = w-z;
	  z = w;
	  if((msq = delta.normOf()) < Qp->getEpsilon()) 
	    break;
	}
      return min(n, Qp->getColorMax()-1);           // return distance color
    case RELIEF:
      epsilon = 0.1;
      eps2 = epsilon/2;
      w = pq.appliedto(z);
      return relief(w, epsilon, eps2);
    case CARTESIAN:
      epsilon = 0.1;
      eps2 = epsilon/2;
      w = pq.appliedto(z);
      return cartesian(w, epsilon, eps2);
    default:
      return 0;
    }
 }

__declspec(dllexport) POLAR reliefFunction(COMPLEX z)
{
  COMPLEX w;
  double epsilon, eps2;
  double wRe, wIm;

  epsilon = 0.1;
  eps2 = epsilon/2;

  w = pq.appliedto(z);

  wRe = w.rpart();
  wIm = w.ipart();

  pw.setMod(sqrt(wRe*wRe+wIm*wIm));
  pw.setAngle(atan2(wIm, wRe));

  return pw;
 }
__declspec(dllexport) COMPLEX cartesianFunction(COMPLEX z)
{
  return pq.appliedto(z);
 }

/* built-in predicates, callable from Prolog */
/* ----------------------------------------- */

TF EXPFUNC  p_setpq(ENGid eid)                      // set pq parms from prolog
{
  int status;

  status = pq.displaypq(eid);
  *(PQ *)lpvMem = pq;                                // set shared mem
  return status;
}

int relief(COMPLEX w, double epsilon, double eps2)
{
  double discriminant, absvalue, theta, theta1;
  int angle;

  absvalue = sqrt(w.normOf());
  pw.setMod(absvalue);                               // polar return value
  if (absvalue > 8)
    return HIGHCUTOFF;                               // high up the pole
  discriminant = 4*absvalue + eps2;
  if (discriminant < epsilon)
    return BACKGROUND;                               // function is vanishing
  discriminant -= floor(discriminant);               // contours at 0.25
  if(discriminant < epsilon)                         
    return CONTOUR;                                  // on a contour
  
  theta = atan2(w.ipart(), w.rpart());               // angles
  pw.setAngle(theta);                                // polar return value
  theta1 = w.ipart() < 0 ? pi -theta : theta;
  angle = ((PQ *)lpvMem)->getAngle();
  discriminant = 180*theta1/(pi*angle) + eps2;
  discriminant -= floor(discriminant);               // 
  if(discriminant < epsilon)
    return GRADIENT;                                 // on a gradient
  return BACKGROUND;

};

int cartesian(COMPLEX w, double epsilon, double eps2)
{
  double absvalue, discriminant;

  absvalue = sqrt(w.normOf());
  if (absvalue > 8)
    return HIGHCUTOFF;                               // high up the pole
  discriminant = 4*w.rpart() + eps2;
  discriminant -= floor(discriminant);               // contours at 0.25
  if(discriminant < epsilon)                         
    return CONTOUR;                                  // on a contour
  
  discriminant = 4*w.ipart() + eps2;
  discriminant -= floor(discriminant);               // 
  if(discriminant < epsilon)
    return GRADIENT;                                 // on a gradient
  return BACKGROUND;

};





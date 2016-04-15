#include <windows.h>
#include "amzi.h"
#include "pq.h"

extern LPVOID lpvMem;                          // addr of shared memory

int PQ::displaypq(ENGid eid)   // acquire pq parms from prolog, set up PQ data
{                                              // 1: Scale  2: N/D  3: Legend
  TERM   t, tlist;                             
  pTYPE  pt;
  int    temp, popStatus;
  double tempf, *p;

  pt = lsGetParmType(eid, 1);                  // get type of 1st par 
  switch(pt)
    {
    case pINT:
      lsGetParm(eid, 1, cINT, &temp);             
      Scale = (double) temp;
      break;
    case pFLOAT:
      lsGetParm(eid, 1, cDOUBLE, &Scale);             
      break;
    default:
     return FALSE;
    }

  pt = lsGetParmType(eid, 2);                  // get type of 2nd par 
  if(pt = pSTRUCT)
    lsGetParm(eid, 2, cTERM, &t);              // ok, put it in t
  else
    return FALSE;

  pt = lsGetParmType(eid, 3);                  // get type of 3rd par 
  if(pt = pSTR)
    lsGetParm(eid, 3, cSTR, header);             
  else
    return FALSE;

  pt = lsGetArgType(eid, t, 1);              // get type of 1st arg of 2nd par
  switch(pt)
    {
    case pINT:
      lsGetArg(eid, t, 1, cINT, &temp);             
      num[0]= (double)temp;
      numDegree = 0;
      break;
    case pFLOAT:
      lsGetArg(eid, t, 1, cDOUBLE, &num[0]);             
      numDegree = 0;
      break;
    case pLIST:
      lsGetArg(eid, t, 1, cTERM, &tlist);             
      popStatus = OK;
      for(p = num, numDegree = -2; 
	  popStatus == OK; 
	  p++, numDegree++) 
	popStatus = lsPopList(eid, &tlist, cDOUBLE, p);
      break;
    default:
      return FALSE;
    }
  
  pt = lsGetArgType(eid, t, 2);               // get type of 2nd arg of 2nd par
  switch(pt)
    {
    case pINT:
      lsGetArg(eid, t, 2, cINT, &temp);             
      (Scale) /= (double)temp;
      denomDegree = 0;
      break;
    case pFLOAT:
      lsGetArg(eid, t, 2, cDOUBLE, &tempf);             
      (Scale) /= tempf;
      denomDegree = 0;
      break;
    case pLIST:
      lsGetArg(eid, t, 2, cTERM, &tlist);             
      popStatus = OK;
      for(p = denom, denomDegree = -2; 
			 popStatus == OK; 
			 p++, denomDegree++)  
		  popStatus = lsPopList(eid, &tlist, cDOUBLE, p);
      break;
    default:
      return FALSE;
    }
  return TRUE;
}

COMPLEX PQ::appliedto(COMPLEX z)               // pq function applied to z
{
COMPLEX numerator, denominator, result;

  if(numDegree == 0)
    return (COMPLEX)Scale / z.polyapply(denom, denomDegree); 
  else
    if(denomDegree == 0)
      return (COMPLEX)Scale * z.polyapply(num, numDegree);
    else
      {
	numerator   = z.polyapply(num,   numDegree);
	denominator = z.polyapply(denom, denomDegree); 
	result = (COMPLEX)Scale*numerator/denominator;
	return result;
      }
}





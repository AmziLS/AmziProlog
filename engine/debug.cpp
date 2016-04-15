/***************************************************************************\
*
* debug.cpp -- Methods used during debugging
*
* Copyright (c) 1992-2002 by Amzi! inc.  All Rights Reserved.
*
* $Log: debug.cpp,v $
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.3  2002/05/15 16:59:07  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.2  2001/03/09 20:13:59  dennis
* consolidating for KW delivery, fixed jni memory leak
*
* Revision 1.1  2001/02/28 02:22:38  dennis
* fixed some number bugs, some assert/retract bugs
*
*
*
\***************************************************************************/

#include "inc.h"
#include "pch.h"
#include <algorithm>
#include <ctime>

#ifdef PROBE

typedef std::map<LPredicate, int> EscapeMap;
typedef EscapeMap::iterator EscapeIter;
typedef std::pair<LPredicate, int> EscapePair;

bool operator<(WamHit a, WamHit b)
{
   if ( a.hits > b.hits )
      return true;
   else
      return false;
}

bool operator<(WamTime a, WamTime b)
{
   if ( a.time > b.time )
      return true;
   else
      return false;
}

void Probe::Init()
{
   wam_hits.resize(N_OPS);
   wam_times.resize(N_OPS);
}

void Probe::wam_init()
{
   int i;
   for (i=0; i<N_OPS; i++)
   {
      wam_hits[i].op = i;
      wam_hits[i].hits = 0;
      wam_times[i].op = i;
      wam_times[i].time = 0;
   }
   last_time = clock();
   this_op = 0;
}

void Probe::report()
{
   DUMP << NL << NL;
   DUMP << "--------------PROBE-------------" << NL << NL;
   int i;

   DUMP << "--------------WAM Hits-------------" << NL << NL;
   std::sort(wam_hits.begin(), wam_hits.end());
   for (i=0; i<N_OPS; i++)
   {
      DUMP << ops[wam_hits[i].op] << " hits = " << wam_hits[i].hits << NL;
   }
   DUMP << "--------------WAM Times-------------" << NL << NL;
   std::sort(wam_times.begin(), wam_times.end());
   for (i=0; i<N_OPS; i++)
   {
      DUMP << ops[wam_times[i].op] << " time = " << wam_times[i].time << NL;
   }
}

#endif //PROBE
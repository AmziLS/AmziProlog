/***************************************************************************\
*
* probe.cpp -- Methods used during debugging
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* $Log: probe.cpp,v $
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.2  2002/05/15 16:59:09  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.1  2001/03/09 20:20:23  dennis
* added probe files
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

bool operator<(PredInfo a, PredInfo b)
{
   if ( a.time > b.time )
      return true;
   if ( a.time < b.time )
      return false;
   if ( a.hits > b.hits )
      return true;
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
   start_time = last_time = clock();
   this_op = 0;
}

void Probe::escape_start(PATOM pred, ARITY ar)
{
   ei = escape_map.find( LPredicate(pred,ar) );

   if (ei == escape_map.end())
   {
      Info *pinfo = new Info();
      std::pair<EscapeIter, bool> result = escape_map.insert( EscapePair(LPredicate(pred,ar), pinfo) );
      ei = result.first;
   }
   else
   {
      ei->second->hits++;
   }

   LASSERT(ei->second->t1 == 0, aS("recursive escape call"));
   ei->second->t1 = clock();
}

void Probe::escape_end(PATOM pred, ARITY ar)
{
   ei->second->time += clock() - ei->second->t1;
   ei->second->t1 = 0;
}

void Probe::whoami_hit(PATOM pred, ARITY ar)
{
   ei = whoami_map.find( LPredicate(pred,ar) );

   if (ei == whoami_map.end())
   {
      Info *pinfo = new Info();
      std::pair<EscapeIter, bool> result = whoami_map.insert( EscapePair(LPredicate(pred,ar), pinfo) );
      ei = result.first;
   }
   else
   {
      ei->second->hits++;
   }
}

void Probe::function_start(LString func)
{
   fi = function_map.find( func );

   if (fi == function_map.end())
   {
      Info *pinfo = new Info();
      std::pair<FunctionIter, bool> result =
            function_map.insert( FunctionPair(func, pinfo) );
      fi = result.first;
   }
   else
   {
      fi->second->hits++;
   }

   fi->second->t1 = clock();
}

void Probe::function_end(LString func)
{
   fi->second->time += clock() - fi->second->t1;
}


void Probe::report()
{
   int i;
   DUMP << NL << NL;
   DUMP << "--------------PROBE-------------" << NL << NL;
   DUMP << "Total Time = " << time << NL;

   DUMP << NL << "--------------WAM Hits-------------" << NL << NL;
   std::sort(wam_hits.begin(), wam_hits.end());
   for (i=0; i<N_OPS; i++)
   {
      DUMP << ops[wam_hits[i].op] << " hits = " << wam_hits[i].hits << NL;
   }

   DUMP << NL << "--------------WAM Times-------------" << NL << NL;
   std::sort(wam_times.begin(), wam_times.end());
   for (i=0; i<N_OPS; i++)
   {
      DUMP << ops[wam_times[i].op] << " time = " << wam_times[i].time << NL;
   }

   DUMP << NL << "--------------Escape Hits-------------" << NL << NL;
   EscapeIter ei = escape_map.begin();
   std::vector<PredInfo> escape_times;

   while (ei != escape_map.end())
   {
      escape_times.push_back( PredInfo(
         ei->first.functor,
         ei->first.arity,
         ei->second->hits,
         ei->second->time) );
      ei++;
   }
   std::sort(escape_times.begin(), escape_times.end());
   for (i=0; i<escape_times.size(); i++)
   {
      DUMP << PREDAR(escape_times[i].pred, escape_times[i].ar) <<
         " hits: " << escape_times[i].hits <<
         " time: " << escape_times[i].time << NL;
   }

   DUMP << NL << "--------------WhoAmI Hits-------------" << NL << NL;
   EscapeIter wi = whoami_map.begin();
   std::vector<PredInfo> whoami_hits;

   while (wi != whoami_map.end())
   {
      whoami_hits.push_back( PredInfo(
         wi->first.functor,
         wi->first.arity,
         wi->second->hits,
         wi->second->time) );
      wi++;
   }
   std::sort(whoami_hits.begin(), whoami_hits.end());
   for (i=0; i<whoami_hits.size(); i++)
   {
      DUMP << PREDAR(whoami_hits[i].pred, whoami_hits[i].ar) <<
         " hits: " << whoami_hits[i].hits << NL;
   }

   DUMP << NL << "--------------Function Hits-------------" << NL << NL;
   FunctionIter fi = function_map.begin();
   while (fi != function_map.end())
   {
      DUMP << fi->first <<
         " hits: " << fi->second->hits <<
         " time: " << fi->second->time << NL;
      fi++;
   }
}

#endif //PROBE

/**************************************************************************
* 
* probe.h - performance probe for internal use.
* 
* $Log: probe.h,v $
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.2  2001/08/24 16:06:44  dennis
* fixed header file comments at end of files, removed ostream include
* which isn't apparently necessary
*
* Revision 1.1  2001/03/09 20:20:23  dennis
* added probe files
*
*
*
**************************************************************************/

#ifndef PROBE_H
#define PROBE_H

// PROBE will only be defined if LANDFILL is, in debug.h
#ifdef PROBE

#define PROBE_WAM_INIT pPROBE->wam_init()
#define PROBE_WAM_STOP pPROBE->wam_stop()
#define PROBE_WAM(X) pPROBE->wam_hit(X)
#define PROBE_ESCAPE_START(P,A) pPROBE->escape_start(P,A)
#define PROBE_ESCAPE_END(P,A) pPROBE->escape_end(P,A)
#define PROBE_WHOAMI_HIT(P,A) pPROBE->whoami_hit(P,A)
#define PROBE_FUNCTION_START(F) pPROBE->function_start(F)
#define PROBE_FUNCTION_END(F) pPROBE->function_end(F)

#define N_OPS 57

class WamHit
{
public:
   int op;
   int hits;
};

class WamTime
{
public:
   int op;
   clock_t time;
};

class Info
{
public:
   int hits;
   clock_t time;
   clock_t t1;

   Info() { hits = 1; time = 0; t1 = 0; }
};

class PredInfo
{
public:
   PATOM pred;
   ARITY ar;
   int hits;
   clock_t time;

   PredInfo(PATOM p, ARITY a, int h, clock_t t)
   { pred = p; ar = a; hits = h; time = t; }
};

typedef std::map<LPredicate, Info*> EscapeMap;
typedef EscapeMap::iterator EscapeIter;
typedef std::pair<LPredicate, Info*> EscapePair;

typedef std::map<LString, Info*> FunctionMap;
typedef FunctionMap::iterator FunctionIter;
typedef std::pair<LString, Info*> FunctionPair;

// We need to define < for STL sorts and maps
bool operator<(WamHit a, WamHit b);
bool operator<(WamTime a, WamTime b);
bool operator<(PredInfo a, PredInfo b);

class Probe
{
public:
   LEngine *m_peng;

   clock_t last_time;
   clock_t escape_time;
   EscapeIter ei;
   int this_op;
   clock_t start_time;
   clock_t time;
   clock_t function_time;
   FunctionIter fi;

   std::vector<WamTime> wam_times;
   std::vector<WamHit> wam_hits;
   EscapeMap escape_map;
   EscapeMap whoami_map;
   FunctionMap function_map;

   Probe(LEngine *peng)
   { m_peng = peng; }

   void Init();
   
   void wam_init();

   void wam_stop()
   {  time = clock() - start_time; }

   void wam_hit(int OP)
   {
      wam_hits[OP].hits++;
      clock_t now = clock();
      wam_times[this_op].time += now - last_time;
      last_time = now;
      this_op = OP;
   }

   void escape_start(PATOM pred, ARITY ar);
   void escape_end(PATOM pred, ARITY ar);

   void whoami_hit(PATOM pred, ARITY ar);

   void function_start(LString func);
   void function_end(LString func);

   void report();
};
#else  // no PROBE

#define PROBE_WAM_INIT
#define PROBE_WAM_STOP
#define PROBE_WAM(X)
#define PROBE_ESCAPE_START(P,A)
#define PROBE_ESCAPE_END(P,A)
#define PROBE_WHOAMI_HIT(P,A)
#define PROBE_FUNCTION_START(F)
#define PROBE_FUNCTION_END(F)

#endif // PROBE

#endif //PROBE_H

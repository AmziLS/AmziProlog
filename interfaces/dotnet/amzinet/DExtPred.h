#pragma once

#include "vcclr.h" 
#include "amzinet.h"

#using <mscorlib.dll> 
using namespace System; 
using namespace System::Runtime::InteropServices; 

class DExtPred
{
public:
   gcroot<PrologPredicate^>   dptr;
   gcroot<LogicServer ^>      that;
   DExtPred*                  pnext;

   DExtPred();
   virtual ~DExtPred();
};

DExtPred::DExtPred()
{
}

DExtPred::~DExtPred()
{
}

// amzinet.h

#pragma once

using namespace System;
using namespace System::IO;
using namespace System::Text;

using System::Runtime::InteropServices::Marshal;

ref class LogicServer;
ref class LSException;
public delegate Boolean PrologPredicate(LogicServer^);

public enum class exLSTYPE : int {
   BADENG,
   ABORT,
   INTERNAL,
   FATAL,
   INIT,
   API,
   LOAD,
   EXEC,
   READ,
   UNKNOWN
   };

public enum class pLSTYPE : int {
   pERR = -1,
   pATOM,
   pINT,
   pSTR,
   pFLOAT,
   pSTRUCT,
   pLIST,
   pTERM,
   pADDR,
   pVAR,
   pWSTR,
   pWATOM,
   pREAL
   };

public enum class dLSTYPE : int {
   dAATOM,
   dASTR,
   dINT,
   dLONG,
   dSHORT,
   dFLOAT,
   dDOUBLE,
   dADDR,
   dTERM,
   dWSTR,
   dWATOM,
   dMOD,
   dGOAL
   };


namespace amzinet
{
   public ref class LogicServer
	{
	};

   public ref class LSException
   {
   };

}


#pragma once

#include <amzi.h>

using namespace System::Runtime::Serialization;

[ Serializable ]

// MS Documentation is confused on whether this should be
// System::Exception or System::ApplicationException ???
public ref class LSException : public ApplicationException
{
private:
   void * m_eng;

public:
   LSException(ENGid e) 
   { 
      m_eng = e; 
   }

   virtual property String ^Message 
      {
	      String ^get() override
         {
            return GetMsg();
         }
      }

protected:
	LSException( SerializationInfo^ info, StreamingContext context ) 
	: ApplicationException( info, context )
    {}


public:

   String ^GetMsg()
   {
      char buf[100000];
      
	  if (m_eng == NULL) return "";
	  lsGetExceptMsg(m_eng, buf, 100000);
      String ^s = gcnew String(buf);
      return s;
   }

   String ^GetMessage()
   {
      return GetMsg();
   }

   int GetRC()
   {
	  if (m_eng == NULL) return 0;
      return lsGetExceptRC(m_eng);
   }

   int GetExceptType() 
   {
	  if (m_eng == NULL) return 0;
      return lsGetExceptType(m_eng);
   }

   String ^GetReadBuffer()
   {
      char buf[100000];
	  if (m_eng == NULL) return "";
      lsGetExceptReadBuffer(m_eng, buf, 100000);
      String ^s = gcnew String(buf);
      return s;
   }

   int GetReadLineno()
   {
	  if (m_eng == NULL) return 0;
      return lsGetExceptLineno(m_eng);
   }

   String ^GetReadFileName()
   {
      char buf[100000];
	  if (m_eng == NULL) return "";
      lsGetExceptReadFileName(m_eng, buf, 100000);
      String ^s = gcnew String(buf);
      return s;
   }

   String ^GetCallStack()
   {
      char buf[100000];
	  if (m_eng == NULL) return "";
      lsGetExceptCallStack(m_eng, buf, 100000);
      String ^s = gcnew String(buf);
      return s;
   }

};


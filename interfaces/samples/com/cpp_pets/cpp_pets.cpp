// cpp_pets.cpp 
// 
// A simple expert system using the ComLogicServer
//

#include "stdafx.h"

#import  "amzicom.tlb"

using namespace amzicom;

int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR     lpCmdLine,
                     int       nCmdShow)
{
   char  buf[1000] = "Your pet is a  ";
   
   // Initialize COM
   HRESULT hr = CoInitialize(NULL);
   if (FAILED(hr)) exit(1);

   // Get a Logic Server
   IComLogicServerPtr pICLS(__uuidof(ComLogicServer));

   try
   {
      // Initialize and load our XPL file
      pICLS->Init("");
      pICLS->Load("pets.xpl");

      // Assert the sound
      pICLS->AssertaStr("sound(woof)");

      // Call Prolog to get the pet and convert the string
      long term = pICLS->ExecStr("pet(X)");
      BSTR str = pICLS->GetStrArg(term, 1);
      wcstombs(&buf[14], str, 1000);

      // Display it and free the string created by GetStrArg
   	MessageBox(NULL, buf, "Pet", MB_OK);
      SysFreeString (str);

      // Close down
      pICLS->Close();
      pICLS->Release();
   }
   catch (_com_error e) 
   {
      // Get the Logic Server exception message
      BSTR errmsg = pICLS->GetExceptMsg();

      // Convert it, free it and display it
      wcstombs(buf, errmsg, 1000);
      SysFreeString(errmsg);
      MessageBox(NULL, buf, "ERROR", MB_OK);
   }


   CoUninitialize();

	return 0;
}




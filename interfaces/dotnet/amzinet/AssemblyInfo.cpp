#include "CorHdr.h"
#include "stdafx.h"

using namespace System::Reflection;
using namespace System::Runtime::CompilerServices;

//
// General Information about an assembly is controlled through the following 
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
//
[assembly:AssemblyTitleAttribute("Amzi! Logic Server .NET Wrapper")];
[assembly:AssemblyDescriptionAttribute("")];
[assembly:AssemblyConfigurationAttribute("")];
[assembly:AssemblyCompanyAttribute("Amzi! inc.")];
[assembly:AssemblyProductAttribute("Amzi! Prolog + Logic Server")];
[assembly:AssemblyCopyrightAttribute("Copyright (c) 2002-06 Amzi! inc. All Rights Reserved.")];
[assembly:AssemblyTrademarkAttribute("Amzi! is a registered trademark and Logic Server is a trademark of Amzi! inc.")];
[assembly:AssemblyCultureAttribute("")];	

// See CorHdr.h CorAssemblyFlags and AssemblyLoadAttribute
//    afNonSideBySideAppDomain=   0x0010,     // The assembly cannot execute with other versions if
                                            // they are executing in the same application domain.
//    afNonSideBySideProcess  =   0x0020,     // The assembly cannot execute with other versions if
                                            // they are executing in the same process.
//    afNonSideBySideMachine  =   0x0030,     // The assembly cannot execute with other versions if
                                            // they are executing on the same machine.
// Using afNonSideBySideProcess does not work under ASP.NET
//[assembly:AssemblyFlagsAttribute(0x0020)];

//
// Version information for an assembly consists of the following four values:
//
//      Major Version
//      Minor Version 
//      Build Number
//      Revision
//
// You can specify all the value or you can default the Revision and Build Numbers 
// by using the '*' as shown below:

[assembly:AssemblyVersionAttribute("2.0.0.0")];

//
// In order to sign your assembly you must specify a key to use. Refer to the 
// Microsoft .NET Framework documentation for more information on assembly signing.
//
// Use the attributes below to control which key is used for signing. 
//
// Notes: 
//   (*) If no key is specified, the assembly is not signed.
//   (*) KeyName refers to a key that has been installed in the Crypto Service
//       Provider (CSP) on your machine. KeyFile refers to a file which contains
//       a key.
//   (*) If the KeyFile and the KeyName values are both specified, the 
//       following processing occurs:
//       (1) If the KeyName can be found in the CSP, that key is used.
//       (2) If the KeyName does not exist and the KeyFile does exist, the key 
//           in the KeyFile is installed into the CSP and used.
//   (*) In order to create a KeyFile, you can use the sn.exe (Strong Name) utility.
//        When specifying the KeyFile, the location of the KeyFile should be
//        relative to the project directory.
//   (*) Delay Signing is an advanced option - see the Microsoft .NET Framework
//       documentation for more information on this.
//
//[assembly:AssemblyDelaySignAttribute(false)];
//[assembly:AssemblyKeyFileAttribute("amzinet.snk")];
//[assembly:AssemblyKeyNameAttribute("")];


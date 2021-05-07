The .NET interface is built as a Visual Studio project, amzinet.sln.  It can be built in the Visual Studio environment, or from the batch files provided.

It requires access to the various Amzi! Prolog + Logic Server files.  This means the core Amzi! system must be installed, but it is not necessary to have the Eclipse IDE installed.  In particular the build requires:

- The amzi/apls/include directory must be in the list of include files, as it contains the amzi.h header file.
- The amzi/apls/lib directory must be in the list of library directories, as it contains amzi.lib, required to resolove all of the references to the Logic Server API functions.

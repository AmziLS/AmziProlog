# The Projects of Amzi! Prolog + Logic Server
### amzi_projects.md

These are the projects, in the order in which they need to be built for the full system build.  This is the order in the make files.  The first step, though, is creating the directory structure for the release, which is also done in the make files.  It is:

```
  amzi
    apls
      bin   (environment-specific binaries, e.g. .dll, .so)
      lib
      abin    (prolog binaries)
      include
      config
      lsapis  (logic server extensions, e.g. java, .NET)
      samples
      eclipse_plugin
    
```

## amzi-apls-make
The top level make files for each supported environment.  See amzi_build.md.

## amzi-apls-engine
This is Warren Abstract Machine (WAM), Amzi!’s virtual machine engine.  It is a C++ program that’s been ported to many different environments.  It builds amzi.dll on Windows and libamzi.so on Mac and Linux.

## amzi-apls-run
This is a small executable program that reads a compiled (.XPL) Prolog file and calls the dynamic engine library with it.  It allows the running of .XPL files from the command line.  The build creates the executable program arun(.exe).

Note a bit of cleverness.  If arun doesn’t have an argument with a file to load, it assumes there is a .XPL to load of the same name as arun.  Which there won’t be, unless a copy was made of arun with a different name.  So, for example, there is a copy of arun called acmp which runs the compiler, acmp.xpl, and one called alis which runs the listener, alis.xpl.

## amzi-apls-linkrun
This is a C++ program that creates the Prolog linker which links multiple Prolog object modules (.PLMs) into a single executable .XPL file.  It builds the executable program alnk(.exe).

## amzi-apls-osutils
This creates a Logic Server Extension (LSX) that implements various operating system dependent utilities, such as directory services.  These all become extended predicates for, for example, finding the current directory and changing directories.  The build creates aosutils.lsx on all systems.  It’s a DLL on Windows and a SO on Unix.

## amzi-apls-compile
The Amzi! Prolog compiler is a bootstrapped Prolog program.  In the make file is a working version of acmp.xpl, the compiler.  It is used by the above modules to compile and link a new version of the compiler for the platform being built.  It is acmp.xpl in the abin directory. As long as the underlying data structures of Prolog aren’t changed, this is not a problem.  But…

*WARNING — if the underlying data structures of compiled code are changed, then great care is necessary in rebuilding the compiler.  It is important to always keep a couple of good copies of acmp.xpl for the build process, as it is easy to get the WAM and the compiler out of sync with each other.  In fact, the only way to know for sure that such a change has been successful is to rebuild the compiler with itself three times in a row.*

*NOTE - The compiler source code is currently the only part of the system that is not open source and licensed under the M.I.T. license.  The binary version, acmp.xpl, is open and is available in the amzi-apls-make for copying to a new release.*

## amzi-apls-libs
Builds for Prolog libraries of predicates, such as list.plm and date_time.plm.

## amzi-apls-listen
This creates the command line listener, also written in Prolog.  alis.xpl is the Prolog module and alis(.exe) is the runtime copy that runs it.

## amzi-apls-docs
Builds for the full system documentation.  It includes a program that automatically generates an index and table of contents based on mark-ups in the documents.  The output is the contents of the apls/docs directory.

## amzi-apls-samples
This is really a big copy rather than a build.  It copies all the samples into the release directory of samples, amzi/apls/samples.

## amzi-apls-wide (Windows only)
This builds a relatively simple IDE for developing Prolog programs under Windows.  The executable is wide(.exe).

## amzi-interfaces-make
This project contains the top level make files for building all the Logic Server API interfaces for an environment.  Java is supported for all environments, but many of the others are Windows specific.  Some could easily be supported on the Mac and Linux, such as MySQL, TclTk and CGI, but they simply haven’t been implemented.  The outputs are stored in the amzi/apls/lsapis directory.

## amzi-eclipse
These are the files for building a new version of the Eclipse plug-in for Amzi! Prolog + Logic Server, which is the interactive IDE for Prolog, including a full source code debugger.  The make files for a platform copy over the finished plug-in from this project.

## amzi-apls-distribution
These are the binary distribution files for each release for each platform.  This repository should be positioned under the ‘release’ directory for the builds to properly create the distribution files in it.



# Amzi! Prolog

Source code for the entire Amzi! Prolog system.

Amzi! Prolog is made of the following:
  - **Prolog Virtual Machine** -- Warren Abstrace Machine (WAM)
  - **Logic Server** -- a runtime library for compiled Prolog programs that execute on the Prolog-VM (*Prolog analogue to a database server*)
  - **C API and foreign language bindings** -- to embed Logic Server into other languages (*analogue to an interface to a database server*)
    - allows asserting clauses (facts or rules) and querying logic bases
  - **Logic Server Extension** -- .lsx files that implement extended predicates/functions, implemented in other languages
  - **Command line tools** -- to build, run, and "listen to" Prolog programs
  - **Eclipse plugin** -- for comfortable compiling, linking, and *debugging*

See also: [Amzi! Prolog + Logic Server Overview](https://www.amzi.com/AmziPrologLogicServer/white_papers/amzi_overview.php)

Copyright (c) Amzi! inc. 2016, 2021    
Licensed under the MIT Open Source License.

# Contents

The engine/ folder contains Amzi! Prolog's core, made of the Prolog-VM and the Logic Server runtime library.
The Logic Server's primary API is exposed in C found under engine/, but has also various foreign language bindings found under langbindings/, together with usage examples found under langbindings/samples/.

The Prolog compiler, listener, and runner are found under run/. acmp, arun, and alis are the exact same executable, their actual purpose is identified by their filename only: for arun, the engine/Logic Server simply executes the passed .xpl file, for alis it loads alis.xpl (source under listen/), and for acmp it loads acmp.xpl (found under compile/ and the source under https://github.com/AmziLS/AmziPrologCompiler).

linkrun/ contains the source for the Prolog linker. libs/ contains Prolog libraries that will be compiled to .plm files.

OS related extensions to Prolog, for handling files, directories, environment variables, and displaying message boxes are available under osutils/. More extensions for ODBC, MySQL, Tcl/Tk, and sockets can be found in the langbindings/ folder.

Finally, apart from a simple IDE (under wide-IDE/), there is also an Eclipse plugin that provides a more complete experience, under eclipse_plugin/.

# Building and Debugging

For building the whole system, see the make/ subdirectory. There are [detailed building instructions for Windows](https://github.com/AmziLS/AmziProlog/blob/master/Windows%20compilation%20instructions.md).

Individual components can be built using the make files in each subdirectory. It is also possible to load the .sln files in Visual Studio for debugging. Make sure that you have set up the environment variables correctly (see /bin/amzi_vars_winXX.bat).

# Releases

[Releases](https://github.com/AmziLS/AmziProlog/releases) are available as usual on GitHub, on the right side bar. They contain the built runtime library, and command line tools, but also samples, language bindings, and a ready to use Eclipse plugin. It's the easiest way to get started.

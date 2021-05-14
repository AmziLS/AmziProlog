# Amzi! Prolog

Source code for the entire Amzi! Prolog system.

Amzi! Prolog is made of the following:
  - **Prolog Virtual Machine**
  - **Logic Server** -- a runtime library for compiled Prolog programs that execute on the Prolog-VM
  - **C API and foreign language bindings** to embed Logic Server into other languages (similar to SQL)
    - allows asserting clauses (facts or rules) and querying the logic base
  - **Command line tools** to build, run, and "listen to" Prolog programs
  - **Eclipse plugin** -- for comfortable compiling, linking, and *debugging*

Copyright (c) Amzi! inc. 2016, 2021    
Licensed under the MIT Open Source License.

# Contents

The engine/ folder contains Amzi! Prolog's core, made of the Prolog-VM and the Logic Server runtime library.
The Logic Server's primary API is exposed in C found under engine/, but has also various foreign language bindings found under langbindings/, together with usage examples found under langbindings/samples.

The Prolog linker can be found under linkrun/, the listener and some related tools under listen/.

libs/ contains Prolog libraries that will be compiled to .plm files.

Several command line tools are available to "listen to", run, compile and link Prolog programs. Apart from a simple IDE (under wide-IDE/), there is also an Eclipse plugin that provides a more complete experience, under eclipse_plugin/.

# Building and Debugging

For building the whole system, see the make/ subdirectory. There are [detailed building instructions for Windows](https://github.com/AmziLS/AmziProlog/blob/master/Windows%20compilation%20instructions.md).

Individual components can be built using the make files in each subdirectory. It is also possible to load the .sln files in Visual Studio for debugging. Make sure that you have set up the environment variables correctly (see /bin/amzi_vars_winXX.bat).

# Releases

[Releases](https://github.com/AmziLS/AmziProlog/releases) are available as usual on GitHub, on the right side bar. They contain the built runtime library, and command line tools, but also samples, language bindings, and a ready to use Eclipse plugin. It's the easiest way to get started.

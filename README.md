# Amzi! Prolog

Source code for the entire Amzi! Prolog system.

Amzi! Prolog is made of the following:
  - **Prolog Virtual Machine** -- Warren Abstrace Machine (WAM)
  - **Logic Server** -- a runtime library wrapping around the Prolog-VM, for executing, modifying, and interacting with compiled Prolog programs
  - **C API and foreign language bindings** -- the [Logic Server API (LSAPI)](https://www.amzi.com/manuals/amzi/ls/lsapirf.htm) to embed Logic Server into C and other languages, to allow:
    - asserting or retracting clauses (facts or rules) and querying logic bases
    - calling Prolog predicates
    - type specific operations on data (for lists and structures)
    - converting data between the host language and Prolog
    - adding extended predicates, and manipulating their parameters
    - error handling
  - **Logic Server Extensions** -- .lsx files that provide extended predicates/functions, implemented in other languages
  - **Command line tools** -- to interpret (`alis`), compile (`acmp`), link (`alnk`) and execute (`arun`) Prolog programs
    - the Prolog listener alis is an interactive interpreter; the process is aka. [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)
  - **Eclipse IDE plugin** -- for comfortable compiling, linking, and *debugging*

File types:
  - **.pro** -- <u>Prolog source code (program or module)</u>
  - **.plm** -- Compiled Prolog module (Prolog/WAM byte code file -- machine independent)
  - **.xpl** -- Executable Prolog library (a linked collection of .plm files)
  - **.lsx** -- Logic Server extension (a renamed DLL/SO file)

Common terms:
  - Logic base = Prolog program
  - The Logic Server is a service, with an interface similar to a database server, which allows to query and update the logic base (= Prolog program).
  - Loading of a source code file (.pro), then interpreting it, is called "consulting". This is in contrast to compiled files (.plm/.xpl), which are simply said to be "loaded".
    - **Note:** neither loading compiled code, nor consulting source code, will automatically execute the [main/0 predicate](https://www.amzi.com/manuals/amzi/ls/lsprguid.htm#MainEntryPoints) -- use `arun` if you want that.

See also: [Amzi! Prolog Quick Overview](http://www.amzi.com/manuals/amzi/pro/pug_overview.htm) and the [Detailed Amzi! Prolog Overview](https://www.amzi.com/AmziPrologLogicServer/white_papers/amzi_overview.php)

Copyright (c) Amzi! inc. 2016, 2021    
Licensed under the MIT Open Source License.

## Documentation
[User's Guide and Reference](https://www.amzi.com/manuals/amzi/pro/top.htm)    
[Programmer's Guide](https://www.amzi.com/manuals/amzi/ls/lsprguid.htm)    
[Logic Server User's Guide and Reference](https://www.amzi.com/manuals/amzi/ls/lstop.htm)   
    
[Prolog Articles / Whitepapers](https://www.amzi.com/manuals/amzi/articles/doc.html)    
[Adventure in Prolog](https://www.amzi.com/manuals/amzi/aip/advfrtop.htm) -- A tutorial/guided tour through Prolog    
[Building Expert Systems in Prolog](https://www.amzi.com/manuals/amzi/xsip/xsipfrtop.htm) --  An advanced tutorial

### Videos
[Getting Started with Eclipse Amzi! Prolog](https://www.youtube.com/watch?v=EMxLnn2I9yo)    
[Amzi! Prolog Source Code Debugger in Eclipse](https://www.youtube.com/watch?v=fewTmnarfu8)    

## Repository Contents

The `/engine` folder contains Amzi! Prolog's core, made of the Prolog-VM and the Logic Server runtime library.
The Logic Server's primary API is exposed in C found under `/engine`, but has also various foreign language bindings found under `/langbindings`.

The Prolog compiler, listener, and runner are found under `/run`. `acmp`, `arun`, and `alis` are the exact same executable, their actual purpose is identified by their filename only:
- for `arun`, the engine/Logic Server simply executes the passed `.xpl` file
- for `alis` it loads `alis.xpl` (source under `/listen`)
- for `acmp` it loads `acmp.xpl` (found under `/compile` and the source under https://github.com/AmziLS/AmziPrologCompiler)

`/linkrun` contains the source for the Prolog linker. `/libs` contains [Prolog libraries](https://www.amzi.com/manuals/amzi/libs/doc.html) that will be compiled to .plm files.

A core extension to Prolog -- which provides OS related functions for handling files, directories, environment variables, and displaying message boxes -- is available under `/extensions/osutils`. Non-core extensions exist for ODBC, MySQL, Tcl/Tk, and sockets, which can be found in the `/extensions` folder.

Examples for all core Prolog uses, language bindings, and Prolog extensions are available under `/samples`.

Finally, apart from a simple Windows IDE (under `/winIDE`), there is also an Eclipse plugin that provides a more complete experience, found under `/eclipse_plugin`.

## Building and Debugging

For building the whole system, see the `/make` subdirectory. There are [detailed building instructions for Windows](https://github.com/AmziLS/AmziProlog/blob/master/Windows%20compilation%20instructions.md).

Individual components can be built using the make files in each subdirectory. It is also possible to load the .sln files in Visual Studio for debugging. Make sure that you have set up the environment variables correctly (see `/bin/amzi_vars_winXX.bat`).

## Releases

[Releases](https://github.com/AmziLS/AmziProlog/releases) are available as usual on GitHub, on the right side bar. They contain the built runtime library, and command line tools, but also samples, language bindings, Logic Server extensions, and a ready to use Eclipse plugin. It's the easiest way to get started.

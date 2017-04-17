# Porting and/or Extending Amzi! Prolog + Logic Server
### amzi_portability_extensibility.md

Amzi! Prolog + Logic Server is designed to be easily ported to different computing environments, and easily extended to integrate with different tools and services.

At the core of the system is the Amzi! version of the Warren Abstract Machine (WAM), called the engine.  It is a C++ program that has been ported to many different environments with the environment-specific bits fairly well isolated and \#IFDEF’d.

It is not necessary to understand the WAM to maintain Amzi!, but it is interesting. The best resource for learning the WAM is Ait-Kaci’s A Tutorial Reconstruction of Warren’s Abstract Machine.  It is out of print but can be free online for non-commerical use. Commercial users should compensate the author for use of this work that has been the basis of many Prolog vendor’s, including this one, understanding of the WAM.

The original paper, by David Warren, is in the *Original WAM paper.pdf* file.

## Call In
The engine has a full C API that lets outside programs call into the Prolog engine, running programs, making queries, etc..  There are entry points that allow functions such as:

- load an .xpl
- create a Prolog term
- call the term
- extract the variable in the result
- loop through lists or backtracking searches

and many more.  

This C API can then be used to create interfaces to the engine from other languages.  One of the simplest is a header file that implements a C++ interface to the engine.

The Java interface makes use of the Java Native Interface (JNI), to implement a wrapper for Java.

Basically, any language or development tool that supports a C interface (or C++ or Java) can be made to call into and out of Amzi! Prolog using a natural, native syntax.

## Call Out
The API also supports the creating of extended (written in a language other than Prolog) predicates external to the engine.  The project amzi-apls-osutils is an example of this.  It is a C++ program that implements predicates such as current_directory/1 in C++ and makes that predicate available to the Prolog engine.

Extended predicates can be written as parts of applications that are calling Prolog, or can be implemented in stand-alone dynamic libraries for loading and sharing.

## Porting
Porting Amzi! Prolog + Logic Server to a different environment basically means getting the C++ engine to build in the new environment.  The linker and runtime also need to be built, but they are much simpler programs, and go easily once the engine is running on the new platform.

One the engine is running, it can be easily tested by running .xpl files built on other platforms.  This is because Amzi! byte code (WAM code) representation is machine independent.

The rest of the Amzi! system can then be built using the compiler .xpl, acmp.xpl.

Ports to other platforms have been done in literally a matter of hours, all going well.  Other times, getting the right tools in place can require a couple of days.

### Steps for porting Amzi!:

#### amzi-apls-engine

The code is all ifdef'd to allow builds on various platforms. Potential problem areas: The engine uses the C++ standard template library, and makes heavy use of wide character functions, as all characters are Unicode internally.

env.h

This file contains codes for various supported environments, and then the definitions that pertain for that environment. For examples the variable ENVgl3 defines the 32-bit Linux Gnu environment. You'll see an ifdef in env.h for that variable, under which other variables are defined, such as GNU, LINUX, BIG or LITTLE ENDIAN, etc. These are the variables refered to in the bulk of the code.

So the first task is to create a new environment variable for the new platform, and include the definitions for it.

lenv.h lenv.cpp

We've attempted, not totally successfully, to put all of the environment specific code in this module. Usually it has to do with various I/O sorts of things, and platforms that have non-standard or missing wide character support. It's probably not necessary to anything at first, but as you start to build and run into errors, much of the work will be getting the right ifdefs in lenv.*.

One function that might need rewriting is the open used for system files. Various platforms have different ways of finding the directory (such as amzi/apls/bin or amzi/apls/abin) that contain files of interest to the system.

Make sure you don't break the logic of existing ifdefs!

inc.h

This is the master include file. Different platforms might require different includes for the various standard C++ files. This might need some attention.

Once these files are set, then usually the engine should build OK. New makefiles are probably required for the platform.

#### amzi-apls-run

arun is a very simple Logic Server program. It simply loads a .xpl and calls its main/0 predicate. The only confusion in the code is it figures some things out about its name. If its name is not arun(.exe) then it assumes you want to load the .xpl file of the same name. This lets you make stand-alone applications.

Once you've built arun and the engine, you should be able to run the listener, alias.xpl, with arun alis. Did it work?

If not, then the best testing thing is to build hello.xpl from hello.pro on a different platform, bring the hello.xpl file to the new platform, and try arun hello. You can use this method of testing for increasingly complex Prolog programs until you have arun working for alis.xpl and acmp.xpl.

#### amzi-apls-linkrun

The linker is a moderate sized C++ program that takes binary .plm files and combines them into a single .xpl file. It's make file uses the same env and lenv files as the engine, so it should build without much difficulty once the engine has built.

#### amzi-apls-compile

Now its time for the final exam, rebuilding the compiler.

FIRST - rename acmp.xpl to acmp_good.xpl. SECOND - rename acmp.xpl to acmp_good_copy.xpl. Two backups, incase you mess up the first backup by mistake. This is the danger of a bootstrapped system.

Clean the compiler, removing the .plm files, and build it.

Clean the compiler, removing the .plm files, and build it.

Clean the compiler, removing the .plm files, and build it.

That wasn't a typo. You have to do it three times to be sure that you've got everything working. It's quite possible to have the compiler quite happily build the compiler, but the resulting compiler is not quite right so the next build fails. And, don't ask, certain pathological cases can creep into the second one. Only three successive builds of the compiler ensures that you've got most of the system up and running correctly.

#### amzi-apls-listen

Once the compiler is built, the listener debugger is easy.

--------------

At this point the core Amzi! system is up and running. You can run the compiler, linker, listener/debugger at the command line. You can run compiled Prolog code at the command line. You can embed Logic Servers in C++ programs and call Prolog code.

Next are the various add ons. Some common ones:

#### amzi-apls-osutils

This is an LSX of machine dependent predicates for handling directories and files.

#### amzi-interfaces-java

This is the Java wrapper on the Logic Server API (LSAPI). It requires building the C++ program which is the Java Native Interface (JNI) connection between the Java Logic Server classes and the Prolog engine.

#### amzi-apls-wide

This is the Windows MFC IDE. Good luck.


## Changing the system

Treacherous are any changes that affect the format of the byte codes (WAM codes). The compiler generates .plm files, which are read by the linker to create .xpl files, which are read by the loader in the engine for execution. A change in this sequence requires great care, and is not for the feint hearted. Contact Amzi!.

Tricky are additions to the built-in predicates that are implemented in Prolog. These require first adding the new predicate to alib.pro, which contains the built-ins implemented in Prolog, and then modifying the builtins.pro compiler file, which tells the compiler which predicates are built-ins implemented in alib.pro. Note that for many cases, new predicates should probably be implemented using the external Prolog libraries, rather than as part of the core system.

Triple compile test. If you make either of the above changes, it is always best to recompile the compiler with the compiler built from the recompiled compiler. This means rebuilding the compiler from scratch three times, each time using the acmp.xpl file from the previous build. This is the only sure test that a change has been implemented correctly.

Safest changes are to the builtin predicates implemented in C++ in the engine. These either work or don't with no impact on the previous workings of the system. Many of these are probably best implemented using the external LSX libraries of extended predicates, rather than as part of the core system.

Normal changes are anything else. You can modify the listener, the debugger, the engine, the execution, the reader, the I/O, etc. etc. with risk/reward ratios common to all other software development.

Ports to other platforms are relatively easy and safe. Most of the work in a port is simply adding some new \#defines and mucking about in the I/O related parts of the system.

## C++ Issues
The code is all ifdef'd to allow builds on various platforms. Potential problem areas: The engine uses the C++ standard template library, and makes heavy use of wide character functions, as all characters are Unicode internally.

These are some of the key areas in making changes to the engine software:

env.h

This file contains codes for various supported environments, and then the definitions that pertain for that environment. For examples the variable ENVgl3 defines the 32-bit Linux Gnu environment. You'll see an ifdef in env.h for that variable, under which other variables are defined, such as GNU, LINUX, BIG or LITTLE ENDIAN, etc. These are the variables refered to in the bulk of the code.

So the first task is to create a new environment variable for the new platform, and include the definitions for it.

lenv.h lenv.cpp

We've attempted, not totally successfully, to put all of the environment specific code in this module. Usually it has to do with various I/O sorts of things, and platforms that have non-standard or missing wide character support. It's probably not necessary to do anything at first, but as you start to build and run into errors, much of the work will be getting the right ifdefs in lenv.*.

One function that might need rewriting is the open used for system files. Various platforms have different ways of finding the directory (such as amzi/bin or amzi/abin) that contain files of interest to the system.

Make sure you don't break the logic of existing ifdefs!

inc.h

This is the master include file. Different platforms might require different includes for the various standard C++ files. This might need some attention.

Once these files are set, then usually the engine should build OK. New makefiles are probably required for the platform.




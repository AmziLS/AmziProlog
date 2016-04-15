# Building the *Amzi! Prolog + Logic Server* Core and Interfaces
### amzi_build.md

These are the directions for building the **core** set of tools for *Amzi! Prolog*, and the various external *Amzi! Logic Server* **interfaces**.  Note that this does not build the Eclipse-based IDE, which is in a separate project, but does copy the plug-in files from that repository to the release.

The *core* set of tools contains the command line interface for interpreting, compiling, linking and running Prolog modules, as well as the modules necessary for distributing applications.

The *interfaces* contain the modules necessary for developing and delivering applications that integrate Prolog and Java, or .NET, or Delphi, or ODBC, or MySQL or…

The interfaces are of two different types.

**Logic Server APIs (LSAPIs)** which are APIs for calling into and out of Prolog from various languages, such as Java, VB, Delphi.

**Logic Server Extensions (LSXs)** which are dynamic libraries, DLLs under Windows, or .SOs under Unix, but always with the extension .LSX.  These are libraries that implement extended predicates which can be called from Prolog and implement connectivity to other services, such as operating system utilities, ODBC, Sockets, or MySQL.

The build requires this directory structure for the various projects.  The build starts from amzi\apls\amzi-apls-make.

```
amzi
  source
    apls
      amzi-apls-make
      amzi-apls-engine
      amzi-apls-
      . . .
    eclipse
      amzi-eclipse
    interfaces
      amzi-interfaces-make
      amzi-interfaces-cgi
      amzi-interfaces-
      . . .
  release
    amzi-apls-distribution
```

The various makes for the various environments will create directories under amzi/release such as ‘mac’ ‘win64’ for the builds for those environments.

NOTE — the only portion of the system that is not open source is the Amzi! Compiler, which is, itself, a Prolog program.  It translates Prolog source into the byte code (WAM code) format used by the Amzi! runtime engine.  It is, like all compiled Prolog code, a machine-independent binary file, acmp.xpl.  It should not be necessary to change this file, unless extreme changes are being made to the system and it’s base-level representations. If you desire access to the source code for this module, contact Amzi!.

## Using the top level makes

The Amzi! *core* and *interfaces* are built using a hierarchy of make files.  The top level make files are in the project amzi-apls-make.  If everything is set up correctly, you can use them to build the whole system like so:

**On a Mac:**
```
>make -f make_mac64.txt 
```
**On Windows:**
```
>bmake -f make_win64.txt
```
or
```
>bmake -f make_win32.txt
```
(Note that on Windows we use Borland’s old make utility, bmake.exe, which is included in this project.)

**On CentOS64:**
```
>make -f cent64.txt
```
(Note that as of this writing the CentOS 64 build has not been brought up to the release 10.x open source release.  If you want to make it work, modify it and all the sub-make files using the make_mac64.txt files as a template.)

## Sub makes

The top make files called similarly named make files in each of the individual projects that make up the whole Amzi! system.  For example, in amzi-apls-listen is a file called make_mac64.txt that is the make for the listener on the Mac.  In amzi-interfaces-java is the make for the Java interface to Amzi!.

These sub-make files can be called individually for rebuilding just portions of the system, or for testing.

There are two sections of projects.

- The **core** projects.  These are all named amzi-apls-x and correspond to core components such as the listener, the compiler, and the engine.
- The **interface** projects.  These are all named amzi-interface-x and correspond to the various supported external interfaces, such as Java and .NET.  They are platform independent, so, for example, .NET is not supported on the Mac.

## System Requirements

The core Amzi! system is built using two languages.  C++ and Amzi! Prolog.  That is, it’s a boot-strapped system.

It has an architecture similar to Java’s.  There is a virtual machine, or engine, or Warren Abstract Machine (WAM) that runs compiled Prolog byte codes or WAM codes.  That core engine is written in C++ and has been ported to many different environments, with environment specific code pretty well isolated and IFDEF’d.

Much of the rest of the system is written in Amzi! Prolog, using the machine independent byte code version of the Prolog compiler, also written in Prolog, to start the boot strap process.

The various external interfaces use the languages and tools of that interface, like Java, or .NET, or MySQL.

There is an early, relatively simple IDE, for Windows only, built in C++ that is part of the core system.

The Eclipse-based IDE uses the Java interface, and that build is not part of the core system.  That IDE does, however, use all of the core components, such as the compiler, listener, debugger, etc.

### Windows

The builds have been developed and tested on Windows 7, in both 32 and 64 bit modes.  

Visual Studio 2010 was used for the C++ portions of the system.  The Windows make files called the command line version of Visual Studio, but the Visual Studio projects, .SLN files, can be opened, modified, debugged, etc. from the interactive environment.  The project names usually begin with w, such as wengine.sln and wlink.sln.

On Windows these environment variables are used by the make files for the core system:

AIDE_RELEASE=%AMZI_TARGET%\ide
AIDE_SOURCE=c:\Work\amzi\github\amzi\source\eclipse\amzi-eclipse
AMZI_DEV_DIR=%AMZI_TARGET%\apls
AMZI_DIR=%AMZI_TARGET%\apls
AMZI_TARGET=c:\Work\amzi\github\amzi\release\win64
APLS_RELEASE=%AMZI_TARGET%\apls
APLS_SOURCE=c:\Work\amzi\github\amzi\source\apls

Note that AMZI_DIR needs to be put on the PATH, to be able to run Prolog

And these are used in the interface builds:

java_home=c:\Program Files (x86)\Java\jdk1.6.0_18
MYSQL=%MYSQL64%
MYSQL32=c:\Program Files (x86)\MySQL\MySQL Server 4.1
MYSQL64=c:\Program Files\MySQL\MySQL Server 5.5

### Mac

This is the bash profile used for the builds on the Mac.  It’s in the file mac-bash-profile.txt in the amzi-apls-make project.

export AMZI_WORK=/Users/dennis/Work/amzi/github/amzi
export AMZI_BUILD=mac
export AMZI_RELEASE=$AMZI_WORK/release/$AMZI_BUILD
export APLS_RELEASE=$AMZI_RELEASE/apls
export AMZI_DIR=$AMZI_WORK/release/$AMZI_BUILD/apls
export APLS_SOURCE=$AMZI_WORK/source/apls
export AIDE_SOURCE=$AMZI_WORK/source/eclipse/amzi-eclipse
export AIDE_RELEASE=$AMZI_RELEASE/ide
export AMZI_DEV_DIR=$AMZI_WORK/release/mac/apls
export CLASSPATH=./:$CLASSPATH:$AMZI_DIR/lsapis/java
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$AMZI_DIR/lib
export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:$AMZI_DIR/lib
export JAVA_HOME=$(/usr/libexec/java_home)

PATH=$PATH:$HOME/.local/bin:$HOME/bin:$PATH:$AMZI_DIR/bin:$AMZI_DIR/lib
export PATH

The provide C++ compiler on the Mac is used for the engine and other C++ builds.

The current builds were done on OS X Yosemite.

### Linux

There are make files included for CentOS 64, but they haven’t been upgraded to the release 10.x open source project and file organization.

They are similar to the Mac make files and can be modified, using the Mac makes as templates, to build a Linux version.

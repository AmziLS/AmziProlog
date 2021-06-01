# Installing and building the Amzi! IDE Eclipse plug-in

## Install

1. Download a current ZIP release of Amzi Prolog (11.1 or higher), and unpack it anywhere you wish
    * Hint: the unpack destination directory is the *installation directory* and will contain the following folders: `/bin`, `/abin`, `/docs`, `/langbindings`, ...
3. Set the environment variable `AMZI_DIR` to the installation directory of Amzi Prolog
    * Do it manually, or automatically by executing `Configure Amzi Dirs (Release).bat`
4. Download and install any version of Eclipse -- tested with 2020-12 (4.18.0)
5. In Eclipse select `Help|Install New Software...` and enter the following URL in the `Work with:` text box:    
    https://raw.github.com/AmziLS/eclipse_IDE_plugin_update_site/master/
    * Alternatively, you can install from the local `eclipse_plugin` directory under `AMZI_DIR`
5. Check `Amzi! Eclipse Feature` and its sub components
6. Now just click `Next` and follow the instructions

## Building

1. Download and install Eclipse 4.5.1 for plug-in development
2. Rebuild all
3. Open com.amzi.prolog-update_site
4. Delete all the jar files in features, plugins and both artifacts.jar and content.jar
5. Open site.xml in com.amzi.prolog-update_site and ‘Build All’
6. Copy the new files in com.amzi.prolog-update_site to a new location, and it is the new plug-in

## Creating a New Version

Issue - uninstalling the Amzi! Plug-in removes the feature, com.amzi.prolog, but does not remove the 5 plug-in modules.  Nor does it remove the plug-in modules from artifacts.xml, but the feature is removed.  It might be necessary to remove these by hand for now, although they don’t seem to do any harm when a new version is installed.

- open com.amzi.prolog, amzi.product
- on the overview, set the version
- in the contents, set the version by clicking ‘properties’ of com.amzi.prolog
- open com.amzi.prolog-primary_feature
- open feature.xml
- overview tab - set new version
- plug-in tab - set new version
- select all the plugins, press ‘versions’ button and force feature into plugins.  (They should all be the new version now.)

- open com.amzi.prolog-update_site
- features & plugins, delete old .jar files.  (1 feature, 5 plug-ins)
- delete artifacts.jar and contents.jar
- open site.xml
- site map - expand and remove old com.amzi.prolog feature
- add new version of feature, and drag underneath amzi_eclipse_feature
- build all

- copy the new features, plug-ins, artifacts, contents & site.xml files to the distribution directory.

--Dennis


## History

The Eclipse plug-in was developed around 2002 by Amzi! developer Mary Kroening.  It was the first Eclipse IDE for Prolog code that included a colorized editor, cross reference capability, and most important a full four-port debugger that highlights lines of code as Prolog goes through its backtracking search.  The debugger also keeps windows open with the full stack trace and variable bindings.

The debugger works in conjunction with Prolog code that runs in debug mode.  Understand that much of Prolog is written in Prolog, so for example Prolog listeners are often written in Prolog.  In the case of Amzi! the Prolog debugger is written in Prolog.  It communicates via the Amzi! Java interface with Eclipse, providing information about the current line of code, status, call stack, etc.

Mary has since passed away, cancer.  She was the force behind much of the outward appearance of Amzi! Prolog + Logic Server, such as the Eclipse IDE, and a major contributor to the World-wide popularity of the software.

I have re-built the IDE a couple of times, but always find Eclipse RCP to be extremely brittle.  It is basically unchanged from that 2002 version.  The build reflected in the first github version supports building the plug-in, but not the full RCP stand-alone IDE.  I believe this might be best for open source anyway as it appears to work across platforms, running at least on both a Mac and Windows.

I welcome more experienced Eclipse developers to work with the IDE plug-in and bring it up to date with current Eclipse best practices.

## See also

Further relevant documentation can also be found here:    
https://github.com/AmziLS/apls/tree/master/devdocs    
https://github.com/AmziLS/apls/tree/master/docs    

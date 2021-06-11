# Installing and building the Amzi! IDE Eclipse plug-in

## Install

1. [Install Amzi! Prolog](../Setup%20and%20getting%20started.md#installation) (11.1 or higher)
2. Download and install any version of Eclipse
    * Tested successfully with version 2020-12 (4.18.0)
    * **Pitfall:** The bitness of the Eclipse install must match that of the Amzi! Prolog release. For example, a 64 bit Eclipse install requires that `AMZI_DIR` points to a 64 bit Amzi! Prolog release.
3. In Eclipse select `Help|Install New Software...` and enter the following URL in the `Work with:` text box:    
    https://raw.github.com/AmziLS/eclipse_IDE_plugin_update_site/master/
    * Alternatively, you can install from the local `eclipse_plugin` directory under `AMZI_DIR`
4. Then press `Add...` and confirm with `Add`
    * Optionally give the `Location` (= URL) a `Name`
5. Check `Amzi! Eclipse Feature` and all its sub components
6. Now just press `Next` and follow the instructions
    * The installation will proceed in the background (progress shown in the status bar)
7. Open the Prolog perspective
    * After the installation is completed and Eclipse was restarted, go in the menu "Window|Perspective|Open Perspective|Other..." and select "Prolog", then confirm.

### Introductory videos and articles

When the installation is complete, have a look at the following videos:
  * [Getting Started with Eclipse Amzi! Prolog](https://www.youtube.com/watch?v=EMxLnn2I9yo)    
  * [Amzi! Prolog Source Code Debugger in Eclipse](https://www.youtube.com/watch?v=fewTmnarfu8)    

Articles:
  * [Eclipse IDE Quick Start](http://www.amzi.com/manuals/amzi/pro/pug_ide_quickstart.htm)
  * [Eclipse IDE](http://www.amzi.com/manuals/amzi/pro/pug_ide.htm)

## Building

See [BUILD-README.md](workspace/com.amzi.prolog-update_site/BUILD-README.md) for rebuilding all the plugin projects and the update site.

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


## History (Comments by Dennis)

The Eclipse plug-in was developed around 2002 by Amzi! developer Mary Kroening.  It was the first Eclipse IDE for Prolog code that included a colorized editor, cross reference capability, and most important a full four-port debugger that highlights lines of code as Prolog goes through its backtracking search.  The debugger also keeps windows open with the full stack trace and variable bindings.

The debugger works in conjunction with Prolog code that runs in debug mode.  Understand that much of Prolog is written in Prolog, so for example Prolog listeners are often written in Prolog.  In the case of Amzi! the Prolog debugger is written in Prolog.  It communicates via the Amzi! Java interface with Eclipse, providing information about the current line of code, status, call stack, etc.

Mary has since passed away, cancer.  She was the force behind much of the outward appearance of Amzi! Prolog + Logic Server, such as the Eclipse IDE, and a major contributor to the World-wide popularity of the software.

## See also

Further relevant documentation can also be found here:    
https://github.com/AmziLS/apls/tree/master/devdocs    
https://github.com/AmziLS/apls/tree/master/docs    

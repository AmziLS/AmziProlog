# Building instructions for Windows

0. Install prerequisites
    - Install Visual Studio 2017 (with C++ support) or later
    - For a full build (including langbindings and extensions):
      - Install Tcl 8.6 into `C:\Tcl`
      - Install a Java Development Kit (any version should do)
      - Install MySQL Server (and ensure the development components/header files are selected as well)

1. Adapt `amzi_vars_win32.bat` and `amzi_vars_win64.bat` to match your system
    - Adapt the call of vcvarsXX.bat to match the Visual Studio installation directory
    - Ensure `%MYSQL%` is set to the desired MySQL installation directory containing the `/include` sub dir
    - Adapt `%JAVA_HOME%` to point to desired JDK home directory (just above `/bin` sub dir)

2. Open `Amzi Prolog 32 Bit Environment (Source).lnk` or `Amzi Prolog 64 Bit Environment (Source).lnk`

3. Enter the following commands:
    * for Win32:    
    ````
    cd make
    gmake -f make_win32.txt
    ````
    * for Win64:    
    ````
    cd make
    gmake -f make_win64.txt
    ````
4. Build the Eclipse IDE plugin:
    * Follow the [instructions](https://github.com/AmziLS/AmziProlog/blob/master/eclipse_plugin/workspace/com.amzi.prolog-update_site/BUILD-README.md) given in `/eclipse_plugin/workspace/com.amzi.prolog-update_site/BUILD-README.md`

5. The build result will be available in `/release`

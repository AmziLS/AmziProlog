# Windows compilation instructions

0. Install prerequisites
    - install Visual Studio 2017 or later
    - for a full build (including langbindings):
      - install Tcl 8.6 into C:\Tcl
      - install a Java Development Kit (any version should do)
      - install MySQL Server (and ensure the development components/header files are selected as well)

1. Adapt amzi_vars_win32.bat and amzi_vars_win64.bat to match your system
    - adapt the call of vcvarsXX.bat to match the Visual Studio installation directory
    - ensure %MYSQL% is set to the desired MySQL installation directory containing the \include sub dir
    - adapt %JAVA_HOME% to point to desired JDK home directory (just above \bin sub dir)

2. Open "Amzi Prolog 32 Bit Environment" or "Amzi Prolog 64 Bit Environment"

3. Enter the following commands:
    * for Win32:    
    ````
    cd make
    nmake -f make_win32.txt
    ````
    * for Win64:    
    ````
    cd make  
    nmake -f make_win64.txt
    ````

4. The build result will be available in \release

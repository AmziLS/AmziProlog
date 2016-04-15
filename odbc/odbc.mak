# Microsoft Developer Studio Generated NMAKE File, Based on odbc.dsp
!IF "$(CFG)" == ""
CFG=odbc - Win32 Debug
!MESSAGE No configuration specified. Defaulting to odbc - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "odbc - Win32 Release" && "$(CFG)" != "odbc - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "odbc.mak" CFG="odbc - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "odbc - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "odbc - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "odbc - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release

ALL : ".\$(AMZI_DEV_DIR)\bin\aodbc.lsx"


CLEAN :
	-@erase "$(INTDIR)\aodbc.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\aodbc.exp"
	-@erase "$(OUTDIR)\aodbc.lib"
	-@erase ".\$(AMZI_DEV_DIR)\bin\aodbc.lsx"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "$(AMZI_DEV_DIR)\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "ODBC_EXPORTS" /Fp"$(INTDIR)\odbc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\odbc.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\aodbc.pdb" /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\aodbc.lsx" /implib:"$(OUTDIR)\aodbc.lib" 
LINK32_OBJS= \
	"$(INTDIR)\aodbc.obj" \
	"..\..\..\lib\amzi.lib"

".\$(AMZI_DEV_DIR)\bin\aodbc.lsx" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "odbc - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug

ALL : ".\$(AMZI_DEV_DIR)\bin\aodbc.lsx"


CLEAN :
	-@erase "$(INTDIR)\aodbc.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\aodbc.exp"
	-@erase "$(OUTDIR)\aodbc.lib"
	-@erase "$(OUTDIR)\aodbc.pdb"
	-@erase ".\$(AMZI_DEV_DIR)\bin\aodbc.ilk"
	-@erase ".\$(AMZI_DEV_DIR)\bin\aodbc.lsx"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /I "$(AMZI_DEV_DIR)\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "ODBC_EXPORTS" /Fp"$(INTDIR)\odbc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\odbc.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\aodbc.pdb" /debug /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\aodbc.lsx" /implib:"$(OUTDIR)\aodbc.lib" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\aodbc.obj" \
	"..\..\..\lib\amzi.lib"

".\$(AMZI_DEV_DIR)\bin\aodbc.lsx" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("odbc.dep")
!INCLUDE "odbc.dep"
!ELSE 
!MESSAGE Warning: cannot find "odbc.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "odbc - Win32 Release" || "$(CFG)" == "odbc - Win32 Debug"
SOURCE=.\aodbc.c

"$(INTDIR)\aodbc.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 


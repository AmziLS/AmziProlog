# Microsoft Developer Studio Generated NMAKE File, Based on wosutils.dsp
!IF "$(CFG)" == ""
CFG=wosutils - Win32 Debug
!MESSAGE No configuration specified. Defaulting to wosutils - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "wosutils - Win32 Release" && "$(CFG)" != "wosutils - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wosutils.mak" CFG="wosutils - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wosutils - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "wosutils - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "wosutils - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release

ALL : ".\$(AMZI_DEV_DIR)\bin\aosutils.lsx"


CLEAN :
	-@erase "$(INTDIR)\osutils.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\aosutils.exp"
	-@erase "$(OUTDIR)\aosutils.lib"
	-@erase ".\$(AMZI_DEV_DIR)\bin\aosutils.lsx"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "$(AMZI_DEV_DIR)\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "_UNICODE" /Fp"$(INTDIR)\wosutils.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wosutils.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\aosutils.pdb" /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\aosutils.lsx" /implib:"$(OUTDIR)\aosutils.lib" 
LINK32_OBJS= \
	"$(INTDIR)\osutils.obj"

".\$(AMZI_DEV_DIR)\bin\aosutils.lsx" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wosutils - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug

ALL : ".\$(AMZI_DEV_DIR)\bin\aosutils.lsx"


CLEAN :
	-@erase "$(INTDIR)\osutils.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\aosutils.exp"
	-@erase "$(OUTDIR)\aosutils.lib"
	-@erase "$(OUTDIR)\aosutils.pdb"
	-@erase ".\$(AMZI_DEV_DIR)\bin\aosutils.ilk"
	-@erase ".\$(AMZI_DEV_DIR)\bin\aosutils.lsx"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /I "$(AMZI_DEV_DIR)\include" /D "_DEBUG" /D "_UNICODE" /D "WIN32" /D "_WINDOWS" /Fp"$(INTDIR)\wosutils.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wosutils.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib $(AMZI_DEV_DIR)\lib\amzi.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\aosutils.pdb" /debug /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\aosutils.lsx" /implib:"$(OUTDIR)\aosutils.lib" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\osutils.obj"

".\$(AMZI_DEV_DIR)\bin\aosutils.lsx" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("wosutils.dep")
!INCLUDE "wosutils.dep"
!ELSE 
!MESSAGE Warning: cannot find "wosutils.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "wosutils - Win32 Release" || "$(CFG)" == "wosutils - Win32 Debug"
SOURCE=.\osutils.cpp

"$(INTDIR)\osutils.obj" : $(SOURCE) "$(INTDIR)"



!ENDIF 


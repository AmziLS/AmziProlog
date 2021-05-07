# Microsoft Developer Studio Generated NMAKE File, Based on wsock.dsp
!IF "$(CFG)" == ""
CFG=wsock - Win32 Debug
!MESSAGE No configuration specified. Defaulting to wsock - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "wsock - Win32 Release" && "$(CFG)" != "wsock - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wsock.mak" CFG="wsock - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wsock - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "wsock - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "wsock - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release

ALL : ".\$(AMZI_DEV_DIR)\bin\asock.lsx"


CLEAN :
	-@erase "$(INTDIR)\sockets.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\asock.exp"
	-@erase "$(OUTDIR)\asock.lib"
	-@erase ".\$(AMZI_DEV_DIR)\bin\asock.lsx"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "$(AMZI_DEV_DIR)\include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WSOCK_EXPORTS" /Fp"$(INTDIR)\wsock.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wsock.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\asock.pdb" /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\asock.lsx" /implib:"$(OUTDIR)\asock.lib" 
LINK32_OBJS= \
	"$(INTDIR)\sockets.obj" \
	"..\..\..\lib\amzi.lib"

".\$(AMZI_DEV_DIR)\bin\asock.lsx" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wsock - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : ".\$(AMZI_DEV_DIR)\bin\asock.lsx" "$(OUTDIR)\wsock.bsc"


CLEAN :
	-@erase "$(INTDIR)\sockets.obj"
	-@erase "$(INTDIR)\sockets.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\asock.exp"
	-@erase "$(OUTDIR)\asock.lib"
	-@erase "$(OUTDIR)\asock.pdb"
	-@erase "$(OUTDIR)\wsock.bsc"
	-@erase ".\$(AMZI_DEV_DIR)\bin\asock.ilk"
	-@erase ".\$(AMZI_DEV_DIR)\bin\asock.lsx"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /I "$(AMZI_DEV_DIR)\include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "WSOCK_EXPORTS" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\wsock.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wsock.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\sockets.sbr"

"$(OUTDIR)\wsock.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\asock.pdb" /debug /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\asock.lsx" /implib:"$(OUTDIR)\asock.lib" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\sockets.obj" \
	"..\..\..\lib\amzi.lib"

".\$(AMZI_DEV_DIR)\bin\asock.lsx" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE="$(InputPath)"
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

$(DS_POSTBUILD_DEP) : ".\$(AMZI_DEV_DIR)\bin\asock.lsx" "$(OUTDIR)\wsock.bsc"
   del d:\amzi\dev\a6\bin\asock.ilk
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("wsock.dep")
!INCLUDE "wsock.dep"
!ELSE 
!MESSAGE Warning: cannot find "wsock.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "wsock - Win32 Release" || "$(CFG)" == "wsock - Win32 Debug"
SOURCE=.\sockets.c

!IF  "$(CFG)" == "wsock - Win32 Release"


"$(INTDIR)\sockets.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wsock - Win32 Debug"


"$(INTDIR)\sockets.obj"	"$(INTDIR)\sockets.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 


!ENDIF 


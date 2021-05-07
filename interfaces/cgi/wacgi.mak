# Microsoft Developer Studio Generated NMAKE File, Based on wacgi.dsp
!IF "$(CFG)" == ""
CFG=wacgi - Win32 Debug
!MESSAGE No configuration specified. Defaulting to wacgi - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "wacgi - Win32 Release" && "$(CFG)" != "wacgi - Win32 Debug" && "$(CFG)" != "wacgi - Win32 Win32 Test Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wacgi.mak" CFG="wacgi - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wacgi - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "wacgi - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "wacgi - Win32 Win32 Test Release" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "wacgi - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release

ALL : ".\$(AMZI_DEV_DIR)\lsapis\cgi\acgi.exe"


CLEAN :
	-@erase "$(INTDIR)\amzicgi.obj"
	-@erase "$(INTDIR)\amzisub.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase ".\$(AMZI_DEV_DIR)\lsapis\cgi\acgi.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "$(AMZI_DEV_DIR)\include" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\wacgi.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wacgi.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ws2_32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\acgi.pdb" /machine:I386 /out:"$(AMZI_DEV_DIR)\lsapis\cgi\acgi.exe" 
LINK32_OBJS= \
	"$(INTDIR)\amzicgi.obj" \
	"$(INTDIR)\amzisub.obj"

".\$(AMZI_DEV_DIR)\lsapis\cgi\acgi.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wacgi - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : ".\$(AMZI_DEV_DIR)\lsapis\cgi\acgi.exe" "$(OUTDIR)\wacgi.bsc"


CLEAN :
	-@erase "$(INTDIR)\amzicgi.obj"
	-@erase "$(INTDIR)\amzicgi.sbr"
	-@erase "$(INTDIR)\amzisub.obj"
	-@erase "$(INTDIR)\amzisub.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\acgi.pdb"
	-@erase "$(OUTDIR)\wacgi.bsc"
	-@erase ".\$(AMZI_DEV_DIR)\lsapis\cgi\acgi.exe"
	-@erase ".\$(AMZI_DEV_DIR)\lsapis\cgi\acgi.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MLd /W3 /Gm /GX /ZI /Od /I "$(AMZI_DEV_DIR)\include" /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\wacgi.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wacgi.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\amzicgi.sbr" \
	"$(INTDIR)\amzisub.sbr"

"$(OUTDIR)\wacgi.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ws2_32.lib /nologo /subsystem:console /incremental:yes /pdb:"$(OUTDIR)\acgi.pdb" /debug /machine:I386 /out:"$(AMZI_DEV_DIR)\lsapis\cgi\acgi.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\amzicgi.obj" \
	"$(INTDIR)\amzisub.obj"

".\$(AMZI_DEV_DIR)\lsapis\cgi\acgi.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wacgi - Win32 Win32 Test Release"

OUTDIR=.\wacgi___Win32_Win32_Test_Release
INTDIR=.\wacgi___Win32_Win32_Test_Release

ALL : ".\$(AMZI_DEV_DIR)\lsapis\cgi\testcgi.exe"


CLEAN :
	-@erase "$(INTDIR)\amzicgi.obj"
	-@erase "$(INTDIR)\amzisub.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase ".\$(AMZI_DEV_DIR)\lsapis\cgi\testcgi.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /ML /W3 /GX /O2 /I "$(AMZI_DEV_DIR)\include" /D "TEST" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Fp"$(INTDIR)\wacgi.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wacgi.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib ws2_32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\testcgi.pdb" /machine:I386 /out:"$(AMZI_DEV_DIR)\lsapis\cgi\testcgi.exe" 
LINK32_OBJS= \
	"$(INTDIR)\amzicgi.obj" \
	"$(INTDIR)\amzisub.obj"

".\$(AMZI_DEV_DIR)\lsapis\cgi\testcgi.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("wacgi.dep")
!INCLUDE "wacgi.dep"
!ELSE 
!MESSAGE Warning: cannot find "wacgi.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "wacgi - Win32 Release" || "$(CFG)" == "wacgi - Win32 Debug" || "$(CFG)" == "wacgi - Win32 Win32 Test Release"
SOURCE=.\amzicgi.c

!IF  "$(CFG)" == "wacgi - Win32 Release"


"$(INTDIR)\amzicgi.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wacgi - Win32 Debug"


"$(INTDIR)\amzicgi.obj"	"$(INTDIR)\amzicgi.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wacgi - Win32 Win32 Test Release"


"$(INTDIR)\amzicgi.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\amzisub.c

!IF  "$(CFG)" == "wacgi - Win32 Release"


"$(INTDIR)\amzisub.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wacgi - Win32 Debug"


"$(INTDIR)\amzisub.obj"	"$(INTDIR)\amzisub.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wacgi - Win32 Win32 Test Release"


"$(INTDIR)\amzisub.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 


!ENDIF 


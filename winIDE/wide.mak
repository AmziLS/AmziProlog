# Microsoft Developer Studio Generated NMAKE File, Based on wide.dsp
!IF "$(CFG)" == ""
CFG=wide - Win32 DebugA
!MESSAGE No configuration specified. Defaulting to wide - Win32 DebugA.
!ENDIF 

!IF "$(CFG)" != "wide - Win32 ReleaseW" && "$(CFG)" != "wide - Win32 ReleaseA" && "$(CFG)" != "wide - Win32 DebugW" && "$(CFG)" != "wide - Win32 DebugA"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wide.mak" CFG="wide - Win32 DebugA"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wide - Win32 ReleaseW" (based on "Win32 (x86) Application")
!MESSAGE "wide - Win32 ReleaseA" (based on "Win32 (x86) Application")
!MESSAGE "wide - Win32 DebugW" (based on "Win32 (x86) Application")
!MESSAGE "wide - Win32 DebugA" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "wide - Win32 ReleaseW"

OUTDIR=.\ReleaseW
INTDIR=.\ReleaseW
# Begin Custom Macros
OutDir=.\ReleaseW
# End Custom Macros

ALL : ".\$(AMZI_DEV_DIR)\bin\wideW.exe" "$(OUTDIR)\wide.bsc"


CLEAN :
	-@erase "$(INTDIR)\amziexcept.obj"
	-@erase "$(INTDIR)\amziexcept.sbr"
	-@erase "$(INTDIR)\browbrow.obj"
	-@erase "$(INTDIR)\browbrow.sbr"
	-@erase "$(INTDIR)\compile.obj"
	-@erase "$(INTDIR)\compile.sbr"
	-@erase "$(INTDIR)\conview.obj"
	-@erase "$(INTDIR)\conview.sbr"
	-@erase "$(INTDIR)\cpwide.obj"
	-@erase "$(INTDIR)\cpwide.res"
	-@erase "$(INTDIR)\cpwide.sbr"
	-@erase "$(INTDIR)\cpwin.obj"
	-@erase "$(INTDIR)\cpwin.sbr"
	-@erase "$(INTDIR)\debug.obj"
	-@erase "$(INTDIR)\debug.sbr"
	-@erase "$(INTDIR)\editdoc.obj"
	-@erase "$(INTDIR)\editdoc.sbr"
	-@erase "$(INTDIR)\link.obj"
	-@erase "$(INTDIR)\link.sbr"
	-@erase "$(INTDIR)\listen.obj"
	-@erase "$(INTDIR)\listen.sbr"
	-@erase "$(INTDIR)\mainfrm.obj"
	-@erase "$(INTDIR)\mainfrm.sbr"
	-@erase "$(INTDIR)\peditvw.obj"
	-@erase "$(INTDIR)\peditvw.sbr"
	-@erase "$(INTDIR)\prodoc.obj"
	-@erase "$(INTDIR)\prodoc.sbr"
	-@erase "$(INTDIR)\project.obj"
	-@erase "$(INTDIR)\project.sbr"
	-@erase "$(INTDIR)\proprog.obj"
	-@erase "$(INTDIR)\proprog.sbr"
	-@erase "$(INTDIR)\spypoint.obj"
	-@erase "$(INTDIR)\spypoint.sbr"
	-@erase "$(INTDIR)\stdafx.obj"
	-@erase "$(INTDIR)\stdafx.sbr"
	-@erase "$(INTDIR)\unlock.obj"
	-@erase "$(INTDIR)\unlock.sbr"
	-@erase "$(INTDIR)\utils.obj"
	-@erase "$(INTDIR)\utils.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\wide.bsc"
	-@erase ".\$(AMZI_DEV_DIR)\bin\wideW.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "$(AMZI_DEV_DIR)\include" /D "NDEBUG" /D "_UNICODE" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\wide.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\cpwide.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wide.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\amziexcept.sbr" \
	"$(INTDIR)\browbrow.sbr" \
	"$(INTDIR)\compile.sbr" \
	"$(INTDIR)\conview.sbr" \
	"$(INTDIR)\cpwide.sbr" \
	"$(INTDIR)\cpwin.sbr" \
	"$(INTDIR)\debug.sbr" \
	"$(INTDIR)\editdoc.sbr" \
	"$(INTDIR)\link.sbr" \
	"$(INTDIR)\listen.sbr" \
	"$(INTDIR)\mainfrm.sbr" \
	"$(INTDIR)\peditvw.sbr" \
	"$(INTDIR)\prodoc.sbr" \
	"$(INTDIR)\project.sbr" \
	"$(INTDIR)\proprog.sbr" \
	"$(INTDIR)\spypoint.sbr" \
	"$(INTDIR)\stdafx.sbr" \
	"$(INTDIR)\unlock.sbr" \
	"$(INTDIR)\utils.sbr"

"$(OUTDIR)\wide.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib /nologo /entry:"wWinMainCRTStartup" /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\wideW.pdb" /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\wideW.exe" 
LINK32_OBJS= \
	"$(INTDIR)\amziexcept.obj" \
	"$(INTDIR)\browbrow.obj" \
	"$(INTDIR)\compile.obj" \
	"$(INTDIR)\conview.obj" \
	"$(INTDIR)\cpwide.obj" \
	"$(INTDIR)\cpwin.obj" \
	"$(INTDIR)\debug.obj" \
	"$(INTDIR)\editdoc.obj" \
	"$(INTDIR)\link.obj" \
	"$(INTDIR)\listen.obj" \
	"$(INTDIR)\mainfrm.obj" \
	"$(INTDIR)\peditvw.obj" \
	"$(INTDIR)\prodoc.obj" \
	"$(INTDIR)\project.obj" \
	"$(INTDIR)\proprog.obj" \
	"$(INTDIR)\spypoint.obj" \
	"$(INTDIR)\stdafx.obj" \
	"$(INTDIR)\unlock.obj" \
	"$(INTDIR)\utils.obj" \
	"$(INTDIR)\cpwide.res"

".\$(AMZI_DEV_DIR)\bin\wideW.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"

OUTDIR=.\ReleaseA
INTDIR=.\ReleaseA

ALL : ".\$(AMZI_DEV_DIR)\bin\wideA.exe"


CLEAN :
	-@erase "$(INTDIR)\amziexcept.obj"
	-@erase "$(INTDIR)\browbrow.obj"
	-@erase "$(INTDIR)\compile.obj"
	-@erase "$(INTDIR)\conview.obj"
	-@erase "$(INTDIR)\cpwide.obj"
	-@erase "$(INTDIR)\cpwide.res"
	-@erase "$(INTDIR)\cpwin.obj"
	-@erase "$(INTDIR)\debug.obj"
	-@erase "$(INTDIR)\editdoc.obj"
	-@erase "$(INTDIR)\link.obj"
	-@erase "$(INTDIR)\listen.obj"
	-@erase "$(INTDIR)\mainfrm.obj"
	-@erase "$(INTDIR)\peditvw.obj"
	-@erase "$(INTDIR)\prodoc.obj"
	-@erase "$(INTDIR)\project.obj"
	-@erase "$(INTDIR)\proprog.obj"
	-@erase "$(INTDIR)\spypoint.obj"
	-@erase "$(INTDIR)\stdafx.obj"
	-@erase "$(INTDIR)\unlock.obj"
	-@erase "$(INTDIR)\utils.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase ".\$(AMZI_DEV_DIR)\bin\wideA.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "$(AMZI_DEV_DIR)\include" /D "NDEBUG" /D "_MBCS" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /Fp"$(INTDIR)\wide.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\cpwide.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wide.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\wideA.pdb" /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\wideA.exe" 
LINK32_OBJS= \
	"$(INTDIR)\amziexcept.obj" \
	"$(INTDIR)\browbrow.obj" \
	"$(INTDIR)\compile.obj" \
	"$(INTDIR)\conview.obj" \
	"$(INTDIR)\cpwide.obj" \
	"$(INTDIR)\cpwin.obj" \
	"$(INTDIR)\debug.obj" \
	"$(INTDIR)\editdoc.obj" \
	"$(INTDIR)\link.obj" \
	"$(INTDIR)\listen.obj" \
	"$(INTDIR)\mainfrm.obj" \
	"$(INTDIR)\peditvw.obj" \
	"$(INTDIR)\prodoc.obj" \
	"$(INTDIR)\project.obj" \
	"$(INTDIR)\proprog.obj" \
	"$(INTDIR)\spypoint.obj" \
	"$(INTDIR)\stdafx.obj" \
	"$(INTDIR)\unlock.obj" \
	"$(INTDIR)\utils.obj" \
	"$(INTDIR)\cpwide.res"

".\$(AMZI_DEV_DIR)\bin\wideA.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"

OUTDIR=.\DebugW
INTDIR=.\DebugW
# Begin Custom Macros
OutDir=.\DebugW
# End Custom Macros

ALL : ".\$(AMZI_DEV_DIR)\bin\wideW.exe" "$(OUTDIR)\wide.bsc"


CLEAN :
	-@erase "$(INTDIR)\amziexcept.obj"
	-@erase "$(INTDIR)\amziexcept.sbr"
	-@erase "$(INTDIR)\browbrow.obj"
	-@erase "$(INTDIR)\browbrow.sbr"
	-@erase "$(INTDIR)\compile.obj"
	-@erase "$(INTDIR)\compile.sbr"
	-@erase "$(INTDIR)\conview.obj"
	-@erase "$(INTDIR)\conview.sbr"
	-@erase "$(INTDIR)\cpwide.obj"
	-@erase "$(INTDIR)\cpwide.res"
	-@erase "$(INTDIR)\cpwide.sbr"
	-@erase "$(INTDIR)\cpwin.obj"
	-@erase "$(INTDIR)\cpwin.sbr"
	-@erase "$(INTDIR)\debug.obj"
	-@erase "$(INTDIR)\debug.sbr"
	-@erase "$(INTDIR)\editdoc.obj"
	-@erase "$(INTDIR)\editdoc.sbr"
	-@erase "$(INTDIR)\link.obj"
	-@erase "$(INTDIR)\link.sbr"
	-@erase "$(INTDIR)\listen.obj"
	-@erase "$(INTDIR)\listen.sbr"
	-@erase "$(INTDIR)\mainfrm.obj"
	-@erase "$(INTDIR)\mainfrm.sbr"
	-@erase "$(INTDIR)\peditvw.obj"
	-@erase "$(INTDIR)\peditvw.sbr"
	-@erase "$(INTDIR)\prodoc.obj"
	-@erase "$(INTDIR)\prodoc.sbr"
	-@erase "$(INTDIR)\project.obj"
	-@erase "$(INTDIR)\project.sbr"
	-@erase "$(INTDIR)\proprog.obj"
	-@erase "$(INTDIR)\proprog.sbr"
	-@erase "$(INTDIR)\spypoint.obj"
	-@erase "$(INTDIR)\spypoint.sbr"
	-@erase "$(INTDIR)\stdafx.obj"
	-@erase "$(INTDIR)\stdafx.sbr"
	-@erase "$(INTDIR)\unlock.obj"
	-@erase "$(INTDIR)\unlock.sbr"
	-@erase "$(INTDIR)\utils.obj"
	-@erase "$(INTDIR)\utils.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\wide.bsc"
	-@erase "$(OUTDIR)\wideW.pdb"
	-@erase ".\$(AMZI_DEV_DIR)\bin\wideW.exe"
	-@erase ".\$(AMZI_DEV_DIR)\bin\wideW.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /I "$(AMZI_DEV_DIR)\include" /D "_DEBUG" /D "_UNICODE" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\wide.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\cpwide.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wide.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\amziexcept.sbr" \
	"$(INTDIR)\browbrow.sbr" \
	"$(INTDIR)\compile.sbr" \
	"$(INTDIR)\conview.sbr" \
	"$(INTDIR)\cpwide.sbr" \
	"$(INTDIR)\cpwin.sbr" \
	"$(INTDIR)\debug.sbr" \
	"$(INTDIR)\editdoc.sbr" \
	"$(INTDIR)\link.sbr" \
	"$(INTDIR)\listen.sbr" \
	"$(INTDIR)\mainfrm.sbr" \
	"$(INTDIR)\peditvw.sbr" \
	"$(INTDIR)\prodoc.sbr" \
	"$(INTDIR)\project.sbr" \
	"$(INTDIR)\proprog.sbr" \
	"$(INTDIR)\spypoint.sbr" \
	"$(INTDIR)\stdafx.sbr" \
	"$(INTDIR)\unlock.sbr" \
	"$(INTDIR)\utils.sbr"

"$(OUTDIR)\wide.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib /nologo /entry:"wWinMainCRTStartup" /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\wideW.pdb" /debug /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\wideW.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\amziexcept.obj" \
	"$(INTDIR)\browbrow.obj" \
	"$(INTDIR)\compile.obj" \
	"$(INTDIR)\conview.obj" \
	"$(INTDIR)\cpwide.obj" \
	"$(INTDIR)\cpwin.obj" \
	"$(INTDIR)\debug.obj" \
	"$(INTDIR)\editdoc.obj" \
	"$(INTDIR)\link.obj" \
	"$(INTDIR)\listen.obj" \
	"$(INTDIR)\mainfrm.obj" \
	"$(INTDIR)\peditvw.obj" \
	"$(INTDIR)\prodoc.obj" \
	"$(INTDIR)\project.obj" \
	"$(INTDIR)\proprog.obj" \
	"$(INTDIR)\spypoint.obj" \
	"$(INTDIR)\stdafx.obj" \
	"$(INTDIR)\unlock.obj" \
	"$(INTDIR)\utils.obj" \
	"$(INTDIR)\cpwide.res"

".\$(AMZI_DEV_DIR)\bin\wideW.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"

OUTDIR=.\DebugA
INTDIR=.\DebugA
# Begin Custom Macros
OutDir=.\DebugA
# End Custom Macros

ALL : ".\$(AMZI_DEV_DIR)\bin\wideA.exe" "$(OUTDIR)\wide.bsc"


CLEAN :
	-@erase "$(INTDIR)\amziexcept.obj"
	-@erase "$(INTDIR)\amziexcept.sbr"
	-@erase "$(INTDIR)\browbrow.obj"
	-@erase "$(INTDIR)\browbrow.sbr"
	-@erase "$(INTDIR)\compile.obj"
	-@erase "$(INTDIR)\compile.sbr"
	-@erase "$(INTDIR)\conview.obj"
	-@erase "$(INTDIR)\conview.sbr"
	-@erase "$(INTDIR)\cpwide.obj"
	-@erase "$(INTDIR)\cpwide.res"
	-@erase "$(INTDIR)\cpwide.sbr"
	-@erase "$(INTDIR)\cpwin.obj"
	-@erase "$(INTDIR)\cpwin.sbr"
	-@erase "$(INTDIR)\debug.obj"
	-@erase "$(INTDIR)\debug.sbr"
	-@erase "$(INTDIR)\editdoc.obj"
	-@erase "$(INTDIR)\editdoc.sbr"
	-@erase "$(INTDIR)\link.obj"
	-@erase "$(INTDIR)\link.sbr"
	-@erase "$(INTDIR)\listen.obj"
	-@erase "$(INTDIR)\listen.sbr"
	-@erase "$(INTDIR)\mainfrm.obj"
	-@erase "$(INTDIR)\mainfrm.sbr"
	-@erase "$(INTDIR)\peditvw.obj"
	-@erase "$(INTDIR)\peditvw.sbr"
	-@erase "$(INTDIR)\prodoc.obj"
	-@erase "$(INTDIR)\prodoc.sbr"
	-@erase "$(INTDIR)\project.obj"
	-@erase "$(INTDIR)\project.sbr"
	-@erase "$(INTDIR)\proprog.obj"
	-@erase "$(INTDIR)\proprog.sbr"
	-@erase "$(INTDIR)\spypoint.obj"
	-@erase "$(INTDIR)\spypoint.sbr"
	-@erase "$(INTDIR)\stdafx.obj"
	-@erase "$(INTDIR)\stdafx.sbr"
	-@erase "$(INTDIR)\unlock.obj"
	-@erase "$(INTDIR)\unlock.sbr"
	-@erase "$(INTDIR)\utils.obj"
	-@erase "$(INTDIR)\utils.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\wide.bsc"
	-@erase "$(OUTDIR)\wideA.pdb"
	-@erase ".\$(AMZI_DEV_DIR)\bin\wideA.exe"
	-@erase ".\$(AMZI_DEV_DIR)\bin\wideA.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /I "$(AMZI_DEV_DIR)\include" /D "_DEBUG" /D "_MBCS" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\wide.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\cpwide.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wide.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\amziexcept.sbr" \
	"$(INTDIR)\browbrow.sbr" \
	"$(INTDIR)\compile.sbr" \
	"$(INTDIR)\conview.sbr" \
	"$(INTDIR)\cpwide.sbr" \
	"$(INTDIR)\cpwin.sbr" \
	"$(INTDIR)\debug.sbr" \
	"$(INTDIR)\editdoc.sbr" \
	"$(INTDIR)\link.sbr" \
	"$(INTDIR)\listen.sbr" \
	"$(INTDIR)\mainfrm.sbr" \
	"$(INTDIR)\peditvw.sbr" \
	"$(INTDIR)\prodoc.sbr" \
	"$(INTDIR)\project.sbr" \
	"$(INTDIR)\proprog.sbr" \
	"$(INTDIR)\spypoint.sbr" \
	"$(INTDIR)\stdafx.sbr" \
	"$(INTDIR)\unlock.sbr" \
	"$(INTDIR)\utils.sbr"

"$(OUTDIR)\wide.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\wideA.pdb" /debug /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\wideA.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\amziexcept.obj" \
	"$(INTDIR)\browbrow.obj" \
	"$(INTDIR)\compile.obj" \
	"$(INTDIR)\conview.obj" \
	"$(INTDIR)\cpwide.obj" \
	"$(INTDIR)\cpwin.obj" \
	"$(INTDIR)\debug.obj" \
	"$(INTDIR)\editdoc.obj" \
	"$(INTDIR)\link.obj" \
	"$(INTDIR)\listen.obj" \
	"$(INTDIR)\mainfrm.obj" \
	"$(INTDIR)\peditvw.obj" \
	"$(INTDIR)\prodoc.obj" \
	"$(INTDIR)\project.obj" \
	"$(INTDIR)\proprog.obj" \
	"$(INTDIR)\spypoint.obj" \
	"$(INTDIR)\stdafx.obj" \
	"$(INTDIR)\unlock.obj" \
	"$(INTDIR)\utils.obj" \
	"$(INTDIR)\cpwide.res"

".\$(AMZI_DEV_DIR)\bin\wideA.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("wide.dep")
!INCLUDE "wide.dep"
!ELSE 
!MESSAGE Warning: cannot find "wide.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "wide - Win32 ReleaseW" || "$(CFG)" == "wide - Win32 ReleaseA" || "$(CFG)" == "wide - Win32 DebugW" || "$(CFG)" == "wide - Win32 DebugA"
SOURCE=.\amziexcept.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\amziexcept.obj"	"$(INTDIR)\amziexcept.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\amziexcept.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\amziexcept.obj"	"$(INTDIR)\amziexcept.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\amziexcept.obj"	"$(INTDIR)\amziexcept.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\browbrow.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\browbrow.obj"	"$(INTDIR)\browbrow.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\browbrow.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\browbrow.obj"	"$(INTDIR)\browbrow.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\browbrow.obj"	"$(INTDIR)\browbrow.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\compile.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\compile.obj"	"$(INTDIR)\compile.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\compile.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\compile.obj"	"$(INTDIR)\compile.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\compile.obj"	"$(INTDIR)\compile.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\conview.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\conview.obj"	"$(INTDIR)\conview.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\conview.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\conview.obj"	"$(INTDIR)\conview.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\conview.obj"	"$(INTDIR)\conview.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\cpwide.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\cpwide.obj"	"$(INTDIR)\cpwide.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\cpwide.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\cpwide.obj"	"$(INTDIR)\cpwide.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\cpwide.obj"	"$(INTDIR)\cpwide.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\cpwide.rc

"$(INTDIR)\cpwide.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=.\cpwin.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\cpwin.obj"	"$(INTDIR)\cpwin.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\cpwin.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\cpwin.obj"	"$(INTDIR)\cpwin.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\cpwin.obj"	"$(INTDIR)\cpwin.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\debug.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\debug.obj"	"$(INTDIR)\debug.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\debug.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\debug.obj"	"$(INTDIR)\debug.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\debug.obj"	"$(INTDIR)\debug.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\editdoc.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\editdoc.obj"	"$(INTDIR)\editdoc.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\editdoc.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\editdoc.obj"	"$(INTDIR)\editdoc.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\editdoc.obj"	"$(INTDIR)\editdoc.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\link.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\link.obj"	"$(INTDIR)\link.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\link.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\link.obj"	"$(INTDIR)\link.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\link.obj"	"$(INTDIR)\link.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\listen.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\listen.obj"	"$(INTDIR)\listen.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\listen.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\listen.obj"	"$(INTDIR)\listen.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\listen.obj"	"$(INTDIR)\listen.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\mainfrm.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\mainfrm.obj"	"$(INTDIR)\mainfrm.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\mainfrm.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\mainfrm.obj"	"$(INTDIR)\mainfrm.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\mainfrm.obj"	"$(INTDIR)\mainfrm.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\peditvw.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\peditvw.obj"	"$(INTDIR)\peditvw.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\peditvw.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\peditvw.obj"	"$(INTDIR)\peditvw.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\peditvw.obj"	"$(INTDIR)\peditvw.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\prodoc.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\prodoc.obj"	"$(INTDIR)\prodoc.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\prodoc.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\prodoc.obj"	"$(INTDIR)\prodoc.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\prodoc.obj"	"$(INTDIR)\prodoc.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\project.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\project.obj"	"$(INTDIR)\project.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\project.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\project.obj"	"$(INTDIR)\project.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\project.obj"	"$(INTDIR)\project.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\proprog.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\proprog.obj"	"$(INTDIR)\proprog.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\proprog.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\proprog.obj"	"$(INTDIR)\proprog.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\proprog.obj"	"$(INTDIR)\proprog.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\spypoint.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\spypoint.obj"	"$(INTDIR)\spypoint.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\spypoint.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\spypoint.obj"	"$(INTDIR)\spypoint.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\spypoint.obj"	"$(INTDIR)\spypoint.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\stdafx.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\stdafx.obj"	"$(INTDIR)\stdafx.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\stdafx.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\stdafx.obj"	"$(INTDIR)\stdafx.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\stdafx.obj"	"$(INTDIR)\stdafx.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\unlock.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\unlock.obj"	"$(INTDIR)\unlock.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\unlock.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\unlock.obj"	"$(INTDIR)\unlock.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\unlock.obj"	"$(INTDIR)\unlock.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\utils.cpp

!IF  "$(CFG)" == "wide - Win32 ReleaseW"


"$(INTDIR)\utils.obj"	"$(INTDIR)\utils.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 ReleaseA"


"$(INTDIR)\utils.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugW"


"$(INTDIR)\utils.obj"	"$(INTDIR)\utils.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "wide - Win32 DebugA"


"$(INTDIR)\utils.obj"	"$(INTDIR)\utils.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 


!ENDIF 


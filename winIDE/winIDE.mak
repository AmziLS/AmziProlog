# Microsoft Developer Studio Generated NMAKE File, Based on winIDE.dsp
!IF "$(CFG)" == ""
CFG=winIDE - Win32 DebugA
!MESSAGE No configuration specified. Defaulting to winIDE - Win32 DebugA.
!ENDIF 

!IF "$(CFG)" != "winIDE - Win32 ReleaseW" && "$(CFG)" != "winIDE - Win32 ReleaseA" && "$(CFG)" != "winIDE - Win32 DebugW" && "$(CFG)" != "winIDE - Win32 DebugA"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "winIDE.mak" CFG="winIDE - Win32 DebugA"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "winIDE - Win32 ReleaseW" (based on "Win32 (x86) Application")
!MESSAGE "winIDE - Win32 ReleaseA" (based on "Win32 (x86) Application")
!MESSAGE "winIDE - Win32 DebugW" (based on "Win32 (x86) Application")
!MESSAGE "winIDE - Win32 DebugA" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"

OUTDIR=.\ReleaseW
INTDIR=.\ReleaseW
# Begin Custom Macros
OutDir=.\ReleaseW
# End Custom Macros

ALL : ".\$(AMZI_DEV_DIR)\bin\winIDE_W.exe" "$(OUTDIR)\winIDE.bsc"


CLEAN :
	-@erase "$(INTDIR)\amziexcept.obj"
	-@erase "$(INTDIR)\amziexcept.sbr"
	-@erase "$(INTDIR)\browbrow.obj"
	-@erase "$(INTDIR)\browbrow.sbr"
	-@erase "$(INTDIR)\compile.obj"
	-@erase "$(INTDIR)\compile.sbr"
	-@erase "$(INTDIR)\conview.obj"
	-@erase "$(INTDIR)\conview.sbr"
	-@erase "$(INTDIR)\cpWinIDE.obj"
	-@erase "$(INTDIR)\cpWinIDE.res"
	-@erase "$(INTDIR)\cpWinIDE.sbr"
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
	-@erase "$(OUTDIR)\winIDE.bsc"
	-@erase ".\$(AMZI_DEV_DIR)\bin\winIDE_W.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "$(AMZI_DEV_DIR)\include" /D "NDEBUG" /D "_UNICODE" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\winIDE.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\cpWinIDE.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\winIDE.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\amziexcept.sbr" \
	"$(INTDIR)\browbrow.sbr" \
	"$(INTDIR)\compile.sbr" \
	"$(INTDIR)\conview.sbr" \
	"$(INTDIR)\cpWinIDE.sbr" \
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

"$(OUTDIR)\winIDE.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib /nologo /entry:"wWinMainCRTStartup" /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\winIDE_W.pdb" /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\winIDE_W.exe" 
LINK32_OBJS= \
	"$(INTDIR)\amziexcept.obj" \
	"$(INTDIR)\browbrow.obj" \
	"$(INTDIR)\compile.obj" \
	"$(INTDIR)\conview.obj" \
	"$(INTDIR)\cpWinIDE.obj" \
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
	"$(INTDIR)\cpWinIDE.res"

".\$(AMZI_DEV_DIR)\bin\winIDE_W.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"

OUTDIR=.\ReleaseA
INTDIR=.\ReleaseA

ALL : ".\$(AMZI_DEV_DIR)\bin\winIDE_A.exe"


CLEAN :
	-@erase "$(INTDIR)\amziexcept.obj"
	-@erase "$(INTDIR)\browbrow.obj"
	-@erase "$(INTDIR)\compile.obj"
	-@erase "$(INTDIR)\conview.obj"
	-@erase "$(INTDIR)\cpWinIDE.obj"
	-@erase "$(INTDIR)\cpWinIDE.res"
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
	-@erase ".\$(AMZI_DEV_DIR)\bin\winIDE_A.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "$(AMZI_DEV_DIR)\include" /D "NDEBUG" /D "_MBCS" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /Fp"$(INTDIR)\winIDE.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\cpWinIDE.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\winIDE.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\winIDE_A.pdb" /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\winIDE_A.exe" 
LINK32_OBJS= \
	"$(INTDIR)\amziexcept.obj" \
	"$(INTDIR)\browbrow.obj" \
	"$(INTDIR)\compile.obj" \
	"$(INTDIR)\conview.obj" \
	"$(INTDIR)\cpWinIDE.obj" \
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
	"$(INTDIR)\cpWinIDE.res"

".\$(AMZI_DEV_DIR)\bin\winIDE_A.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"

OUTDIR=.\DebugW
INTDIR=.\DebugW
# Begin Custom Macros
OutDir=.\DebugW
# End Custom Macros

ALL : ".\$(AMZI_DEV_DIR)\bin\winIDE_W.exe" "$(OUTDIR)\winIDE.bsc"


CLEAN :
	-@erase "$(INTDIR)\amziexcept.obj"
	-@erase "$(INTDIR)\amziexcept.sbr"
	-@erase "$(INTDIR)\browbrow.obj"
	-@erase "$(INTDIR)\browbrow.sbr"
	-@erase "$(INTDIR)\compile.obj"
	-@erase "$(INTDIR)\compile.sbr"
	-@erase "$(INTDIR)\conview.obj"
	-@erase "$(INTDIR)\conview.sbr"
	-@erase "$(INTDIR)\cpWinIDE.obj"
	-@erase "$(INTDIR)\cpWinIDE.res"
	-@erase "$(INTDIR)\cpWinIDE.sbr"
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
	-@erase "$(OUTDIR)\winIDE.bsc"
	-@erase "$(OUTDIR)\winIDE_W.pdb"
	-@erase ".\$(AMZI_DEV_DIR)\bin\winIDE_W.exe"
	-@erase ".\$(AMZI_DEV_DIR)\bin\winIDE_W.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /I "$(AMZI_DEV_DIR)\include" /D "_DEBUG" /D "_UNICODE" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\winIDE.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\cpWinIDE.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\winIDE.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\amziexcept.sbr" \
	"$(INTDIR)\browbrow.sbr" \
	"$(INTDIR)\compile.sbr" \
	"$(INTDIR)\conview.sbr" \
	"$(INTDIR)\cpWinIDE.sbr" \
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

"$(OUTDIR)\winIDE.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib /nologo /entry:"wWinMainCRTStartup" /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\winIDE_W.pdb" /debug /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\winIDE_W.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\amziexcept.obj" \
	"$(INTDIR)\browbrow.obj" \
	"$(INTDIR)\compile.obj" \
	"$(INTDIR)\conview.obj" \
	"$(INTDIR)\cpWinIDE.obj" \
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
	"$(INTDIR)\cpWinIDE.res"

".\$(AMZI_DEV_DIR)\bin\winIDE_W.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"

OUTDIR=.\DebugA
INTDIR=.\DebugA
# Begin Custom Macros
OutDir=.\DebugA
# End Custom Macros

ALL : ".\$(AMZI_DEV_DIR)\bin\winIDE_A.exe" "$(OUTDIR)\winIDE.bsc"


CLEAN :
	-@erase "$(INTDIR)\amziexcept.obj"
	-@erase "$(INTDIR)\amziexcept.sbr"
	-@erase "$(INTDIR)\browbrow.obj"
	-@erase "$(INTDIR)\browbrow.sbr"
	-@erase "$(INTDIR)\compile.obj"
	-@erase "$(INTDIR)\compile.sbr"
	-@erase "$(INTDIR)\conview.obj"
	-@erase "$(INTDIR)\conview.sbr"
	-@erase "$(INTDIR)\cpWinIDE.obj"
	-@erase "$(INTDIR)\cpWinIDE.res"
	-@erase "$(INTDIR)\cpWinIDE.sbr"
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
	-@erase "$(OUTDIR)\winIDE.bsc"
	-@erase "$(OUTDIR)\winIDE_A.pdb"
	-@erase ".\$(AMZI_DEV_DIR)\bin\winIDE_A.exe"
	-@erase ".\$(AMZI_DEV_DIR)\bin\winIDE_A.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /I "$(AMZI_DEV_DIR)\include" /D "_DEBUG" /D "_MBCS" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\winIDE.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\cpWinIDE.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\winIDE.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\amziexcept.sbr" \
	"$(INTDIR)\browbrow.sbr" \
	"$(INTDIR)\compile.sbr" \
	"$(INTDIR)\conview.sbr" \
	"$(INTDIR)\cpWinIDE.sbr" \
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

"$(OUTDIR)\winIDE.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\winIDE_A.pdb" /debug /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\winIDE_A.exe" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\amziexcept.obj" \
	"$(INTDIR)\browbrow.obj" \
	"$(INTDIR)\compile.obj" \
	"$(INTDIR)\conview.obj" \
	"$(INTDIR)\cpWinIDE.obj" \
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
	"$(INTDIR)\cpWinIDE.res"

".\$(AMZI_DEV_DIR)\bin\winIDE_A.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("winIDE.dep")
!INCLUDE "winIDE.dep"
!ELSE 
!MESSAGE Warning: cannot find "winIDE.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "winIDE - Win32 ReleaseW" || "$(CFG)" == "winIDE - Win32 ReleaseA" || "$(CFG)" == "winIDE - Win32 DebugW" || "$(CFG)" == "winIDE - Win32 DebugA"
SOURCE=.\amziexcept.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\amziexcept.obj"	"$(INTDIR)\amziexcept.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\amziexcept.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\amziexcept.obj"	"$(INTDIR)\amziexcept.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\amziexcept.obj"	"$(INTDIR)\amziexcept.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\browbrow.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\browbrow.obj"	"$(INTDIR)\browbrow.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\browbrow.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\browbrow.obj"	"$(INTDIR)\browbrow.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\browbrow.obj"	"$(INTDIR)\browbrow.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\compile.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\compile.obj"	"$(INTDIR)\compile.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\compile.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\compile.obj"	"$(INTDIR)\compile.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\compile.obj"	"$(INTDIR)\compile.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\conview.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\conview.obj"	"$(INTDIR)\conview.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\conview.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\conview.obj"	"$(INTDIR)\conview.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\conview.obj"	"$(INTDIR)\conview.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\cpWinIDE.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\cpWinIDE.obj"	"$(INTDIR)\cpWinIDE.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\cpWinIDE.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\cpWinIDE.obj"	"$(INTDIR)\cpWinIDE.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\cpWinIDE.obj"	"$(INTDIR)\cpWinIDE.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\cpWinIDE.rc

"$(INTDIR)\cpWinIDE.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=.\cpwin.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\cpwin.obj"	"$(INTDIR)\cpwin.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\cpwin.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\cpwin.obj"	"$(INTDIR)\cpwin.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\cpwin.obj"	"$(INTDIR)\cpwin.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\debug.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\debug.obj"	"$(INTDIR)\debug.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\debug.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\debug.obj"	"$(INTDIR)\debug.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\debug.obj"	"$(INTDIR)\debug.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\editdoc.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\editdoc.obj"	"$(INTDIR)\editdoc.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\editdoc.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\editdoc.obj"	"$(INTDIR)\editdoc.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\editdoc.obj"	"$(INTDIR)\editdoc.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\link.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\link.obj"	"$(INTDIR)\link.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\link.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\link.obj"	"$(INTDIR)\link.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\link.obj"	"$(INTDIR)\link.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\listen.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\listen.obj"	"$(INTDIR)\listen.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\listen.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\listen.obj"	"$(INTDIR)\listen.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\listen.obj"	"$(INTDIR)\listen.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\mainfrm.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\mainfrm.obj"	"$(INTDIR)\mainfrm.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\mainfrm.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\mainfrm.obj"	"$(INTDIR)\mainfrm.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\mainfrm.obj"	"$(INTDIR)\mainfrm.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\peditvw.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\peditvw.obj"	"$(INTDIR)\peditvw.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\peditvw.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\peditvw.obj"	"$(INTDIR)\peditvw.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\peditvw.obj"	"$(INTDIR)\peditvw.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\prodoc.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\prodoc.obj"	"$(INTDIR)\prodoc.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\prodoc.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\prodoc.obj"	"$(INTDIR)\prodoc.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\prodoc.obj"	"$(INTDIR)\prodoc.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\project.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\project.obj"	"$(INTDIR)\project.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\project.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\project.obj"	"$(INTDIR)\project.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\project.obj"	"$(INTDIR)\project.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\proprog.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\proprog.obj"	"$(INTDIR)\proprog.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\proprog.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\proprog.obj"	"$(INTDIR)\proprog.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\proprog.obj"	"$(INTDIR)\proprog.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\spypoint.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\spypoint.obj"	"$(INTDIR)\spypoint.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\spypoint.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\spypoint.obj"	"$(INTDIR)\spypoint.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\spypoint.obj"	"$(INTDIR)\spypoint.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\stdafx.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\stdafx.obj"	"$(INTDIR)\stdafx.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\stdafx.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\stdafx.obj"	"$(INTDIR)\stdafx.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\stdafx.obj"	"$(INTDIR)\stdafx.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\unlock.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\unlock.obj"	"$(INTDIR)\unlock.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\unlock.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\unlock.obj"	"$(INTDIR)\unlock.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\unlock.obj"	"$(INTDIR)\unlock.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\utils.cpp

!IF  "$(CFG)" == "winIDE - Win32 ReleaseW"


"$(INTDIR)\utils.obj"	"$(INTDIR)\utils.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 ReleaseA"


"$(INTDIR)\utils.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugW"


"$(INTDIR)\utils.obj"	"$(INTDIR)\utils.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "winIDE - Win32 DebugA"


"$(INTDIR)\utils.obj"	"$(INTDIR)\utils.sbr" : $(SOURCE) "$(INTDIR)"


!ENDIF 


!ENDIF 


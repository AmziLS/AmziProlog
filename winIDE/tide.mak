# Microsoft Developer Studio Generated NMAKE File, Based on tide.dsp
!IF "$(CFG)" == ""
CFG=tide - Win32 DebugTA
!MESSAGE No configuration specified. Defaulting to tide - Win32 DebugTA.
!ENDIF 

!IF "$(CFG)" != "tide - Win32 DebugTW" && "$(CFG)" != "tide - Win32 DebugTA" && "$(CFG)" != "tide - Win32 ReleaseTW" && "$(CFG)" != "tide - Win32 ReleaseTA"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "tide.mak" CFG="tide - Win32 DebugTA"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "tide - Win32 DebugTW" (based on "Win32 (x86) Application")
!MESSAGE "tide - Win32 DebugTA" (based on "Win32 (x86) Application")
!MESSAGE "tide - Win32 ReleaseTW" (based on "Win32 (x86) Application")
!MESSAGE "tide - Win32 ReleaseTA" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "tide - Win32 DebugTW"

OUTDIR=.\DebugTW
INTDIR=.\DebugTW

ALL : ".\$(AMZI_DEV_DIR)\bin\tideW.exe"


CLEAN :
	-@erase "$(INTDIR)\amziexcept.obj"
	-@erase "$(INTDIR)\browbrow.obj"
	-@erase "$(INTDIR)\compile.obj"
	-@erase "$(INTDIR)\conview.obj"
	-@erase "$(INTDIR)\cpwide.obj"
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
	-@erase "$(INTDIR)\tipco.res"
	-@erase "$(INTDIR)\unlock.obj"
	-@erase "$(INTDIR)\utils.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\tideW.pdb"
	-@erase ".\$(AMZI_DEV_DIR)\bin\tideW.exe"
	-@erase ".\$(AMZI_DEV_DIR)\bin\tideW.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /I "$(AMZI_DEV_DIR)\include" /D "_DEBUG" /D "_UNICODE" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /Fp"$(INTDIR)\tide.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\tipco.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\tide.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib /nologo /entry:"wWinMainCRTStartup" /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\tideW.pdb" /debug /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\tideW.exe" /pdbtype:sept 
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
	"$(INTDIR)\tipco.res"

".\$(AMZI_DEV_DIR)\bin\tideW.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"

OUTDIR=.\DebugTA
INTDIR=.\DebugTA

ALL : ".\$(AMZI_DEV_DIR)\bin\tideA.exe"


CLEAN :
	-@erase "$(INTDIR)\amziexcept.obj"
	-@erase "$(INTDIR)\browbrow.obj"
	-@erase "$(INTDIR)\compile.obj"
	-@erase "$(INTDIR)\conview.obj"
	-@erase "$(INTDIR)\cpwide.obj"
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
	-@erase "$(INTDIR)\tipco.res"
	-@erase "$(INTDIR)\unlock.obj"
	-@erase "$(INTDIR)\utils.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\tideA.pdb"
	-@erase ".\$(AMZI_DEV_DIR)\bin\tideA.exe"
	-@erase ".\$(AMZI_DEV_DIR)\bin\tideA.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /I "$(AMZI_DEV_DIR)\include" /D "_DEBUG" /D "_MBCS" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /Fp"$(INTDIR)\tide.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\tipco.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\tide.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\tideA.pdb" /debug /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\tideA.exe" /pdbtype:sept 
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
	"$(INTDIR)\tipco.res"

".\$(AMZI_DEV_DIR)\bin\tideA.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"

OUTDIR=.\ReleaseTW
INTDIR=.\ReleaseTW
# Begin Custom Macros
OutDir=.\ReleaseTW
# End Custom Macros

ALL : ".\$(AMZI_DEV_DIR)\bin\tideW.exe" "$(OUTDIR)\tide.bsc"


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
	-@erase "$(INTDIR)\tipco.res"
	-@erase "$(INTDIR)\unlock.obj"
	-@erase "$(INTDIR)\unlock.sbr"
	-@erase "$(INTDIR)\utils.obj"
	-@erase "$(INTDIR)\utils.sbr"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\tide.bsc"
	-@erase ".\$(AMZI_DEV_DIR)\bin\tideW.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "$(AMZI_DEV_DIR)\include" /D "NDEBUG" /D "_UNICODE" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\tide.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\tipco.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\tide.bsc" 
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

"$(OUTDIR)\tide.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib /nologo /entry:"wWinMainCRTStartup" /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\tideW.pdb" /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\tideW.exe" 
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
	"$(INTDIR)\tipco.res"

".\$(AMZI_DEV_DIR)\bin\tideW.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"

OUTDIR=.\ReleaseTA
INTDIR=.\ReleaseTA

ALL : ".\$(AMZI_DEV_DIR)\bin\tideA.exe"


CLEAN :
	-@erase "$(INTDIR)\amziexcept.obj"
	-@erase "$(INTDIR)\browbrow.obj"
	-@erase "$(INTDIR)\compile.obj"
	-@erase "$(INTDIR)\conview.obj"
	-@erase "$(INTDIR)\cpwide.obj"
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
	-@erase "$(INTDIR)\tipco.res"
	-@erase "$(INTDIR)\unlock.obj"
	-@erase "$(INTDIR)\utils.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase ".\$(AMZI_DEV_DIR)\bin\tideA.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "$(AMZI_DEV_DIR)\include" /D "NDEBUG" /D "_MBCS" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /Fp"$(INTDIR)\tide.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\tipco.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\tide.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=$(AMZI_DEV_DIR)\lib\amzi.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\tideA.pdb" /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\tideA.exe" 
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
	"$(INTDIR)\tipco.res"

".\$(AMZI_DEV_DIR)\bin\tideA.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("tide.dep")
!INCLUDE "tide.dep"
!ELSE 
!MESSAGE Warning: cannot find "tide.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "tide - Win32 DebugTW" || "$(CFG)" == "tide - Win32 DebugTA" || "$(CFG)" == "tide - Win32 ReleaseTW" || "$(CFG)" == "tide - Win32 ReleaseTA"
SOURCE=.\amziexcept.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\amziexcept.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\amziexcept.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\amziexcept.obj"	"$(INTDIR)\amziexcept.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\amziexcept.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\browbrow.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\browbrow.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\browbrow.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\browbrow.obj"	"$(INTDIR)\browbrow.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\browbrow.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\compile.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\compile.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\compile.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\compile.obj"	"$(INTDIR)\compile.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\compile.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\conview.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\conview.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\conview.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\conview.obj"	"$(INTDIR)\conview.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\conview.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\cpwide.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\cpwide.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\cpwide.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\cpwide.obj"	"$(INTDIR)\cpwide.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\cpwide.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\cpwin.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\cpwin.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\cpwin.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\cpwin.obj"	"$(INTDIR)\cpwin.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\cpwin.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\debug.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\debug.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\debug.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\debug.obj"	"$(INTDIR)\debug.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\debug.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\editdoc.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\editdoc.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\editdoc.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\editdoc.obj"	"$(INTDIR)\editdoc.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\editdoc.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\link.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\link.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\link.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\link.obj"	"$(INTDIR)\link.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\link.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\listen.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\listen.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\listen.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\listen.obj"	"$(INTDIR)\listen.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\listen.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\mainfrm.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\mainfrm.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\mainfrm.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\mainfrm.obj"	"$(INTDIR)\mainfrm.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\mainfrm.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\peditvw.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\peditvw.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\peditvw.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\peditvw.obj"	"$(INTDIR)\peditvw.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\peditvw.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\prodoc.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\prodoc.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\prodoc.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\prodoc.obj"	"$(INTDIR)\prodoc.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\prodoc.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\project.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\project.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\project.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\project.obj"	"$(INTDIR)\project.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\project.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\proprog.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\proprog.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\proprog.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\proprog.obj"	"$(INTDIR)\proprog.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\proprog.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\spypoint.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\spypoint.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\spypoint.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\spypoint.obj"	"$(INTDIR)\spypoint.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\spypoint.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\stdafx.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\stdafx.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\stdafx.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\stdafx.obj"	"$(INTDIR)\stdafx.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\stdafx.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\tipco.rc

"$(INTDIR)\tipco.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=.\unlock.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\unlock.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\unlock.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\unlock.obj"	"$(INTDIR)\unlock.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\unlock.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

SOURCE=.\utils.cpp

!IF  "$(CFG)" == "tide - Win32 DebugTW"


"$(INTDIR)\utils.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"


"$(INTDIR)\utils.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"


"$(INTDIR)\utils.obj"	"$(INTDIR)\utils.sbr" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"


"$(INTDIR)\utils.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 


!ENDIF 


# Microsoft Developer Studio Project File - Name="tide" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=tide - Win32 DebugTA
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "tide.mak".
!MESSAGE 
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

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "tide - Win32 DebugTW"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "DebugTW"
# PROP BASE Intermediate_Dir "DebugTW"
# PROP BASE Target_Dir ""
# PROP Use_MFC 1
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "DebugTW"
# PROP Intermediate_Dir "DebugTW"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "$(AMZI_DEV_DIR)\include" /D "_DEBUG" /D "_UNICODE" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 $(AMZI_DEV_DIR)\lib\amzi.lib /nologo /entry:"wWinMainCRTStartup" /subsystem:windows /debug /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\tideW.exe" /pdbtype:sept

!ELSEIF  "$(CFG)" == "tide - Win32 DebugTA"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "DebugTA"
# PROP BASE Intermediate_Dir "DebugTA"
# PROP BASE Target_Dir ""
# PROP Use_MFC 1
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "DebugTA"
# PROP Intermediate_Dir "DebugTA"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "$(AMZI_DEV_DIR)\include" /D "_DEBUG" /D "_MBCS" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 $(AMZI_DEV_DIR)\lib\amzi.lib /nologo /subsystem:windows /debug /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\tideA.exe" /pdbtype:sept

!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTW"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "ReleaseTW"
# PROP BASE Intermediate_Dir "ReleaseTW"
# PROP BASE Target_Dir ""
# PROP Use_MFC 1
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "ReleaseTW"
# PROP Intermediate_Dir "ReleaseTW"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "$(AMZI_DEV_DIR)\include" /D "NDEBUG" /D "_UNICODE" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /FR /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 $(AMZI_DEV_DIR)\lib\amzi.lib /nologo /entry:"wWinMainCRTStartup" /subsystem:windows /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\tideW.exe"

!ELSEIF  "$(CFG)" == "tide - Win32 ReleaseTA"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "ReleaseTA"
# PROP BASE Intermediate_Dir "ReleaseTA"
# PROP BASE Target_Dir ""
# PROP Use_MFC 1
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "ReleaseTA"
# PROP Intermediate_Dir "ReleaseTA"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "$(AMZI_DEV_DIR)\include" /D "NDEBUG" /D "_MBCS" /D "WIN32" /D "_WINDOWS" /D "_TIBCO" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 $(AMZI_DEV_DIR)\lib\amzi.lib /nologo /subsystem:windows /machine:I386 /out:"$(AMZI_DEV_DIR)\bin\tideA.exe"

!ENDIF 

# Begin Target

# Name "tide - Win32 DebugTW"
# Name "tide - Win32 DebugTA"
# Name "tide - Win32 ReleaseTW"
# Name "tide - Win32 ReleaseTA"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\amziexcept.cpp
# End Source File
# Begin Source File

SOURCE=.\browbrow.cpp
# End Source File
# Begin Source File

SOURCE=.\compile.cpp
# End Source File
# Begin Source File

SOURCE=.\conview.cpp
# End Source File
# Begin Source File

SOURCE=.\cpwide.cpp
# End Source File
# Begin Source File

SOURCE=.\cpwin.cpp
# End Source File
# Begin Source File

SOURCE=.\debug.cpp
# End Source File
# Begin Source File

SOURCE=.\editdoc.cpp
# End Source File
# Begin Source File

SOURCE=.\link.cpp
# End Source File
# Begin Source File

SOURCE=.\listen.cpp
# End Source File
# Begin Source File

SOURCE=.\mainfrm.cpp
# End Source File
# Begin Source File

SOURCE=.\peditvw.cpp
# End Source File
# Begin Source File

SOURCE=.\prodoc.cpp
# End Source File
# Begin Source File

SOURCE=.\project.cpp
# End Source File
# Begin Source File

SOURCE=.\proprog.cpp
# End Source File
# Begin Source File

SOURCE=.\spypoint.cpp
# End Source File
# Begin Source File

SOURCE=.\stdafx.cpp
# End Source File
# Begin Source File

SOURCE=.\tipco.rc
# End Source File
# Begin Source File

SOURCE=.\unlock.cpp
# End Source File
# Begin Source File

SOURCE=.\utils.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\amziexcept.h
# End Source File
# Begin Source File

SOURCE=.\browbrow.h
# End Source File
# Begin Source File

SOURCE=.\compile.h
# End Source File
# Begin Source File

SOURCE=.\conview.h
# End Source File
# Begin Source File

SOURCE=.\cpwide.h
# End Source File
# Begin Source File

SOURCE=.\cpwin.h
# End Source File
# Begin Source File

SOURCE=.\debug.h
# End Source File
# Begin Source File

SOURCE=.\editdoc.h
# End Source File
# Begin Source File

SOURCE=.\link.h
# End Source File
# Begin Source File

SOURCE=.\listen.h
# End Source File
# Begin Source File

SOURCE=.\mainfrm.h
# End Source File
# Begin Source File

SOURCE=.\peditvw.h
# End Source File
# Begin Source File

SOURCE=.\prodoc.h
# End Source File
# Begin Source File

SOURCE=.\project.h
# End Source File
# Begin Source File

SOURCE=.\proprog.h
# End Source File
# Begin Source File

SOURCE=.\resource.h
# End Source File
# Begin Source File

SOURCE=.\spypoint.h
# End Source File
# Begin Source File

SOURCE=.\stdafx.h
# End Source File
# Begin Source File

SOURCE=.\unlock.h
# End Source File
# Begin Source File

SOURCE=.\utils.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\RES\bitmap1.bmp
# End Source File
# Begin Source File

SOURCE=.\RES\cogs.ico
# End Source File
# Begin Source File

SOURCE=.\RES\cpwide.ico
# End Source File
# Begin Source File

SOURCE=.\RES\editdoc.ico
# End Source File
# Begin Source File

SOURCE=.\RES\icon1.ico
# End Source File
# Begin Source File

SOURCE=.\RES\IDR_MAIN.BMP
# End Source File
# Begin Source File

SOURCE=.\RES\TCOGS.ICO
# End Source File
# Begin Source File

SOURCE=.\RES\TCPWIDE.ICO
# End Source File
# Begin Source File

SOURCE=.\RES\TEDITDOC.ICO
# End Source File
# Begin Source File

SOURCE=.\RES\tibco.bmp
# End Source File
# Begin Source File

SOURCE=.\RES\tibco.ico
# End Source File
# Begin Source File

SOURCE=.\RES\toolbar.bmp
# End Source File
# Begin Source File

SOURCE=.\RES\toolbar1.bmp
# End Source File
# End Group
# End Target
# End Project

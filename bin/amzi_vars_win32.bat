@echo off


REM ***** adapt lines below *****

REM adapt path to match your VS installation directory
call "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvars32.bat"

REM adapt path to match your MySQL installation directory
set MYSQL=C:\Program Files\MySQL\MySQL Server 8.0

REM disable or adapt path if needed (MUST point to a JDK)
set JAVA_HOME=C:\Program Files (x86)\Java\jdk1.8.0_161\

REM *****     end adapt     *****


set APLS_SOURCE=%CD%
set APLS_RELEASE=%APLS_SOURCE%\release

REM for release builds
set AMZI_DIR=%APLS_RELEASE%

REM for debug builds
set AMZI_DEV_DIR=%APLS_RELEASE%

set PATH=%JAVA_HOME%\bin;%APLS_SOURCE%\make;%APLS_RELEASE%\bin;%AMZI_DIR%\bin;%PATH%

REM blank line
echo[

echo Environment initialized for Amzi! Prolog

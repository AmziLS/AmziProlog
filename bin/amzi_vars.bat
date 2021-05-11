@echo off

set AMZI_DIR=%CD%

REM for release builds
set APLS_RELEASE=%AMZI_DIR%

REM for debug builds
set AMZI_DEV_DIR=%AMZI_DIR%

set PATH=%AMZI_DIR%\bin;%PATH%

REM blank line
echo[

echo Environment initialized for Amzi! Prolog (Release)

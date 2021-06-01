@echo off

if exist release\ (
  set LOCAL_AMZI_DIR=%CD%\release
) else (
  set LOCAL_AMZI_DIR=%CD%
)

setx AMZI_DIR "%LOCAL_AMZI_DIR%"

REM for release builds
setx APLS_RELEASE "%LOCAL_AMZI_DIR%"

REM for debug builds
setx AMZI_DEV_DIR "%LOCAL_AMZI_DIR%"

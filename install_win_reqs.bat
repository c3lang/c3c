@echo off

set DOWNLOAD_URL=https://aka.ms/vs/17/release

mkdir tmp 2> NUL

if not exist "tmp\vs_buildtools.exe" (
    bitsadmin /transfer /download /priority foreground %DOWNLOAD_URL%/vs_buildtools.exe %CD%\tmp\vs_buildtools.exe
)

echo Preparing Build Tools, please wait...
tmp\vs_BuildTools.exe --quiet --wait --layout tmp\ --add Microsoft.VisualStudio.Component.Windows10SDK.19041

echo Installing Build Tools, please wait...
tmp\vs_BuildTools.exe --quiet --wait --noweb --add Microsoft.VisualStudio.Component.Windows10SDK.19041

REM rmdir tmp /s /q

Unicode true

!include "MUI2.nsh"

Name "C3 Compiler"
!ifndef INSTALLER_NAME
  !define INSTALLER_NAME "c3-setup-default.exe"
!endif

OutFile "${INSTALLER_NAME}"
InstallDir "$LOCALAPPDATA\c3"
RequestExecutionLevel user
SetCompressor lzma

!define MUI_ABORTWARNING
!define MUI_ICON "logo.ico"

!insertmacro MUI_PAGE_WELCOME
!insertmacro MUI_PAGE_LICENSE "..\..\LICENSE"
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

!insertmacro MUI_LANGUAGE "English"

Section "C3 Compiler"
  SectionIn RO
  SetOutPath "$INSTDIR"
  File "..\..\LICENSE"
  File "..\..\README.md"
  File "..\..\releasenotes.md"
  File "..\..\build\c3c.exe"
  File /nonfatal "..\..\build\c3c.pdb"

  SetOutPath "$INSTDIR\c3c_rt"
  File /nonfatal /r "..\..\build\c3c_rt\*"

  SetOutPath "$INSTDIR\lib"
  File /r "..\..\lib\*"
  WriteUninstaller "$INSTDIR\\Uninstall.exe"
SectionEnd

Section "Add to PATH" SecPath
  DetailPrint "Adding $INSTDIR to PATH..."
  
  EnVar::Check "Path" "$INSTDIR"
  Pop $0
  ${If} $0 != 0
    EnVar::AddValue "Path" "$INSTDIR"
    Pop $0
  ${EndIf}
SectionEnd

Section "Setup MSVC SDK" SecMSVC
  ExecWait '"$INSTDIR\c3c.exe" fetch-sdk windows --accept-license' $0
  
  ${If} $0 == 0
    DetailPrint "SDK successfully initialized."
  ${Else}
    DetailPrint "Error: c3c.exe exited with code $0"
    MessageBox MB_OK|MB_ICONSTOP \
      "The MSVC SDK setup failed (Error Code: $0). Please check your connection and try again."
  ${EndIf}
SectionEnd

Section "Uninstall"
  Delete "$INSTDIR\LICENSE"
  Delete "$INSTDIR\Uninstall.exe"
  Delete "$INSTDIR\c3c.exe"
  Delete "$INSTDIR\c3c.pdb"
  Delete "$INSTDIR\README.md"
  Delete "$INSTDIR\releasenotes.md"
  RMDir /r "$INSTDIR\c3c_rt"
  RMDir /r "$INSTDIR\msvc_sdk"
  RMDir /r "$INSTDIR\lib"

  EnVar::Check "Path" "$INSTDIR"
  Pop $0
  ${If} $0 == 0
    EnVar::DeleteValue "Path" "$INSTDIR"
    Pop $0
  ${EndIf}

  RMDir "$INSTDIR"
SectionEnd
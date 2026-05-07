!include "nsDialogs.nsh"
!include "LogicLib.nsh"
!include "Sections.nsh"

Var MSVCDialog
Var MSVCLink
Var MSVCAcceptRadio
Var MSVCDeclineRadio

Function ShowMSVCLicensePage
  StrCpy $MSVCChoice "0"

  SectionGetFlags ${SecMSVC} $0
  IntOp $0 $0 & ${SF_SELECTED}
  ${If} $0 == 0
    Abort
  ${EndIf}

  GetDlgItem $0 $HWNDPARENT 1037
  SendMessage $0 ${WM_SETTEXT} 0 "STR:MSVC SDK License Agreement"
  GetDlgItem $0 $HWNDPARENT 1038
  SendMessage $0 ${WM_SETTEXT} 0 "STR:Please review the Microsoft terms before continuing."

  nsDialogs::Create 1018
  Pop $MSVCDialog

  ${If} $MSVCDialog == error
    Return
  ${EndIf}

  ${NSD_CreateLabel} 0 0 100% 12u "Please review the Microsoft license terms:"
  Pop $0

  ${NSD_CreateLink} 0 12u 100% 12u "https://visualstudio.microsoft.com/license-terms/vs2022-ga-diagnosticbuildtools/"
  Pop $MSVCLink
  ${NSD_OnClick} $MSVCLink _OpenMSVCLink

  ${NSD_CreateRadioButton} 0 36u 100% 12u "I accept the MSVC SDK License terms."
  Pop $MSVCAcceptRadio
    
  ${NSD_CreateRadioButton} 0 48u 100% 12u "I do not accept."
  Pop $MSVCDeclineRadio

  ${NSD_Check} $MSVCDeclineRadio 

  nsDialogs::Show
FunctionEnd

Function _OpenMSVCLink
  ExecShell "open" "https://visualstudio.microsoft.com/license-terms/vs2022-ga-diagnosticbuildtools/"
FunctionEnd

Function LeaveMSVCLicensePage
  ${NSD_GetState} $MSVCAcceptRadio $0
  ${If} $0 == ${BST_CHECKED}
    StrCpy $MSVCChoice "1"
  ${EndIf}
FunctionEnd
<#
.SYNOPSIS
    C3 install script.
.DESCRIPTION
    This script is used to install C3 on Windows from the command line.
.PARAMETER C3Version
    Specifies the version of C3 to install.
    The default value is 'latest'. You can also specify it by setting the
    environment variable 'C3_VERSION'.
.PARAMETER C3Home
    Specifies C3's home directory.
    The default value is '$Env:USERPROFILE\.c3'. You can also specify it by
    setting the environment variable 'C3_HOME'.
.PARAMETER NoPathUpdate
    If specified, the script will not update the PATH environment variable.
.PARAMETER C3Repourl
    Specifies C3's repo url.
    The default value is 'https://github.com/c3lang/c3c'. You can also specify it by
    setting the environment variable 'C3_REPOURL'.
.LINK
    https://c3-lang.org/
.LINK
    https://github.com/c3lang/c3c
#>
param (
    [string] $C3Version = 'latest',
    [string] $C3Home = "$Env:USERPROFILE\.c3",
    [switch] $NoPathUpdate,
    [string] $C3Repourl = 'https://github.com/c3lang/c3c'
)

Set-StrictMode -Version Latest

function Publish-Env {
    if (-not ("Win32.NativeMethods" -as [Type])) {
        Add-Type -Namespace Win32 -Name NativeMethods -MemberDefinition @"
[DllImport("user32.dll", SetLastError = true, CharSet = CharSet.Auto)]
public static extern IntPtr SendMessageTimeout(
    IntPtr hWnd, uint Msg, UIntPtr wParam, string lParam,
    uint fuFlags, uint uTimeout, out UIntPtr lpdwResult);
"@
    }

    $HWND_BROADCAST = [IntPtr] 0xffff
    $WM_SETTINGCHANGE = 0x1a
    $result = [UIntPtr]::Zero

    [Win32.Nativemethods]::SendMessageTimeout($HWND_BROADCAST,
        $WM_SETTINGCHANGE,
        [UIntPtr]::Zero,
        "Environment",
        2,
        5000,
        [ref] $result
    ) | Out-Null
}

function Write-Env {
    param(
        [String] $name,
        [String] $val,
        [Switch] $global
    )

    $RegisterKey = if ($global) {
        Get-Item -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager'
    } else {
        Get-Item -Path 'HKCU:'
    }

    $EnvRegisterKey = $RegisterKey.OpenSubKey('Environment', $true)
    if ($null -eq $val) {
        $EnvRegisterKey.DeleteValue($name)
    } else {
        $RegistryValueKind = if ($val.Contains('%')) {
            [Microsoft.Win32.RegistryValueKind]::ExpandString
        } elseif ($EnvRegisterKey.GetValue($name)) {
            $EnvRegisterKey.GetValueKind($name)
        } else {
            [Microsoft.Win32.RegistryValueKind]::String
        }
        $EnvRegisterKey.SetValue($name, $val, $RegistryValueKind)
    }
    Publish-Env
}

function Get-Env {
    param(
        [String] $name,
        [Switch] $global
    )

    $RegisterKey = if ($global) {
        Get-Item -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager'
    } else {
        Get-Item -Path 'HKCU:'
    }

    $EnvRegisterKey = $RegisterKey.OpenSubKey('Environment')
    $RegistryValueOption = [Microsoft.Win32.RegistryValueOptions]::DoNotExpandEnvironmentNames
    $EnvRegisterKey.GetValue($name, $null, $RegistryValueOption)
}

# Not yet
# function Get-TargetTriple() {
#   try {
#     # NOTE: this might return X64 on ARM64 Windows, which is OK since emulation is available.
#     # It works correctly starting in PowerShell Core 7.3 and Windows PowerShell in Win 11 22H2.
#     # Ideally this would just be
#     #   [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture
#     # but that gets a type from the wrong assembly on Windows PowerShell (i.e. not Core)
#     $a = [System.Reflection.Assembly]::LoadWithPartialName("System.Runtime.InteropServices.RuntimeInformation")
#     $t = $a.GetType("System.Runtime.InteropServices.RuntimeInformation")
#     $p = $t.GetProperty("OSArchitecture")
#     # Possible OSArchitecture Values: https://learn.microsoft.com/dotnet/api/system.runtime.interopservices.architecture
#     # Rust supported platforms: https://doc.rust-lang.org/stable/rustc/platform-support.html
#     switch ($p.GetValue($null).ToString())
#     {
#       "X86" { return "i686-pc-windows-msvc" }
#       "X64" { return "x86_64-pc-windows-msvc" }
#       "Arm" { return "thumbv7a-pc-windows-msvc" }
#       "Arm64" { return "aarch64-pc-windows-msvc" }
#     }
#   } catch {
#     # The above was added in .NET 4.7.1, so Windows PowerShell in versions of Windows
#     # prior to Windows 10 v1709 may not have this API.
#     Write-Verbose "Get-TargetTriple: Exception when trying to determine OS architecture."
#     Write-Verbose $_
#   }

#   # This is available in .NET 4.0. We already checked for PS 5, which requires .NET 4.5.
#   Write-Verbose("Get-TargetTriple: falling back to Is64BitOperatingSystem.")
#   if ([System.Environment]::Is64BitOperatingSystem) {
#     return "x86_64-pc-windows-msvc"
#   } else {
#     return "i686-pc-windows-msvc"
#   }
# }

if ($Env:C3_VERSION) {
    $C3Version = $Env:C3_VERSION
}

if ($Env:C3_HOME) {
    $C3Home = $Env:C3_HOME
}

if ($Env:C3_NO_PATH_UPDATE) {
    $NoPathUpdate = $true
}

if ($Env:C3_REPOURL) {
    $C3Repourl = $Env:C3_REPOURL -replace '/$', ''
}

# Repository name
#$ARCH = Get-TargetTriple

# if (-not @("x86_64-pc-windows-msvc", "aarch64-pc-windows-msvc") -contains $ARCH) {
#     throw "ERROR: could not find binaries for this platform ($ARCH)."
# }

# $BINARY = "c3-$ARCH"
$BINARY = "c3-windows"

if ($C3Version -eq 'latest') {
    $DOWNLOAD_URL = "$C3Repourl/releases/latest/download/$BINARY.zip"
} else {
    # Check if version is incorrectly specified without prefix 'v', and prepend 'v' in this case
    $C3Version = "v" + ($C3Version -replace '^v', '')
    $DOWNLOAD_URL = "$C3Repourl/releases/download/$C3Version/$BINARY.zip"
}

$BinDir = $C3Home

Write-Host "This script will automatically download and install C3 ($C3Version) for you."
Write-Host "Getting it from this url: $DOWNLOAD_URL"
Write-Host "The binary will be installed into '$BinDir'"

$TEMP_FILE = [System.IO.Path]::GetTempFileName()

try {
    Invoke-WebRequest -Uri $DOWNLOAD_URL -OutFile $TEMP_FILE

    # Remove previous install
    if (Test-Path -Path $BinDir) {
        Remove-Item -Path $BinDir -Recurse -Force | Out-Null
    }

    $ZIP_FILE = $TEMP_FILE + ".zip"
    Rename-Item -Path $TEMP_FILE -NewName $ZIP_FILE

    # Extract c3 from the downloaded zip file
    Expand-Archive -Path $ZIP_FILE -DestinationPath $Env:USERPROFILE -Force
    Rename-Item -Path "$Env:USERPROFILE/c3-windows-Release" -NewName $BinDir
} catch {
    Write-Host "Error: '$DOWNLOAD_URL' is not available or failed to download"
    exit 1
} finally {
    Remove-Item -Path $ZIP_FILE
}

# Add c3 folder to PATH if the folder is not already in the PATH variable
if (!$NoPathUpdate) {
    $PATH = Get-Env 'PATH'
    if ($PATH -notlike "*$BinDir*") {
        Write-Output "Adding $BinDir to PATH"
        # For future sessions
        Write-Env -name 'PATH' -val "$BinDir;$PATH"
        # For current session
        $Env:PATH = "$BinDir;$PATH"
        Write-Output "You may need to restart your shell"
    } else {
        Write-Output "$BinDir is already in PATH"
    }
} else {
    Write-Output "You may need to update your PATH manually to use c3"
}

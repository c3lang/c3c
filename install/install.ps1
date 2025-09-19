<#
.SYNOPSIS
    C3 install script.
.DESCRIPTION
    This script installs C3 on Windows from the command line.
.PARAMETER C3Version
    Specifies the version of C3 to install.
    Default is 'latest'. Can also be set via environment variable 'C3_VERSION'.
.PARAMETER C3Home
    Specifies C3's installation directory.
    Default is '$Env:USERPROFILE\.c3'. Can also be set via environment variable 'C3_HOME'.
.PARAMETER NoPathUpdate
    If specified, the script will not modify the PATH environment variable.
.PARAMETER C3Repourl
    Specifies the repository URL of C3.
    Default is 'https://github.com/c3lang/c3c'. Can also be set via environment variable 'C3_REPOURL'.
.LINK
    https://c3-lang.org/
.LINK
    https://github.com/c3lang/c3c
#>

# Script parameters with defaults
param (
    [string] $C3Version = 'latest',
    [string] $C3Home = "$Env:USERPROFILE\.c3",
    [switch] $NoPathUpdate,
    [string] $C3Repourl = 'https://github.com/c3lang/c3c'
)

# Enable strict mode for better error handling
Set-StrictMode -Version Latest

# Function to broadcast environment variable changes to Windows system
function Publish-Env {
    # Add P/Invoke type if it does not exist
    if (-not ("Win32.NativeMethods" -as [Type])) {
        Add-Type -Namespace Win32 -Name NativeMethods -MemberDefinition @"
[DllImport("user32.dll", SetLastError = true, CharSet = CharSet.Auto)]
public static extern IntPtr SendMessageTimeout(
    IntPtr hWnd, uint Msg, UIntPtr wParam, string lParam,
    uint fuFlags, uint uTimeout, out UIntPtr lpdwResult);
"@
    }

    # Constants for broadcasting environment changes
    $HWND_BROADCAST = [IntPtr] 0xffff
    $WM_SETTINGCHANGE = 0x1a
    $result = [UIntPtr]::Zero

    # Broadcast the message to all windows
    [Win32.Nativemethods]::SendMessageTimeout($HWND_BROADCAST,
        $WM_SETTINGCHANGE,
        [UIntPtr]::Zero,
        "Environment",
        2,
        5000,
        [ref] $result
    ) | Out-Null
}

# Function to write or update an environment variable in the registry
function Write-Env {
    param(
        [String] $name,
        [String] $val,
        [Switch] $global
    )

    # Determine the registry key based on scope (user or system)
    $RegisterKey = if ($global) {
        Get-Item -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager'
    } else {
        Get-Item -Path 'HKCU:'
    }

    $EnvRegisterKey = $RegisterKey.OpenSubKey('Environment', $true)

    # If value is null, delete the variable
    if ($null -eq $val) {
        $EnvRegisterKey.DeleteValue($name)
    } else {
        # Determine the correct registry value type
        $RegistryValueKind = if ($val.Contains('%')) {
            [Microsoft.Win32.RegistryValueKind]::ExpandString
        } elseif ($EnvRegisterKey.GetValue($name)) {
            $EnvRegisterKey.GetValueKind($name)
        } else {
            [Microsoft.Win32.RegistryValueKind]::String
        }
        $EnvRegisterKey.SetValue($name, $val, $RegistryValueKind)
    }

    # Broadcast the change to the system
    Publish-Env
}

# Function to get an environment variable from the registry
function Get-Env {
    param(
        [String] $name,
        [Switch] $global
    )

    # Determine registry key based on scope
    $RegisterKey = if ($global) {
        Get-Item -Path 'HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager'
    } else {
        Get-Item -Path 'HKCU:'
    }

    $EnvRegisterKey = $RegisterKey.OpenSubKey('Environment')
    $RegistryValueOption = [Microsoft.Win32.RegistryValueOptions]::DoNotExpandEnvironmentNames

    # Retrieve the value without expanding environment variables
    $EnvRegisterKey.GetValue($name, $null, $RegistryValueOption)
}

# Override defaults if environment variables exist
if ($Env:C3_VERSION) { $C3Version = $Env:C3_VERSION }
if ($Env:C3_HOME) { $C3Home = $Env:C3_HOME }
if ($Env:C3_NO_PATH_UPDATE) { $NoPathUpdate = $true }
if ($Env:C3_REPOURL) { $C3Repourl = $Env:C3_REPOURL -replace '/$', '' }

# Set binary name
$BINARY = "c3-windows"

# Determine the download URL based on version
if ($C3Version -eq 'latest') {
    $DOWNLOAD_URL = "$C3Repourl/releases/latest/download/$BINARY.zip"
} else {
    # Ensure version starts with 'v'
    $C3Version = "v" + ($C3Version -replace '^v', '')
    $DOWNLOAD_URL = "$C3Repourl/releases/download/$C3Version/$BINARY.zip"
}

$BinDir = $C3Home

Write-Host "This script will automatically download and install C3 ($C3Version) for you."
Write-Host "Getting it from this url: $DOWNLOAD_URL"
Write-Host "The binary will be installed into '$BinDir'"

# Create temporary file for download
$TEMP_FILE = [System.IO.Path]::GetTempFileName()

try {
    # Download the binary
    Invoke-WebRequest -Uri $DOWNLOAD_URL -OutFile $TEMP_FILE

    # Remove previous installation if it exists
    if (Test-Path -Path $BinDir) {
        Remove-Item -Path $BinDir -Recurse -Force | Out-Null
    }

    # Rename temp file to .zip
    $ZIP_FILE = $TEMP_FILE + ".zip"
    Rename-Item -Path $TEMP_FILE -NewName $ZIP_FILE

    # Extract downloaded zip
    Expand-Archive -Path $ZIP_FILE -DestinationPath $Env:USERPROFILE -Force

    # Rename extracted folder to target installation directory
    Rename-Item -Path "$Env:USERPROFILE/c3-windows-Release" -NewName $BinDir
} catch {
    Write-Host "Error: '$DOWNLOAD_URL' is not available or failed to download"
    exit 1
} finally {
    # Cleanup temporary zip file
    Remove-Item -Path $ZIP_FILE
}

# Update PATH environment variable if requested
if (!$NoPathUpdate) {
    $PATH = Get-Env 'PATH'
    if ($PATH -notlike "*$BinDir*") {
        Write-Output "Adding $BinDir to PATH"

        # Persist PATH for future sessions
        Write-Env -name 'PATH' -val "$BinDir;$PATH"

        # Update PATH for current session
        $Env:PATH = "$BinDir;$PATH"
        Write-Output "You may need to restart your shell"
    } else {
        Write-Output "$BinDir is already in PATH"
    }
} else {
    Write-Output "You may need to update your PATH manually to use c3"
}

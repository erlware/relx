#! /usr/bin/pwsh

param (
    [Parameter(Mandatory=$true, Position=0)]
    [int]$otp_version
)

# Terminate on error
$ErrorActionPreference = "Stop"

# Download erlang
"Download esl-erlang_$otp_version.0~windows_amd64.exe..."
Invoke-WebRequest -Uri "https://packages.erlang-solutions.com/erlang/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_$otp_version.0~windows_amd64.exe" -OutFile "esl-erlang_$otp_version.0~windows_amd64.exe"

# Install erlang (runs in background)
"Installing esl-erlang_$otp_version.0~windows_amd64.exe..."
& ".\esl-erlang_$otp_version.0~windows_amd64.exe" '/S'

# Expected installation folder varies by version
switch($otp_version) {
    19 { $erlroot = "$env:ProgramFiles\erl8.0" }
    20 { $erlroot = "$env:ProgramFiles\erl9.0" }
    21 { $erlroot = "$env:ProgramFiles\erl10.2" }
    22 { $erlroot = "$env:ProgramFiles\erl10.4" }
    Default { $erlroot = "$env:ProgramFiles\erl-$otp_version.0" }
}

# Wait for it..
while (@(Get-Process | Where-Object { $_.name -match 'esl-erlang' }).length -gt 0) {
    Start-Sleep -Milliseconds 100
}

# Update system path
"Add $erlroot\bin to PATH"
$oldpath = (Get-ItemProperty -Path 'Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment' -Name PATH).path
$newpath = "$erlroot\bin;$oldpath"
Set-ItemProperty -Path 'Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment' -Name PATH -Value $newPath

# Cleanup
Remove-Item "esl-erlang_$otp_version.0~windows_amd64.exe"

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
$erlroot = "$PSScriptRoot\erl"
"Installing esl-erlang_$otp_version.0~windows_amd64.exe..."
& ".\esl-erlang_$otp_version.0~windows_amd64.exe" '/S' "/D=$erlroot"

# Wait for it..
while (@(Get-Process | Where-Object { $_.name -match 'esl-erlang' }).length -gt 0) {
    Start-Sleep -Milliseconds 100
}

# Verify installation
if (! (Test-Path "$erlroot\bin\") ) { Write-Error "Erlang NOT installed in $erlroot?!" }
"Erlang installed in $erlroot"

# Verify tools
if (! (Test-Path "$erlroot\bin\erl.exe") )     { Write-Error "Erlang MISSING erl.exe?!" }
if (! (Test-Path "$erlroot\bin\escript.exe") ) { Write-Error "Erlang MISSING escript.exe?!" }

# Set output of step to erlang path
"::set-output name=erlpath::$erlroot"

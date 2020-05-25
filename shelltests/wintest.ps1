#! /usr/bin/pwsh
param (
    [Parameter(Mandatory=$true, Position=0)]
    [string]$erlpath
)

# Test build, install, start, ping, stop, uninstall of powershell_release
$release = "powershell_release"

# Terminate on error
$ErrorActionPreference = "Stop"

# Get ERTS version
# $erts_vsn = & "$erlpath\bin\erl.exe" -boot no_dot_erlang -noshell -eval 'io:format(\"~s\", [erlang:system_info(version)]), halt().'

# Add erlpath to PATH
"*** Add to PATH $erlpath\bin"
$env:PATH = "$env:PATH;$erlpath\bin"
""

# CD to script location (shelltests)
Set-Location $PSScriptRoot

# Clean all builds (continue on error)
"*** Clean"
Get-ChildItem -Path . -Filter _build -Recurse | ForEach-Object { 
    "Remove $($_.FullName).."
    Remove-Item $_.FullName -Recurse -Force -ErrorAction SilentlyContinue
}
""

# Create temporary build folder for rebar3
"*** Build rebar3"
$rebar3_dir = "$PSScriptRoot\$(([System.Guid]::NewGuid()).Guid)"
mkdir $rebar3_dir | Out-Null

# Clone latest rebar3 and build with relx as a checkout
Push-Location $rebar3_dir
& git clone "https://github.com/erlang/rebar3" .
mkdir _checkouts | Out-Null
New-Item -ItemType SymbolicLink -Path "_checkouts\relx" -Target "$PSScriptRoot\..\..\relx" | Out-Null
(Get-Content rebar.config) -replace 'relx(.*)build/default/lib/', 'relx$1checkouts' | Set-Content rebar.config -Encoding ASCII
cmd /c bootstrap.bat
Pop-Location
""

# Function to run rebar3
function Rebar() {
    & $erlpath\bin\escript.exe "$rebar3_dir\rebar3" @args
    if ($LASTEXITCODE -ne 0) {
        Write-Error "rebar3 ${args} exited with status $LASTEXITCODE"
    }
}

# Our release source
Set-Location ".\$release\"

"*** Build release"
Rebar release
""

"*** Rebuild dev release (test for symlink issues)"
Rebar as dev release
Rebar as dev release
""

# Go to release
Set-Location "_build\default\rel\$release\bin"

"*** Install service"
& ".\$release.ps1" install
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to install service"
}
""

"*** Start service"
& ".\$release.ps1" start
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to start service"
}
""

"*** Ping service"
& ".\$release.ps1" ping
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to ping service"
}
""

"*** Stop service"
& ".\$release.ps1" stop
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to stop service"
}
""

"*** Uninstall service"
& ".\$release.ps1" uninstall
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to uninstall service"
}
""

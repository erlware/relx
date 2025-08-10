#! /usr/bin/pwsh
# Test build, install, start, ping, stop, uninstall of powershell_release
$release = "powershell_release"

# Terminate on error
$ErrorActionPreference = "Stop"

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
Remove-Item -Path "vendor\relx\src\*" -Recurse -Force
Remove-Item -Path "vendor\relx\priv\*" -Recurse -Force
Remove-Item -Path "vendor\relx\rebar*" -Force
Copy-Item -Path "$PSScriptRoot\..\..\relx\src\*" -Destination "vendor\relx\src" -Recurse | Out-Null
Copy-Item -Path "$PSScriptRoot\..\..\relx\priv\*" -Destination "vendor\relx\priv" -Recurse | Out-Null
Copy-Item -Path "$PSScriptRoot\..\..\relx\rebar*" -Destination "vendor\relx" -Recurse #| Out-Null
cmd /c bootstrap.bat
Pop-Location
""

# Function to run rebar3
function Rebar() {
    & escript.exe "$rebar3_dir\rebar3" @args
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

Get-Process | Out-String -Stream
Get-Service -Name powershell_release_0.1.0 | Out-String -Stream

"*** Start service"
& ".\$release.ps1" start
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to start service"
}
""

Get-Process | Out-String -Stream
Get-Service -Name powershell_release_0.1.0 | Out-String -Stream

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

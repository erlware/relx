:: A script to build relx on Windows
:: Requires rebar3

:: Get dependencies, compile and escriptize relx
@cmd /c @rebar3 escriptize

:: Create a shortcut file for running the relx command
@set relx_cmd=relx.cmd
@echo @echo off> %relx_cmd%
@echo setlocal>> %relx_cmd%
@echo set relx=%%~f0>> %relx_cmd%
@echo escript ^"%%relx:.cmd=%%^" %%*>> %relx_cmd%

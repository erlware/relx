%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
{incl_app, relx}.
{level, details}.
{incl_dirs_r, ["src", "test"]}.
{excl_mods, [bin_dtl, erl_script_dtl, extended_bin_dtl,
                   extended_bin_windows_dtl, erl_ini_dtl,
                   bin_windows_dtl, nodetool_dtl,
                   install_upgrade_escript_dtl, nodetool_dtl,
                   sys_config_dtl, vm_args_dtl, relx]}.

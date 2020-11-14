%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2019. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
%% This is an -*- erlang -*- file.
%%
{application, kernel,
 [
  {description, "ERTS  CXC 138 10"},
  {vsn, "7.1-test"},
  {modules, [application]},
  {registered, [application_controller,
		erl_reply,
		auth,
		boot_server,
		code_server,
		disk_log_server,
		disk_log_sup,
		erl_prim_loader,
		error_logger,
		file_server_2,
		fixtable_server,
		global_group,
		global_name_server,
		heart,
		init,
		kernel_config,
		kernel_refc,
		kernel_sup,
                logger,
                logger_handler_watcher,
                logger_sup,
		net_kernel,
		net_sup,
		rex,
		user,
	        os_server,
                ddll_server,
                erl_epmd,
                inet_db,
                pg,
                pg2]},
  {applications, []},
  {env, [{logger_level, notice},
         {logger_sasl_compatible, false},
         {shell_docs_ansi,auto}
        ]},
  {mod, {kernel, []}},
  {runtime_dependencies, ["erts-11.0", "stdlib-3.13", "sasl-3.0"]}
 ]
}.

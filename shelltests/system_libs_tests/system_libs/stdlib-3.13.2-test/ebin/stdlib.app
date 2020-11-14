%% This is an -*- erlang -*- file.
%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2020. All Rights Reserved.
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
{application, stdlib,
 [{description, "ERTS  CXC 138 10"},
  {vsn, "3.13.2-test"},
  {modules, [array]},
  {registered,[timer_server,rsh_starter,take_over_monitor,pool_master,
               dets]},
  {applications, [kernel]},
  {env, []},
  {runtime_dependencies, ["sasl-3.0","kernel-7.0","erts-11.0","crypto-3.3",
			  "compiler-5.0"]}
]}.

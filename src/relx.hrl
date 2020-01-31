%% Copyright 2012 Erlware, LLC. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-include_lib("erlware_commons/include/ec_cmd_log.hrl").

-define(RLX_ERROR, ?EC_ERROR).
-define(RLX_WARN, ?EC_WARN).
-define(RLX_INFO, ?EC_INFO).
-define(RLX_DEBUG, ?EC_DEBUG).

%% This is the default form of error messages for the Relx
%% system. It is expected that everything that returns an error use
%% this and that they all expose a format_error/1 message that returns
%% an iolist.
-define(RLX_ERROR(Reason), {error, {?MODULE, Reason}}).

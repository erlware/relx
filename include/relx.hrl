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

-define(RCL_ERROR, 0).
-define(RCL_INFO, 1).
-define(RCL_DEBUG, 2).

%% This is the default form of error messages for the Relcool
%% system. It is expected that everything that returns an error use
%% this and that they all expose a format_error/1 message that returns
%% an iolist.
-define(RCL_ERROR(Reason),
        {error, {?MODULE, Reason}}).

%% logging macros
-define(log_debug(Msg, State), rlx_log:log(debug, Msg, [], State)).
-define(log_warn(Msg, State), rlx_log:log(warn, Msg, [], State)).
-define(log_error(Msg, State), rlx_log:log(error, Msg, [], State)).
-define(log_info(Msg, State), rlx_log:log(info, Msg, [], State)).

-define(log_debug(Msg, Args, State), rlx_log:log(debug, Msg, Args, State)).
-define(log_warn(Msg, Args, State), rlx_log:log(warn, Msg, Args, State)).
-define(log_error(Msg, Args, State), rlx_log:log(error, Msg, Args, State)).
-define(log_info(Msg, Args, State), rlx_log:log(info, Msg, Args, State)).

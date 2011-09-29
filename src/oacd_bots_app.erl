-module(oacd_bots_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    oacd_bots_sup:start_link().

stop(_State) ->
    ok.

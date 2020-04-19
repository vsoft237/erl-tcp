%%%-------------------------------------------------------------------
%% @doc erl-tcp public API
%% @end
%%%-------------------------------------------------------------------

-module(tcp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tcp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

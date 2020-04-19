%%%-------------------------------------------------------------------
%% @doc erl-tcp public API
%% @end
%%%-------------------------------------------------------------------

-module(tcp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Reply = tcp_sup:start_link(),
    tcp_interface:start(),
    Reply.

stop(_State) ->
    ok.

%% internal functions

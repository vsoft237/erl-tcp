%%%-------------------------------------------------------------------
%% @doc erl-tcp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tcp_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(LSock) ->
    supervisor:start_child(?SERVER, [LSock]).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 10},

    TLS = #{
        id => tcp_listener_sup,
        start => {tcp_listener_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [tcp_listener_sup]
    },

    TAS = #{
        id => tcp_acceptor_sup,
        start => {tcp_acceptor_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [tcp_acceptor_sup]
    },

    ChildSpecs = [TLS, TAS],

    {ok, {SupFlags, ChildSpecs}}.

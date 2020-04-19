-module(tcp_listener_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(PortCfg) ->
    supervisor:start_child(?SERVER, PortCfg).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 10},

    Server = #{
        id => tcp_listener_server,
        start => {tcp_listener_server, start_link, []},
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [tcp_listener_server]
    },

    ChildSpecs = [Server],

    {ok, {SupFlags, ChildSpecs}}.
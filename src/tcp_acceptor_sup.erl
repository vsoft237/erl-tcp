-module(tcp_acceptor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(LSock, Type, Ssl) ->
    supervisor:start_child(?SERVER, [LSock, Type, Ssl]).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one, intensity => 10, period => 10},

    Server = #{
        id => tcp_acceptor_server,
        start => {tcp_acceptor_server, start_link, []},
        restart => temporary,
        shutdown => brutal_kill,
        type => worker,
        modules => [tcp_acceptor_server]
    },

    ChildSpecs = [Server],

    {ok, {SupFlags, ChildSpecs}}.

%% @author ZhaoJiqiong
%% @doc @todo Add description to tcp_interface.


-module(tcp_interface).

-define(DEFAULT_PORTS, [[20001, normal, false]]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

start() ->
	{ok, Data} = application:get_env(tcp, ports),
	Ports = get_ports(Data, []),
	start(Ports).

start([Port|T]) ->
	tcp_listener_sup:start_child(Port),
	start(T);
start([]) ->
	ok.

get_ports([H | T], Loop) ->
	{_,[{_, Port},{_, Type},{_, Ssl}]} = H,
	get_ports(T, [[Port, Type, Ssl]|Loop]);
get_ports([], Loop) ->
	case Loop of
		[] ->
			?DEFAULT_PORTS;
		_ ->
			Loop
	end.

%%-----------------------------------------



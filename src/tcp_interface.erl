%% @author ZhaoJiqiong
%% @doc @todo Add description to tcp_interface.


-module(tcp_interface).

-define(TCP_PATH, "priv/config/tcp.config").

-define(DEFAULT_PORTS, [[20001, normal]]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

start() ->
%%	{ok, [{tcp, Data}]} = file:consult(?TCP_PATH),

	{ok, Data} = application:get_env(eos, tcp),
	Ports = get_ports(Data),
	start(Ports).

start([Port|T]) ->
	tcp_listener_sup:start_child(Port),
	start(T);
start([]) ->
	ok.

get_ports([{Type, Value} | T]) ->
	case Type of
		ports ->
			Value;
		_ ->
			get_ports(T)
	end;
get_ports([]) ->
	?DEFAULT_PORTS.

%%-----------------------------------------



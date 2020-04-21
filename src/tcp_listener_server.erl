%% @author YouShuxiang
%% @doc @todo Add description to tcp_listener_server.


-module(tcp_listener_server).
-behaviour(gen_server).

-define(TCP_LISTEN_OPTION, [
	{mode, binary},
    {packet, 0},
    {active, false},
    {reuseaddr, true},
    {nodelay, true},
    {delay_send, true},
    {exit_on_close, false},
    {send_timeout, 10000},
    {keepalive, true},
    {send_timeout_close, true}
]).


-define(TCP_SSL_LISTEN_OPTION, [
    {mode, binary},
    {packet, 0},
    {active, false},
	{verify, verify_none},
    {certfile, "priv/ssl/ssl.crt"},
    {keyfile, "priv/ssl/ssl.key"}
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {port}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/3]).

start_link(Port, Type, Ssl) ->
    gen_server:start_link(?MODULE, [Port, Type, Ssl], []).

%% init/1
%% ====================================================================
init([Port, Type, Ssl]) ->
	Option = get_listen_option(Ssl),
	TcpFun = tcp_fun(Ssl),
	case TcpFun:listen(Port, Option) of
		{ok, LSock} ->
			tcp_acceptor_sup:start_child(LSock, Type, Ssl),
			{ok, #state{port = Port}};
		{error, _Reason} ->
			{stop, listen_failure, state}
	end.

%% handle_call/3
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

get_listen_option(Ssl) ->
    case Ssl of
		true ->
			?TCP_SSL_LISTEN_OPTION;
		_ ->
			?TCP_LISTEN_OPTION
	end.

tcp_fun(Ssl) ->
	case Ssl of
		true ->
			ssl;
		false ->
			gen_tcp
	end.



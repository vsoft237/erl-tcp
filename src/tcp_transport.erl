-module(tcp_transport).

%% ===========================API============================
-export([tcp_fun/1]).

tcp_fun(Ssl) ->
	case Ssl of
		true ->
			ssl;
		_ ->
			gen_tcp
	end.


%% ===========================Internal============================


-module(tcp_response).

%% ===========================API============================
-export([send/2, broadcast/2]).

send(SockPid, Bin) ->
	Msg = {tcp_send, Bin},
	SockPid ! Msg.

broadcast(List, Bin) ->
	lists:foreach(fun(X) ->
		X ! Bin	
	end, List).


%% ===========================Internal============================


%% @author YouShuxiang
%% @doc @todo Add description to packet.

-module(tcp_receiver).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/4]).

start(Socket, ConnectPid, Type, Ssl) ->
	TcpFun = tcp_transport:tcp_fun(Ssl),
	Fun =
	fun () ->
		loop(Socket, ConnectPid, Type, TcpFun)
	end,	
 	erlang:spawn_link(Fun).	


%% ====================================================================
%% Internal functions
%% ====================================================================

loop(Socket, ConnectPid, Type, TcpFun) ->
	case handle_packet(Socket, ConnectPid, Type, TcpFun) of
		ok ->
			loop(Socket, ConnectPid, Type, TcpFun);
		{error, _Reason} ->			
			TcpFun:close(Socket)
	end.

handle_packet(Socket, ConnectPid, Type, TcpFun) ->
	case handle_head(Socket, Type, TcpFun) of
		{ok, Args} ->
			send_data(ConnectPid, Args);
		ignore ->
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

handle_head(Socket, Type, TcpFun) ->	
	case Type of
		web ->
			handle_web_head(Socket, TcpFun);
		_ ->
			handle_normal_head(Socket, TcpFun)
	end.

handle_normal_head(Socket, TcpFun) ->
	Len = application:get_env(tcp, data_len),
	case TcpFun:recv(Socket, Len) of
		{ok, Bin} ->
			Bl = Len * 8,
			<<DataLen:Bl>> = Bin,
			handle_normal_head_1(Socket, TcpFun, DataLen);
		Error ->
			Error
	end.

handle_normal_head_1(Socket, TcpFun, Len) ->
	case TcpFun:recv(Socket, Len) of
		{ok, Bin} ->
			packet:unpack(Bin);
		Error ->
			Error
	end.

handle_web_head(Socket, TcpFun) ->
	case handle_web_head_0(Socket, TcpFun) of
		{ok, Bin, Opcode} ->
			packet:unpack_web(Bin, Opcode);
		Error ->
			Error
	end.

handle_web_head_0(Socket, TcpFun) ->
	case TcpFun:recv(Socket, 2) of
		{ok, Bin} ->
			<<_Fin:1, _Rsv:3, Opcode:4, _Mask:1, Len:7>> = Bin,
			case Opcode of
				Opcode when Opcode == 1 orelse Opcode == 2 ->
					handle_web_head_1(Socket, Opcode, Len, TcpFun);
				_ ->
					ignore
			end;
		Error ->
			Error
	end.

handle_web_head_1(Socket, Opcode, Len, TcpFun) ->
	case get_web_data_length(Socket, Len, TcpFun) of
		{ok, DataLen} ->
			handle_web_head_2(Socket, DataLen, Opcode, TcpFun);
		Error ->
			Error
	end.

get_web_data_length(Socket, Len, TcpFun) ->
	case Len of
		126 ->
			get_web_data_length_1(Socket, 2, TcpFun);
		127 ->
			get_web_data_length_1(Socket, 8, TcpFun);
		_ ->
			{ok, Len}
	end.

get_web_data_length_1(Socket, Len, TcpFun) ->
	case TcpFun:recv(Socket, Len) of
		{ok, Bin} ->
			BL = Len * 8,
			<<DataLen:BL>> = Bin,
			{ok, DataLen};
		Error ->
			Error
	end.

handle_web_head_2(Socket, Len, Opcode, TcpFun) ->
	case TcpFun:recv(Socket, Len+4) of
		{ok, Bin} ->
			NewBin = web_packet:decode(Bin, Len),
			{ok, NewBin, Opcode};
		Error ->
			Error
	end.

send_data(ConnectPid, Args) ->
	send_to_parent(ConnectPid, Args),
	ok.

send_to_parent(PID, Args) ->
    Msg = {data, Args},
    PID ! Msg.


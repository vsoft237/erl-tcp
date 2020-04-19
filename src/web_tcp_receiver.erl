%% @author YouShuxiang
%% @doc @todo Add description to packet.


-module(web_tcp_receiver).

-include("packet.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2]).

start(Socket, ConnectPid) ->
	Fun =
	fun () ->
		loop(Socket, ConnectPid)
	end,	
 	erlang:spawn_link(Fun).	


%% ====================================================================
%% Internal functions
%% ====================================================================

loop(Socket, ConnectPid) ->
	case handle_packet(Socket, ConnectPid) of
		ok ->
			loop(Socket, ConnectPid);
		ignore ->
			loop(Socket, ConnectPid);
		{error, _Reason} ->
%%            ?DEBUG("tcp close: ~p~n", [Reason]),
			gen_tcp:close(Socket)
	end.

handle_packet(Socket, ConnectPid) ->
	case handle_head(Socket) of
		{ok, Args} ->
			handle_data(Socket, Args, ConnectPid);
		{web_json, Data} ->
			ConnectPid ! {web_json, Data},
			ok;
		ignore ->
			ignore;
		{error, Reason} ->
			{error, Reason}
	end.

handle_head(Socket) ->
	case gen_tcp:recv(Socket, 2) of
		{ok, Bin} ->
			<<_Fin:1, _Rsv:3, Opcode:4, _Mask:1, Len:7>> = Bin,
%%			?DEBUG("Fin:~p, Rsv:~p, Opcode:~p, Mask:~p, Len:~p~n", [Fin, Rsv, Opcode, Mask, Len]),
			DataLenth = get_data_lenth(Socket, Len),
			HeadBin = handle_head_1(Socket, DataLenth),
%%			?DEBUG("DataBin: ~p~n", [HeadBin]),
			case Opcode of
				2 ->
%%                    <<Cmd:16, Data/binary>> = HeadBin,
%%                    ?DEBUG("Cmd: ~p~n", [Cmd]);
					case HeadBin of
						<<Cmd:16, Crc:16, Seq:32, Data/binary>> ->
							{ok, {Cmd, Crc, Seq, Data}};
						_ ->
							{error, cannot_read_data}
					end;
				1 ->
					{web_json, HeadBin};
				_ ->
					ignore
			end;
		{error, Reason} ->
%%			?DEBUG("socket error: ~p~n", [Reason]),
			{error, Reason}
	end.

handle_head_1(Socket, Len) ->
	case gen_tcp:recv(Socket, Len+4) of
		{ok, Bin} ->
%%			?DEBUG("Bin: ~p~n", [Bin]),
			<<Masking:4/binary, Payload:Len/binary>> = Bin,
			%%	<<Masking:4/binary, Payload:Len/binary, Next/binary>> = Rest,
			HeadBin = unmask(Payload, Masking),
			HeadBin;
		{error, Reason} ->
			{error, Reason}
	end.

get_data_lenth(Socket, Len) ->
	case Len of
		126 ->
			do_get_data_lenth(Socket, 2);
		127 ->
			do_get_data_lenth(Socket, 8);
		_ ->
			Len
	end.

do_get_data_lenth(Socket, Len) ->
	case gen_tcp:recv(Socket, Len) of
		{ok, Bin} ->
			Bl = Len * 8,
			<<DataLen:Bl>> = Bin,
			DataLen;
		{error, Reason} ->
%%			?DEBUG("socket error: ~p~n", [Reason]),
			{error, Reason}
	end.


handle_data(_Socket, Args, ConnectPid) ->
	{Cmd, _Crc, _Seq, DataBin} = Args,
%%	?DEBUG("Cmd = ~p, DataSize = ~p~n", [Cmd, DataSize]),
	unpack_data(ConnectPid, Cmd, DataBin).

unpack_data(ConnectPid, Cmd, DataBin) ->
	case catch packet:unpack(Cmd, 0, DataBin) of
		{ok, Cmd, Data, Module} ->
            send_to_parent(ConnectPid, Cmd, Data, Module),
			ok;
		_Error ->
            ignore
	end.

send_to_parent(PID, Cmd, Data, Module) ->
    Msg = {data, {Cmd, Data, Module}},
    PID ! Msg.

%%
unmask(Payload, Masking) ->
	unmask(Payload, Masking, <<>>).

unmask(Payload, <<MA:8, MB:8, MC:8, MD:8>> = Masking, Acc) ->
	case size(Payload) of
		0 ->
			Acc;
		1 ->
			<<A:8>> = Payload,
			<<Acc/binary, (MA bxor A)>>;
		2 ->
			<<A:8, B:8>> = Payload,
			<<Acc/binary, (MA bxor A), (MB bxor B)>>;
		3 ->
			<<A:8, B:8, C:8>> = Payload,
			<<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
		_Other ->
			<<A:8, B:8, C:8, D:8, Rest/binary>> = Payload,
			Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
			unmask(Rest, Masking, Acc1)
	end.
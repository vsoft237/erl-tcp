%% @author YouShuxiang
%% @doc @todo Add description to packet.


-module(tcp_receiver).

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
			gen_tcp:close(Socket)
	end.

handle_packet(Socket, ConnectPid) ->
	case handle_head(Socket) of
		{ok, Args} ->
			handle_data(Socket, Args, ConnectPid);
		ignore ->
			ignore;
		{error, Reason} ->
			{error, Reason}
	end.

handle_head(Socket) ->	
	case gen_tcp:recv(Socket, ?PACK_HEAD_LENGTH) of
		{ok, Bin} ->
			<<DataSize:32, DataStatus:8, Flag:32, Cmd:16>> = Bin,
			{ok, {Cmd, DataSize, DataStatus, Flag}};
		{error, Reason} ->
			{error, Reason}
	end.

handle_data(Socket, Args, ConnectPid) ->
	{Cmd, DataSize, DateStatus, Flag} = Args,
	case DataSize of
		0 ->
            send_to_parent(ConnectPid, Cmd, null, Flag),
			ok;
		_ ->
			case gen_tcp:recv(Socket, DataSize) of
				{ok, DataBin} ->
                    unpack_data(ConnectPid, Cmd, DateStatus, DataBin, Flag);
				{error, Reason} ->
					{error, Reason}
			end
	end.

unpack_data(ConnectPid, Cmd, DataStatus, DataBin, Flag) ->
	case catch packet:unpack(Cmd, DataStatus, DataBin) of
		{ok, Cmd, Data} ->
            send_to_parent(ConnectPid, Cmd, Data, Flag),
			ok;
		_Error ->
            ignore
	end.

send_to_parent(PID, Cmd, Data, Flag) ->
    Msg = {data, {Cmd, Data, Flag}},
    PID ! Msg.
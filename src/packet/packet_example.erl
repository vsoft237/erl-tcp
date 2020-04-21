%% =========================================================
%% 使用说明
%%  
%% =========================================================
-module(packet_example).


%% ===========================API============================
-export([unpack/1, unpack_web/2]).
-export([pack_data/1, pack_error/1]).

unpack(Bin) ->
	<<Cmd:16, DataBin/binary>> = Bin,
	Data = protobuf_packet:decode_msg(Cmd, DataBin),
	{ok, {Cmd, Data}}.

unpack_web(Bin, Opcode) ->
	case Opcode of
		2 ->
			unpack(Bin);
		1 ->
			other_pack
	end.

pack_data(Args) ->
	{Cmd, Data} = Args,
	DataBin = protobuf_packet:encode(Data),
	<<0:16, Cmd:16, DataBin/binary>>.

pack_error(Args) ->
	{Cmd, ErrorCode} = Args,
	<<ErrorCode:16, Cmd:16>>. 

%% ===========================Internal============================


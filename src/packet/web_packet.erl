%% @author YouShuxiang
%% @doc @todo Add description to packet.


-module(web_packet).

%% ====================================================================
%% API functions
%% ====================================================================
-export([encode/1, encode/2]).
-export([decode/2]).

encode(Bin) ->
	encode(Bin, 2).

encode(Bin, OpCode) ->
	DataSize = erlang:byte_size(Bin),
	{Len, ExLen} =
		case DataSize of
			D when D < 126 ->
				{DataSize, 0};
			D when D < 65536 ->
				{126, D};
			_ ->
				{127, DataSize}
		end,
	case ExLen of
		0 ->
			<<1:1, 0:3, OpCode:4, 0:1, Len:7, Bin/binary>>;
		E when E > 65535 ->
			<<1:1, 0:3, OpCode:4, 0:1, Len:7, ExLen:64, Bin/binary>>;
		_ ->
			<<1:1, 0:3, OpCode:4, 0:1, Len:7, ExLen:16, Bin/binary>>
	end.

decode(Bin, Len) ->
	<<Masking:4/binary, Payload:Len/binary>> = Bin,
	unmask(Payload, Masking).

%% ====================================================================
%% Internal functions
%% ====================================================================
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




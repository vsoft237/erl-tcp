%% @author YouShuxiang
%% @doc @todo Add description to packet.


-module(web_packet).

-include("packet.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([pack_web_bin/1, pack_web_bin/2]).

pack_web_bin(BB) ->
	DataSize = erlang:byte_size(BB),
	{Len, ExLen} =
		case DataSize of
			D when D < 126 ->
				{DataSize, 0};
			D when D < 65536 ->
				{126, D};
			_ ->
				{127, DataSize}
		end,
	Frame =
		case ExLen of
			0 ->
				<<1:1, 0:3, 2:4, 0:1, Len:7, BB/binary>>;
			E when E > 65535 ->
				<<1:1, 0:3, 2:4, 0:1, Len:7, ExLen:64, BB/binary>>;
			_ ->
				<<1:1, 0:3, 2:4, 0:1, Len:7, ExLen:16, BB/binary>>
		end,
	Frame.

pack_web_bin(BB, OpCode) ->
	DataSize = erlang:byte_size(BB),
	{Len, ExLen} =
		case DataSize of
			D when D < 126 ->
				{DataSize, 0};
			D when D < 65536 ->
				{126, D};
			_ ->
				{127, DataSize}
		end,
	Frame =
		case ExLen of
			0 ->
				<<1:1, 0:3, OpCode:4, 0:1, Len:7, BB/binary>>;
			E when E > 65535 ->
				<<1:1, 0:3, OpCode:4, 0:1, Len:7, ExLen:64, BB/binary>>;
			_ ->
				<<1:1, 0:3, OpCode:4, 0:1, Len:7, ExLen:16, BB/binary>>
		end,
	Frame.

%% ====================================================================
%% Internal functions
%% ====================================================================




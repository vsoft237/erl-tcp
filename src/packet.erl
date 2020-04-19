%% @author YouShuxiang
%% @doc @todo Add description to packet.


-module(packet).

-include("packet.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([unpack/3, pack/3, pack_error/2, pack_client/2]).
-export([get_data_status/1]).

get_data_status(Atom) ->
	case Atom of
		raw ->
			?RAW_DATA;
		compression ->
			?COMPRESSION_DATA;
		encryption ->
			?ENCRYPTION_DATA
	end.

pack(Cmd, DateStatus, Data) ->
	case Data of
        null ->
            pack_empty(Cmd);
        _ ->
            pack_proto(Cmd, DateStatus, Data)
    end.

pack_empty(Cmd) ->
    BB = <<Cmd:16>>,
    {ok, BB}.

pack_proto(Cmd, DateStatus, Data) ->
	Bin = all_pb:encode_msg(Data),
	NewBin = handle_data_out(DateStatus, Bin),
	BB = <<Cmd:16, NewBin/binary>>,
	{ok, BB}.

unpack(Cmd, DataStatus, Bin) ->
	NewBin = handl_data_in(DataStatus, Bin),
	{ProtoName, Module} = data_proto_data:get(Cmd),
	Data = all_pb:decode_msg(NewBin, ProtoName),
	{ok, Cmd, Data, Module}.

pack_error(Cmd, ErrorID) ->
	DataSize = ?EMPTY_DATA_SIZE,
	DataStatus = ?RAW_DATA,
	BB = <<ErrorID:16, DataSize:16, DataStatus:8, Cmd:16>>,
	{ok, BB}.

pack_client(Cmd, Data) ->
	String = "encode_m_" ++ erlang:integer_to_list(Cmd) ++ "_tos",
	Fun = lists2:list_to_atom(String),
	PbList = all_pb:Fun(Data),
	Bin = erlang:iolist_to_binary(PbList),
	DataSize = erlang:byte_size(Bin),
	BB = <<DataSize:16, 0:8, Cmd:16, Bin/binary>>,
	{ok, BB}.

%% ====================================================================
%% Internal functions
%% ====================================================================


handle_data_out(DataStatus, Data) ->
	case DataStatus of
		?RAW_DATA ->
			Data;
		?COMPRESSION_DATA->
			compress_data(Data);
		?ENCRYPTION_DATA ->
			encrypt_data(Data)
	end.

%% 压缩数据
compress_data(Data) ->
	zlib:compress(Data).

%% 加密数据
encrypt_data(Data) ->
	%% TODO 未实现
	Data.

%% ====================================================================

handl_data_in(DataStatus, Data) ->
	case DataStatus of
		?RAW_DATA ->
			Data;
		?COMPRESSION_DATA->
			uncompress_data(Data);
		?ENCRYPTION_DATA ->
			decrypt_data(Data)
	end.

%% 解压数据
uncompress_data(Data) ->
	zlib:uncompress(Data).

%% 解密密数据
decrypt_data(Data) ->
	%% TODO 未实现
	Data.


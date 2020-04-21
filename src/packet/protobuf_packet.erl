-module(protobuf_packet).

%% ===========================API============================
-export([encode/1, decode/2]).

encode(Data) ->
	all_pb:encode_msg(Data).

decode(Cmd, Bin) ->
	S = lists:concat(["m_", Cmd, "_tos"]),
	ProtoName = to_atom(S),
	all_pb:decode_msg(Bin, ProtoName).

%% ===========================Internal============================

to_atom(List) ->
	case catch(list_to_existing_atom(List)) of
        {'EXIT', _} -> 
            erlang:list_to_atom(List);
        Atom -> 
            Atom
    end.

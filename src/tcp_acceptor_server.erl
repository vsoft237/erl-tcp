-module(tcp_acceptor_server).

-behaviour(gen_server).

-include("tcp.hrl").

-define(HEARBEAT_TIMEOUT, (30000)). %% 客户端心跳包时间

-export([start/2, start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

start(LSock, Type) ->
	gen_server:start(?MODULE, [LSock, Type], []).

start_link(LSock, Type) ->
	gen_server:start_link(?MODULE, [LSock, Type], []).

init([LSock, Type]) ->
	process_flag(trap_exit, true),
	Self = self(),
	gen_server:cast(Self, start_accept),
	{ok, #tcp_state{lsock = LSock, type = Type}}.

%% call
handle_call(_Msg, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%% cast
handle_cast(start_accept, State) ->
	NewState = start_accept(State),	
	{noreply, NewState};

handle_cast(_Msg, State) ->
	{noreply, State}.

%% =================================================================
%% info
%% =================================================================

%% 接受数据
handle_info({data, {Cmd, Data, Module}}, State) ->
%%	?DEBUG("====receiv data==== Cmd:~p, Data:~p, Module:~p~n", [Cmd, Data, Module]),
	router:rout_msg(Cmd, Data, Module, State),
	{noreply, State};

%% info
handle_info({web_json, Json}, State) ->
	router:rout_json(Json, State),
	{noreply, State};

%% 发数据
handle_info({tcp_send, Bin}, State) ->
    #tcp_state{type = Type, socket = Socket} = State,
	NewBin =
	case Type of
		normal ->
			Bin;
		_ ->
			web_packet:pack_web_bin(Bin)
	end,
	case Type of
		ssl ->
			ssl:send(Socket, NewBin);
		_ ->
			gen_tcp:send(Socket, NewBin)
	end,
	{noreply, State};

handle_info({tcp_json_send, Bin}, State) ->
	#tcp_state{type = Type, socket = Socket} = State,
	case Type of
		web ->
			NewBin = web_packet:pack_web_bin(Bin, 1),
			gen_tcp:send(Socket, NewBin);
		ssl ->
			NewBin = web_packet:pack_web_bin(Bin, 1),
%%			lager:info("tcp_ssl_json_send:~p~n", [NewBin]),
			ssl:send(Socket, NewBin);
		_ ->
			ok
	end,
	{noreply, State};

%% Web Socket 首次接入
handle_info({Type, Socket, HeaderData}, State) ->
	HeaderList = binary:split(HeaderData, <<"\r\n">>, [global]),
	HeaderList1 = [list_to_tuple(binary:split(Header, <<": ">>)) || Header <- HeaderList],
    case lists:keyfind(<<"Sec-WebSocket-Key">>, 1, HeaderList1) of
        false ->
            {stop, normal, State};
        {_, SecWebSocketKey} ->
            Sha1 = crypto:hash(sha, [SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
            Base64 = base64:encode(Sha1),
            Handshake = [
                <<"HTTP/1.1 101 Switching Protocols\r\n">>,
                <<"Upgrade: websocket\r\n">>,
                <<"Connection: Upgrade\r\n">>,
                <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
                <<"\r\n">>
            ],
			case Type of
				ssl ->
					ssl:send(Socket, Handshake),
					RecvPid = web_ssl_receiver:start(Socket, self()),
					ssl:controlling_process(Socket, RecvPid);
				_ ->
					gen_tcp:send(Socket, Handshake),
					RecvPid = web_tcp_receiver:start(Socket, self()),
					gen_tcp:controlling_process(Socket, RecvPid)
			end,
			UserAgent = get_user_agent(HeaderList1),
            NewState = State#tcp_state{
                receiver_pid = RecvPid,
				user_agent = UserAgent
            },
            {noreply, NewState}
    end;



handle_info(exit_without_connect, State) ->
	{stop, normal, State};

handle_info({'EXIT', _Pid, _Reason}, State) ->
	#tcp_state{
	   receiver_pid = RecvPid
	  } = State,
	stop_receiver(RecvPid),	
	{stop, normal, State};

handle_info(timeout, State) ->
	%% TODO 客户端心跳检测逻辑实现
	{stop, hearbeat_timeout, State};

%% 保存玩家PID
handle_info({save_player_pid, RolePID}, State = #tcp_state{player_pid = OldRolePid}) ->
	catch OldRolePid ! {'EXIT', self(), exit},
	catch unlink(OldRolePid),
	link(RolePID),
	{noreply, State#tcp_state{player_pid = RolePID}};

handle_info(kill, State) ->
	{stop, normal, State};

handle_info(_Info, State) ->
	{noreply, State}.

%% terminate
terminate(_Reason, State) ->
	#tcp_state{
	   receiver_pid = RecvPid
    } = State,
	stop_receiver(RecvPid),
	ok.

%% code change
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%------------------------------------------------------
%% Internal functions
%%------------------------------------------------------



%% 创建接收socket
start_accept(#tcp_state{lsock = LSock, type = Type = ssl} = State) ->
	case ssl:transport_accept(LSock) of
		{ok, Socket} ->
			tcp_acceptor_sup:start_child(LSock, Type),
			NewState = ssl_get_ip_addr(Socket, State),
			ssl_connection:handshake(Socket, infinity),
			ssl:setopts(Socket, [{active, once}]),
			NewState;
		_ ->
			State
	end;
start_accept(#tcp_state{lsock = LSock, type = Type} = State) ->
	case gen_tcp:accept(LSock) of
		{ok, Socket} ->
			tcp_acceptor_sup:start_child(LSock, Type),
            NewState = get_ip_addr(Socket, State),
            case Type of
                normal ->
                    RecvPid = tcp_receiver:start(Socket, self()),
                    gen_tcp:controlling_process(Socket, RecvPid),
                    NewState#tcp_state{
                        receiver_pid = RecvPid
                    };
                web ->
                    inet:setopts(Socket, [{active, once}]),
                    NewState
            end;
		_ ->
			State
	end.

%% 获取客户端ip地址
get_ip_addr(Socket, State) ->	
	case inet:peername(Socket) of 
		{ok, {Address, Port}} ->
			State#tcp_state{socket = Socket, ip = inet:ntoa(Address), port = Port};
		{error, _NetErr} ->
			State#tcp_state{socket = Socket}
	end.
ssl_get_ip_addr(Socket,  State) ->
	case ssl:peername(Socket)  of
		{ok, {Address, Port}} ->
			State#tcp_state{socket = Socket, ip = inet:ntoa(Address), port = Port};
		{error, _NetErr} ->
			State#tcp_state{socket = Socket}
	end.

stop_receiver(RecvPid) ->
	case erlang:is_pid(RecvPid) of
        false ->
			ok;
		true->
			case erlang:is_process_alive(RecvPid) of
				true ->
					exit(RecvPid, 'DOWN');
				_ ->
					ok
			end
	end.

get_user_agent(List) ->
	Str3 = "undefined",
	case lists:keyfind(<<"User-Agent">>, 1, List) of
		{_, Binary} ->
			String = binary_to_list(Binary),
			Str1 = "iPhone|Windows NT|Windows PC|Windows|Android|Mac",
			Str2 = "Linux",
			case re:run(String, Str1) of
				{match, [{Start, Len}]} ->
					lists:sublist(String, Start+1, Len);
				_ ->
					case re:run(String, Str2) of
						{match, [{Start, Len}]} ->
							lists:sublist(String, Start+1, Len);
						_ ->
							Str3
					end
			end;
		_ ->
			Str3
	end.

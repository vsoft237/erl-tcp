-record(tcp_state, {
	lsock,
	type,
	ssl,
	socket,
	receiver_pid,
	main_pid,
	ip,
	port,
	user_agent
}).

-define(TCP_TIMEOUT, 60000).		%% 1分钟 TCP连接超时时间

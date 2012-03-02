
-module(pipe).

-behaviour(gen_server).

-define(DRV_NAME, "pipe_drv").

% API
-export([
	start/0,
	start_link/0,
        open/2, %% debug
        connect/2, 
        bind/2,
        close/1,
        send/2,
        recv/1,
        socket/0,
        test/0
]).

% gen_server callbacks
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {port}).

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% Open socket   return socket(AF_UNIX, SOCK_STREAM, 0);
%% Return string represent handle of socket
-spec socket() -> {ok, integer()} | {error, any()}.
socket() ->
	gen_server:call(?MODULE, {socket}).

%% strcpy(serveraddr.sun_path, SERVER_PATH);
%% rc = connect(sd, (struct sockaddr *)&serveraddr, SUN_LEN(&serveraddr));
%% ServerAddr is file path: example: /var/run/rtpserver.sock
-spec connect(string(), integer()) -> ok | {error, any()}.
connect(ServerAddr, SocketHandle) -> 
	gen_server:call(?MODULE, {connect, ServerAddr, SocketHandle}).


-spec bind(string(), integer()) -> ok | {error, any()}.
bind(LocalAddr, SocketHandle) -> 
	gen_server:call(?MODULE, {bind, LocalAddr, SocketHandle}).

    
%% rc = send(sd, buffer, sizeof(buffer), 0);
-spec send(string(), integer()) -> ok | {error, any()}.
send(Data, SocketHandle) -> 
	gen_server:call(?MODULE, {send, Data, SocketHandle}).

%%  close(sd);
-spec close(integer()) -> ok | {error, any()}.
close(Handle) -> 
	gen_server:call(?MODULE, {close, Handle}).

%%  recv(sd, &buffer);
-spec recv(integer()) -> {ok, string()} | {error, any()}.
recv(SocketHandle) -> 
	gen_server:call(?MODULE, {recv, SocketHandle}).

%% Debug
open(StrArg,IntArg) ->
	gen_server:call(?MODULE, {open, StrArg, IntArg}).



test() ->
    io:format("Load driver ~n ",[]),
    {ok, _} = start(),

    io:format("Open STREAM SOCKET ~n ",[]),
    {ok, Handle} = socket(),
    io:format("Opened socket ~p ~n ",[Handle]),

    %% LocalAddr = "/var/tmp/unixsocketclient.sock",
    %% io:format("Binding to ~p ~n ",[LocalAddr]),
    %% ok = bind(LocalAddr, Handle),
    %% io:format("Binded to ~p ~p ~n ",[LocalAddr, Handle]),

    Addr = "../tests/tcp_socket",
    io:format("Connecting to ~p ~n ",[Addr]),
    ok = connect(Addr, Handle),
    io:format("Connected to ~p ~p ~n ",[Addr, Handle]),

    Data = "Data portion 1",
    io:format("Sending ~p ~n ",[Data]),
    ok = send(Data, Handle),
    io:format("Sended ~n "),
 
    io:format("Receiving ~n "),
    {ok, Res} = recv(Handle),
    io:format("Received ~p ~n ", [Res]),

    io:format("Closing socket ~p ~n ",[Handle]),
    ok = close(Handle),
    io:format("Closed socket ~p ~n ",[Handle]),
    


    io:format("Open STREAM SOCKET ~n ",[]),
    {ok, Handle1} = socket(),
    io:format("Opened socket ~p ~n ",[Handle1]),

    io:format("Connecting to ~p ~n ",[Addr]),
    ok = connect(Addr, Handle1),
    io:format("Connected to ~p ~p ~n ",[Addr, Handle1]),

    Data1 = "Data portion 2",
    io:format("Sending ~p ~n ",[Data1]),
    ok = send(Data1, Handle1),
    io:format("Sended ~n "),
 
    io:format("Receiving ~n "),
    {ok, Res1} = recv(Handle1),
    io:format("Received ~p ~n ", [Res1]),

    Data2 = "Data portion 3",
    io:format("Sending ~p ~n ",[Data2]),
    ok = send(Data2, Handle1),
    io:format("Sended ~n "),
 
    io:format("Receiving ~n "),
    {ok, Res2} = recv(Handle1),
    io:format("Received ~p ~n ", [Res2]),

    io:format("Closing socket ~p ~n ",[Handle1]),
    ok = close(Handle1),
    io:format("Closed socket ~p ~n ",[Handle1]).


init([]) ->
	erl_ddll:start(),
	Path = case code:priv_dir(syslog) of
		{error, _} ->
			case load_path(?DRV_NAME++".so") of
				{error, _} ->
					error;
				{ok, P} ->
					P
			end;
		P ->
			P
	end,

        io:format("Load driver ~p ~p ~n ",[Path, ?DRV_NAME]),

	case Path of
		error ->
			{stop, no_driver};
		Path ->
			case erl_ddll:load_driver(Path, ?DRV_NAME) of
				ok ->
					Port = open_port({spawn, ?DRV_NAME}, [binary]),
					{ok, #state{port = Port}};
				{error, Error} ->
					error_logger:format("Error loading driver: " ++ erl_ddll:format_error(Error), []),
					{stop, bad_driver}
			end
	end.

handle_call({open, StrArg, IntArg}, _From, #state{port = Port} = State) ->
	port_command(State#state.port, erlang:term_to_binary({open, StrArg, IntArg})),
	Reply = receive	{Port, {data, Bin}} ->
                        io:format("Recieved from driver: ~p", [{data,Bin}]),
			binary_to_term(Bin)
                after 1000 -> {error, timeout}
                end,
	{reply, Reply, State};

handle_call({socket}, _From, #state{port = Port} = State) ->
	port_command(State#state.port, erlang:term_to_binary({socket})),
	Reply = receive	{Port, {data, Bin}} ->
%%                        io:format("Recv: ~p", [{data,Bin}]),
			binary_to_term(Bin)
                after 1000 -> {error, timeout}
                end,
%%        io:format("Opened socket reply: ~p ~n", [Reply]),

        case Reply of
            {ok, Handle} -> 
                {reply, {ok, Handle}, State};
            {error, ErrorDesc} ->
                {reply, {error, ErrorDesc}, State}
            end;

handle_call({connect, ServerAddr, SocketHandle}, _From, #state{port = Port} = State) ->
%%        io:format("Socket: ~p", [ServerAddr]),
	port_command(State#state.port, erlang:term_to_binary({connect, ServerAddr, SocketHandle})),
	Reply = receive	{Port, {data, Bin}} ->
%%                        io:format("Recv: ~p", [{data,Bin}]),
			binary_to_term(Bin)
                after 1000 -> {error, timeout}
                end,
	{reply, Reply, State};


handle_call({bind, LocalAddr, SocketHandle}, _From, #state{port = Port} = State) ->
%%        io:format("Socket: ~p", [ServerAddr]),
	port_command(State#state.port, erlang:term_to_binary({bind, LocalAddr, SocketHandle})),
	Reply = receive	{Port, {data, Bin}} ->
%%                        io:format("Recv: ~p", [{data,Bin}]),
			binary_to_term(Bin)
                after 1000 -> {error, timeout}
                end,
	{reply, Reply, State};


handle_call({send, Data, SocketHandle}, _From, #state{port = Port} = State) ->
	port_command(State#state.port, erlang:term_to_binary({send, Data, SocketHandle})),
	Reply = receive	{Port, {data, Bin}} ->
%%                        io:format("Recv: ~p", [{data,Bin}]),
			binary_to_term(Bin)
                after 1000 -> {error, timeout}
                end,
	{reply, Reply, State};


handle_call({close, Handle}, _From, #state{port = Port} = State) ->
	port_command(State#state.port, erlang:term_to_binary({close, Handle})),
	Reply = receive	{Port, {data, Bin}} ->
%%                        io:format("Recv: ~p", [{data,Bin}]),
			binary_to_term(Bin)
                after 1000 -> {error, timeout}
                end,
	{reply, Reply, State};

handle_call({recv, SocketHandle}, _From, #state{port = Port} = State) ->
	port_command(State#state.port, erlang:term_to_binary({recv, SocketHandle})),
	Reply = receive	{Port, {data, Bin}} ->
%%                        io:format("Recv: ~p", [{data,Bin}]),
			binary_to_term(Bin)
                after 1000 -> {error, timeout}
                end,
%%        io:format("Recv: ~p", [Reply]),
	{reply, Reply, State};


handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_, _, _) ->
	ok.

load_path(File) ->
	case lists:zf(fun(Ebin) ->
		Priv = Ebin ++ "/../priv/",
		case file:read_file_info(Priv ++ File) of
                    {ok, _} -> {true, Priv};
			_ -> false
                    end
		end, 
         code:get_path()) of
		[Dir|_] ->
			{ok, Dir};
		[] ->
			error_logger:format("Error: ~s not found in code path", [File]),
			{error, enoent}
	        end.


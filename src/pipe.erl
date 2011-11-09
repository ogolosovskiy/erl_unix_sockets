
-module(pipe).

-behaviour(gen_server).

-define(DRV_NAME, "pipe_drv").

% API
-export([
	start/0,
	start_link/0,
        open/2, %% debug
        socket/1, 
        close/0,
        send/1,
        recv/0
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

%% sd = socket(AF_UNIX, SOCK_STREAM, 0);
%% serveraddr.sun_family = AF_UNIX;
%% strcpy(serveraddr.sun_path, SERVER_PATH);
%% rc = connect(sd, (struct sockaddr *)&serveraddr, SUN_LEN(&serveraddr));
%% ServerAddr is file path: example: /var/run/rtpserver.sock
-spec socket(string()) -> ok | {error, any()}.
socket(ServerAddr) -> 
	gen_server:call(?MODULE, {socket, ServerAddr}).
    
%% rc = send(sd, buffer, sizeof(buffer), 0);
-spec send(string()) -> ok | {error, any()}.
send(Data) -> 
	gen_server:call(?MODULE, {send, Data}).

%%  close(sd);
-spec close() -> ok | {error, any()}.
close() -> 
	gen_server:call(?MODULE, {close}).

%%  recv(sd, &buffer);
-spec recv() -> {ok, string()} | {error, any()}.
recv() -> 
	gen_server:call(?MODULE, {recv}).

%% Debug
open(StrArg,IntArg) ->
	gen_server:call(?MODULE, {open, StrArg, IntArg}).


%%% API %%%

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
                        io:format("Recv: ~p", [{data,Bin}]),
			binary_to_term(Bin)
                after 1000 -> {error, timeout}
                end,
	{reply, Reply, State};

handle_call({socket, ServerAddr}, _From, #state{port = Port} = State) ->
	port_command(State#state.port, erlang:term_to_binary({open, ServerAddr})),
	Reply = receive	{Port, {data, Bin}} ->
                        io:format("Recv: ~p", [{data,Bin}]),
			binary_to_term(Bin)
                after 1000 -> timeout
                end,
	{reply, Reply, State};

handle_call({send, Data}, _From, #state{port = Port} = State) ->
	port_command(State#state.port, erlang:term_to_binary({send, Data})),
	Reply = receive	{Port, {data, Bin}} ->
                        io:format("Recv: ~p", [{data,Bin}]),
			binary_to_term(Bin)
                after 1000 -> timeout
                end,
	{reply, Reply, State};


handle_call({close}, _From, #state{port = Port} = State) ->
	port_command(State#state.port, erlang:term_to_binary({close})),
	Reply = receive	{Port, {data, Bin}} ->
                        io:format("Recv: ~p", [{data,Bin}]),
			binary_to_term(Bin)
                after 1000 -> timeout
                end,
	{reply, Reply, State};

handle_call({recv}, _From, #state{port = Port} = State) ->
	port_command(State#state.port, erlang:term_to_binary({recv})),
	Reply = receive	{Port, {data, Bin}} ->
                        io:format("Recv: ~p", [{data,Bin}]),
			binary_to_term(Bin)
                after 1000 -> timeout
                end,
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
			error_logger:format("Error: ~s not found in code path\n", [File]),
			{error, enoent}
	        end.


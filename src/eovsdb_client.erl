-module(eovsdb_client).

-behaviour(gen_server).

-include("eovsdb_logger.hrl").

-define(SERVER, ?MODULE).
-define(CONNECT_TIMEOUT, 5000).
-define(DEFAULT_RETRY_CONNECT_TIME, 5000).
-define(STATE, eovsdb_client_state).

-record(?STATE, { mref                   :: reference(),
                  ipaddr                 :: inet:ip_address(),
                  port                   :: integer(),
                  database               :: binary(),
                  conn_pid               :: pid(),
                  connection_timeout = 0 :: integer() }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([connect/2,
         signal_connect/1,
         list_dbs/1,
         get_schema/1,
         get_schema/2,
         transaction/2,
         transaction/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

list_dbs(Pid) ->
    gen_server:call(Pid, list_dbs).

get_schema(Pid) ->
    gen_server:call(Pid, get_schema).
get_schema(Pid, DB) ->
    gen_server:call(Pid, {get_schema, DB}).

transaction(Pid, Op) ->
    gen_server:call(Pid, {transaction, Op}).
transaction(Pid, DB, Op) ->
    gen_server:call(Pid, {transaction, DB, Op}).

signal_connect(Pid) ->
    gen_server:cast(Pid, connect).

connect(Host, Opts) when is_list(Host) ->
    HostBin = list_to_binary(Host),
    [AddrBin, PortBin] = binary:split(HostBin, <<":">>),
    Port = binary_to_integer(PortBin),
    Addr0 = binary_to_list(AddrBin),
    Addr1 = case inet:parse_address(Addr0) of
                {error, einval} ->
                    case inet_gethost_native:gethostbyname(Addr0) of
                        {error, _} = E ->
                            ?ERR("[~p] couldn't connect to ~p~n", [?MODULE, Host]),
                            error(E);
                        {ok, Res} ->
                            [TmpAddr|_T] = element(6, Res),
                            TmpAddr
                    end;
                {ok, TmpAddr} -> TmpAddr
            end,
    gen_server:start_link(?MODULE, [Addr1, Port, Opts], []).

%% ------------------------------------------------------------------
%% callbacks
%% ------------------------------------------------------------------

init([Host, Port, Opts]) ->
    signal_connect(self()),
    DB = proplists:get_value(database, Opts),
    TimeOut = proplists:get_value(connection_timeout, Opts, ?DEFAULT_RETRY_CONNECT_TIME),
    {ok, #?STATE{ipaddr = Host,
                 port = Port,
                 database = DB,
                 connection_timeout = TimeOut }}.

handle_call(list_dbs, _From,
            State = #?STATE{ conn_pid = Conn }) ->
    Reply = eovsdb_protocol:list_dbs(Conn),
    {reply, Reply, State};
handle_call(get_schema, _From,
            State = #?STATE{ conn_pid = Conn, database = DB }) ->
    Reply = eovsdb_protocol:get_schema(Conn, DB),
    {reply, Reply, State};
handle_call({get_schema, DB}, _From,
            State = #?STATE{ conn_pid = Conn }) ->
    Reply = eovsdb_protocol:get_schema(Conn, DB),
    {reply, Reply, State};
handle_call({transaction, Ops},
            _From, State = #?STATE{ conn_pid = Conn, database = DB }) ->
    Reply = eovsdb_protocol:transaction(Conn, DB, Ops),
    {reply, Reply, State};
handle_call({transaction, DB, Ops},
            _From, State = #?STATE{ conn_pid = Conn }) ->
    Reply = eovsdb_protocol:transaction(Conn, DB, Ops),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(connect, State = #?STATE{ ipaddr = Host,
                                      port = Port,
                                      connection_timeout = TimeOut}) ->
    NewState =
        case gen_tcp:connect(Host, Port, [binary,
                                          {packet, raw},
                                          {active, false}]) of
            {ok, Socket} ->
                case eovsdb_client_sup:start_child(Socket) of
                    {ok, Conn} ->
                        gen_tcp:controlling_process(Socket, Conn),
                        MRef = erlang:monitor(process, Conn),
                        State#?STATE{ mref = MRef, conn_pid = Conn };
                    {error, ChildReason} ->
                        HostStr = inet_parse:ntoa(Host),
                        ?WARN("can't start eovsdb_protocol for ~s:~p: ~p~n", [HostStr, Port, ChildReason]),
                        retry_connect(self(), TimeOut),
                        State
                end;
            {error, TcpReason} ->
                HostStr = inet_parse:ntoa(Host),
                ?WARN("tcp error connecting to ~s:~p: ~p~n", [HostStr, Port, TcpReason]),
                retry_connect(self(), TimeOut),
                State
        end,
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MRef, process, _ConnPid, _Reason},
            State = #?STATE{mref = MRef}) ->
    signal_connect(self()),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

retry_connect(Pid, WaitTime) ->
    timer:apply_after(WaitTime, ?MODULE, signal_connect, [Pid]).

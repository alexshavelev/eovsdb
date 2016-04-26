-module(eovsdb_client).

-behaviour(gen_server).

-include("eovsdb_logger.hrl").

-define(SERVER, ?MODULE).
-define(CONNECT_TIMEOUT, 5000).
-define(DEFAULT_RETRY_CONNECT_TIME, 5000).
-define(STATE, eovsdb_client_state).

-record(?STATE, { ipaddr                 :: inet:ip_address(),
                  port                   :: integer(),
                  database               :: binary(),
                  schema                 :: map(),
                  conn_pid               :: pid(),
                  conn_ref               :: reference(),
                  connection_timeout = 0 :: integer(),
                  monitor_pid            :: pid(),
                  monitor_ref            :: reference(),
                  monitor_select         :: term()}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([connect/2,
         close/1,
         signal_connect/1,
         list_dbs/1,
         get_schema/1,
         get_schema/2,
         get_table_schema/3,
         regist_schema/1,
         regist_schema/2,
         get_regist_schema/1,
         get_columns/2,
         transaction/2,
         transaction/3,
         monitor/2,
         monitor_cancel/1]).

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

get_table_schema(Pid, Table, Col) ->
    gen_server:call(Pid, {get_table_schema, Table, Col}).

regist_schema(Pid) ->
    gen_server:cast(Pid, regist_schema).
regist_schema(Pid, DB) ->
    gen_server:cast(Pid, {regist_schema, DB}).

get_regist_schema(Pid) ->
    gen_server:call(Pid, get_regist_schema).

get_columns(Pid, Table) ->
    gen_server:call(Pid, {get_columns, Table}).

transaction(Pid, Op) ->
    gen_server:call(Pid, {transaction, Op}).
transaction(Pid, DB, Op) ->
    gen_server:call(Pid, {transaction, DB, Op}).

monitor(Pid, Select) ->
    gen_server:call(Pid, {monitor, self(), Select}).

monitor_cancel(Pid) ->
    gen_server:call(Pid, monitor_cancel).

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
    gen_server:start(?MODULE, [Addr1, Port, Opts], []).

close(Pid) ->
    gen_server:call(Pid, close_session).

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
handle_call({get_table_schema, Table, Col}, _From,
            State = #?STATE{ schema = Schema }) ->
    Reply = table_schema(Table, Col, Schema),
    {reply, Reply, State};
handle_call({get_schema, DB}, _From,
            State = #?STATE{ conn_pid = Conn }) ->
    Reply = eovsdb_protocol:get_schema(Conn, DB),
    {reply, Reply, State};
handle_call(get_regist_schema, _From,
            State = #?STATE{ schema = Schema }) ->
    {reply, Schema, State};
handle_call({get_columns, Table}, _From,
            State = #?STATE{ conn_pid = Conn, database = DB }) ->
    Reply = eovsdb_protocol:get_columns(Conn, DB, Table),
    {reply, Reply, State};
handle_call({transaction, Ops},
            _From, State = #?STATE{ conn_pid = Conn, database = DB }) ->
    Reply = eovsdb_protocol:transaction(Conn, DB, Ops),
    {reply, Reply, State};
handle_call({transaction, DB, Ops},
            _From, State = #?STATE{ conn_pid = Conn }) ->
    Reply = eovsdb_protocol:transaction(Conn, DB, Ops),
    {reply, Reply, State};
handle_call({monitor, MPid, Select},
            _From, State = #?STATE{ conn_pid = Conn, database = DB }) ->
    Reply = eovsdb_protocol:monitor(Conn, self(), DB, Select),
    MRef = erlang:monitor(process, MPid),
    {reply, Reply, State#?STATE{ monitor_ref = MRef,
                                 monitor_pid = MPid,
                                 monitor_select = Select}};
handle_call(monitor_cancel, _From,
            State = #?STATE{ conn_pid = ConnPid,
                             monitor_ref = MonRef}) ->
    eovsdb_protocol:monitor_cancel(ConnPid),
    erlang:demonitor(MonRef),
    {reply, ok, State#?STATE{ monitor_ref = undefined,
                              monitor_pid = undefined,
                              monitor_select = undefined}};
handle_call(close_session, _From,
            State = #?STATE{ conn_pid = ConnPid, conn_ref = ConnRef }) ->
    erlang:demonitor(ConnRef),
    eovsdb_protocol:close_session(ConnPid),
    {stop, normal, State#?STATE{ conn_ref = undefined, conn_pid = undefined}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(regist_schema,
            State = #?STATE{ conn_pid = Conn, database = DB }) ->
    {ok, #{<<"tables">> := Tables}} = eovsdb_protocol:get_schema(Conn, DB),
    {noreply, State#?STATE{ schema = Tables}};
handle_cast({regist_schema, DB},
            State = #?STATE{ conn_pid = Conn }) ->
    {ok, #{<<"tables">> := Tables}} = eovsdb_protocol:get_schema(Conn, DB),
    {noreply, State#?STATE{ schema = Tables}};
handle_cast(connect, State = #?STATE{ ipaddr = Host,
                                      port = Port,
                                      connection_timeout = TimeOut}) ->
    NewState =
        case gen_tcp:connect(Host, Port, [binary,
                                          {packet, raw},
                                          {active, false}]) of
            {ok, Socket} ->
                case eovsdb_protocol_sup:start_child(Socket) of
                    {ok, Conn} ->
                        case State#?STATE.monitor_ref of
                            Ref when is_reference(Ref) ->
                                ?INFO("[~p] monitor reconnecting: ~p ~n", [?MODULE, Ref]),
                                gen_tcp:controlling_process(Socket, Conn),
                                MRef = erlang:monitor(process, Conn),
                                eovsdb_protocol:monitor(Conn,
                                                        State#?STATE.monitor_pid,
                                                        State#?STATE.database,
                                                        State#?STATE.monitor_select),
                                State#?STATE{ conn_ref = MRef, conn_pid = Conn };
                            _ ->
                                gen_tcp:controlling_process(Socket, Conn),
                                MRef = erlang:monitor(process, Conn),
                                State#?STATE{ conn_ref = MRef, conn_pid = Conn }
                        end;
                    {error, ChildReason} ->
                        HostStr = inet_parse:ntoa(Host),
                        ?WARN("[~p] can't start eovsdb_protocol for ~s:~p: ~p~n", [?MODULE, HostStr, Port, ChildReason]),
                        retry_connect(self(), TimeOut),
                        State
                end;
            {error, TcpReason} ->
                HostStr = inet_parse:ntoa(Host),
                ?WARN("[~p] tcp error connecting to ~s:~p: ~p~n", [?MODULE, HostStr, Port, TcpReason]),
                retry_connect(self(), TimeOut),
                State
        end,
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', ConnRef, process, _ConnPid, _Reason},
            State = #?STATE{conn_ref = ConnRef,
                            monitor_ref = MonRef}) when is_reference(MonRef) ->
    ?WARN("[~p] monitor down: ~p ~n", [?MODULE, MonRef]),
    signal_connect(self()),
    erlang:demonitor(ConnRef),
    {noreply, State};
handle_info({'DOWN', ConnRef, process, _ConnPid, _Reason},
            State = #?STATE{conn_ref = ConnRef}) ->
    signal_connect(self()),
    erlang:demonitor(ConnRef),
    {noreply, State};
handle_info({'DOWN', MonRef, process, _MonPid, _Reason},
            State = #?STATE{monitor_ref = MonRef,
                            conn_pid = ConnPid }) ->
    eovsdb_protocol:monitor_cancel(ConnPid),
    erlang:demonitor(MonRef),
    {noreply, State};
handle_info({ovsdb_monitor, _} = Update,
            State = #?STATE{monitor_pid = MPid}) ->
    erlang:send(MPid, Update),
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

table_schema(TabName, ColName, Schema)
  when is_atom(TabName),
       is_atom(ColName) ->
    TabNameBin = atom_to_binary(TabName, utf8),
    ColNameBin = atom_to_binary(ColName, utf8),
    table_schema(TabNameBin, ColNameBin, Schema);
table_schema(TabName, ColName, Schema) ->
    case maps:get(TabName, Schema, false) of
        false -> {error, {TabName, not_found}};
        #{<<"columns">> := Columns} ->
            case maps:get(ColName, Columns, false) of
                false ->
                    {error, {ColName, not_found}};
                #{<<"type">> := Type} ->
                    column_schema(Type)
            end
    end.

column_schema(#{<<"key">> := #{<<"enum">> := [<<"set">>, Enum]}}) ->
    {enum, Enum};
column_schema(#{<<"key">> := #{<<"key">> := _, <<"value">> := _}}) ->
    map;
column_schema(#{<<"key">> := #{<<"refTable">> := Table, <<"min">> := _}}) ->
    {set, {ref_table, Table}};
column_schema(#{<<"key">> := #{<<"min">> := _, <<"type">> := Type}}) ->
    {set, Type};
column_schema(#{<<"key">> := #{<<"refTable">> := Table}}) ->
    {ref_table, Table};
column_schema(#{<<"key">> := Type}) ->
    Type;
column_schema(Type) ->
    Type.

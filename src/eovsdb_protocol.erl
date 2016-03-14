-module(eovsdb_protocol).

-behaviour(gen_server).

-include("eovsdb_logger.hrl").

%% API
-export([start_link/1]).

%% Protocol API
-export([echo_reply/2, list_dbs/1, get_schema/2, transaction/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SYNC_SEND_TIMEOUT, 5000).
-define(STATE, eovsdb_protocol_state).

-record(?STATE, { socket                       :: inet:socket(),
                  address                      :: inet:ip_address(),
                  port                         :: integer(),
                  protocol        = tcp        :: atom(),
                  buffer          = <<>>       :: binary(),
                  pending_message = maps:new() :: map() }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

list_dbs(Pid) ->
    Json = eovsdb_methods:q(list_dbs, 0, []),
    gen_server:call(Pid, {sync_send, Json}).

get_schema(Pid, DB) ->
    Json = eovsdb_methods:q(get_schema, 0, [DB]),
    gen_server:call(Pid, {sync_send, Json}).

transaction(Pid, DB, Op) when not is_list(Op) ->
    transaction(Pid, DB, [Op]);
transaction(Pid, DB, Ops) when is_list(Ops) ->
    Json = eovsdb_methods:q(transact, 0, [DB] ++ Ops),
    gen_server:call(Pid, {sync_send, Json}).

echo_reply(Id, Params) ->
    Json = eovsdb_methods:q(echo_reply, Id, Params),
    ok = gen_server:cast(self(), {send_json, Json}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Socket]) ->
    {ok, {Address, Port}} = inet:peername(Socket),
    eovsdb_util:setopts(tcp, Socket, [{active, once}]),
    AddrStr = inet_parse:ntoa(Address),
    ?INFO("connected to ~s:~p pid:~p\n", [AddrStr, Port, self()]),
    {ok, #?STATE{socket = Socket, protocol = tcp}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({sync_send, Data0}, From,
            State = #?STATE{ socket = Socket,
                             protocol = Proto,
                             pending_message = PM0 }) ->
    Id = eovsdb_util:rand_id(),
    Data = Data0#{ id => Id },
    PM = maps:put(Id, #{ from => From, reply => noreply }, PM0),
    eovsdb_util:send_json(Proto, Socket, Data),
    {noreply, State#?STATE{ pending_message = PM }};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({send_json, Data},
            State = #?STATE{socket = Socket, protocol = Proto}) ->
    eovsdb_util:send_json(Proto, Socket, Data),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, Socket, Data},
            State = #?STATE{socket = Socket, protocol = Proto}) ->
    eovsdb_util:setopts(Proto, Socket, [{active, once}]),
    handle_tcp(State, Data);
handle_info({tcp_closed, _Socket}, State) ->
    terminate_connection(State, tcp_closed).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_tcp(State, <<>>) ->
    {noreply, State};
handle_tcp(State = #?STATE{ buffer = Buf }, Data0) ->
    Data1 = <<Buf/binary, Data0/binary>>,
    case jsone:try_decode(Data1) of
        {ok, Term, Left} ->
            NewState = handle_message(Term, State),
            handle_tcp(NewState#?STATE{buffer = <<>>}, Left);
        {error, _Error} ->
            {noreply, State#?STATE{buffer = Data1}}
    end.

handle_message(#{ <<"method">> := <<"echo">> } = Data, State) ->
    #{ <<"id">> := Id, <<"params">> := Params } = Data,
    ok = echo_reply(Id, Params),
    State;
handle_message(Data, State = #?STATE{pending_message = PM0}) ->
    #{ <<"id">> := Id,
       <<"result">> := Result,
       <<"error">> := Err } = Data,
    case maps:get(Id, PM0, not_found) of
        not_found -> State;
        #{ from := From } ->
            ReplyData = case Err of
                            null -> { ok, Result };
                            _ -> { error, { Err, Result }}
                        end,
            gen_server:reply(From, ReplyData),
            PM = maps:remove(Id, PM0),
            State#?STATE{pending_message = PM}
    end.

terminate_connection(State = #?STATE{ socket = Socket,
                                      protocol = Proto },
                     Reason) ->
    ?WARN("[~p] terminating: ~p~n", [?MODULE, Reason]),
    ok = eovsdb_util:close(Proto, Socket),
    {stop, normal, State#?STATE{socket = undefined}}.

-module(eovsdb_client).

-behaviour(gen_server).

-include("eovsdb_logger.hrl").

-define(SERVER, ?MODULE).
-define(CONNECT_TIMEOUT, 5000).
-define(RETRY_CONNECT_TIME, 5000).
-define(STATE, eovsdb_client_state).

-record(?STATE, { mref   :: reference(),
                  ipaddr :: inet:ip_address(),
                  port   :: integer() }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([connect/2, signal_connect/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

signal_connect(Pid) ->
    gen_server:cast(Pid, connect).

connect(Host, Port) ->
    gen_server:start_link(?MODULE, [Host, Port], []).

%% ------------------------------------------------------------------
%% callbacks
%% ------------------------------------------------------------------

init([Host, Port]) ->
    signal_connect(self()),
    {ok, #?STATE{ipaddr = Host, port = Port}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(connect, State = #?STATE{ ipaddr = Host, port = Port }) ->
    NewState =
        case gen_tcp:connect(Host, Port, [binary,
                                          {packet, raw},
                                          {active, false}]) of
            {ok, Socket} ->
                case eovsdb_client_sup:start_child(Socket) of
                    {ok, Conn} ->
                        gen_tcp:controlling_process(Socket, Conn),
                        MRef = erlang:monitor(process, Conn),
                        State#?STATE{ mref = MRef };
                    {error, ChildReason} ->
                        HostStr = inet_parse:ntoa(Host),
                        ?WARN("can't start eovsdb_protocol for ~s:~p: ~p\n",
                              [HostStr, Port, ChildReason]),
                        retry_connect(self(), ?RETRY_CONNECT_TIME),
                        State
                end;
            {error, TcpReason} ->
                HostStr = inet_parse:ntoa(Host),
                ?WARN("tcp error connecting to ~s:~p: ~p\n",
                      [HostStr, Port, TcpReason]),
                retry_connect(self(), ?RETRY_CONNECT_TIME),
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

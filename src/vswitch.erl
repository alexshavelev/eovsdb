-module(vswitch).

-export([add_br/1]).
-export([del_br/1]).

add_br(BrName) ->
    {ok, Pid} = eovsdb_client:connect("127.0.0.1:6632", [{ database, <<"Open_vSwitch">>}]),
    {ok, [Rows|_]} = eovsdb_client:transaction(Pid, [eovsdb_op:select(['_uuid', bridges], <<"Open_vSwitch">>, [])]),
    [Row|_] = maps:get(<<"rows">>, Rows),
    Open_vSwitch_Uuid = maps:get(<<"_uuid">>, Row),
    [<<"set">>, Bridges] = maps:get(<<"bridges">>, Row),
    Interface = #{name => BrName, type => <<"internal">> },
    Port = #{name => BrName,interfaces => [<<"named-uuid">>,<<"interface_br">>]},
    Bridge = #{name => BrName,ports => [<<"named-uuid">>,<<"port_br">>], protocols => <<"OpenFlow10">>},
    eovsdb_client:transaction(Pid, [eovsdb_op:insert(<<"Interface">>, Interface, <<"interface_br">>),
                                    eovsdb_op:insert(<<"Port">>, Port, <<"port_br">>),
                                    eovsdb_op:insert(<<"Bridge">>, Bridge, <<"bridge_br">>),
                                    eovsdb_op:update(<<"Open_vSwitch">>,
                                                     [{<<"_uuid">>, <<"==">> ,Open_vSwitch_Uuid}],
                                                     #{bridges => [set, Bridges ++ [[<<"named-uuid">>, <<"bridge_br">>]]]}),
                                    eovsdb_op:mutate(<<"Open_vSwitch">>,
                                                     [{<<"_uuid">>, <<"==">> ,Open_vSwitch_Uuid}],
                                                     [{<<"next_cfg">>, <<"+=">>, 1}]),
                                    eovsdb_op:commit(true)]),
    eovsdb_client:close(Pid).

del_br(BrName) ->
    {ok, Pid} = eovsdb_client:connect("127.0.0.1:6632", [{ database, <<"Open_vSwitch">>}]),
    {ok, [OvsRows|_]} = eovsdb_client:transaction(Pid, [eovsdb_op:select(['_uuid', bridges], <<"Open_vSwitch">>, [])]),
    {ok, [BrRows|_]} = eovsdb_client:transaction(Pid, [eovsdb_op:select(['_uuid'], <<"Bridge">>, [{name, '==', BrName}])]),
    [BrRow|_] = maps:get(<<"rows">>, BrRows),
    [OvsRow|_] = maps:get(<<"rows">>, OvsRows),
    Open_vSwitch_Uuid = maps:get(<<"_uuid">>, OvsRow),
    Br_Uuid = maps:get(<<"_uuid">>, BrRow),
    [<<"set">>, Bridges] = maps:get(<<"bridges">>, OvsRow),
    eovsdb_client:transaction(Pid, [eovsdb_op:delete(<<"Bridge">>, [{name, '==', BrName}]),
                                    eovsdb_op:update(<<"Open_vSwitch">>,
                                                     [{<<"_uuid">>, <<"==">> ,Open_vSwitch_Uuid}],
                                                     #{bridges => [set, Bridges -- [Br_Uuid]]}),
                                    eovsdb_op:mutate(<<"Open_vSwitch">>,
                                                     [{<<"_uuid">>, <<"==">> ,Open_vSwitch_Uuid}],
                                                     [{<<"next_cfg">>, <<"+=">>, 1}]),
                                    eovsdb_op:commit(true)]),
    eovsdb_client:close(Pid).

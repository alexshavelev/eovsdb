eovsdb [![Build Status](https://travis-ci.org/shun159/eovsdb.svg?branch=master)](https://travis-ci.org/shun159/eovsdb)
===

eovsdb supports [RFC7047](https://tools.ietf.org/html/rfc7047) protocol specification, and support parallel connection and auto reconnect to OVSDB server

## Feature Overviews

- Reconnect automatically
- Keepalive
- TCP socket support

## installing

```bash
(prompt)$ apt-get install make git
(prompt)$ git clone https://github.com/shun159/eovsdb
(prompt)$ cd eovsdb && make
```

## Usage

### select

```erlang
{ ok, Pid } = eovsdb_client:connect("127.0.0.1:6632", [{ database, <<"Open_vSwitch">>}]).
%% SELECT * FROM Bridge;
eovsdb_client:transaction(Pid, [eovsdb_op:select("*", <<"Bridge">>, [])]).
%% SELECT * FROM Bridge WHERE name=br1;
eovsdb_client:transaction(Pid, [eovsdb_op:select("*", <<"Bridge">>, [{<<"name">>, <<"==">>, <<"br1">>}])]).
```

### Add Bridge to openvswitch

```erlang
{ ok, Pid } = eovsdb_client:connect("127.0.0.1:6632", [{ database, <<"Open_vSwitch">>}]).
{ok, [Rows|_] } = eovsdb_client:transaction(Pid, [eovsdb_op:select([<<"_uuid">>, <<"bridges">>], <<"Open_vSwitch">>, [])]).
[Row|_] = maps:get(<<"rows">>, Rows).
Open_vSwitch_Uuid = maps:get(<<"_uuid">>, Row).
[<<"set">>, Bridges ] = Open_vSwitch_Bridges0 = maps:get(<<"bridges">>, Row).
Interface = #{<<"name">> => <<"br-int">>, <<"type">> => <<"internal">> }.
Port = #{<<"name">> => <<"br-int">>,<<"interfaces">> => [<<"named-uuid">>,<<"interface_br_int">>]}.
Controller = #{<<"target">> => <<"tcp:127.0.0.1:6633">> }.
Bridge = #{<<"name">> => <<"br-int">>,<<"ports">> => [<<"named-uuid">>,<<"port_br_int">>], <<"controller">> => [<<"named-uuid">>, <<"ofc_br_int">>], <<"protocols">> => <<"OpenFlow10">>}.
eovsdb_client:transaction(Pid, [eovsdb_op:insert(<<"Interface">>, Interface, <<"interface_br_int">>),
                                eovsdb_op:insert(<<"Port">>, Port, <<"port_br_int">>),
                                eovsdb_op:insert(<<"Controller">>, Controller, <<"ofc_br_int">>),
                                eovsdb_op:insert(<<"Bridge">>, Bridge, <<"bridge_br_int">>),
                                eovsdb_op:update(<<"Open_vSwitch">>,
                                                 [{<<"_uuid">>, <<"==">> ,Open_vSwitch_Uuid }],
                                                 #{ <<"bridges">> => [<<"set">>, Bridges ++ [[<<"named-uuid">>, <<"bridge_br_int">>]]], other_config => [map, [[<<"flow-restore-wait">>, <<"true">>]]]}),
                                eovsdb_op:mutate(<<"Open_vSwitch">>,
                                                 [{<<"_uuid">>, <<"==">> ,Open_vSwitch_Uuid }],
                                                 [{<<"next_cfg">>, <<"+=">>, 1}]),
                                eovsdb_op:commit(true)]).
```

## Status

work-in-progress

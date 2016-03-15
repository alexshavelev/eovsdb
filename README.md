eovsdb
===

```erlang
{ ok, Pid } = eovsdb_client:connect("127.0.0.1:6632", [{ database, <<"Open_vSwitch">>}]).
{ ok, Schema } = eovsdb_client:get_schema(Pid),
{ ok, DBList } = eovsdb_client:list_dbs(Pid),
{ ok, Data } = eovsdb_client:transaction(Pid, [eovsdb_op:select("*", <<"Bridge">>, [])]).
```

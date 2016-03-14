-module(eovsdb_methods).

-export([q/3]).

q(echo, Id, Params) ->
    #{ id => Id, method => echo, params => Params };
q(echo_reply, Id, Params) ->
    #{ id => Id, error => null, result => Params };
q(list_dbs, Id, Params) ->
    #{ id => Id, method => list_dbs, params => Params };
q(get_schema, Id, Params) ->
    #{ id => Id, method => get_schema, params => Params };
q(transact, Id, Params) ->
    #{ id => Id, method => transact, params => Params };
q(cancel, Id, Params) ->
    #{ id => Id, method => cancel, params => Params };
q(monitor, Id, Params) ->
    #{ id => Id, method => monitor, params => Params };
q(update, Id, Params) ->
    #{ id => Id, method => update, params => Params };
q(monitor_cancel, Id, Params) ->
    #{ id => Id, method => monitor_cancel, params => Params };
q(lock, Id, Params) ->
    #{ id => Id, method => lock, params => Params };
q(steal, Id, Params) ->
    #{ id => Id, method => steal, params => Params };
q(unlock, Id, Params) ->
    #{ id => Id, method => unlock, params => Params };
q(Invalid, _, _) ->
    { error, { not_supported, Invalid }}.

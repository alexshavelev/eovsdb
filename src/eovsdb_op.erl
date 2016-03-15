-module(eovsdb_op).

-export([select/3]).
-export([insert/2, insert/3]).
-export([update/3]).
-export([delete/2]).
-export([mutate/3]).
-export([commit/1]).
-export([abort/0]).

select("*", Table, Conds) ->
    Where = [[C, F, V] || {C, F, V} <- Conds],
    #{ op => select,
       table => Table,
       where => Where };
select(Cols, Table, Conds) ->
    Where = [[C, F, V] || {C, F, V} <- Conds],
    #{ op => select,
       table => Table,
       where => Where,
       columns => Cols }.

insert(Table, Row) ->
    #{ op => insert,
       table => Table,
       row => Row }.
insert(Table, Row, Id) ->
    maps:merge(
      insert(Table, Row),
      #{ <<"uuid-name">> => Id }
     ).

update(Table, Conds, Row) ->
    Where = [[C, F, V] || {C, F, V} <- Conds],
    #{ op => update,
       table => Table,
       where => Where,
       row => Row }.

delete(Table, Conds) ->
    Where = [[C, F, V] || {C, F, V} <- Conds],
    #{ op => delete,
       table => Table,
       where => Where }.

mutate(Table, Conds, Muts0) ->
    Where = [[C, F, V] || {C, F, V} <- Conds],
    Muts = [[C, M, V] || {C, M, V} <- Muts0],
    #{ op => mutate,
       table => Table,
       where => Where,
       mutations => Muts }.

commit(Mode) ->
    #{ op => commit,
       durable => Mode }.

abort() ->
    #{ op => abort }.

-module(eovsdb_op).

-export([select/3]).

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

%% Transact API
%%   1.select
%% [A, B, C] = eovsdb_methods:transaction([
%%   eovsdb_op:select([], Tbl),
%%   eovsdb_op:select([Col1, Col2], Tbl, [{Col1, <<">">>, 10}]),
%%   eovsdb_op:select([Col1, Col2], Tbl, [{Col1, <<">">>, 10}])
%% ]

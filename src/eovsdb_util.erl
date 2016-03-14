-module(eovsdb_util).

-export([send_json/3,
         setopts/3,
         close/2,
         rand_id/0]).

send_json(Protocol, Socket, Map) ->
    Json = jsone:encode(Map),
    send(Protocol, Socket, Json).

setopts(tcp, Socket, Opts) ->
    inet:setopts(Socket, Opts);
setopts(tls, Socket, Opts) ->
    ssl:setopts(Socket, Opts).

send(tcp, Socket, Data) ->
    gen_tcp:send(Socket, Data);
send(tls, Socket, Data) ->
    ssl:send(Socket, Data).

close(_, undefined) ->
    ok;
close(tcp, Socket) ->
    gen_tcp:close(Socket);
close(tls, Socket) ->
    ssl:close(Socket).

rand_id() ->
    IdBin = crypto:rand_bytes(4),
    binary:decode_unsigned(IdBin, big).

-module(koozy).

-export([register_name/2]).
-export([unregister_name/1]).
-export([whereis_name/1]).


-spec
register_name(term(), pid()) ->
    yes | no.
register_name(Name, Pid) when Pid == self() ->
    koozy_zk:register_name(Name, Pid);
register_name(_, _) ->
    no.

-spec
unregister_name(term()) ->
    true.
unregister_name(Name) ->
    koozy_zk:unregister_name(Name).

-spec
whereis_name(term()) ->
    pid() | undefined.
whereis_name(Name) ->
    koozy_zk:whereis_name(Name).

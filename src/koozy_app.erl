-module(koozy_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).


start(_StartType, _StartArgs) ->
    case koozy_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

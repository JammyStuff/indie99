-module(indie99_02_inets).
-export([init/0, stop/0]).

init() ->
    inets:start(),
    ssl:start().

stop() ->
    ssl:stop(),
    inets:stop().

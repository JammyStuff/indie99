-module(indie99_02_bcrypt).

-export([init/0, stop/0]).

init() ->
    bcrypt:start().

stop() ->
    bcrypt:stop().

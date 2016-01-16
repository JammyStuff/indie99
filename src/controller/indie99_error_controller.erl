-module(indie99_error_controller, [Req, SessionID]).
-export([error_500/2]).

error_500('GET', []) ->
    ok.

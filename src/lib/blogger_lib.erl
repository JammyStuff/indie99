-module(blogger_lib).
-export([is_logged_in/1, register/2]).

is_logged_in(RequestContext) ->
    proplists:get_value(blogger, RequestContext) =/= undefined.

register(Username, Password) ->
    Time = erlang:universaltime(),
    Blogger = blogger:new(id, Username, "", Time, Time),
    BloggerHashed = Blogger:set_password(Password),
    BloggerHashed:save().

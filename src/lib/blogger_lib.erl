-module(blogger_lib).
-export([register/2]).

register(Username, Password) ->
    Time = erlang:universaltime(),
    Blogger = blogger:new(id, Username, "", Time, Time),
    BloggerHashed = Blogger:set_password(Password),
    BloggerHashed:save().

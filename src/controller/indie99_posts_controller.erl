-module(indie99_posts_controller, [Req, SessionID]).
-export([view/2]).

view('GET', [PostSlug]) ->
    case boss_db:find_first(post, [slug, equals, PostSlug]) of
        undefined ->
            not_found;
        Post ->
            {ok, [{title, Post:title()}, {post, Post}]}
    end.

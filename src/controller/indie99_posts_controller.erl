-module(indie99_posts_controller, [Req, SessionID]).
-export([index/2, view/2]).

index('GET', []) ->
    Posts = boss_db:find(post, [],
                               [{order_by, published_at}, {descending, true}]),
    {ok, [{title, "Posts"}, {posts, Posts}]}.

view('GET', [PostSlug]) ->
    case boss_db:find_first(post, [slug, equals, PostSlug]) of
        undefined ->
            not_found;
        Post ->
            {ok, [{title, Post:title()}, {post, Post}]}
    end.

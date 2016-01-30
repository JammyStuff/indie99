-module(indie99_posts_controller, [Req, SessionID]).
-export([page/2, view/2]).

-define(POSTS_PER_PAGE, 5).

page('GET', [PageNumberStr]) ->
    PageNumber = list_to_integer(PageNumberStr),
    Offset = (PageNumber - 1) * ?POSTS_PER_PAGE,
    Posts = boss_db:find(post, [],
                               [{order_by, published_at},
                                {descending, true},
                                {limit, ?POSTS_PER_PAGE},
                                {offset, Offset}]),
    PostsCount = boss_db:count(post),
    PreviousPage = PageNumber - 1,
    NextPage = if
                   PostsCount > (PageNumber * ?POSTS_PER_PAGE) ->
                       PageNumber + 1;
                   true ->
                       undefined
               end,
    {ok, [{title, "Posts"}, {posts, Posts}, {previous_page, PreviousPage},
          {next_page, NextPage}]}.

view('GET', [PostSlug]) ->
    case boss_db:find_first(post, [slug, equals, PostSlug]) of
        undefined ->
            not_found;
        Post ->
            {ok, [{title, Post:title()}, {post, Post}]}
    end.

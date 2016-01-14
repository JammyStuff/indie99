-module(indie99_admin_controller, [Req, SessionID]).
-export([before_filters/2, posts/3]).

posts('GET', [], _RequestContext) ->
    Posts = boss_db:find(post, [],
                               [{order_by, created_at}, {descending, true}]),
    {ok, [{title, "Posts Admin"}, {posts, Posts}]};

posts('GET', ["create"], _RequestContext) ->
    {render_other, [{action, "posts_create"}], [{title, "Create Post"}]};

posts('POST', ["create"], RequestContext) ->
    Blogger = proplists:get_value(blogger, RequestContext),
    Title = Req:post_param("title"),
    Content = Req:post_param("content"),
    Post = post:new(id, Title, Content, undefined, Blogger:id(),
                    erlang:universaltime(), undefined, undefined),
    case Post:validate() of
        {error, Errors} ->
            {render_other, [{action, "posts_create"}],
                           [{title, "Create Post"}, {form_errors, Errors}]};
        ok ->
            {ok, SavedPost} = Post:save(),
            {redirect, io_lib:format("/p/~s", [SavedPost:slug()])}
    end;

posts('GET', [PostId], _RequestContext) ->
    case boss_db:find(PostId) of
        Post when element(1, Post) =:= post ->
            {render_other, [{action, "posts_edit"}],
                           [{title, "Edit Post"}, {post, Post}]};
        _ ->
            not_found
    end;

posts('POST', [PostId], _RequestContext) ->
    Title = Req:post_param("title"),
    Content = Req:post_param("content"),
    case boss_db:find(PostId) of
        Post when element(1, Post) =:= post ->
            UpdatedPost = Post:set([{title, Title}, {content, Content}]),
            case UpdatedPost:validate() of
                {error, Errors} ->
                    {render_other, [{action, "posts_edit"}],
                                   [{title, "Edit Post"},
                                    {post, Post},
                                    {form_errors, Errors}]};
                ok ->
                    {ok, SavedPost} = Post:save(),
                    {redirect, io_lib:format("/p/~s", [SavedPost:slug()])}
            end;
        _ ->
            not_found
    end;

posts('GET', [PostId, "delete"], _RequestContext) ->
    case boss_db:find(PostId) of
        Post when element(1, Post) =:= post ->
            {render_other, [{action, "posts_delete"}],
                           [{title, "Delete Post"}]};
        _ ->
            not_found
    end;

posts('POST', [PostId, "delete"], _RequestContext) ->
    case boss_db:find(PostId) of
        Post when element(1, Post) =:= post ->
            ok = boss_db:delete(PostId),
            {redirect, "/admin/posts"};
        _ ->
            not_found
    end.

before_filters(DefaultFilters, _RequestContext) ->
    DefaultFilters ++ [require_login_filter].

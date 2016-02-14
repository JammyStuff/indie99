-module(indie99_setup_controller, [Req, SessionID]).
-export([before_filters/2, step1/2, step2/3]).

step1('GET', []) ->
    case boss_db:count(blogger) of
        0 ->
            {ok, [{title, "Setup"}]};
        _ ->
            not_found
    end;

step1('POST', []) ->
    case boss_db:count(blogger) of
        0 ->
            Username = Req:post_param("username"),
            FullName = Req:post_param("full_name"),
            Password = Req:post_param("password"),
            PasswordConfirmation = Req:post_param("password_confirmation"),
            if
                Password =/= PasswordConfirmation ->
                    ok_with_errors(["Password did not match confirmation"]);
                true ->
                    Blogger = blogger:new(id, Username, undefined, FullName,
                        undefined, undefined, undefined, undefined),
                    case Blogger:set_password(Password) of
                        {error, Errors} ->
                            ok_with_errors(Errors);
                        {ok, BloggerWithPassword} ->
                            case BloggerWithPassword:validate() of
                                {error, Errors} ->
                                    ok_with_errors(Errors);
                                ok ->
                                    {ok, SavedBlogger} =
                                        BloggerWithPassword:save(),
                                    boss_session:set_session_data(SessionID,
                                        blogger, SavedBlogger:id()),
                                    {redirect, "/setup/step2"}
                            end
                    end
            end;
        _ ->
            not_found
    end.

step2('GET', [], _RequestContext) ->
    {ok, [{title, "Setup"}]};

step2('POST', [], RequestContext) ->
    Blogger = proplists:get_value(blogger, RequestContext),
    EmailAddress = Req:post_param("email_address"),
    TwitterUsername = Req:post_param("twitter_username"),
    UpdatedBlogger = Blogger:set([{email_address, EmailAddress},
                                  {twitter_username, TwitterUsername}]),
    case UpdatedBlogger:validate() of
        {error, Errors} ->
            ok_with_errors(Errors);
        ok ->
            {ok, SavedBlogger} = UpdatedBlogger:save(),
            {redirect, "/"}
    end.

ok_with_errors(Errors) ->
    {ok, [{title, "Setup"}, {form_errors, Errors}]}.

before_filters(DefaultFilters, RequestContext) ->
    Action = proplists:get_value(action, RequestContext),
    case Action of
        "step1" ->
            DefaultFilters -- [setup_filter];
        _ ->
            DefaultFilters ++ [require_login_filter]
    end.

-module(indie99_setup_controller, [Req, SessionID]).
-export([before_filters/2, step1/2, step2/2]).

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
            Password = Req:post_param("password"),
            PasswordConfirmation = Req:post_param("password_confirmation"),
            if
                Password =/= PasswordConfirmation ->
                    ok_with_errors(["Password did not match confirmation"]);
                true ->
                    Blogger = blogger:new(id, Username, undefined, undefined,
                                          undefined),
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
                                    {redirect, "/"}
                            end
                    end
            end;
        _ ->
            not_found
    end.

step2('GET', []) ->
    not_found.

ok_with_errors(Errors) ->
    {ok, [{title, "Setup"}, {form_errors, Errors}]}.

before_filters(DefaultFilters, _RequestContext) ->
    DefaultFilters -- [setup_filter].

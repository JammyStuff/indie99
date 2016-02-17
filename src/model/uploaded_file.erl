-module(uploaded_file, [Id, Type::string(), MimeType::string(), Path::string(),
                        BloggerId, CreatedAt::datetime(),
                        UpdatedAt::datetime()]).
-export([before_create/0, before_update/0, validation_tests/0]).

-belongs_to(blogger).

-define(ALLOWED_TYPES, ["image", "other"]).

before_create() ->
    Time = erlang:universaltime(),
    UploadedFile = set([{created_at, Time}, {updated_at, Time}]),
    {ok, UploadedFile}.

before_update() ->
    UploadedFile = set(updated_at, erlang:universaltime()),
    {ok, UploadedFile}.

validation_tests() ->
    [
        {fun() ->
            (length(MimeType) >= 3) and (length(MimeType) =< 100)
         end,
         "MIME type must be between 3 and 100 characters long"},
        {fun() ->
            if
                is_list(Path) ->
                    length(Path) =< 256;
                true ->
                    true
            end
         end,
         "Path can be up to 256 characters long"},
        {fun() ->
            lists:any(fun(T) -> T =:= Type end, ?ALLOWED_TYPES)
         end,
         "Type is not recognised"}
    ].

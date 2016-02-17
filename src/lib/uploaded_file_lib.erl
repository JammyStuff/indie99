-module(uploaded_file_lib).
-export([checksum_file/1, handle_image_upload/3, handle_upload/4, mime_type/1]).

-define(IMAGE_TYPES, ["image/gif", "image/jpeg", "image/png"]).
-define(UPLOADS_DIR, "./priv/static/uploads").

checksum_file(FileName) ->
    case file:read_file(FileName) of
        {ok, Content} ->
            Hash = crypto:hash(sha256, Content),
            <<HashInt:256/integer>> = Hash,
            {ok, io_lib:format("~64.16.0b", [HashInt])};
        {error, Error} ->
            {error, Error}
    end.

extension_for_mime("image/gif") ->
    ".gif";
extension_for_mime("image/jpeg") ->
    ".jpg";
extension_for_mime("image/png") ->
    ".png";
extension_for_mime(_) ->
    undefined.

handle_image_upload(Source, OriginalName, Blogger) ->
    {ok, Type} = mime_type(Source),
    case lists:any(fun(T) -> Type =:= T end, ?IMAGE_TYPES) of
        true ->
            handle_upload("image", Source, OriginalName, Blogger);
        false ->
            {error, ["File was not an accepted image type"]}
    end.

handle_upload(Type, Source, OriginalName, Blogger) ->
    {ok, MimeType} = mime_type(Source),
    Extension = case extension_for_mime(MimeType) of
        undefined ->
            filename:extension(OriginalName);
        MimeExtension ->
            MimeExtension
    end,
    case checksum_file(Source) of
        {ok, Checksum} ->
            Destination = filename:join([?UPLOADS_DIR,
                io_lib:format("~s~s", [Checksum, Extension])]),
            case file:rename(Source, Destination) of
                ok ->
                    UploadedFile = uploaded_file:new(id, Type, MimeType,
                        Destination, Blogger:id(), undefined, undefined),
                    UploadedFile:save();
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

mime_type(FileName) ->
    Output = os:cmd(io_lib:format("file -b --mime-type ~s", [FileName])),
    case re:run(Output, "^cannot open") of
        nomatch ->
            {match, [Type]} = re:run(Output, "^(?<type>\\S+)",
                                     [{capture, [type], list}]),
            {ok, Type};
        _ ->
            {error, Output}
    end.

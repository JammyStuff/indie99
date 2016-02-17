-module(uploaded_file_tests).
-include_lib("eunit/include/eunit.hrl").

uploaded_file_test_() ->
    [
        {"Validations for MIME type",
         ?_test(val_mime_type())},
        {"Validations for path",
         ?_test(val_path())},
        {"Validations for type",
         ?_test(val_type())}
    ].

val_mime_type() ->
    OkFile = create_uploaded_file(undefined, [{mime_type, "abcdefghijklmnopq" ++
        "rstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdef" ++
        "ghijklmnopqrstuv"}]),
    ?assertEqual(ok, OkFile:validate()),
    TooLongFile = create_uploaded_file(undefined, [{mime_type, "abcdefghijkl" ++
        "mnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyza" ++
        "bcdefghijklmnopqrstuvw"}]),
    ?assertEqual(101, length(TooLongFile:mime_type())),
    ?assertEqual({error,
        ["MIME type must be between 3 and 100 characters long"]},
        TooLongFile:validate()),
    OkFile2 = create_uploaded_file(undefined, [{mime_type, "abc"}]),
    ?assertEqual(ok, OkFile2:validate()),
    TooShortFile = create_uploaded_file(undefined, [{mime_type, "ab"}]),
    ?assertEqual({error,
        ["MIME type must be between 3 and 100 characters long"]},
        TooShortFile:validate()).

val_path() ->
    OkFile = create_uploaded_file(undefined, [{path, "abcdefghijklmnopqrstuv" ++
        "wxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijk" ++
        "lmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz" ++
        "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmno" ++
        "pqrstuvwxyzabcdefghijklmnopqrstuv"}]),
    ?assertEqual(ok, OkFile:validate()),
    TooLongFile = create_uploaded_file(undefined, [{path, "abcdefghijklmnopq" ++
        "rstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdef" ++
        "ghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstu" ++
        "vwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghij" ++
        "klmnopqrstuvwxyzabcdefghijklmnopqrstuvw"}]),
    ?assertEqual(257, length(TooLongFile:path())),
    ?assertEqual({error, ["Path can be up to 256 characters long"]},
        TooLongFile:validate()).

val_type() ->
    ImageFile = create_uploaded_file(undefined, [{type, "image"}]),
    ?assertEqual(ok, ImageFile:validate()),
    OtherFile = create_uploaded_file(undefined, [{type, "other"}]),
    ?assertEqual(ok, OtherFile:validate()),
    InvalidFile = create_uploaded_file(undefined, [{type, "invalid"}]),
    ?assertEqual({error, ["Type is not recognised"]}, InvalidFile:validate()).

create_uploaded_file(Blogger) ->
    File = uploaded_file:new(id, "image", "image/jpeg", undefined, undefined,
                             undefined, undefined),
    case Blogger of
        undefined ->
            File;
        _ ->
            File:set(blogger_id, Blogger:id())
    end.

create_uploaded_file(Blogger, AttrVals) ->
    File = create_uploaded_file(Blogger),
    File:set(AttrVals).

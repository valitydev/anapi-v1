%% -*- mode: erlang -*-
-module(swag_client_utils).

-export([to_binary/1]).
-export([to_list/1]).
-export([to_float/1]).
-export([to_int/1]).
-export([to_lower/1]).
-export([to_upper/1]).
-export([set_resp_headers/2]).
-export([to_header/1]).
-export([to_qs/1]).
-export([to_binding/1]).
-export([binary_to_existing_atom/2]).
-export([get_opt/2]).
-export([get_opt/3]).
-export([priv_dir/0]).
-export([priv_dir/1]).
-export([priv_path/1]).
-export([get_url/2]).
-export([fill_url/3]).
-export([join/1]).
-export([join/2]).


-spec to_binary(iodata() | atom() | number()) -> binary().

to_binary(V) when is_binary(V)  -> V;
to_binary(V) when is_list(V)    -> iolist_to_binary(V);
to_binary(V) when is_atom(V)    -> atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_float(V)   -> float_to_binary(V);
to_binary(_)                    -> erlang:error(badarg).

-spec to_list(iodata() | atom() | number()) -> string().

to_list(V) when is_list(V)      -> V;
to_list(V)                      -> binary_to_list(to_binary(V)).

-spec to_float(iodata() | number()) -> float().

to_float(V) when is_integer(V) -> float(V);
to_float(V) when is_float(V)   -> V;
to_float(V) ->
    Data = iolist_to_binary([V]),
    case binary:split(Data, <<$.>>) of
        [Data] ->
            float(binary_to_integer(Data));
        [<<>>, _] ->
            binary_to_float(<<$0, Data/binary>>);
        _ ->
            binary_to_float(Data)
    end.

%%

-spec to_int(integer() | binary() | list()) -> integer().

to_int(Data) when is_integer(Data) ->
    Data;
to_int(Data) when is_binary(Data) ->
    binary_to_integer(Data);
to_int(Data) when is_list(Data) ->
    list_to_integer(Data);
to_int(_) ->
    erlang:error(badarg).

-spec set_resp_headers([{binary(), iodata()}], cowboy_req:req()) -> cowboy_req:req().

set_resp_headers([], Req) ->
    Req;
set_resp_headers([{K, V} | T], Req0) ->
    Req = cowboy_req:set_resp_header(K, V, Req0),
    set_resp_headers(T, Req).

-spec to_header(iodata() | atom() | number()) -> binary().

to_header(Name) ->
    Prepared = to_binary(Name),
    to_lower(Prepared).

-spec to_qs(iodata() | atom() | number()) -> binary().

to_qs(Name) ->
    to_binary(Name).

-spec to_binding(iodata() | atom() | number()) -> atom().

to_binding(Name) ->
    Prepared = to_binary(Name),
    binary_to_atom(Prepared, utf8).

-spec binary_to_existing_atom(binary(), latin1 | unicode | utf8) -> atom().
binary_to_existing_atom(Bin, Encoding) when is_binary(Bin) ->
    try erlang:binary_to_existing_atom(Bin, Encoding)
    catch
        _:_ ->
            erlang:error(badarg)
    end.

-spec get_opt(any(), []) -> any().

get_opt(Key, Opts) ->
    get_opt(Key, Opts, undefined).

-spec get_opt(any(), [], any()) -> any().

get_opt(Key, Opts, Default) ->
    case lists:keyfind(Key, 1, Opts) of
        {_, Value} -> Value;
        false -> Default
    end.

-spec priv_dir() -> file:filename().

priv_dir() ->
    {ok, AppName} = application:get_application(),
    priv_dir(AppName).

-spec priv_dir(Application :: atom()) -> file:filename().

priv_dir(AppName) ->
    case code:priv_dir(AppName) of
        Value when is_list(Value) ->
            Value ++ "/";
        _Error ->
            select_priv_dir([filename:join(["apps", atom_to_list(AppName), "priv"]), "priv"])
     end.

-spec priv_path(Relative :: file:filename()) -> file:filename().

priv_path(Relative) ->
    filename:join(priv_dir(), Relative).

-include_lib("kernel/include/file.hrl").

select_priv_dir(Paths) ->
    case lists:dropwhile(fun test_priv_dir/1, Paths) of
        [Path | _] -> Path;
        _          -> exit(no_priv_dir)
    end.

test_priv_dir(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = directory}} ->
            false;
        _ ->
            true
    end.


%%

-spec to_lower(binary()) -> binary().

to_lower(S) ->
    to_case(lower, S, <<>>).

-spec to_upper(binary()) -> binary().

to_upper(S) ->
    to_case(upper, S, <<>>).

to_case(_Case, <<>>, Acc) ->
    Acc;

to_case(_Case, <<C, _/binary>>, _Acc) when C > 127 ->
    erlang:error(badarg);

to_case(Case = lower, <<C, Rest/binary>>, Acc) ->
    to_case(Case, Rest, <<Acc/binary, (to_lower_char(C))>>);

to_case(Case = upper, <<C, Rest/binary>>, Acc) ->
    to_case(Case, Rest, <<Acc/binary, (to_upper_char(C))>>);

to_case(_, _, _) ->
    erlang:error(badarg).


to_lower_char(C) when is_integer(C), $A =< C, C =< $Z ->
    C + 32;
to_lower_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 ->
    C + 32;
to_lower_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE ->
    C + 32;
to_lower_char(C) ->
    C.

to_upper_char(C) when is_integer(C), $a =< C, C =< $z ->
    C - 32;
to_upper_char(C) when is_integer(C), 16#E0 =< C, C =< 16#F6 ->
    C - 32;
to_upper_char(C) when is_integer(C), 16#F8 =< C, C =< 16#FE ->
    C - 32;
to_upper_char(C) ->
    C.

-spec join([iodata(), ...]) -> binary().

join(List) ->
    join($\s, List).

-spec join(char() | iodata(), [iodata(), ...]) -> binary().

join(Delim, List) ->
    iolist_to_binary(join_(Delim, List)).

join_(_, [H]) ->
    [H];

join_(Delim, [H | T]) ->
    [H, Delim | join_(Delim, T)].

-type url() :: string().

-spec get_url(Endpoint::swag_client:endpoint(), Path::string()) -> url().
get_url({Host, Port}, Path) ->
    Host ++ ":" ++ integer_to_list(Port) ++ Path;
get_url(Url, Path) ->
    Url ++ Path.

-spec fill_url(Url::string(), Params::map(), Qs::map()) -> url().
fill_url(Url, Params, Qs) ->
    Fun = fun(K, V, AccIn) ->
        re:replace(
            AccIn,
            <<":", (to_binary(K))/binary>>,
            to_binary(V),
            [{return, list}]
        )
    end,
    case maps:size(Qs) > 0 of
        % true ->
        true ->
            Url1 = hackney_url:make_url(Url, [], maps:to_list(Qs)),
            maps:fold(Fun, Url1, Params);
        false -> maps:fold(Fun, Url, Params)
    end.

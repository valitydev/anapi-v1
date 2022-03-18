%% -*- mode: erlang -*-
-module(swag_client_param_validator).

-export([validate/2]).

-type param_base_rule() ::
    swag_client_common_validator:param_rule() |
    {enum, [atom()]} |
    {max, Max :: number(), Type :: exclusive | inclusive} |
    {min, Min :: number(), Type :: exclusive | inclusive} |
    {max_length, MaxLength :: integer()} |
    {min_length, MaxLength :: integer()} |
    {pattern, Pattern :: iodata() | unicode:charlist()} |
    boolean().
-type collection_format() ::
    'csv' |
    'ssv' |
    'tsv' |
    'pipes'.
-type param_rule() ::
    param_base_rule() |
    {'list', collection_format(), [param_base_rule()]}.

-export_type([param_rule/0]).


%% API

-spec validate(Rule :: param_rule(), Param :: swag_client:value()) ->
    ok | {ok, Prepared :: swag_client:value()} | error.

validate(true, _Value) ->
    ok;

validate(false, _Value) ->
    error;

validate({'list', Format, Ruleset}, Value) ->
    try
        Values = parse_array(Format, Value),
        {ok, [validate_ruleset(Ruleset, V) || V <- Values]}
    catch
        _:_ ->
            error
    end;

validate({enum, Values}, Value) ->
    try
        FormattedValue = swag_client_utils:binary_to_existing_atom(Value, utf8),
        case lists:member(FormattedValue, Values) of
            true -> {ok, FormattedValue};
            false -> error
        end
    catch
        error:badarg ->
            error
    end;

validate({max, Max, Type}, Value) ->
    Result = case Value of
        _ when Value < Max andalso Type =:= exclusive ->
            true;
        _ when Value =< Max andalso Type =:= inclusive  ->
            true;
        _ ->
            false
    end,
    case Result of
        true -> ok;
        false -> error
    end;

validate({min, Min, Type}, Value) ->
    Result = case Value of
        _ when Value > Min andalso Type =:= exclusive ->
            true;
        _ when Value >= Min andalso Type =:= inclusive ->
            true;
        _ ->
            false
    end,
    case Result of
        true -> ok;
        false -> error
    end;

validate({max_length, MaxLength}, Value) ->
    case size(Value) =< MaxLength of
        true -> ok;
        false -> error
    end;

validate({min_length, MinLength}, Value) ->
    case size(Value) >= MinLength of
        true -> ok;
        false -> error
    end;

validate({pattern, Pattern}, Value) ->
    {ok, MP} = re:compile(Pattern, [unicode, ucp]),
    case re:run(Value, MP) of
        {match, _} -> ok;
        _ -> error
    end;

validate({RuleName, _} = Rule, Value) when
    RuleName =:= type;
    RuleName =:= format
->
    swag_client_common_validator:validate(Rule, Value).


%% Internal

-spec validate_ruleset(
    Ruleset :: [param_base_rule()],
    Value   :: swag_client:value()
) ->
    Value   :: swag_client:value().

validate_ruleset(Ruleset, Value) ->
    lists:foldl(
        fun(R, V0) ->
            case validate(R, Value) of
                {ok, V} -> V;
                ok -> V0;
                error -> throw(wrong_param)
            end
        end,
        Value,
        Ruleset
    ).

-spec parse_array(
    Format :: collection_format(),
    Array  :: binary()
) ->
    Values :: [binary()].

parse_array(Format, Array) ->
    binary:split(Array, get_split_pattern(Format), [global]).

get_split_pattern('csv') ->
    <<",">>;
get_split_pattern('ssv') ->
    <<" ">>;
get_split_pattern('tsv') ->
    <<"\t">>;
get_split_pattern('pipes') ->
    <<"|">>.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.
-spec validate_integer_test() -> _.
-spec validate_int64_test() -> _.
-spec validate_int32_test() -> _.
-spec validate_float_test() -> _.
-spec validate_binary_test() -> _.
-spec validate_byte_test() -> _.
-spec validate_boolean_test() -> _.
-spec validate_date_test() -> _.
-spec validate_datetime_test() -> _.
-spec validate_email_test() -> _.
-spec validate_ip_address_test() -> _.
-spec validate_uri_test() -> _.
-spec validate_http_url_test() -> _.
-spec validate_enum_test() -> _.
-spec validate_max_test() -> _.
-spec validate_min_test() -> _.
-spec validate_max_length_test() -> _.
-spec validate_min_length_test() -> _.
-spec validate_pattern_test() -> _.
-spec validate_array_test() -> _.

validate_integer_test() ->
    ?assertEqual({ok, 2}, validate({type, 'integer'}, 2)),
    ?assertEqual({ok, 6}, validate({type, 'integer'}, <<"6">>)),
    ?assertEqual(error,   validate({type, 'integer'}, <<"nope">>)).

validate_int64_test() ->
    ?assertEqual({ok, 2}, validate({format, 'int64'},2)),
    ?assertEqual({ok, 6}, validate({format, 'int64'},<<"6">>)),
    ?assertEqual(error, validate({format, 'int64'}, 922337203685477581)),
    ?assertEqual(error, validate({format, 'int64'},-9223372036854775809)).

validate_int32_test() ->
    ?assertEqual({ok, 6}, validate({format, 'int32'}, 6)),
    ?assertEqual({ok, 21}, validate({format, 'int32'}, <<"21">>)),
    ?assertEqual(error, validate({format, 'int32'}, -2147483649)),
    ?assertEqual(error, validate({format, 'int32'},2147483648)).

validate_float_test() ->
    ?assertEqual({ok, 1.9}, validate({type, 'float'}, <<"1.9">>)),
    ?assertEqual({ok, 3.0}, validate({type, 'float'}, <<"3">>)),
    ?assertEqual(error, validate({type, 'float'}, <<"c">>)).

validate_binary_test() ->
    ?assertEqual(ok, validate({type, 'binary'}, <<"f">>)),
    ?assertEqual(error, validate({type, 'binary'}, [])),
    ?assertEqual(error, validate({type, 'binary'}, 3)).

validate_byte_test() ->
    ?assertEqual(ok, validate({type, 'byte'}, <<"0YXRg9C5">>)),
    ?assertEqual(error, validate({type, 'byte'}, <<"g">>)).

validate_boolean_test() ->
    ?assertEqual({ok, true}, validate({type, 'boolean'}, <<"true">>)),
    ?assertEqual({ok, false}, validate({type, 'boolean'}, <<"false">>)),
    ?assertEqual({ok, false}, validate({type, 'boolean'}, false)),
    ?assertEqual(error, validate({type, 'boolean'}, <<"nope">>)).

validate_date_test() ->
    ?assertEqual(ok, validate({format, 'date'}, <<"2014-03-19">>)),
    ?assertEqual(error, validate({format, 'date'}, <<"2014-19-03">>)),
    ?assertEqual(error, validate({format, 'date'}, <<"2013">>)),
    ?assertEqual(error, validate({format, 'date'}, <<"nope">>)),
    ?assertEqual(error, validate({format, 'date'}, <<"2014-03-19 18:00:05-04:00">>)).

validate_datetime_test() ->
    ?assertEqual(ok, validate({format, 'date-time'}, <<"2014-03-19T18:35:03-04:00">>)),
    ?assertEqual(error, validate({format, 'date-time'}, <<"2014-11-19">>)),
    ?assertEqual(error, validate({format, 'date-time'}, <<"nope">>)).

validate_email_test() ->
    ?assertEqual(ok, validate({format, 'email'}, <<"me@example.com">>)),
    ?assertEqual(error, validate({format, 'email'}, <<"m\\e@example.com">>)),
    ?assertEqual(error, validate({format, 'email'}, <<"nope">>)).

validate_ip_address_test() ->
    ?assertEqual(ok, validate({format, 'ip-address'}, <<"127.0.0.1">>)),
    ?assertEqual(ok, validate({format, 'ip-address'}, <<"::1">>)),
    ?assertEqual(error, validate({format, 'ip-address'}, <<"127.0.0.0.1">>)),
    ?assertEqual(error, validate({format, 'ip-address'}, <<"nope">>)).

validate_uri_test() ->
    ?assertEqual(ok, validate({format, 'uri'}, <<"http://localhost">>)),
    ?assertEqual(ok, validate({format, 'uri'}, <<"ftp://user@host:21/home">>)),
    ?assertEqual(error, validate({format, 'uri'}, <<"127.0.0.1">>)),
    ?assertEqual(error, validate({format, 'uri'}, <<"example.com">>)).

validate_http_url_test() ->
    ?assertEqual(ok, validate({format, 'http-url'}, <<"http://localhost">>)),
    ?assertEqual(error, validate({format, 'http-url'}, <<"ftp://user@host:21/home">>)).

validate_enum_test() ->
    ?assertEqual({ok, sad}, validate({enum, [i, am, sad]} , <<"sad">>)),
    ?assertEqual(error, validate({enum, ['All work and no play', 'makes Jack a dull boy']}, <<"Artem">>)),
    ?assertEqual(error, validate({enum, []}, <<"">>)).

validate_max_test() ->
    ?assertEqual(ok, validate({max, 10, inclusive}, 10)),
    ?assertEqual(error, validate({max, 10, exclusive}, 10)),
    ?assertEqual(ok, validate({max, 32, inclusive}, 21)),
    ?assertEqual(ok, validate({max, 32, exclusive}, 21)).

validate_min_test() ->
    ?assertEqual(ok, validate({min, 33, inclusive}, 33)),
    ?assertEqual(error, validate({min, 33, exclusive}, 33)),
    ?assertEqual(ok, validate({min, 57, inclusive}, 60)),
    ?assertEqual(ok, validate({min, 57, inclusive}, 60)).

validate_max_length_test() ->
    ?assertEqual(ok, validate({max_length, 5}, <<"hello">>)),
    ?assertEqual(ok, validate({max_length, 5}, <<"h">>)),
    ?assertEqual(error, validate({max_length, 5}, <<"hello?">>)).

validate_min_length_test() ->
    ?assertEqual(ok, validate({min_length, 5}, <<"hello">>)),
    ?assertEqual(ok, validate({min_length, 5}, <<"hello?">>)),
    ?assertEqual(error, validate({min_length, 5}, <<"h">>)).

validate_pattern_test() ->
    ?assertEqual(ok, validate({pattern, <<"[abc]">>},  <<"adcv">>)),
    ?assertEqual(error, validate({pattern, <<"[abc]">>},  <<"fgh0">>)),
    ?assertEqual(ok, validate({pattern, <<"^[0-9]{2}\/[0-9]{2}$">>}, <<"22/22">>)),
    ?assertEqual(error, validate({pattern, <<"^[0-9]{2}\/[0-9]{2}$">>}, <<"22/225">>)).

validate_array_test() ->
    ?assertEqual({ok, [10,11,12]}, validate({list, 'csv', [{type, 'integer'}]}, <<"10,11,12">>)),
    ?assertEqual(error, validate({list, 'csv', [{type, 'integer'}]}, <<"10,xyi,12">>)).

-endif.

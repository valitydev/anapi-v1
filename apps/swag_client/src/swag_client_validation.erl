%% -*- mode: erlang -*-
-module(swag_client_validation).

-export([prepare_request_param/3]).
-export([validate_response/2]).

-type rule()          :: schema | {required, boolean()} | swag_client_param_validator:param_rule().
-type data_type()     :: 'list' | atom().
-type response_spec() :: {data_type(), swag_client:param_name()} | undefined.

-type error() :: #{
    type        := error_type(),
    description => swag_client:error_reason()
}.

-type error_type() ::
    no_match        |
    not_found       |
    not_in_range    |
    wrong_length    |
    wrong_size      |
    schema_violated |
    wrong_type |
    wrong_array.

-export_type([rule/0]).
-export_type([response_spec/0]).
-export_type([error/0]).
-export_type([error_type/0]).

-define(catch_error(Block),
    try
        {ok, Block}
    catch
        throw:{wrong_param, _Name, Error} ->
            {error, Error}
    end
).


%% API

-spec prepare_request_param(
    Rules :: [rule()],
    Name  :: swag_client:param_name(),
    Value :: swag_client:value()
) ->
    {ok,    Value :: swag_client:value()} |
    {error, Error :: error()}.

prepare_request_param(Rules, Name, Value) ->
    ?catch_error(validate_param(Rules, Name, Value)).

-spec validate_response(
    Spec :: response_spec(),
    Resp :: swag_client:object() | [swag_client:object()] | undefined
) ->
    ok |
    {error, Error :: error()}.

validate_response({DataType, SchemaName}, Body) ->
    Result = case DataType of
        'list' ->
            ?catch_error([validate(schema, SchemaName, Item, response) || Item <- Body]);
        _ ->
            ?catch_error(validate(schema, SchemaName, Body, response))
    end,
    case Result of
        E = {error, _} -> E;
        _ -> ok
    end;
validate_response(undefined, undefined) ->
    ok;
validate_response(undefined, _) ->
    {error, map_error(schema, <<"Must be empty">>)}.


%% Internal

-spec validate_param(
    Rules :: [rule()],
    Name  :: swag_client:param_name(),
    Value :: swag_client:value()
) ->
    Prepared :: swag_client:value() | no_return().

validate_param(Rules, Name, Value) ->
    lists:foldl(
        fun(Rule, Acc) ->
            case validate(Rule, Name, Acc, request) of
                ok             -> Acc;
                {ok, Prepared} -> Prepared
            end
        end,
        Value,
        Rules
    ).

validate(Rule = {required, true}, Name, undefined, _MsgType) ->
    report_validation_error(Rule, Name);
validate({required, _}, _Name, _, _MsgType) ->
    ok;
validate(_, _Name, undefined, _MsgType) ->
    ok;
validate(Rule = schema, Name, Value, MsgType) ->
    case swag_client_schema_validator:validate(Value, Name, MsgType) of
        ok ->
            ok;
        {error, Reason} ->
            report_validation_error(Rule, Name, Reason)
    end;
validate(Rule, Name, Value, _MsgType) ->
    case swag_client_param_validator:validate(Rule, Value) of
        ok ->
            ok;
        Ok = {ok, _} ->
            Ok;
        error ->
            report_validation_error(Rule, Name)
    end.

-spec report_validation_error(Rule :: rule(), Param :: swag_client:param_name()) ->
    no_return().

report_validation_error(Rule, Param) ->
    report_validation_error(Rule, Param, undefined).

-spec report_validation_error(
    Rule        :: rule(),
    Param       :: swag_client:param_name(),
    Description :: swag_client:error_reason() | undefined
) ->
    no_return().

report_validation_error(Rule, Param, Description) ->
    throw({wrong_param, Param, map_error(Rule, Description)}).

-spec map_error(Rule :: rule(), Description :: swag_client:error_reason() | undefined) ->
   Error :: error().
map_error(Rule, Description) ->
    Error = #{type => map_violated_rule(Rule)},
    case Description of
        undefined -> Error;
        _ -> Error#{description => Description}
    end.

-spec map_violated_rule(Rule :: rule()) ->
    ErrorType :: error_type().

map_violated_rule({type, _Type})   ->  wrong_type;
map_violated_rule({enum, _})       ->  not_in_range;
map_violated_rule({max, _, _})     ->  wrong_size;
map_violated_rule({min, _, _})     ->  wrong_size;
map_violated_rule({max_length, _}) ->  wrong_length;
map_violated_rule({min_length, _}) ->  wrong_length;
map_violated_rule({pattern, _}) ->  no_match;
map_violated_rule(schema) ->  schema_violated;
map_violated_rule({required, _}) ->  not_found;
map_violated_rule({list, _, _}) ->  wrong_array.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.
-spec validate_required_test() -> _.

validate_required_test() ->
    ?assertEqual(ok, validate({required, true}, 'Name', <<"test">>, request)),
    ?assertEqual(ok, validate({required, false}, 'Name', <<"test">>, request)),
    ?assertThrow({wrong_param, _, _}, validate({required, true}, 'Name', undefined, request)).

-endif.

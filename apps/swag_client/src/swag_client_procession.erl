%% -*- mode: erlang -*-
-module(swag_client_procession).

-export([process_request/5]).
-export([process_response/2]).

%%

-type params() :: #{
    header      := any(),
    binding     := any(),
    body        := any(),
    qs_val      := any()
}.
-type opts()   :: swag_client:transport_opts().
-type object() :: swag_client:object().
-type url()    :: string().
-type method() :: atom().
-type code()   :: pos_integer().
-type body()   :: binary().

%% IMPORTANT (TODO): Please note that form_data is not supported
-type param_source() ::
    qs_val  |
    binding |
    header  |
    body.

-type request_spec() :: [{
    swag_client:param_name(),
    #{source := param_source(), rules := [swag_client_validation:rule()]}
}].

-type response_spec() :: swag_client_validation:response_spec().

-export_type([code/0]).
-export_type([body/0]).
-export_type([request_spec/0]).
-export_type([response_spec/0]).

%% API

-spec process_request(method(), url(), params(), request_spec(), opts()) ->
    {ok, code(), list(), body()} |
    {error, {request_validation_failed,  { swag_client_validation:error(), BadParam :: atom()}}} |
    {error, _Reason}.
process_request(Method, BaseUrl, Params, Spec, Opts) ->
    case prepare_request(Spec, BaseUrl, Params) of
        {ok, #{
            url     := Url,
            headers := Headers,
            body    := Body
        }} ->
            hackney:request(Method, Url, Headers, Body, [with_body] ++ Opts);
        Error ->
            Error
    end.

-spec process_response(response_spec(), body()) ->
    {ok, object()} |
    {error, {response_validation_failed, swag_client_validation:error(), _Response}}.
process_response(undefined, <<>>) ->
    {ok, #{}};
process_response(Spec, RespBody) ->
    Resp = jsx:decode(RespBody, [return_maps]),
    case validate_response(Spec, Resp) of
        ok ->
            {ok, Resp};
        Error ->
            Error
    end.

%% Internal

prepare_request(Spec, Url, Params) ->
    case validate_request(Spec, Params) of
        ok ->
            {ok, prepare_params(Url, Params)};
        Error ->
            Error
    end.

prepare_params(Url, #{binding := Bindings, qs_val := Query, header := Headers, body := Body}) ->
    #{
        url     => prepare_url(Url, Bindings, Query),
        headers => maps:to_list(Headers),
        body    => jsx:encode(Body)
    }.

prepare_url(Url, Params, Qs) ->
    swag_client_utils:fill_url(Url, Params, Qs).

validate_request([], _) ->
    ok;
validate_request([ParamSpec | T], Params) ->
    case validate_request_param(ParamSpec, Params) of
        ok ->
            validate_request(T, Params);
        Error ->
            Error
    end.

validate_request_param({Name, #{rules := Rules, source := Source}}, Params) ->
    Value = get_value(Source, Name, Params),
    case swag_client_validation:prepare_request_param(Rules, Name, Value) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, {request_validation_failed, {Reason, Name}}}
    end.

-spec get_value(atom(), any(), params()) ->
    any().
get_value(body, _Name, Params) ->
    maps:get(body, Params);
get_value(qs_val, Name, Params) ->
    QueryParams = maps:get(qs_val, Params),
    get_opt(swag_client_utils:to_binary(Name), QueryParams);
get_value(header, Name, Params) ->
    HeaderParams = maps:get(header, Params),
    get_opt(swag_client_utils:to_binary(Name), HeaderParams);
get_value(binding, Name, Params) ->
    BindingParams = maps:get(binding, Params),
    get_opt(swag_client_utils:to_binary(Name), BindingParams).

get_opt(Key, Opts) ->
    get_opt(Key, Opts, undefined).

get_opt(Key, Opts, Default) ->
    maps:get(Key, Opts, Default).

validate_response(Spec, RespBody) ->
    case swag_client_validation:validate_response(Spec, RespBody) of
        ok ->
            ok;
        {error, Error} ->
            {error, {response_validation_failed, Error, RespBody}}
    end.

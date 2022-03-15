%% -*- mode: erlang -*-

%% basic handler
-module(swag_server_search_handler).

%% Cowboy REST callbacks
-export([allowed_methods/2]).
-export([init/2]).
-export([allow_missing_post/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([charsets_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([known_content_type/2]).
-export([malformed_request/2]).
-export([valid_content_headers/2]).
-export([valid_entity_length/2]).

%% Handlers
-export([handle_request_json/2]).

-record(state, {
    operation_id  :: swag_server:operation_id(),
    logic_handler :: module(),
    swagger_handler_opts :: swag_server_router:swagger_handler_opts(),
    context       :: swag_server:request_context()
}).

-type state()              :: state().
-type content_type()       :: {binary(), binary(), '*' | [{binary(), binary()}]}.
-type processed_response() :: {stop, cowboy_req:req(), state()}.

%% Cowboy REST callbacks

-spec init(Req :: cowboy_req:req(), Opts :: swag_server_router:init_opts()) ->
    {cowboy_rest, Req :: cowboy_req:req(), State :: state()}.

init(Req, {_Operations, LogicHandler, SwaggerHandlerOpts} = InitOpts) ->
    OperationID    = swag_server_utils:get_operation_id(Req, InitOpts),

    error_logger:info_msg("Attempt to process operation: ~p", [OperationID]),

    State = #state{
        operation_id  = OperationID,
        logic_handler = LogicHandler,
        swagger_handler_opts = SwaggerHandlerOpts,
        context       = #{cowboy_req => Req}
    },
    {cowboy_rest, Req, State}.

-spec allowed_methods(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: [binary()], Req :: cowboy_req:req(), State :: state()}.


allowed_methods(
    Req,
    State = #state{
        operation_id = 'SearchChargebacks'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'SearchInvoices'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'SearchPayments'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'SearchPayouts'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(
    Req,
    State = #state{
        operation_id = 'SearchRefunds'
    }
) ->
    {[<<"GET">>], Req, State};

allowed_methods(Req, State) ->
    {[], Req, State}.

-spec is_authorized(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: true | {false, AuthHeader :: iodata()},
        Req   :: cowboy_req:req(),
        State :: state()
    }.

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'SearchChargebacks' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'SearchInvoices' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'SearchPayments' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'SearchPayouts' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(
    Req0,
    State = #state{
        operation_id  = 'SearchRefunds' = OperationID,
        logic_handler = LogicHandler,
        context       = Context
    }
) ->
    From = header,
    Result = swag_server_handler_api:authorize_api_key(
        LogicHandler,
        OperationID,
        From,
        'Authorization',
        Req0,
        Context
    ),
    case Result of
        {true, AuthContext, Req} ->
            NewContext = Context#{
                auth_context => AuthContext
            },
            {true, Req, State#state{context = NewContext}};
        {false, AuthHeader, Req} ->
            {{false, AuthHeader}, Req, State}
    end;

is_authorized(Req, State) ->
    {{false, <<"">>}, Req, State}.

-spec content_types_accepted(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{content_type(), AcceptResource :: atom()}],
        Req   :: cowboy_req:req(),
        State :: state()
    }.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, [{<<"charset">>, <<"utf-8">>}]}, handle_request_json}
    ], Req, State}.

-spec valid_content_headers(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'SearchChargebacks'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'SearchInvoices'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'SearchPayments'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'SearchPayouts'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(
    Req0,
    State = #state{
        operation_id = 'SearchRefunds'
    }
) ->
    Headers = ["X-Request-ID","X-Request-Deadline"],
    {Result, Req} = validate_headers(Headers, Req0),
    {Result, Req, State};

valid_content_headers(Req, State) ->
    {false, Req, State}.

-spec content_types_provided(Req :: cowboy_req:req(), State :: state()) ->
    {
        Value :: [{content_type(), ProvideResource :: atom()}],
        Req   :: cowboy_req:req(),
        State :: state()
    }.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, handle_request_json}
    ], Req, State}.

-spec charsets_provided(Req :: cowboy_req:req(), State :: state()) ->
    {Charsets :: [binary()], Req :: cowboy_req:req(), State :: state()}.

charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.

-spec malformed_request(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: cowboy_req:req(), State :: state()}.

malformed_request(Req, State = #state{context = Context}) ->
    PeerResult = swag_server_handler_api:determine_peer(Req),
    case PeerResult of
        {ok, Peer} ->
            Context1 = Context#{peer => Peer},
            State1   = State#state{context = Context1},
            {false, Req, State1};
        {error, Reason} ->
            error_logger:error_msg("Unable to determine client peer: ~p", [Reason]),
            {true, Req, State}
    end.

-spec allow_missing_post(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: false, Req :: cowboy_req:req(), State :: state()}.

allow_missing_post(Req, State) ->
    {false, Req, State}.

-spec delete_resource(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

delete_resource(Req, State) ->
    handle_request_json(Req, State).

-spec known_content_type(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

known_content_type(Req, State) ->
    {true, Req, State}.

-spec valid_entity_length(Req :: cowboy_req:req(), State :: state()) ->
    {Value :: true, Req :: cowboy_req:req(), State :: state()}.

valid_entity_length(Req, State) ->
    %% @TODO check the length
    {true, Req, State}.


%% Handlers

-spec handle_request_json(Req :: cowboy_req:req(), State :: state()) ->
    processed_response().

handle_request_json(
    Req0,
    State = #state{
        operation_id  = OperationID,
        logic_handler = LogicHandler,
        swagger_handler_opts = SwaggerHandlerOpts,
        context       = Context
    }
) ->
    ValidationOpts = maps:get(validation_opts, SwaggerHandlerOpts, #{}),
    case populate_request(LogicHandler, OperationID, Req0, ValidationOpts) of
        {ok, Populated, Req1} ->
            {Status, Resp} = handle_request(LogicHandler, OperationID, Populated, Context),
            ok = validate_response(Status, Resp, OperationID, ValidationOpts),
            process_response(ok, encode_response(Resp), Req1, State);
        {error, Reason, Req1} ->
            process_response(error, Reason, Req1, State)
    end.


%% Internal

populate_request(LogicHandler, OperationID, Req, ValidationOpts) ->
    Spec = get_request_spec(OperationID),
    swag_server_handler_api:populate_request(LogicHandler, OperationID, Spec, Req, ValidationOpts).

handle_request(LogicHandler, OperationID, Populated, Context) ->
    swag_server_logic_handler:handle_request(LogicHandler, OperationID, Populated, Context).

validate_response(error, _, _, _) ->
    ok;
validate_response(ok, {Code, _Headers, Body}, OperationID, ValidationOpts) ->
    Spec = get_response_spec(OperationID, Code),
    swag_server_handler_api:validate_response(OperationID, Spec, Body, ValidationOpts).

encode_response(Resp) ->
    swag_server_handler_api:encode_response(Resp).

process_response(Status, Result, Req0, State = #state{operation_id = OperationID}) ->
    Req = swag_server_handler_api:process_response(Status, Result, Req0, OperationID),
    {stop, Req, State}.

validate_headers(_, Req) ->
    {true, Req}.

-spec get_request_spec(OperationID :: swag_server:operation_id()) ->
    Spec :: swag_server_handler_api:request_spec() | no_return().


get_request_spec('SearchChargebacks') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'partyID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }},
        {'offset', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {min, 0, inclusive}, true
, {required, false}]
        }},
        {'invoiceID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'paymentID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'chargebackID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'chargebackStatuses', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, {enum, ['pending', 'accepted', 'rejected', 'cancelled']}, true
]}, true
, {required, false}]
        }},
        {'chargebackStages', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, {enum, ['chargeback', 'pre_arbitration', 'arbitration']}, true
]}, true
, {required, false}]
        }},
        {'chargebackCategories', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, {enum, ['fraud', 'dispute', 'authorisation', 'processing_error']}, true
]}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ];
get_request_spec('SearchInvoices') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'partyID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }},
        {'invoiceIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'invoiceStatus', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['unpaid', 'cancelled', 'paid', 'fulfilled']}, true
, {required, false}]
        }},
        {'invoiceID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'externalID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'invoiceAmountFrom', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, {min, 1, inclusive}, true
, {required, false}]
        }},
        {'invoiceAmountTo', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, {min, 1, inclusive}, true
, {required, false}]
        }},
        {'excludedShops', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ];
get_request_spec('SearchPayments') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'partyID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }},
        {'invoiceIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentStatus', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['pending', 'processed', 'captured', 'cancelled', 'refunded', 'failed']}, true
, {required, false}]
        }},
        {'paymentFlow', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['instant', 'hold']}, true
, {required, false}]
        }},
        {'paymentMethod', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['bankCard', 'paymentTerminal']}, true
, {required, false}]
        }},
        {'paymentTerminalProvider', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['euroset', 'wechat', 'alipay', 'zotapay', 'qps', 'uzcard', 'rbs']}, true
, {required, false}]
        }},
        {'invoiceID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'paymentID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'externalID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'payerEmail', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'email'}, {max_length, 100}, true
, {required, false}]
        }},
        {'payerIP', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'ip-address'}, {max_length, 45}, true
, {required, false}]
        }},
        {'payerFingerprint', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 1000}, true
, {required, false}]
        }},
        {'customerID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'first6', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^\\d{6}$"}, true
, {required, false}]
        }},
        {'last4', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^\\d{4}$"}, true
, {required, false}]
        }},
        {'rrn', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {pattern, "^[a-zA-Z0-9]{12}$"}, true
, {required, false}]
        }},
        {'approvalCode', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'bankCardTokenProvider', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['applepay', 'googlepay', 'samsungpay', 'yandexpay']}, true
, {required, false}]
        }},
        {'bankCardPaymentSystem', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['visa', 'mastercard', 'visaelectron', 'maestro', 'forbrugsforeningen', 'dankort', 'amex', 'dinersclub', 'discover', 'unionpay', 'jcb', 'nspkmir', 'elo', 'rupay', 'dummy', 'uzcard']}, true
, {required, false}]
        }},
        {'paymentAmountFrom', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, {min, 1, inclusive}, true
, {required, false}]
        }},
        {'paymentAmountTo', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int64'}, {min, 1, inclusive}, true
, {required, false}]
        }},
        {'excludedShops', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ];
get_request_spec('SearchPayouts') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'partyID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }},
        {'offset', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {min, 0, inclusive}, true
, {required, false}]
        }},
        {'payoutID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'payoutToolType', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['PayoutAccount', 'Wallet', 'PaymentInstitutionAccount']}, true
, {required, false}]
        }},
        {'excludedShops', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ];
get_request_spec('SearchRefunds') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'partyID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'fromTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'toTime', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {format, 'date-time'}, true
, {required, true}]
        }},
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }},
        {'offset', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {min, 0, inclusive}, true
, {required, false}]
        }},
        {'invoiceIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'invoiceID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'paymentID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'refundID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'externalID', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'refundStatus', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['pending', 'succeeded', 'failed']}, true
, {required, false}]
        }},
        {'excludedShops', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'continuationToken', #{
            source => qs_val,
            rules  => [{type, 'binary'}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_server:operation_id(), Code :: cowboy:http_status()) ->
    Spec :: swag_server_handler_api:response_spec() | no_return().


get_response_spec('SearchChargebacks', 200) ->
    {'inline_response_200_8', 'inline_response_200_8'};

get_response_spec('SearchChargebacks', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('SearchChargebacks', 401) ->
    undefined;

get_response_spec('SearchChargebacks', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('SearchInvoices', 200) ->
    {'inline_response_200_9', 'inline_response_200_9'};

get_response_spec('SearchInvoices', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('SearchInvoices', 401) ->
    undefined;

get_response_spec('SearchInvoices', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('SearchPayments', 200) ->
    {'inline_response_200_10', 'inline_response_200_10'};

get_response_spec('SearchPayments', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('SearchPayments', 401) ->
    undefined;

get_response_spec('SearchPayments', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('SearchPayouts', 200) ->
    {'inline_response_200_11', 'inline_response_200_11'};

get_response_spec('SearchPayouts', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('SearchPayouts', 401) ->
    undefined;

get_response_spec('SearchPayouts', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('SearchRefunds', 200) ->
    {'inline_response_200_12', 'inline_response_200_12'};

get_response_spec('SearchRefunds', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('SearchRefunds', 401) ->
    undefined;

get_response_spec('SearchRefunds', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(OperationID, Code) ->
    error({invalid_response_code, OperationID, Code}).

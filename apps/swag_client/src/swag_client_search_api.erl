%% -*- mode: erlang -*-
-module(swag_client_search_api).

%% generated methods

-export([search_chargebacks/2]).
-export([search_chargebacks/3]).

-export([search_invoices/2]).
-export([search_invoices/3]).

-export([search_payments/2]).
-export([search_payments/3]).

-export([search_payouts/2]).
-export([search_payouts/3]).

-export([search_refunds/2]).
-export([search_refunds/3]).


-spec search_chargebacks(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_chargebacks(Endpoint, Params) ->
    search_chargebacks(Endpoint, Params, []).

-spec search_chargebacks(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_chargebacks(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/chargebacks"),
        Params,
        get_request_spec(search_chargebacks),
        Opts
    ), search_chargebacks).

-spec search_invoices(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_invoices(Endpoint, Params) ->
    search_invoices(Endpoint, Params, []).

-spec search_invoices(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_invoices(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/invoices"),
        Params,
        get_request_spec(search_invoices),
        Opts
    ), search_invoices).

-spec search_payments(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_payments(Endpoint, Params) ->
    search_payments(Endpoint, Params, []).

-spec search_payments(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_payments(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/payments"),
        Params,
        get_request_spec(search_payments),
        Opts
    ), search_payments).

-spec search_payouts(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_payouts(Endpoint, Params) ->
    search_payouts(Endpoint, Params, []).

-spec search_payouts(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_payouts(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/payouts"),
        Params,
        get_request_spec(search_payouts),
        Opts
    ), search_payouts).

-spec search_refunds(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_refunds(Endpoint, Params) ->
    search_refunds(Endpoint, Params, []).

-spec search_refunds(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_refunds(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/refunds"),
        Params,
        get_request_spec(search_refunds),
        Opts
    ), search_refunds).

process_response({ok, Code, Headers, RespBody}, OperationID) ->
    try swag_client_procession:process_response(
        get_response_spec(OperationID, Code),
        RespBody
    ) of
        {ok, Resp} ->
            {ok, Code, Headers, Resp};
        Error ->
            Error
    catch
        error:invalid_response_code ->
            {error, {invalid_response_code, Code}}
    end;
process_response(Error, _) ->
    Error.


-spec get_request_spec(OperationID :: swag_client:operation_id()) ->
    Spec :: swag_client_procession:request_spec() | no_return().


get_request_spec('search_chargebacks') ->
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
get_request_spec('search_invoices') ->
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
get_request_spec('search_payments') ->
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
get_request_spec('search_payouts') ->
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
get_request_spec('search_refunds') ->
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

-spec get_response_spec(OperationID :: swag_client:operation_id(), Code :: swag_client_procession:code()) ->
    Spec :: swag_client_procession:response_spec() | no_return().


get_response_spec('search_chargebacks', 200) ->
    {'inline_response_200_8', 'inline_response_200_8'};

get_response_spec('search_chargebacks', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('search_chargebacks', 401) ->
    undefined;

get_response_spec('search_chargebacks', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('search_invoices', 200) ->
    {'inline_response_200_9', 'inline_response_200_9'};

get_response_spec('search_invoices', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('search_invoices', 401) ->
    undefined;

get_response_spec('search_invoices', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('search_payments', 200) ->
    {'inline_response_200_10', 'inline_response_200_10'};

get_response_spec('search_payments', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('search_payments', 401) ->
    undefined;

get_response_spec('search_payments', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('search_payouts', 200) ->
    {'inline_response_200_11', 'inline_response_200_11'};

get_response_spec('search_payouts', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('search_payouts', 401) ->
    undefined;

get_response_spec('search_payouts', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('search_refunds', 200) ->
    {'inline_response_200_12', 'inline_response_200_12'};

get_response_spec('search_refunds', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('search_refunds', 401) ->
    undefined;

get_response_spec('search_refunds', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).

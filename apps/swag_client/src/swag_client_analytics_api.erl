%% -*- mode: erlang -*-
-module(swag_client_analytics_api).

%% generated methods

-export([get_average_payment/2]).
-export([get_average_payment/3]).

-export([get_current_balances/2]).
-export([get_current_balances/3]).

-export([get_current_balances_group_by_shop/2]).
-export([get_current_balances_group_by_shop/3]).

-export([get_payments_amount/2]).
-export([get_payments_amount/3]).

-export([get_payments_count/2]).
-export([get_payments_count/3]).

-export([get_payments_error_distribution/2]).
-export([get_payments_error_distribution/3]).

-export([get_payments_split_amount/2]).
-export([get_payments_split_amount/3]).

-export([get_payments_split_count/2]).
-export([get_payments_split_count/3]).

-export([get_payments_sub_error_distribution/2]).
-export([get_payments_sub_error_distribution/3]).

-export([get_payments_tool_distribution/2]).
-export([get_payments_tool_distribution/3]).

-export([get_refunds_amount/2]).
-export([get_refunds_amount/3]).


-spec get_average_payment(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_average_payment(Endpoint, Params) ->
    get_average_payment(Endpoint, Params, []).

-spec get_average_payment(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_average_payment(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/analytics/payments/average"),
        Params,
        get_request_spec(get_average_payment),
        Opts
    ), get_average_payment).

-spec get_current_balances(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_current_balances(Endpoint, Params) ->
    get_current_balances(Endpoint, Params, []).

-spec get_current_balances(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_current_balances(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/analytics/balances/current"),
        Params,
        get_request_spec(get_current_balances),
        Opts
    ), get_current_balances).

-spec get_current_balances_group_by_shop(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_current_balances_group_by_shop(Endpoint, Params) ->
    get_current_balances_group_by_shop(Endpoint, Params, []).

-spec get_current_balances_group_by_shop(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_current_balances_group_by_shop(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/analytics/balances/current-shop-balances"),
        Params,
        get_request_spec(get_current_balances_group_by_shop),
        Opts
    ), get_current_balances_group_by_shop).

-spec get_payments_amount(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_amount(Endpoint, Params) ->
    get_payments_amount(Endpoint, Params, []).

-spec get_payments_amount(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_amount(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/analytics/payments/amount"),
        Params,
        get_request_spec(get_payments_amount),
        Opts
    ), get_payments_amount).

-spec get_payments_count(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_count(Endpoint, Params) ->
    get_payments_count(Endpoint, Params, []).

-spec get_payments_count(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_count(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/analytics/payments/count"),
        Params,
        get_request_spec(get_payments_count),
        Opts
    ), get_payments_count).

-spec get_payments_error_distribution(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_error_distribution(Endpoint, Params) ->
    get_payments_error_distribution(Endpoint, Params, []).

-spec get_payments_error_distribution(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_error_distribution(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/analytics/payments/errors"),
        Params,
        get_request_spec(get_payments_error_distribution),
        Opts
    ), get_payments_error_distribution).

-spec get_payments_split_amount(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_split_amount(Endpoint, Params) ->
    get_payments_split_amount(Endpoint, Params, []).

-spec get_payments_split_amount(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_split_amount(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/analytics/payments/split-amount"),
        Params,
        get_request_spec(get_payments_split_amount),
        Opts
    ), get_payments_split_amount).

-spec get_payments_split_count(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_split_count(Endpoint, Params) ->
    get_payments_split_count(Endpoint, Params, []).

-spec get_payments_split_count(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_split_count(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/analytics/payments/split-count"),
        Params,
        get_request_spec(get_payments_split_count),
        Opts
    ), get_payments_split_count).

-spec get_payments_sub_error_distribution(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_sub_error_distribution(Endpoint, Params) ->
    get_payments_sub_error_distribution(Endpoint, Params, []).

-spec get_payments_sub_error_distribution(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_sub_error_distribution(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/analytics/payments/sub-errors"),
        Params,
        get_request_spec(get_payments_sub_error_distribution),
        Opts
    ), get_payments_sub_error_distribution).

-spec get_payments_tool_distribution(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_tool_distribution(Endpoint, Params) ->
    get_payments_tool_distribution(Endpoint, Params, []).

-spec get_payments_tool_distribution(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_payments_tool_distribution(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/analytics/payments-tool"),
        Params,
        get_request_spec(get_payments_tool_distribution),
        Opts
    ), get_payments_tool_distribution).

-spec get_refunds_amount(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_refunds_amount(Endpoint, Params) ->
    get_refunds_amount(Endpoint, Params, []).

-spec get_refunds_amount(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_refunds_amount(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/analytics/refunds/amount"),
        Params,
        get_request_spec(get_refunds_amount),
        Opts
    ), get_refunds_amount).

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


get_request_spec('get_average_payment') ->
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
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'excludeShopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }}
    ];
get_request_spec('get_current_balances') ->
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
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'excludeShopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }}
    ];
get_request_spec('get_current_balances_group_by_shop') ->
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
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'excludeShopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payments_amount') ->
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
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'excludeShopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payments_count') ->
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
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'excludeShopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payments_error_distribution') ->
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
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'excludeShopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payments_split_amount') ->
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
        {'splitUnit', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['minute', 'hour', 'day', 'week', 'month', 'year']}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'excludeShopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payments_split_count') ->
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
        {'splitUnit', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['minute', 'hour', 'day', 'week', 'month', 'year']}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'excludeShopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payments_sub_error_distribution') ->
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
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'excludeShopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }}
    ];
get_request_spec('get_payments_tool_distribution') ->
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
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'excludeShopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }}
    ];
get_request_spec('get_refunds_amount') ->
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
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }},
        {'shopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'excludeShopIDs', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, true
]}, true
, {required, false}]
        }},
        {'paymentInstitutionRealm', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['live', 'test']}, true
, {required, false}]
        }}
    ].

-spec get_response_spec(OperationID :: swag_client:operation_id(), Code :: swag_client_procession:code()) ->
    Spec :: swag_client_procession:response_spec() | no_return().


get_response_spec('get_average_payment', 200) ->
    {'inline_response_200_1', 'inline_response_200_1'};

get_response_spec('get_average_payment', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_average_payment', 401) ->
    undefined;

get_response_spec('get_average_payment', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_current_balances', 200) ->
    {'inline_response_200_1', 'inline_response_200_1'};

get_response_spec('get_current_balances', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_current_balances', 401) ->
    undefined;

get_response_spec('get_current_balances', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_current_balances_group_by_shop', 200) ->
    {'inline_response_200', 'inline_response_200'};

get_response_spec('get_current_balances_group_by_shop', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_current_balances_group_by_shop', 401) ->
    undefined;

get_response_spec('get_current_balances_group_by_shop', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payments_amount', 200) ->
    {'inline_response_200_1', 'inline_response_200_1'};

get_response_spec('get_payments_amount', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payments_amount', 401) ->
    undefined;

get_response_spec('get_payments_amount', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payments_count', 200) ->
    {'inline_response_200_3', 'inline_response_200_3'};

get_response_spec('get_payments_count', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payments_count', 401) ->
    undefined;

get_response_spec('get_payments_count', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payments_error_distribution', 200) ->
    {'inline_response_200_4', 'inline_response_200_4'};

get_response_spec('get_payments_error_distribution', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payments_error_distribution', 401) ->
    undefined;

get_response_spec('get_payments_error_distribution', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payments_split_amount', 200) ->
    {'inline_response_200_5', 'inline_response_200_5'};

get_response_spec('get_payments_split_amount', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payments_split_amount', 401) ->
    undefined;

get_response_spec('get_payments_split_amount', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payments_split_count', 200) ->
    {'inline_response_200_6', 'inline_response_200_6'};

get_response_spec('get_payments_split_count', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payments_split_count', 401) ->
    undefined;

get_response_spec('get_payments_split_count', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payments_sub_error_distribution', 200) ->
    {'inline_response_200_7', 'inline_response_200_7'};

get_response_spec('get_payments_sub_error_distribution', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payments_sub_error_distribution', 401) ->
    undefined;

get_response_spec('get_payments_sub_error_distribution', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_payments_tool_distribution', 200) ->
    {'inline_response_200_2', 'inline_response_200_2'};

get_response_spec('get_payments_tool_distribution', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_payments_tool_distribution', 401) ->
    undefined;

get_response_spec('get_payments_tool_distribution', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_refunds_amount', 200) ->
    {'inline_response_200_1', 'inline_response_200_1'};

get_response_spec('get_refunds_amount', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_refunds_amount', 401) ->
    undefined;

get_response_spec('get_refunds_amount', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec(_, _) ->
    error(invalid_response_code).

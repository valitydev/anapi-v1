%% -*- mode: erlang -*-
-module(swag_client_reports_api).

%% generated methods

-export([cancel_report/2]).
-export([cancel_report/3]).

-export([create_report/2]).
-export([create_report/3]).

-export([download_file/2]).
-export([download_file/3]).

-export([get_report/2]).
-export([get_report/3]).

-export([search_reports/2]).
-export([search_reports/3]).


-spec cancel_report(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
cancel_report(Endpoint, Params) ->
    cancel_report(Endpoint, Params, []).

-spec cancel_report(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
cancel_report(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/lk/v1/reports/:reportID/cancel"),
        Params,
        get_request_spec(cancel_report),
        Opts
    ), cancel_report).

-spec create_report(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_report(Endpoint, Params) ->
    create_report(Endpoint, Params, []).

-spec create_report(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
create_report(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        post,
        swag_client_utils:get_url(Endpoint, "/lk/v1/reports"),
        Params,
        get_request_spec(create_report),
        Opts
    ), create_report).

-spec download_file(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
download_file(Endpoint, Params) ->
    download_file(Endpoint, Params, []).

-spec download_file(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
download_file(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/reports/:reportID/files/:fileID/download"),
        Params,
        get_request_spec(download_file),
        Opts
    ), download_file).

-spec get_report(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_report(Endpoint, Params) ->
    get_report(Endpoint, Params, []).

-spec get_report(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
get_report(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/reports/:reportID"),
        Params,
        get_request_spec(get_report),
        Opts
    ), get_report).

-spec search_reports(Endpoint :: swag_client:endpoint(), Params :: map()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_reports(Endpoint, Params) ->
    search_reports(Endpoint, Params, []).

-spec search_reports(Endpoint :: swag_client:endpoint(), Params :: map(), Opts :: swag_client:transport_opts()) ->
    {ok, Code :: integer(), RespHeaders :: list(), Response :: map()} |
    {error, _Reason}.
search_reports(Endpoint, Params, Opts) ->
    process_response(swag_client_procession:process_request(
        get,
        swag_client_utils:get_url(Endpoint, "/lk/v1/reports"),
        Params,
        get_request_spec(search_reports),
        Opts
    ), search_reports).

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


get_request_spec('cancel_report') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'reportID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('create_report') ->
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
        {'reportType', #{
            source => qs_val,
            rules  => [{type, 'binary'}, {enum, ['paymentRegistry']}, true
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
        }}
    ];
get_request_spec('download_file') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'reportID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, true}]
        }},
        {'fileID', #{
            source => binding,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('get_report') ->
    [
        {'X-Request-ID', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 32}, {min_length, 1}, true
, {required, true}]
        }},
        {'reportID', #{
            source => binding,
            rules  => [{type, 'integer'}, {format, 'int64'}, true
, {required, true}]
        }},
        {'X-Request-Deadline', #{
            source => header,
            rules  => [{type, 'binary'}, {max_length, 40}, {min_length, 1}, true
, {required, false}]
        }}
    ];
get_request_spec('search_reports') ->
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
        {'reportTypes', #{
            source => qs_val,
            rules  => [{list, 'csv', [{type, 'binary'}, {enum, ['provisionOfService', 'paymentRegistry', 'paymentRegistryByPayout']}, true
]}, true
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
        {'limit', #{
            source => qs_val,
            rules  => [{type, 'integer'}, {format, 'int32'}, {max, 1000, inclusive}, {min, 1, inclusive}, true
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


get_response_spec('cancel_report', 202) ->
    undefined;

get_response_spec('cancel_report', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('cancel_report', 401) ->
    undefined;

get_response_spec('cancel_report', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('create_report', 201) ->
    {'Report', 'Report'};

get_response_spec('create_report', 400) ->
    {'inline_response_400_1', 'inline_response_400_1'};

get_response_spec('create_report', 401) ->
    undefined;

get_response_spec('download_file', 200) ->
    {'ReportLink', 'ReportLink'};

get_response_spec('download_file', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('download_file', 401) ->
    undefined;

get_response_spec('download_file', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('get_report', 200) ->
    {'Report', 'Report'};

get_response_spec('get_report', 400) ->
    {'DefaultLogicError', 'DefaultLogicError'};

get_response_spec('get_report', 401) ->
    undefined;

get_response_spec('get_report', 404) ->
    {'GeneralError', 'GeneralError'};

get_response_spec('search_reports', 200) ->
    {'inline_response_200_13', 'inline_response_200_13'};

get_response_spec('search_reports', 400) ->
    {'inline_response_400', 'inline_response_400'};

get_response_spec('search_reports', 401) ->
    undefined;

get_response_spec(_, _) ->
    error(invalid_response_code).

-module(swag_server_router).

-export([get_paths/1]).
-export([get_paths/2]).
-export([get_operation/1]).
-export([get_operations/0]).

-type operations() :: #{
    Method :: binary() => OperationID :: swag_server:operation_id()
}.

-type logic_handler(T) :: swag_server:logic_handler(T).

-type swagger_handler_opts() :: #{
    validation_opts => swag_server_validation:validation_opts()
}.

-type init_opts() :: {
    Operations      :: operations(),
    LogicHandler    :: logic_handler(_),
    SwaggerHandlerOpts :: swagger_handler_opts()
}.

-type operation_spec() :: #{
    path    := '_' | iodata(),
    method  := binary(),
    handler := module()
}.

-export_type([swagger_handler_opts/0]).
-export_type([init_opts/0]).
-export_type([operation_spec/0]).

-spec get_paths(LogicHandler :: logic_handler(_)) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler) ->
    get_paths(LogicHandler, #{}).

-spec get_paths(LogicHandler :: logic_handler(_), SwaggerHandlerOpts :: swagger_handler_opts()) ->  [{'_',[{
    Path :: string(),
    Handler :: atom(),
    InitOpts :: init_opts()
}]}].

get_paths(LogicHandler, SwaggerHandlerOpts) ->
    PreparedPaths = maps:fold(
        fun(Path, #{operations := Operations, handler := Handler}, Acc) ->
            [{Path, Handler, Operations} | Acc]
        end,
        [],
        group_paths()
    ),
    [
        {'_',
            [{P, H, {O, LogicHandler, SwaggerHandlerOpts}} || {P, H, O} <- PreparedPaths]
        }
    ].

group_paths() ->
    maps:fold(
        fun(OperationID, #{path := Path, method := Method, handler := Handler}, Acc) ->
            case maps:find(Path, Acc) of
                {ok, PathInfo0 = #{operations := Operations0}} ->
                    Operations = Operations0#{Method => OperationID},
                    PathInfo = PathInfo0#{operations => Operations},
                    Acc#{Path => PathInfo};
                error ->
                    Operations = #{Method => OperationID},
                    PathInfo = #{handler => Handler, operations => Operations},
                    Acc#{Path => PathInfo}
            end
        end,
        #{},
        get_operations()
    ).

-spec get_operation(swag_server:operation_id()) ->
   operation_spec().

get_operation(OperationId) ->
    maps:get(OperationId, get_operations()).

-spec get_operations() -> #{
    swag_server:operation_id() := operation_spec()
}.

get_operations() ->
    #{ 
        'GetAveragePayment' => #{
            path => "/lk/v1/analytics/payments/average",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetCurrentBalances' => #{
            path => "/lk/v1/analytics/balances/current",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetCurrentBalancesGroupByShop' => #{
            path => "/lk/v1/analytics/balances/current-shop-balances",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetPaymentsAmount' => #{
            path => "/lk/v1/analytics/payments/amount",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetPaymentsCount' => #{
            path => "/lk/v1/analytics/payments/count",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetPaymentsErrorDistribution' => #{
            path => "/lk/v1/analytics/payments/errors",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetPaymentsSplitAmount' => #{
            path => "/lk/v1/analytics/payments/split-amount",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetPaymentsSplitCount' => #{
            path => "/lk/v1/analytics/payments/split-count",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetPaymentsSubErrorDistribution' => #{
            path => "/lk/v1/analytics/payments/sub-errors",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetPaymentsToolDistribution' => #{
            path => "/lk/v1/analytics/payments-tool",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'GetRefundsAmount' => #{
            path => "/lk/v1/analytics/refunds/amount",
            method => <<"GET">>,
            handler => 'swag_server_analytics_handler'
        },
        'CancelReport' => #{
            path => "/lk/v1/reports/:reportID/cancel",
            method => <<"POST">>,
            handler => 'swag_server_reports_handler'
        },
        'CreateReport' => #{
            path => "/lk/v1/reports",
            method => <<"POST">>,
            handler => 'swag_server_reports_handler'
        },
        'DownloadFile' => #{
            path => "/lk/v1/reports/:reportID/files/:fileID/download",
            method => <<"GET">>,
            handler => 'swag_server_reports_handler'
        },
        'GetReport' => #{
            path => "/lk/v1/reports/:reportID",
            method => <<"GET">>,
            handler => 'swag_server_reports_handler'
        },
        'SearchReports' => #{
            path => "/lk/v1/reports",
            method => <<"GET">>,
            handler => 'swag_server_reports_handler'
        },
        'SearchChargebacks' => #{
            path => "/lk/v1/chargebacks",
            method => <<"GET">>,
            handler => 'swag_server_search_handler'
        },
        'SearchInvoices' => #{
            path => "/lk/v1/invoices",
            method => <<"GET">>,
            handler => 'swag_server_search_handler'
        },
        'SearchPayments' => #{
            path => "/lk/v1/payments",
            method => <<"GET">>,
            handler => 'swag_server_search_handler'
        },
        'SearchPayouts' => #{
            path => "/lk/v1/payouts",
            method => <<"GET">>,
            handler => 'swag_server_search_handler'
        },
        'SearchRefunds' => #{
            path => "/lk/v1/refunds",
            method => <<"GET">>,
            handler => 'swag_server_search_handler'
        }
    }.

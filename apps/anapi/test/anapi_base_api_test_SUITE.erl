%%%
%%% Copyright 2019 RBKmoney
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

-module(anapi_base_api_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("anapi_dummy_data.hrl").
-include_lib("anapi_bouncer_data.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([
    search_invoices_ok_test/1,
    search_payments_ok_test/1,
    search_refunds_ok_test/1,
    search_payouts_ok_test/1,
    search_chargebacks_ok_test/1,
    get_report_ok_test/1,
    get_report_not_found_test/1,
    search_reports_ok_test/1,
    create_report_ok_test/1,
    cancel_report_ok_test/1,
    cancel_report_bad_request_test/1,
    create_report_without_shop_id_ok_test/1,
    download_report_file_ok_test/1,
    search_by_inaccessible_party_id_error_test/1
]).

-type test_case_name() :: atom().
-type config() :: [{atom(), any()}].
-type group_name() :: atom().

-behaviour(supervisor).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() -> [{group, test_case_name()}].
all() ->
    [
        {group, all_tests}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {all_tests, [], [
            search_invoices_ok_test,
            search_payments_ok_test,
            search_refunds_ok_test,
            search_payouts_ok_test,
            search_chargebacks_ok_test,
            get_report_ok_test,
            get_report_not_found_test,
            search_reports_ok_test,
            create_report_ok_test,
            cancel_report_ok_test,
            cancel_report_bad_request_test,
            create_report_without_shop_id_ok_test,
            search_by_inaccessible_party_id_error_test,
            download_report_file_ok_test
        ]}
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    anapi_ct_helper:init_suite(?MODULE, Config).

-spec end_per_suite(config()) -> _.
end_per_suite(C) ->
    _ = anapi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    _ = [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) -> config().
init_per_group(all_tests, Config) ->
    BasePermissions = [
        {[invoices], read},
        {[party], read},
        {[party], write},
        {[invoices, payments], read}
    ],
    {ok, Token} = anapi_ct_helper:issue_token(BasePermissions, unlimited),
    [{context, anapi_ct_helper:get_context(Token)} | Config];
init_per_group(_, Config) ->
    Config.

-spec end_per_group(group_name(), config()) -> _.
end_per_group(_Group, C) ->
    _ = anapi_utils:maybe(?config(group_test_sup, C), fun anapi_ct_helper:stop_mocked_service_sup/1),
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_Name, C) ->
    [{test_sup, anapi_ct_helper:start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_Name, C) ->
    anapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec search_invoices_ok_test(config()) -> _.
search_invoices_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {merchant_stat, fun('GetInvoices', _) -> {ok, ?STAT_RESPONSE_INVOICES} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"SearchInvoices">>, ?STRING, Config),
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {invoiceStatus, <<"fulfilled">>},
        {partyID, ?STRING},
        {shopID, ?STRING},
        {shopIDs, <<?STRING/binary, ",", ?STRING/binary>>},
        {invoiceID, <<"testInvoiceID">>},
        {invoiceIDs, <<?STRING/binary, ",", ?STRING/binary>>},
        {invoiceAmountFrom, 1},
        {invoiceAmountTo, 20000},
        {continuationToken, <<"come_back_next_time">>},
        {excludedShops, <<"shop1, shop2">>},
        {externalID, ?STRING}
    ],

    {ok, _, _} = anapi_client_searches:search_invoices(?config(context, Config), Query).

-spec search_payments_ok_test(config()) -> _.
search_payments_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {merchant_stat, fun('GetPayments', _) -> {ok, ?STAT_RESPONSE_PAYMENTS} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"SearchPayments">>, ?STRING, Config),
    Params = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {payerEmail, <<"test@test.ru">>},
        {partyID, ?STRING},
        {shopID, ?STRING},
        {shopIDs, <<?STRING/binary, ",", ?STRING/binary>>},
        {paymentStatus, <<"processed">>},
        {paymentFlow, <<"instant">>},
        {paymentMethod, <<"bankCard">>},
        {invoiceID, <<"testInvoiceID">>},
        {invoiceIDs, <<?STRING/binary, ",", ?STRING/binary>>},
        {paymentID, <<"testPaymentID">>},
        {payerFingerprint, <<"blablablalbalbal">>},
        {first6, <<"424242">>},
        {last4, <<"2222">>},
        {rrn, <<"090909090909">>},
        {approval_code, <<"808080">>},
        {bankCardTokenProvider, <<"applepay">>},
        {bankCardPaymentSystem, <<"visa">>},
        {paymentAmountFrom, 1},
        {paymentAmountTo, 20000},
        {continuationToken, <<"come_back_next_time">>},
        {excludedShops, <<"shop1, shop2">>},
        {externalID, ?STRING}
    ],
    Query1 = [{payerIP, <<"192.168.0.1">>} | Params],
    Query2 = [{payerIP, <<"992.168.0.1">>} | Params],
    {ok, _, _} = anapi_client_searches:search_payments(?config(context, Config), Query1),
    {ok, 400, #{
        <<"code">> := <<"invalidRequest">>,
        <<"message">> := <<"Request parameter: payerIP, error type: wrong_format">>
    }} = request_search_payments(?config(context, Config), Query2).

request_search_payments(Context, Query) ->
    % NOTE
    % Adapted from `anapi_client_searches:search_payments/2`.
    {Endpoint, #{header := Headers}, Opts} = anapi_client_lib:make_request(Context, #{}),
    Qs = anapi_client_lib:make_search_query_string(Query),
    UrlPath = swag_client_utils:get_url(Endpoint, "/lk/v1/payments"),
    Url = swag_client_utils:fill_url(UrlPath, #{}, Qs),
    case hackney:request(get, Url, maps:to_list(Headers), <<>>, [with_body] ++ Opts) of
        {ok, Status, _, Response} ->
            {ok, Status, jsx:decode(Response, [return_maps])};
        {error, _} = Error ->
            Error
    end.

-spec search_refunds_ok_test(config()) -> _.
search_refunds_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {merchant_stat, fun('GetPayments', _) -> {ok, ?STAT_RESPONSE_REFUNDS} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"SearchRefunds">>, ?STRING, Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {partyID, ?STRING},
        {shopID, ?STRING},
        {shopIDs, <<?STRING/binary, ",", ?STRING/binary>>},
        {invoiceID, <<"testInvoiceID">>},
        {invoiceIDs, <<?STRING/binary, ",", ?STRING/binary>>},
        {paymentID, <<"testPaymentID">>},
        {refundID, <<"testRefundID">>},
        {refundStatus, <<"succeeded">>},
        {continuationToken, <<"come_back_next_time">>},
        {excludedShops, <<"shop1, shop2">>},
        {externalID, ?STRING}
    ],

    {ok, _, _} = anapi_client_searches:search_refunds(?config(context, Config), Query).

-spec search_payouts_ok_test(config()) -> _.
search_payouts_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {merchant_stat, fun('GetPayouts', _) -> {ok, ?STAT_RESPONSE_PAYOUTS} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"SearchPayouts">>, ?STRING, Config),
    Query = [
        {limit, 2},
        {offset, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {partyID, ?STRING},
        {shopID, ?STRING},
        {shopIDs, <<?STRING/binary, ",", ?STRING/binary>>},
        {payoutID, <<"testPayoutID">>},
        {payoutToolType, <<"Wallet">>},
        {continuationToken, <<"come_back_next_time">>},
        {excludedShops, <<"shop1, shop2">>}
    ],

    {ok, _, _} = anapi_client_searches:search_payouts(?config(context, Config), Query).

-spec search_reports_ok_test(config()) -> _.
search_reports_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {reporting, fun('GetReports', _) -> {ok, ?FOUND_REPORTS} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"SearchReports">>, ?STRING, Config),
    Query0 = [
        {limit, 2},
        {partyID, ?STRING},
        {shopID, ?STRING},
        {shopIDs, <<?STRING/binary, ",", ?STRING/binary>>},
        {from_time, {{2016, 03, 22}, {6, 12, 27}}},
        {to_time, {{2016, 03, 22}, {6, 12, 27}}},
        {report_types, <<?REPORT_TYPE/binary, <<",">>/binary, ?REPORT_TYPE_ALT/binary>>}
    ],
    {ok, _} = anapi_client_reports:search_reports(?config(context, Config), Query0),
    Query1 = [
        {partyID, ?STRING},
        {from_time, {{2016, 03, 22}, {6, 12, 27}}},
        {to_time, {{2016, 03, 22}, {6, 12, 27}}},
        {report_types, ?REPORT_TYPE}
    ],
    {ok, _} = anapi_client_reports:search_reports(?config(context, Config), Query1).

-spec get_report_ok_test(config()) -> _.
get_report_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services([{reporting, fun('GetReport', _) -> {ok, ?REPORT} end}], Config),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_report_op_ctx(
        <<"GetReport">>,
        ?STRING,
        ?STRING,
        ?INTEGER_BINARY,
        [?CTX_ENTITY(?STRING)],
        Config
    ),
    {ok, _} = anapi_client_reports:get_report(?config(context, Config), ?INTEGER).

-spec get_report_not_found_test(config()) -> _.
get_report_not_found_test(Config) ->
    _ = anapi_ct_helper:mock_services([{reporting, fun('GetReport', _) -> {ok, ?REPORT_ALT} end}], Config),
    _ = anapi_ct_helper_bouncer:mock_bouncer_arbiter(anapi_ct_helper_bouncer:judge_always_forbidden(), Config),
    {error, {401, _}} =
        anapi_client_reports:get_report(?config(context, Config), ?INTEGER).

-spec create_report_ok_test(config()) -> _.
create_report_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {reporting, fun
                ('CreateReport', _) -> {ok, ?INTEGER};
                ('GetReport', {?INTEGER}) -> {ok, ?REPORT}
            end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"CreateReport">>, ?STRING, Config),
    Query0 = [
        {partyID, ?STRING},
        {shopID, ?STRING},
        {from_time, {{2016, 03, 22}, {6, 12, 27}}},
        {to_time, {{2016, 03, 22}, {6, 12, 27}}},
        {reportType, ?REPORT_TYPE}
    ],
    {ok, _} = anapi_client_reports:create_report(?config(context, Config), Query0).

-spec cancel_report_ok_test(config()) -> _.
cancel_report_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {reporting, fun
                ('CancelReport', _) -> {ok, ok};
                ('GetReport', {?INTEGER}) -> {ok, ?REPORT}
            end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_report_op_ctx(
        <<"CancelReport">>,
        ?STRING,
        ?STRING,
        ?INTEGER_BINARY,
        [?CTX_ENTITY(?STRING)],
        Config
    ),
    {ok, _} = anapi_client_reports:cancel_report(?config(context, Config), ?INTEGER).

-spec cancel_report_bad_request_test(config()) -> _.
cancel_report_bad_request_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {reporting, fun('GetReport', {?INTEGER}) -> {ok, ?REPORT(<<"provision_of_service">>)} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_report_op_ctx(
        <<"CancelReport">>,
        ?INTEGER_BINARY,
        Config
    ),
    {error, {400, #{<<"message">> := <<"Invalid report type">>}}} =
        anapi_client_reports:cancel_report(?config(context, Config), ?INTEGER).

-spec create_report_without_shop_id_ok_test(config()) -> _.
create_report_without_shop_id_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {reporting, fun
                ('CreateReport', _) -> {ok, ?INTEGER};
                ('GetReport', {?INTEGER}) -> {ok, ?REPORT_WITHOUT_SHOP_ID}
            end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"CreateReport">>, ?STRING, Config),
    Query0 = [
        {partyID, ?STRING},
        {from_time, {{2016, 03, 22}, {6, 12, 27}}},
        {to_time, {{2016, 03, 22}, {6, 12, 27}}},
        {reportType, ?REPORT_TYPE}
    ],
    {ok, _} = anapi_client_reports:create_report(?config(context, Config), Query0).

-spec download_report_file_ok_test(_) -> _.
download_report_file_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {reporting, fun
                ('GetReport', _) -> {ok, ?REPORT};
                ('GeneratePresignedUrl', _) -> {ok, ?STRING}
            end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_report_file_op_ctx(
        <<"DownloadFile">>,
        ?STRING,
        ?STRING,
        ?INTEGER_BINARY,
        ?STRING,
        [?CTX_ENTITY(?STRING)],
        Config
    ),
    {ok, _} = anapi_client_reports:download_file(?config(context, Config), ?INTEGER, ?STRING).

-spec search_chargebacks_ok_test(config()) -> _.
search_chargebacks_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {merchant_stat, fun('GetChargebacks', _) -> {ok, ?STAT_RESPONSE_CHARGEBACKS} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"SearchChargebacks">>, ?STRING, Config),
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {partyID, ?STRING},
        {shopID, ?STRING},
        {shopIDs, <<?STRING/binary, ",", ?STRING/binary>>},
        {invoiceID, <<"testInvoiceID">>},
        {paymentID, <<"testPaymentID">>},
        {chargebackID, <<"testChargebackID">>},
        {chargebackStatuses, <<"pending,accepted">>},
        {chargebackStages, <<"chargeback,pre_arbitration">>},
        {chargebackCategories, <<"fraud,dispute">>},
        {continuationToken, <<"come_back_next_time">>}
    ],
    {ok, _, _} = anapi_client_searches:search_chargebacks(?config(context, Config), Query).

-spec search_by_inaccessible_party_id_error_test(config()) -> _.
search_by_inaccessible_party_id_error_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {merchant_stat, fun('GetChargebacks', _) -> {ok, ?STAT_RESPONSE_CHARGEBACKS} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_arbiter(anapi_ct_helper_bouncer:judge_always_forbidden(), Config),
    Query = [
        {limit, 2},
        {from_time, {{2015, 08, 11}, {19, 42, 35}}},
        {to_time, {{2020, 08, 11}, {19, 42, 35}}},
        {partyID, <<"Totaly not the users party">>},
        {paymentInstitutionRealm, <<"live">>}
    ],
    {error, {401, _}} =
        anapi_client_searches:search_chargebacks(?config(context, Config), Query).

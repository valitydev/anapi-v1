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

-module(anapi_analytics_api_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("anapi_dummy_data.hrl").

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
    get_payments_tool_distribution_ok_test/1,
    get_payments_amount_ok_test/1,
    get_average_payment_ok_test/1,
    get_payments_count_ok_test/1,
    get_payments_error_distribution_ok_test/1,
    get_payments_split_amount_ok_test/1,
    get_payments_split_count_ok_test/1,
    get_refunds_amount_ok_test/1,
    get_current_balances_ok_test/1,
    get_payments_sub_error_distribution_ok_test/1,
    get_current_balances_group_by_shop_ok_test/1,

    analytics_timeout_test/1
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
            get_payments_tool_distribution_ok_test,
            get_payments_amount_ok_test,
            get_average_payment_ok_test,
            get_payments_count_ok_test,
            get_payments_error_distribution_ok_test,
            get_payments_split_amount_ok_test,
            get_payments_split_count_ok_test,
            get_refunds_amount_ok_test,
            get_current_balances_ok_test,
            get_payments_sub_error_distribution_ok_test,
            get_current_balances_group_by_shop_ok_test,
            analytics_timeout_test
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
        {[party], read},
        {[party], write}
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
    _ = anapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-define(STAT_QUERY, [
    {partyID, ?STRING},
    {shopIDs, <<"asdf,asdf2">>},
    {excludeShopIDs, <<"asdf3">>},
    {from_time, {{2015, 08, 11}, {19, 42, 35}}},
    {to_time, {{2020, 08, 11}, {19, 42, 35}}}
]).

-define(STAT_QUERY_SPLIT,
    ?STAT_QUERY ++ [{split_unit, ?SPLIT_UNIT}]
).

-spec get_payments_tool_distribution_ok_test(config()) -> _.
get_payments_tool_distribution_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {analytics, fun('GetPaymentsToolDistribution', _) -> {ok, ?ANALYTICS_PAYMENT_TOOL_DISTRIBUTION_RESP} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"GetPaymentsToolDistribution">>, ?STRING, Config),
    {ok, _} = anapi_client_analytics:get_payments_tool_distribution(?config(context, Config), ?STAT_QUERY).

-spec get_payments_amount_ok_test(config()) -> _.
get_payments_amount_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {analytics, fun('GetPaymentsAmount', _) -> {ok, ?ANALYTICS_AMOUNT_RESP} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"GetPaymentsAmount">>, ?STRING, Config),
    {ok, _} = anapi_client_analytics:get_payments_amount(?config(context, Config), ?STAT_QUERY).

-spec get_average_payment_ok_test(config()) -> _.
get_average_payment_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {analytics, fun('GetAveragePayment', _) -> {ok, ?ANALYTICS_AMOUNT_RESP} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"GetAveragePayment">>, ?STRING, Config),
    {ok, _} = anapi_client_analytics:get_average_payment(?config(context, Config), ?STAT_QUERY).

-spec get_payments_count_ok_test(config()) -> _.
get_payments_count_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {analytics, fun('GetPaymentsCount', _) -> {ok, ?ANALYTICS_COUNT_RESP} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"GetPaymentsCount">>, ?STRING, Config),
    {ok, _} = anapi_client_analytics:get_payments_count(?config(context, Config), ?STAT_QUERY).

-spec get_payments_error_distribution_ok_test(config()) -> _.
get_payments_error_distribution_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {analytics, fun('GetPaymentsErrorDistribution', _) -> {ok, ?ANALYTICS_ERROR_DISTRIBUTION_RESP} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"GetPaymentsErrorDistribution">>, ?STRING, Config),
    {ok, _} = anapi_client_analytics:get_payments_error_distribution(?config(context, Config), ?STAT_QUERY).

-spec get_payments_split_amount_ok_test(config()) -> _.
get_payments_split_amount_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {analytics, fun('GetPaymentsSplitAmount', _) -> {ok, ?ANALYTICS_SPLIT_AMOUNT_RESP} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"GetPaymentsSplitAmount">>, ?STRING, Config),
    {ok, _} = anapi_client_analytics:get_payments_split_amount(?config(context, Config), ?STAT_QUERY_SPLIT).

-spec get_payments_split_count_ok_test(config()) -> _.
get_payments_split_count_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {analytics, fun('GetPaymentsSplitCount', _) -> {ok, ?ANALYTICS_SPLIT_COUNT_RESP} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"GetPaymentsSplitCount">>, ?STRING, Config),
    {ok, _} = anapi_client_analytics:get_payments_split_count(?config(context, Config), ?STAT_QUERY_SPLIT).

-spec get_refunds_amount_ok_test(config()) -> _.
get_refunds_amount_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {analytics, fun('GetRefundsAmount', _) -> {ok, ?ANALYTICS_AMOUNT_RESP} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"GetRefundsAmount">>, ?STRING, Config),
    {ok, _} = anapi_client_analytics:get_refunds_amount(?config(context, Config), ?STAT_QUERY).

-spec get_current_balances_ok_test(config()) -> _.
get_current_balances_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {analytics, fun('GetCurrentBalances', _) -> {ok, ?ANALYTICS_AMOUNT_RESP} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"GetCurrentBalances">>, ?STRING, Config),
    Query = [
        {partyID, ?STRING},
        {shopIDs, <<"asdf,asdf2">>},
        {excludeShopIDs, <<"asdf3">>}
    ],

    {ok, _} = anapi_client_analytics:get_current_balances(?config(context, Config), Query).

-spec get_payments_sub_error_distribution_ok_test(config()) -> _.
get_payments_sub_error_distribution_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {analytics, fun('GetPaymentsSubErrorDistribution', _) -> {ok, ?ANALYTICS_SUB_ERROR_DISTRIBUTION_RESP} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(
        <<"GetPaymentsSubErrorDistribution">>,
        ?STRING,
        Config
    ),
    {ok, _} = anapi_client_analytics:get_payments_sub_error_distribution(?config(context, Config), ?STAT_QUERY).

-spec get_current_balances_group_by_shop_ok_test(config()) -> _.
get_current_balances_group_by_shop_ok_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {analytics, fun('GetCurrentShopBalances', _) -> {ok, ?ANALYTICS_SHOP_AMOUNT_RESP} end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"GetCurrentBalancesGroupByShop">>, ?STRING, Config),
    Query = [
        {partyID, ?STRING},
        {shopIDs, <<"asdf,asdf2">>},
        {excludeShopIDs, <<"asdf3">>}
    ],
    {ok, _} = anapi_client_analytics:get_current_balances_group_by_shop(?config(context, Config), Query).

-spec analytics_timeout_test(config()) -> _.
analytics_timeout_test(Config) ->
    _ = anapi_ct_helper:mock_services(
        [
            {analytics, fun('GetRefundsAmount', _) ->
                timer:sleep(1500),
                {ok, ?ANALYTICS_AMOUNT_RESP}
            end},
            {party_shop, fun('GetShopsIds', _) -> {ok, [?STRING, ?STRING]} end}
        ],
        Config
    ),
    _ = anapi_ct_helper_bouncer:mock_bouncer_assert_party_op_ctx(<<"GetRefundsAmount">>, ?STRING, Config),
    {error, {invalid_response_code, 504}} = anapi_client_analytics:get_refunds_amount(
        ?config(context, Config), ?STAT_QUERY
    ).

%% -*- mode: erlang -*-
-module(swag_client).
%% Type definitions

%% API

-export_type([request_context/0]).
-export_type([auth_context/0]).
-export_type([client_peer/0]).
-export_type([operation_id/0]).
-export_type([api_key/0]).
-export_type([object/0]).
-export_type([endpoint/0]).
-export_type([transport_opts/0]).

-type auth_context() :: any().
-type operation_id() :: atom().
-type api_key()      :: binary().
-type object()       :: map().
-type endpoint()     :: string() | {string(), pos_integer()}.

-type client_peer() :: #{
    ip_address  => IP :: inet:ip_address(),
    port_number => Port :: inet:port_number()
}.

-type request_context() :: #{
    auth_context => AuthContext :: auth_context(),
    peer         => client_peer()
}.

-type transport_opts() :: list(). % you can find it in hackney:request/5

%% Internal

-export_type([param_name/0]).
-export_type([value/0]).
-export_type([error_reason/0]).

-type param_name()    :: atom().
-type value()         :: term().
-type error_reason()  :: binary().

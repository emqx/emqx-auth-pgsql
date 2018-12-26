%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_auth_pgsql).

-behaviour(emqx_auth_mod).

-include("emqx_auth_pgsql.hrl").

-include_lib("emqx/include/emqx.hrl").

-export([init/1, check/3, description/0]).

-record(state, {auth_query, super_query, hash_type}).

-define(UNDEFINED(S), (S =:= undefined orelse S =:= <<>>)).

%%--------------------------------------------------------------------
%% Auth Module Callbacks
%%--------------------------------------------------------------------

init({AuthQuery, SuperQuery, HashType}) ->
    {ok, #state{auth_query = AuthQuery, super_query = SuperQuery, hash_type = HashType}}.

check(#{username := Username}, Password, _State)
    when ?UNDEFINED(Username); ?UNDEFINED(Password) ->
    {error, username_or_password_undefined};

check(Credentials, Password, #state{auth_query  = {AuthSql, AuthParams},
                                    super_query = SuperQuery,
                                    hash_type   = HashType}) ->
    case emqx_auth_pgsql_cli:equery(AuthSql, AuthParams, Credentials) of
        {ok, _, [Record]} ->
            case emqx_passwd:check_pass(erlang:append_element(Record, Password), HashType) of
                ok -> {ok, is_superuser(SuperQuery, Credentials)};
                Error -> Error
            end;
         {ok, _, []} ->
            ignore;
         {error, Reason} ->
            {error, Reason}
     end.

description() -> "Authentication with PostgreSQL".

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | {string(), list()}, emqx_types:credentials()) -> boolean()).
is_superuser(undefined, _Credentials) ->
    false;
is_superuser({SuperSql, Params}, Credentials) ->
    case emqx_auth_pgsql_cli:equery(SuperSql, Params, Credentials) of
        {ok, [_Super], [{true}]} ->
            true;
        {ok, [_Super], [_False]} ->
            false;
        {ok, [_Super], []} ->
            false;
        {error, _Error} ->
            false
    end.


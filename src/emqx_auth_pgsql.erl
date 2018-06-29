%%--------------------------------------------------------------------
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. (http://emqtt.io)
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
%%--------------------------------------------------------------------

-module(emqx_auth_pgsql).

-include("emqx_auth_pgsql.hrl").

-include_lib("emqx/include/emqx.hrl").

-export([check/4]).

check(lwm2m, _Password, Client = #mqtt_client{client_id = ClientId}, _Env) ->
    Headers = [{lwm2m_up_ad_topic, {<<"v1/up/ad">>, 0}},
               {lwm2m_up_dm_topic, {<<"v1/up/dm">>, 0}},
               {lwm2m_up_ol_topic, {<<"v1/up/ol">>, 0}},
               {lwm2m_dn_dm_topic, {<<"v1/dn/dm">>, 0}},
               {lwm2m_model, psm},
               {lwm2m_auto_observe,  true},
               {device_id, <<"aaaaaa">>},
               {tenant_id, <<"aaa">>},
               {product_id, <<"aa">>},
               {device_id, <<"aaaa">>}],
    {ok, Client#mqtt_client{headers = Headers}};

check(mqtt, Password, Client = #mqtt_client{headers = Headers, username = Username} , {{AuthSql, AuthParams}, HashType}) ->
    lager:debug("Sql:~p, params:~p", [AuthSql, AuthParams]),
    case emqx_auth_pgsql_cli:equery(AuthSql, AuthParams, Client) of
        {ok, _, [{_TenantId, _ProductId, _GroupId, _DeviceId, _DeviceUsername, _Token, 1, _}]} ->
            lager:error("Username '~s' login failed for black list", [Username]),
            {stop, Client};
        {ok, _, [{TenantId, ProductId, GroupId, DeviceId, Username, Token, _Status, 1}]} ->
            Headers1 = mqtt_headers(TenantId, ProductId, GroupId, DeviceId, Headers),
            Mountpoint = mountpoint(<<"mqtt">>, TenantId, ProductId, DeviceId),
            case check_pass({Token}, Password, HashType) of
                ok ->
                    ClientId2 = <<TenantId/binary, ":", ProductId/binary, ":", DeviceId/binary>>,
                    {ok, Client#mqtt_client{headers = Headers1, client_id = ClientId2, mountpoint = Mountpoint}};
                Error ->
                    lager:error("Username '~s' login failed for ~p", [Username, Error]),
                    {stop, Client#mqtt_client{headers = Headers1}}
            end;
        {ok, _, [{TenantId, ProductId, GroupId, DeviceId, _, Token, _Status, 1}]} ->
            Headers2 = mqtt_headers(TenantId, ProductId, GroupId, DeviceId, Headers),
            {stop, Client#mqtt_client{headers = Headers2}};
        {ok, _, [{TenantId, ProductId, GroupId, DeviceId, _DeviceUsername, _Token, _Status, 2}]} ->
            Headers3 = mqtt_headers(TenantId, ProductId, GroupId, DeviceId, Headers),
            Mountpoint = mountpoint(<<"mqtt">>, TenantId, ProductId, DeviceId),
            ClientId3 = <<TenantId/binary, ":", ProductId/binary, ":", DeviceId/binary>>,
            Sql = "select id from cert_auth where \"clientID\" = $1 and \"CN\" = $2 and enable = 1 limit 1",
            case emqx_auth_pgsql_cli:equery(Sql, [ClientId3, Username]) of
                {ok, _, [_Id]} ->
                    {ok, Client#mqtt_client{headers = Headers3, client_id = ClientId3, mountpoint = Mountpoint}};
                {ok, _, []} ->
                    lager:error("Username '~s' login failed for ~p", [Username, not_find]),
                    {stop, Client#mqtt_client{headers = Headers3}};
                {error, Reason1} ->
                    lager:error("Username '~s' login failed for ~p", [Username, Reason1]),
                    {stop, Client}
            end;
         {ok, _, []} ->
            lager:error("Username '~s' login failed for ~p", [Username, not_find]),
            {stop, Client};
         {error, Reason} ->
            lager:error("Username '~s' login failed for ~p", [Username, Reason]),
            {stop, Client}
     end.

check_pass({PassHash}, Password, HashType) ->
    check_pass(PassHash, hash(HashType, Password));
check_pass({PassHash, Salt}, Password, {pbkdf2, Macfun, Iterations, Dklen}) ->
    check_pass(PassHash, hash(pbkdf2, {Salt, Password, Macfun, Iterations, Dklen}));
check_pass({PassHash, Salt}, Password, {salt, bcrypt}) ->
    check_pass(PassHash, hash(bcrypt, {Salt, Password}));
check_pass({PassHash, Salt}, Password, {salt, HashType}) ->
    check_pass(PassHash, hash(HashType, <<Salt/binary, Password/binary>>));
check_pass({PassHash, Salt}, Password, {HashType, salt}) ->
    check_pass(PassHash, hash(HashType, <<Password/binary, Salt/binary>>)).

check_pass(PassHash, PassHash) -> ok;
check_pass(_, _)               -> {error, password_error}. 

hash(Type, Password) ->
    emqx_auth_mod:passwd_hash(Type, Password).

mqtt_headers(TenantId, ProductId, GroupId, DeviceId, Headers) ->
    [{tenant_id, TenantId},
     {product_id, ProductId},
     {group_id, GroupId},
     {device_id, DeviceId},
     {is_superuser, false} | Headers].

mountpoint(Protocol, TenantId, ProductId, DeviceId) ->
    emqx_topic:encode(<<>>, [Protocol, TenantId, ProductId, DeviceId]).

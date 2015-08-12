%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015 eMQTT.IO, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc
%%% emqttd authentication with PostgreSQL 'mqtt_users' table.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(emqttd_auth_pgsql).

-author("Feng Lee <feng@emqtt.io>").

-include_lib("emqttd/include/emqttd.hrl").

-export([load/1]).

-behaviour(emqttd_auth_mod).

-export([init/1, check/3, description/0]).

-record(state, {user_table, name_field, pass_field, pass_hash}).

-define(Undefined(S), (S =:= undefined orelse S =:= <<>>)).

load(Env) ->
    ok = emqttd_access_control:register_mod(auth, ?MODULE, Env).

init(Opts) ->
    Mapper = proplists:get_value(field_mapper, Opts),
    {ok, #state{user_table = proplists:get_value(user_table, Opts),
                name_field = proplists:get_value(username, Mapper),
                pass_field = proplists:get_value(password, Mapper),
                pass_hash  = proplists:get_value(password_hash, Opts)}}.

check(#mqtt_client{username = Username}, Password, _State)
    when ?Undefined(Username) orelse ?Undefined(Password) ->
    {error, "Username or Password is undefined!"};

check(#mqtt_client{username = Username}, Password,
      #state{user_table = UserTab, pass_hash = Type,
             name_field = NameField, pass_field = PassField}) ->
    Sql = io_lib:format("select ~s from ~s where ~s = $1 and ~s = $2",
                            [NameField, UserTab, NameField, PassField]),
    case emqttd_pgsql_pool:equeury(pgauth, Sql, [Username, hash(Type, Password)]) of
        {ok, _, [{1}]} -> ok;
        {ok, _, _} -> {error, "Not Found"};
        {error, Error} -> {error, Error}
    end.

description() -> "Authentication by PostgreSQL".

hash(plain, Password) ->
    Password;

hash(md5, Password) ->
    hexstring(crypto:hash(md5, Password));

hash(sha, Password) ->
    hexstring(crypto:hash(sha, Password)).

hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X]));

hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X])).
 

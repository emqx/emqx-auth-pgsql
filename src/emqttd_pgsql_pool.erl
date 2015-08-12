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
%%% emqttd pgsql connection pool.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(emqttd_pgsql_pool).

-export([start_link/3]).

-export([squery/2, equery/3]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {name, id, client}).

-spec start_link(Pool :: atom(), I :: pos_integer(), Opts :: list(tuple())) ->
    {ok, pid()} | ignore | {error, any()}.
start_link(Pool, I, Opts) ->
    gen_server:start_link(?MODULE, [Pool, I, Opts], []).

squery(Pool, Sql) ->
    call(Pool, {squery, Sql}).

equery(Pool, Sql, Params) ->
    call(Pool, {equery, Sql, Params}).

call(Pool, Req) ->
    gen_server:call(gproc_pool:pick_worker(Pool), Req, infinity).

init([Pool, I, Opts]) ->
    Host = proplists:get_value(host, Opts),
    Username = proplists:get_value(username, Opts),
    Password = proplists:get_value(password, Opts),
    case eqgsql:connect(Host, Username, Password, conn_opts(Opts)) of
        {ok, C} ->
            gproc_pool:connect_worker(Pool, {?MODULE, I}),
            {ok, #state{name = Pool, id = I, client = C}};
        {error, Error} ->
            {stop, Error}
    end.

conn_opts(Opts) ->
    conn_opts(Opts, []).
conn_opts([], Acc) ->
    Acc;
conn_opts([Opt = {database, _}|Opts], Acc) ->
    conn_opts(Opts, [Opt|Acc]);
conn_opts([Opt = {port, _}|Opts], Acc) ->
    conn_opts(Opts, [Opt|Acc]);
conn_opts([Opt = {timeout, _}|Opts], Acc) ->
    conn_opts(Opts, [Opt|Acc]);
conn_opts([_Opt|Opts], Acc) ->
    conn_opts(Opts, Acc).

handle_call({squery, Sql}, _From, State = #state{client = C}) ->
    {reply, epgsql:squery(C, Sql), State};

handle_call({equery, Sql, Params}, _From, State = #state{client = C}) ->
    {reply, epgsql:equery(C, Sql, Params), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{name = Pool, id = I}) ->
    gproc_pool:disconnect_worker(Pool, {?MODULE, I}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



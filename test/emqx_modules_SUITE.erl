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

-module(emqx_modules_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

-define(HOOKS, ['client.connected', 'client.disconnected', 'client.subscribe', 'client.unsubscribe', 'message.publish']).

-define(REWRITE, [{rewrite, <<"x/#">>, <<"^x/y/(.+)$">>, <<"z/y/$1">>}]).

all() ->
    [emqx_modules_restart,
     {group, emqx_mod_subscription},
     {group, emqx_mod_rewrite},
     {group, emqx_mod_presence}].

groups() ->
    [{emqx_mod_subscription, [parallel], [connect]},
     {emqx_mod_rewrite, [sequence], [rewrite_sub, publish]},
     {emqx_mod_presence, [parallel], [presence_msg]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    [start_apps(App, DataDir) || App <- [emqx, emqx_modules]],
    Config.

end_per_suite(Config) ->
    [application:stop(App) || App <- [emqx_modules, emqx]],
    Config.

emqx_modules_restart(_Config) ->
    application:stop(emqx_modules),
    UnloadLists = [emqx_hooks:lookup(Hook) || Hook <- ?HOOKS],
    ?assert(lists:append(UnloadLists) =:= []),
    application:start(emqx_modules),
    ReloadLists = [emqx_hooks:lookup(Hook) || Hook <- ?HOOKS],
    ?assert(length(lists:append(ReloadLists)) > 0).

connect(_Config)->
    {ok, C1} = emqttc:start_link([{host, "localhost"}, {client_id, <<"c1">>}, {username, <<"u1">>}]),
    timer:sleep(1000),
    [{Sub, Topic}] = ets:tab2list(mqtt_subscription),
    ?assertEqual(<<"c1">>, Sub),
    ?assertEqual(<<"$client/c1">>, Topic),
    emqttc:disconnect(C1).

rewrite_sub(_Config) ->
    emqx_mod_rewrite:unload(rewrite),
    emqx_mod_rewrite:load(?REWRITE),
    {ok, C1} = emqttc:start_link([{host, "localhost"}, {client_id, <<"c1">>}, {username, <<"u1">>}]),
    timer:sleep(10),
    emqttc:subscribe(C1, <<"x/y/z">>, qos2),
    timer:sleep(10),
    Subs = ets:lookup(mqtt_subscription, <<"c1">>),
    ?assert(lists:member({<<"c1">>, <<"z/y/z">>}, Subs)),
    emqx:publish(emqx_message:make(ct, <<"a/b/c">>, <<"hello">>)),
    emqttc:unsubscribe(C1, <<"x/y/z">>),
    timer:sleep(20),
    UnSubs = ets:lookup(mqtt_subscription, <<"c1">>),
    ?assertEqual(false, lists:member({<<"c1">>, <<"z/y/z">>}, UnSubs)),
    emqttc:disconnect(C1).

publish(_) ->
    Msg = emqx_message:make(ct, <<"test/pubsub">>, <<"hello">>),
    ok = emqx:subscribe(<<"test/+">>),
    timer:sleep(10),
    emqx:publish(Msg),
    ?assert(receive {dispatch, <<"test/+">>, Msg} -> true after 5 -> false end).

presence_msg(_) ->
    Self = self(),
    {ok, P1} = emqttc:start_link([{host, "localhost"}, {client_id, <<"p1">>}]),
    timer:sleep(10),
    ok = emqx:subscribe(<<"$SYS/brokers/+/clients/c1/+">>, Self),
    timer:sleep(10),
    emqttc:disconnect(P1).


start_apps(App, DataDir) ->
    Schema = cuttlefish_schema:files([filename:join([DataDir, atom_to_list(App) ++ ".schema"])]),
    Conf = conf_parse:file(filename:join([DataDir, atom_to_list(App) ++ ".conf"])),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    ct:log("NewConfig:~p~n", [NewConfig]),
    Vals = proplists:get_value(App, NewConfig),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals],
    application:ensure_all_started(App).



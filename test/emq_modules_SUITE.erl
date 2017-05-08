-module(emq_modules_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-define(HOOKS, ['client.connected', 'client.disconnected', 'client.subscribe', 'client.unsubscribe', 'message.publish']).

-define(REWRITE, [{rewrite, <<"x/#">>, <<"^x/y/(.+)$">>, <<"z/y/$1">>}]).


all() ->
    [emq_modules_restart,
     {group, emq_mod_subscription},
     {group, emq_mod_rewrite},
     {group, emq_mod_presence}].

groups() ->
    [{emq_mod_subscription, [parallel], [connect]},
     {emq_mod_rewrite, [sequence], [rewrite_sub, publish]},
     {emq_mod_presence, [parallel], [presence_msg]}
    ].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    [start_apps(App, DataDir) || App <- [emqttd, emq_modules]],
    Config.

end_per_suite(Config) ->
    [application:stop(App) || App <- [emq_modules, emqttd]],
    Config.

emq_modules_restart(_Config) ->
    application:stop(emq_modules),
    UnloadLists = [emqttd_hooks:lookup(Hook) || Hook <- ?HOOKS],
    ?assert(lists:append(UnloadLists) =:= []),
    application:start(emq_modules),
    ReloadLists = [emqttd_hooks:lookup(Hook) || Hook <- ?HOOKS],
    ?assert(length(lists:append(ReloadLists)) > 0).

connect(_Config)->
    {ok, C1} = emqttc:start_link([{host, "localhost"}, {client_id, <<"c1">>}, {username, <<"u1">>}]),
    timer:sleep(1000),
    [{Sub, Topic}] = ets:tab2list(mqtt_subscription),
    ?assertEqual(<<"c1">>, Sub),
    ?assertEqual(<<"$client/c1">>, Topic),
    emqttc:disconnect(C1).

rewrite_sub(_Config) ->
    emq_mod_rewrite:unload(rewrite),
    emq_mod_rewrite:load(?REWRITE),
    {ok, C1} = emqttc:start_link([{host, "localhost"}, {client_id, <<"c1">>}, {username, <<"u1">>}]),
    timer:sleep(10),
    emqttc:subscribe(C1, <<"x/y/z">>, qos2),
    timer:sleep(10),
    Subs = ets:lookup(mqtt_subscription, <<"c1">>),
    ?assert(lists:member({<<"c1">>, <<"z/y/z">>}, Subs)),
    emqttd:publish(emqttd_message:make(ct, <<"a/b/c">>, <<"hello">>)),
    emqttc:unsubscribe(C1, <<"x/y/z">>),
    timer:sleep(20),
    UnSubs = ets:lookup(mqtt_subscription, <<"c1">>),
    ?assertEqual(false, lists:member({<<"c1">>, <<"z/y/z">>}, UnSubs)),
    emqttc:disconnect(C1).

publish(_) ->
    Msg = emqttd_message:make(ct, <<"test/pubsub">>, <<"hello">>),
    ok = emqttd:subscribe(<<"test/+">>),
    timer:sleep(10),
    emqttd:publish(Msg),
    ?assert(receive {dispatch, <<"test/+">>, Msg} -> true after 5 -> false end).

presence_msg(_) ->
    Self = self(),
    {ok, P1} = emqttc:start_link([{host, "localhost"}, {client_id, <<"p1">>}]),
    timer:sleep(10),
    ok = emqttd:subscribe(<<"$SYS/brokers/+/clients/c1/+">>, Self),
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

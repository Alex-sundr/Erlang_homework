-module(bench).
-export([start/0]).
-compile(export_all).

start() ->
    MaxAccesses = 300000,
    Step        = 10000,

    {ok, Dev} = file:open("erl_data_bench_out.csv", [write, exclusive]),
    Pid = spawn(fun() -> collector(Dev) end),
    true = register('$write', Pid),
    start(Step, MaxAccesses, Step).

collector(Dev) ->
    receive
        {Pid, stop} ->
            file:close(Dev),
            Pid ! ok;
        {_Count, _Hits, _Name, _Time, _Size} = Msg ->
            Args = tuple_to_list(Msg),
            file:write(Dev, io_lib:format("~B,~B,~s,~s,~B~n", Args)),
            collector(Dev)
    end.

start(Max1, Max2, _Step) when Max1 > Max2 ->
    '$write' ! {self(), stop},
    receive ok -> ok end;
start(Accesses, MaxAcc, Step) ->
    Max      = 20000,
    InitSize = 1000,
    %%Accesses = 30000,
    %%Loops    = 3,
    Loops    = 1,
    Types    = [ets, pd, dict, gb_trees, map_get, map_find, map_match],

    Init  = [{rand:uniform(Max), X} || X <- lists:seq(1, InitSize)],
    Write = [{rand:uniform(Max), X} || X <- lists:seq(1, Accesses)],
    Read  = [ rand:uniform(Max)     || _ <- lists:seq(1, Accesses)],
    Tests = [make_funs(X) || X <- Types],
    start_loop(Accesses, Tests, Init, lists:zip(Write, Read), Loops, Tests),
    start(Accesses + Step, MaxAcc, Step).

make_funs(X) -> {X, j(init, X), j(save, X), j(read, X), j(size, X), j(drop, X)}.

j(X, Y) -> list_to_atom(atom_to_list(X) ++"_"++ atom_to_list(Y)).

start_loop(_Count, [], _, _, 0, _) ->
    done;
start_loop(Count, [], Init, Data, X, Tests) ->
    start_loop(Count, Tests, Init, Data, X - 1, Tests);
start_loop(Count, [Test | T], Init, Data, Loops, Tests) ->
    Pid = self(),
    spawn(fun() -> do_test(Pid, Test, Init, Data) end),
    receive {Sum, Name, Time, Size} -> report(Count, Sum, Name, Time, Size) end,
    start_loop(Count, T, Init, Data, Loops, Tests).

do_test(Pid, {Name, InitF, SaveF, ReadF, SizeF, DropF}, Init, Data) ->
    {memory, Empty} = process_info(self(), memory),
    Id0 = ?MODULE:InitF(),
    Id2 = lists:foldl(fun(X, Id1) -> ?MODULE:SaveF(Id1, X) end, Id0, Init),
    Foo = fun(Elem, Acc) -> test_one(Elem, Acc, SaveF, ReadF) end,
    {Tc, {Id3, Sum}} = timer:tc(fun() -> lists:foldl(Foo, {Id2, 0}, Data) end),
    Pid ! {Sum, Name, Tc, ?MODULE:SizeF(Id3, Empty)},
    ?MODULE:DropF(Id3).

test_one({Save, Key}, {Id, Hits}, SaveF, ReadF) ->
    NId = ?MODULE:SaveF(Id, Save),
    case ?MODULE:ReadF(NId, Key) of
        true -> {NId, Hits + 1};
        false -> {NId, Hits}
    end.

report(Count, Hits, OName, OTime, Size) ->
    Keys = integer_to_list(Count),
    Name = atom_to_list(OName),
    Time = integer_to_list(OTime),
    Pad0 = lists:duplicate(7 - length(Keys), $\s),
    Pad1 = lists:duplicate(12 - length(Name), $\s),
    Pad2 = lists:duplicate(10 - length(Time), $\s),
    Msg = "~B Keys: ~s~B Hits: ~s~sTime:~s~s Size:~B~n",
    io:format(Msg, [Count, Pad0, Hits, Name, Pad1, Time, Pad2, Size]),
    '$write' ! {Count, Hits, Name, Time, Size}.

init_ets()       -> ets:new(my_table, []).
save_ets(Tid, T) -> ets:insert(Tid, T), Tid.
read_ets(Tid, K) -> ets:lookup(Tid, K) =/= [].
size_ets(Tid, E) -> process_size(E)
                        + ets:info(Tid, memory) * erlang:system_info(wordsize).
drop_ets(Tid)    -> ets:delete(Tid).

init_pd()          -> undefined.
save_pd(_, {K, V}) -> put(K, V), undefined.
read_pd(_, K)      -> get(K) =/= undefined.
size_pd(_, E)      -> process_size(E).
drop_pd(_)         -> ok.

init_dict()          -> dict:new().
save_dict(D, {K, V}) -> dict:store(K, V, D).
read_dict(D, K)      -> dict:find(K, D) =/= error.
size_dict(_, E)      -> process_size(E).
drop_dict(_)         -> ok.

init_gb_trees()          -> gb_trees:empty().
save_gb_trees(G, {K, V}) -> gb_trees:enter(K, V, G).
read_gb_trees(G, K)      -> gb_trees:lookup(K, G) =/= none.
size_gb_trees(_, E)      -> process_size(E).
drop_gb_trees(_)         -> ok.

init_map_get()          -> maps:new().
save_map_get(M, {K, V}) -> maps:put(K, V, M).
read_map_get(M, K)      -> maps:get(K, M, undefined) =/= undefined.
size_map_get(_, E)      -> process_size(E).
drop_map_get(_)         -> ok.

init_map_find()          -> maps:new().
save_map_find(M, {K, V}) -> maps:put(K, V, M).
read_map_find(M, K)      -> maps:find(K, M) =/= error.
size_map_find(_, E)      -> process_size(E).
drop_map_find(_)         -> ok.

init_map_match()          -> #{}.
save_map_match(M, {K, V}) -> M#{K => V}.
read_map_match(M, K)      -> case M of #{K := _} -> true; _ -> false end.
size_map_match(_, E)      -> process_size(E).
drop_map_match(_)         -> ok.

init_list()              -> [].
save_list(L, {K, _} = T) -> lists:keystore(K, 1, L, T).
read_list(L, K)          -> proplists:get_value(K, L) =/= undefined.
size_list(_, E)          -> process_size(E).
drop_list(_)             -> ok.

init_orddict()          -> orddict:new().
save_orddict(O, {K, V}) -> orddict:store(K, V, O).
read_orddict(O, K)      -> orddict:find(K, O) =/= error.
size_orddict(_, E)      -> process_size(E).
drop_orddict(_)         -> ok.

process_size(Empty) -> element(2, process_info(self(), memory)) - Empty.

-module(cache_server_SUITE).
 
-include_lib("common_test/include/ct.hrl").
-compile(export_all).
 

all() ->
  [{group, all}].

groups() -> [
  {all, [sequence], [
    initialize,
    add,
    select,
    select_by_date
  ]}
].

initialize(_Config) ->
  {ok, _Pid} = cache_server:start_link(table1, [{drop_interval, 10}]).

add(_Config) ->
  ok = cache_server:insert(table1, key1, value1, 5),
  ok = cache_server:insert(table1, key2, value2, 10),
  ok = cache_server:insert(table1, key3, value3, 15),
  ok = cache_server:insert(table1, key4, value4, 20),
  ok = cache_server:insert(table1, key5, value5, 25).


select(_Config) ->
    {ok, value1} = cache_server:lookup(table1, key1),
    {ok, value2} = cache_server:lookup(table1, key2),
    {ok, value3} = cache_server:lookup(table1, key3),
    {ok, value4} = cache_server:lookup(table1, key4),
    {ok, value5} = cache_server:lookup(table1, key5).
    
select_by_date(_Config) ->
  {ok,Value}  = cache_server:lookup_by_date(table, {{2019, 8, 3}, {00, 00, 00}}, {{2019, 8, 3}, {15, 10, 00}}),
  ok = is_list(Value).
-module(cache_server).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").
 
%                                          API
-export([start_link/2,
         insert/4,
		 lookup/2,
		 lookup_by_date/3]).

%%                                     gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

  
%==========================================
t_now() -> calendar:datetime_to_gregorian_seconds(calendar:local_time()). 

%==========================================
%                                                    API  

start_link(TableName,[{drop_interval,Time}]) ->  
    gen_server:start_link({local, ?MODULE}, ?MODULE, [TableName,Time], []).

insert(TableName,Key,Val,TTL)   ->
	gen_server:cast(?MODULE, {insert,TableName,Key,Val,TTL}).

lookup(TableName,Key)->
	gen_server:call( ?MODULE, {lookup, TableName, Key}).

lookup_by_date(TableName,DateFrom, DateTo)->
	gen_server:call( ?MODULE, {lookup_by_date,TableName, DateFrom, DateTo}).
%==========================================

%                                                    Gen Server


init([TableName,Time]) ->
      State = ets: new(TableName, [set,public,{keypos,1},named_table]),
        erlang:send_after(Time, self(),{droop_addled,[TableName,Time]}),
    {ok, State}.

 
handle_cast({insert,TableName,Key,Val,TTL}, State) ->
        ets:insert(TableName,{Key,Val,TTL + t_now()}),
      {noreply,State}.

handle_call({lookup, TableName, Key}, _From, State)->
Reply = try ets:select(TableName, [{{'$1','$2','$3'},[{'>','$3', t_now()},{'=:=','$1',Key}],['$2']}])  of
         []-> {ok, empty};
         [Value] ->  {ok, Value};
         Value ->  {ok, Value}
        catch
         error:badarg -> {error, record_not_exist}
        end,
             {reply, Reply, State};  
 
handle_call({lookup_by_date,TableName, DateFrom, DateTo},_From, State)->
		From = calendar:datetime_to_gregorian_seconds(DateFrom),
		To = calendar:datetime_to_gregorian_seconds(DateTo),
  Selector = ets:fun2ms(fun({Key,Val,TTL})when ( From <TTL) and (TTL <  To ) -> [Key,Val,TTL] end ),
   Reply = try ets:select(TableName, Selector) of
              []-> {ok, empty};
              Value ->  {ok, Value}              
           catch
		         error:badarg -> {error,record_not_exist}
           end,
        {reply, Reply, State}.
 

%==========================================

delete_addled(TableName) ->
    delete_addled(TableName, ets:first(TableName), t_now()).

delete_addled('$end_of_table',_,_) ->
  table_clear;

delete_addled(TableName,Key, Now) ->
  NextKey = ets:next(TableName, Key),
  [{_, _, TTL}] = ets:lookup(TableName, Key),
  if
    Now > TTL -> ets:delete(TableName, Key);
    true -> true
  end,
  delete_addled(TableName,NextKey, Now).
 

%==========================================

handle_info(droop_addled, [TableName, Time]) ->
  delete_addled(TableName),	
  erlang:send_after(Time, self(),droop_addled,TableName),
    {noreply, Time};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

  
%handle_call({lookup, TableName, Key}, _From, State)->
% Reply = try ets:select(TableName, [{{'$1','$2','$3'},[{'>','$3', t_now()},{'=:=','$1',Key}],['$2']}])  of
%         []-> {ok,empty};
 %        Value ->  {ok,Value}
 %       catch
  %       error:badarg -> {error,record_not_exist}
 %       end,
 %            {reply,Reply, State};

%    {ok, Pid3} = cache_server:start_link(table1 , [{drop_interval, 3}]).

%    ok = cache_server:insert(table1 , key, value, 6).
%    ok = cache_server:insert(table1 , key1, value1, 10).
%    ok = cache_server:insert(table1 , key2, value2, 100).

%     {ok, Value} = cache_server:lookup(table1, key).
%    {ok, Value1} = cache_server:lookup(table1, key1).
%    {ok, Value2} = cache_server:lookup(table1, key2).

 %             DateFrom = {{2019,8,2},{0,19,39}}.
   %          DateTo = {{2019,8,3},{24,30,39}}.
  %              {ok, ValueN} = cache_server:lookup_by_date(table1, DateFrom, DateTo).
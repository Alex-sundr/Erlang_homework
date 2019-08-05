
-module(my_cache).
-export([create/1,insert/4,lookup/2,delete_obsolete/1]).
 

 t_now() -> calendar:datetime_to_gregorian_seconds(calendar:local_time()). 

 

create(TableName) ->
   try ets: new(TableName, [set,protected,{keypos,1},named_table]) of
        TableName -> create
  catch  	 
	error:badarg ->  {error,TableName, elredy_created}
  
 end.
 

insert(TableName,Key,Val,TTL) ->
    try ets:insert(TableName,{Key,Val,TTL + t_now()}) of
		true -> ok
    catch
		error:badarg ->  {error, error,tale_no_created}
    end.

lookup(TableName,Key) ->
    try ets:select(TableName, [{{'$1','$2','$3'},[{'>','$3', t_now()},{'=:=','$1',Key}],['$$']}])  of
		[Value] -> {ok, Value};
		[]-> empty
    catch
		error:badarg -> {error,record_not_exist}
    end.    


delete_obsolete(TableName) ->
    try
		ets:select_delete(TableName,[{{'$_','$_','$3'},[{'=<','$3',t_now()}],['$$']}])  of
		_ -> ok
    catch
		error:badarg -> {error, not_exists}
    end.

  
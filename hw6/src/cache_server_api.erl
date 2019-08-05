-module(cache_server_api).

-export([init/2, parce_body/3]).
 

 init(Req0, Opts) ->
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain">>
	}, <<"Hello world!">>, Req0),
	{ok, Req, Opts}.

%init(Req0, Opts) ->
%	{Method, Req} = cowboy_req:method(Req0),
%	HasBody = cowboy_req:has_body(Req),
	%{ok, Req1} = parce_body(Method, HasBody, Req),
	%{ok, Req1, Opts}.

 
parce_body(<<"POST">>, true, Req0) ->
	{ok, PostVals, Req} = cowboy_req:body_qs(Req0),
	[{BinJson, true}] = PostVals,
	Json = jsx:decode(BinJson),
	Action = proplists:get_value(<<"action">>, Json),
	
	RB = case action(Action, Json) of
		{ok, Ok} -> jsx:encode([<<"ok">>, Ok]);
		{error, Error} -> jsx:encode([<<"error">>, Error])
	end,

		cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], RB, Req);	
	

parce_body(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);

parce_body(_, _, Req) ->
	cowboy_req:reply(405, Req).



action(<<"insert">>, Json)->
	Key = proplists:get_value(<<"key">>, Json),
	Value = proplists:get_value(<<"value">>, Json),
	TTL  = proplists:get_value(<<"life">>, Json),
	cache_server:insert(table1,Key, Value,TTL);

action(<<"lookup">>, Json) ->
	Key = proplists:get_value(<<"key">>, Json),
	cache_server:lookup(table1,Key);

action(<<"lookup_by_date">>, Json) ->
	DateFrom = proplists:get_value(<<"date_from">>, Json),
	DateTo = proplists:get_value(<<"date_to">>, Json),
	cache_server:lookup_by_date(DateFrom, DateTo);

action(_, _)->{error, ["No Method"]}.

 


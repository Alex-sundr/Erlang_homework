
-module(p01).

-export([last/1]).
 
-include_lib("eunit/include/eunit.hrl").

last([])->
	[];

last([H|[]])->
	H;

last([_|T])->
	last(T).     
	 


%      Unit test for last() function     
last_test() ->
    ?assertEqual(f, last([a,b,c,d,e,f])),
    ?assertEqual([], last([])),
    ?assertEqual(10, last([5,e,10])),
    ok.
 

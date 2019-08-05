


-module(p02).
-export([but_last/1]).
-include_lib("eunit/include/eunit.hrl").


but_last([])->
	[];
but_last([A])->
	[A];	
but_last([A,B|[]])->
	[A,B];
but_last([_|T])->
	but_last(T).


%  Unit test for but_last() function     
but_last_test() ->
    ?assertEqual([e,f], but_last([a,b,c,d,e,f])),
    ?assertEqual([s,1.2], but_last([a,3,5,1.2,12,s,1.2])),
    ?assertEqual([], but_last([])),
    ?assertEqual([a], but_last([a])),
    ok.
 
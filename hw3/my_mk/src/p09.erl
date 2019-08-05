-module(p09).

-export([pack/1]).

-include_lib("eunit/include/eunit.hrl").

pack([])->
	[];

pack([H|[]])->
	[H|[]];


pack([H|T])->
    pack(T,[H],[]).


pack([],InList,Acc) ->
    p05:reverse([InList|Acc]);

pack([H|T],[H|T1],Acc) ->
    pack(T,[H,H|T1],Acc);

pack([H|T],InList,Acc ) ->
    pack(T,[H],[InList|Acc]). 

% EUnit test for pack() function      
    pack_test() ->
    ?assertEqual([[a,a,a,a],[b],[c,c,c],[a,a],[d],[e,e,e,e]], pack([a,a,a,a,b,c,c,c,a,a,d,e,e,e,e])),
    ?assertEqual([], pack([])),
    ?assertEqual([v], pack([v])),
    ok.

-module(p03).
-export([element_at/2]).
-include_lib("eunit/include/eunit.hrl").

element_at([],_)->
	undefined;

element_at([H|_],1)->
	H;

element_at([_|T],N)->
	element_at(T,N-1).


element_at_test() ->
    ?assertEqual(c, element_at([a,b,c,d,e,f], 3)),
    ?assertEqual(undefined, element_at([a,b,c,d,e,f], 10)),
    ?assertEqual(undefined, element_at([], 4)),
    ?assertEqual(undefined, element_at([a,s,d], 0)),
    ok.


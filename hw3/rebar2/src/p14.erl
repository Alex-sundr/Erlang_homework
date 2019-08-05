-module(p14).
-export([duplicate/1]).
-include_lib("eunit/include/eunit.hrl").


duplicate([])->
	[];

duplicate(List)->
    duplicate(List,[]).


duplicate([H|[]],Acc)->
	p05:reverse([H,H|Acc]);


duplicate([H|T],Acc)->
	duplicate(T,[H,H|Acc]).

%  Eunit test for duplicate() function 
duplicate_test() ->
    ?assertEqual([a,a,b,b,c,c,c,c,d,d], duplicate([a,b,c,c,d])),
    ?assertEqual([], duplicate([])),
    ?assertEqual([a,a], duplicate([a])),
    ok.

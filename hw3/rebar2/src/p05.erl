-module(p05).

-export([reverse/1]).

-include_lib("eunit/include/eunit.hrl").


reverse([])->
    [];
reverse([H|[]])->
    [H|[]];

reverse(List)-> 
	reverse(List,[]).

reverse([],Acc)->
	Acc;	

reverse([H|T], Acc)->
    reverse(T,[H|Acc]).


  %    Eunit test for reverse() function     
reverse_test() ->
    ?assertEqual([3,2,1], reverse([1,2,3])),
    ?assertEqual([c,4,2.2,a], reverse([a,2.2,4,c])),
    ?assertEqual([], reverse([])),
    ?assertEqual([a], reverse([a])),
    ok.
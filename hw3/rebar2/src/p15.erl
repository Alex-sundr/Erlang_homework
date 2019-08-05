-module(p15).

-export([replicate/2]).
-include_lib("eunit/include/eunit.hrl").



replicate(List,N)->
    replicate(List,[],N).

replicate([H|[]],Acc,N)->
	p07:flatten(p05:reverse([[letter_list(N,H)] | Acc]));


replicate([H|T],Acc,N)->
	replicate(T,[[letter_list(N,H)]|  Acc],N).


letter_list(1,Letter)->
	[Letter];

letter_list(Number,Letter)->
	[Letter|letter_list(Number-1,Letter)].	

%   Eunit test for replicate() function   
replicate_test() ->
    ?assertEqual([a,a,a,b,b,b,c,c,c], replicate([a,b,c], 3)),
    ?assertEqual([a,a,a,a,b,b,b,b,c,c,c,c],replicate([a,b,c], 4)),
    ?assertEqual([], replicate([], 3)),
    ?assertEqual([], replicate([a,b,c], 0)),

    ok.
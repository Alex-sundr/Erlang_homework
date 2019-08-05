-module(p07).
-export([flatten/1]).

-include_lib("eunit/include/eunit.hrl").


flatten(L) ->
     p05:reverse(flatten(L,[])).

flatten([],Acc)->
    Acc;


flatten([[]|T],Acc) ->
      flatten(T,Acc);


flatten([[H|T]|T1],Acc) ->
      flatten(T1, flatten([H|T],Acc)); 

   
flatten([H|T],Acc) -> 
 	flatten(T,[H|Acc]).
    

%   Test for flatten() function     %%
flatten_test() ->
    ?assertEqual([a,b,c,d,e,f], flatten([a,[],[b,[c,d],e,[[[f]]]]])),
    ?assertEqual([a,b,c,e,b,d,e], flatten([a,[],[b,[c,[e,[[[b]]]],d],e]])),
    ?assertEqual([a,b,c,d,e], flatten([a,b,c,d,e])),
    ?assertEqual([], flatten([])),
    ?assertEqual([a], flatten([a])),
    ok.
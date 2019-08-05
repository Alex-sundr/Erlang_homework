-module(p07).
-export([flatten/1]).

 


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
    


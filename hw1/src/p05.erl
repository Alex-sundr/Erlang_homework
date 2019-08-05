-module(p05).

-export([reverse/1]).



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
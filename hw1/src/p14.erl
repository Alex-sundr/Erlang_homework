-module(p14).
-export([duplicate/1]).

duplicate(List)->
    duplicate(List,[]).

duplicate([H|[]],Acc)->
	p005:reverse([H,H|Acc]);


duplicate([H|T],Acc)->
	duplicate(T,[H,H|Acc]).



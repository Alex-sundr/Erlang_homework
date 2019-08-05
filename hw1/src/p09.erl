-module(p09).

-export([pack/1]).

pack([])->
	[];

pack([H|[]])->
	[H|[]];


pack([H|T])->
    pack(T,[H],[]).


pack([],InList,Acc) ->
    p005:reverse([InList|Acc]);

pack([H|T],[H|T1],Acc) ->
    pack(T,[H,H|T1],Acc);

pack([H|T],InList,Acc ) ->
    pack(T,[H],[InList|Acc]). 
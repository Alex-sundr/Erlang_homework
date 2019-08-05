-module(p08).
-export([compress/1]).

 
 compress([])->
    [];
compress(H)->
    compress(H,[]).

compress([H|[]],[H|T1]) ->
    p005:reverse([H|T1]);

compress([H|[]],Acc) ->
    p005:reverse([H|Acc]);

compress([H|T],[H|T1]) -> 
    compress(T,[H|T1]);

compress([H|T],Acc) ->
    compress(T,[H|Acc]).
-module(p08).
-export([compress/1]).
-include_lib("eunit/include/eunit.hrl").
 
 compress([])->
    [];
compress(H)->
    compress(H,[]).

compress([H|[]],[H|T1]) ->
    p05:reverse([H|T1]);

compress([H|[]],Acc) ->
    p05:reverse([H|Acc]);

compress([H|T],[H|T1]) -> 
    compress(T,[H|T1]);

compress([H|T],Acc) ->
    compress(T,[H|Acc]).


   % EUnit test for compress() function      
compress_test() ->
    ?assertEqual([a,b,c,a,d,e], compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e])),
    ?assertEqual([], compress([])),
    ?assertEqual([a], compress([a])),
    ok.
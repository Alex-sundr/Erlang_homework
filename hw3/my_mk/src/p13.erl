-module(p13).

-export([decode/1]).
-include_lib("eunit/include/eunit.hrl").


decode([])->
	[];
 
decode([H|T]) ->
	p07:flatten(decode(H, T, [])).

decode([], [], Acc) ->
	Acc;

decode([], [H|T], Acc) ->
	decode(H, T, Acc);


decode({0, _}, T, Acc) ->
	decode([], T, [Acc]);

decode({N, H}, T, Acc) ->
	decode({N - 1, H}, T, [Acc|[H]]).


	%   Eunit test for decode() function 
decode_test() ->
    ?assertEqual([a,a,a,a,b,c,c,a,a,d,e,e,e,e], decode([{4,a},{1,b},{2,c},{2,a},{1,d},{4,e}])),
    ?assertEqual([], decode([])),
    ok.
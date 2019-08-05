-module(p12).
-export([decode_modified/1]).
-include_lib("eunit/include/eunit.hrl").

decode_modified([]) ->
	[];
decode_modified([H|T]) ->
	p07:flatten(decode_modified(H, T, [])).

decode_modified([], [], Acc) ->
	Acc;

decode_modified([], [H|T], Acc) ->
	decode_modified(H, T, Acc);

decode_modified({0, _}, T, Acc) ->
	decode_modified([], T, [Acc]);

decode_modified({N, H}, T, Acc) ->
	decode_modified({N - 1, H}, T, [Acc|[H]]);

decode_modified(H, T, Acc) ->
decode_modified([], T, [Acc|[H]]).

%   Eunit test for decode_modified() function   
decode_modified_test() ->
    ?assertEqual([a,a,a,a,b,c,c,a,a,d,e,e,e,e], decode_modified([{4,a},b,{2,c},{2,a},d,{4,e}])),
    ?assertEqual([], decode_modified([])),
    ?assertEqual([a,a,a,a], decode_modified([a,a,a,a])),
    ok.

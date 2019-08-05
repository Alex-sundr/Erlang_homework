-module(p10).

-export([encode/1]).
-include_lib("eunit/include/eunit.hrl").

encode([])->
	[];

encode([H|T])->
	encode(T,H,1,[]).

encode([], Letter, Number, Acc)->
    p05:reverse([{Number, Letter}|Acc]);

encode([H|T],H,Number,Acc)->
    encode(T,H,Number+1,Acc);
    
encode([H|T],Letter,Number,Acc)->
     encode(T,H,1,[{Number,Letter}|Acc]).     


%   Eunit test for encode() function
encode_test() ->
    ?assertEqual([{4,d},{1,b},{2,c},{2,a},{1,d},{4,e}], encode([d,d,d,d,b,c,c,a,a,d,e,e,e,e])),
    ?assertEqual([], encode([])),
    ok.
-module(p11).
-export([encode_modified/1]).
-include_lib("eunit/include/eunit.hrl").

encode_modified([])->
	[];

encode_modified([H|T])->
	encode_modified(T,H,1,[]).

encode_modified([], Letter, 1, Acc)->
    p05:reverse([Letter|Acc]);

encode_modified([], Letter, Number, Acc)->
    p05:reverse([{Number, Letter}|Acc]);

encode_modified([H|T],H,Number,Acc)->
    encode_modified(T,H,Number+1,Acc);

encode_modified([H|T],Letter,1,Acc)->
     encode_modified(T,H,1,[Letter|Acc]);   

encode_modified([H|T],Letter,Number,Acc)->
     encode_modified(T,H,1,[{Number,Letter}|Acc]). 

         %  Eunit test for encode_modified() function
encode_modified_test() ->
    ?assertEqual([{4,a},b,d,{2,c},{2,a},d,{4,e}], encode_modified([a,a,a,a,b,d,c,c,a,a,d,e,e,e,e])),
    ?assertEqual([], encode_modified([])),
    ?assertEqual([a], encode_modified([a])),
    ?assertEqual([a,b], encode_modified([a,b])),
    ok.
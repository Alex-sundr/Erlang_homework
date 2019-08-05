-module(p10).

-export([encode/1]).

encode([])->
	[];

encode([H|T])->
	encode(T,H,1,[]).

encode([], Letter, Number, Acc)->
    p005:reverse([{Number, Letter}|Acc]);

encode([H|T],H,Number,Acc)->
    encode(T,H,Number+1,Acc);
    
encode([H|T],Letter,Number,Acc)->
     encode(T,H,1,[{Number,Letter}|Acc]).     

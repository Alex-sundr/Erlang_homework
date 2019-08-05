-module(p11).
-export([encode_modified/1]).


encode_modified([])->
	[];

encode_modified([H|T])->
	encode_modified(T,H,1,[]).

encode_modified([], Letter, Number, Acc)->
    p005:reverse([{Number, Letter}|Acc]);
  


encode_modified([H|T],H,Number,Acc)->
    encode_modified(T,H,Number+1,Acc);

 encode_modified([H|T],Letter,1,Acc)->
     encode_modified(T,H,1,[Letter|Acc]);

encode_modified([H|T],Letter,Number,Acc)->
     encode_modified(T,H,1,[{Number,Letter}|Acc]). 

         
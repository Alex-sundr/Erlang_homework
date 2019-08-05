-module(bs03).
-export([split/2]).



split(Bin,S)->
   split(Bin, << >>, [],list_to_binary(S)). 

split( << >>, AccBin, Acc,_)->
   lists:reverse([AccBin|Acc]);


split(Bin,AccBin,Acc,Splitter)->
  Bsize = byte_size(Splitter),
  case Bin of
    
    
    <<Splitter:Bsize/binary,Rest/binary>>  ->
			 split(Rest, <<>>, [AccBin|Acc], Splitter);   		 
   
    <<C/utf8, Rest/binary>> ->
    		split(Rest,<<AccBin/binary,C/utf8>>, Acc, Splitter) 
    
       
    end.

 
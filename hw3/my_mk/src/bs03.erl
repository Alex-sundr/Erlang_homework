-module(bs03).
-export([split/2]).
-include_lib("eunit/include/eunit.hrl").


split(Bin,S)->
   Bsize = byte_size(list_to_binary(S)),
   split(Bin, << >>, [],list_to_binary(S),Bsize). 

split( << >>, AccBin, Acc,_,_)->
   lists:reverse([AccBin|Acc]);


split(Bin,AccBin,Acc,Splitter,Bsize)->
    case Bin of
        
    <<Splitter:Bsize/binary,Rest/binary>>  ->
			 split(Rest, <<>>, [AccBin|Acc], Splitter,Bsize);   		 
   
    <<C/utf8, Rest/binary>> ->
    		split(Rest,<<AccBin/binary,C/utf8>>, Acc, Splitter,Bsize) 
        
    end.

 % Eunit  test for split() function         
split_test() ->
    ?assertEqual([<<"Col1">>, <<"Col2">>, <<"Col3">>, <<"Col4">>, <<"Col5">>] ,split(<<"Col1-:-Col2-:-Col3-:-Col4-:-Col5">>, "-:-")),
    ?assertEqual([<<"Col1">>, <<"Col2">>, <<"Col3">>, <<"Col4">>, <<"Col5">>] ,split(<<"Col1-Col2-Col3-Col4-Col5">>, "-")),
    ?assertEqual([<<"Col1">>, <<"Col2">>, <<"Col3">>, <<"Col4">>, <<"Col5">>] ,split(<<"Col1-:-\-:-Col2-:-\-:-Col3-:-\-:-Col4-:-\-:-Col5">>, "-:-\-:-")),
    ?assertEqual([<<"Col1">>, <<"Col2">>, <<"Col3">>, <<"Col4">>, <<"Col5">>] ,split(<<"Col1Delimiter DelimiterCol2Delimiter DelimiterCol3Delimiter DelimiterCol4Delimiter DelimiterCol5">>, "Delimiter Delimiter")),
    ?assertEqual([<<>>] ,split(<<>>, "-:-")),
 ok.
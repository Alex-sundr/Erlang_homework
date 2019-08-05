-module(bs02).
-export([words/1]).
-include_lib("eunit/include/eunit.hrl").

words(Bin)->
   words(Bin, << >>, []).

words( << >>, AccBin, Acc)->
   lists:reverse([AccBin|Acc]);


words(<< " ", Rest/binary>>, AccBin, Acc)->
	words(Rest, <<>>, [AccBin|Acc]);

words(<<C/utf8, Rest/binary>>, AccBin, Acc)->
	words(Rest,<<AccBin/binary,C/utf8>>,Acc).

 % Eunit test for words() function   
	words_test() ->
    ?assertEqual([<<"Text">>, <<"with">>, <<"four">>, <<"words">>], words(<<"Text with four words">>)),
    ?assertEqual([<<>>], words(<<>>)),
    ?assertEqual([<<"Text_with_four_words">>], words(<<"Text_with_four_words">>)),
    ok.
    



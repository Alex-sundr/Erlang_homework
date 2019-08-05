-module(bs02).
-export([words/1]).

words(Bin)->
   words(Bin, << >>, []).

words( << >>, AccBin, Acc)->
   lists:reverse([AccBin|Acc]);


words(<< " ", Rest/binary>>, AccBin, Acc)->
	words(Rest, <<>>, [AccBin|Acc]);

words(<<C/utf8, Rest/binary>>, AccBin, Acc)->
	words(Rest,<<AccBin/binary,C/utf8>>,Acc).



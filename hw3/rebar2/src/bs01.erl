-module(bs01).
-export([first_word/1]).
-include_lib("eunit/include/eunit.hrl").

first_word(Bin)->
	first_word(del_space(Bin), <<>>).

first_word(<< " ", _/binary >>, Acc)->
	Acc;


first_word(<< C/utf8, Rest/binary >>, Acc)->
	first_word(Rest,<<Acc/binary,C/utf8>>);

first_word(<<>>, Acc)->
	Acc.		

del_space(<<$\s, Rest/binary>>) ->
    del_space(Rest);
del_space(Rest) -> Rest.

%    Eunit test for first_word() function     
first_word_test() ->
    ?assertEqual(<<"Some">>, first_word(<<"Some text">>)),
    ?assertEqual(<<"Some">>, first_word(<<"  Some text">>)),
    ?assertEqual(<<"My_text">>, first_word(<<" My_text">>)),
    ?assertEqual(<<"abracadabra_no_spase">>, first_word(<<"abracadabra_no_spase">>)),
    ?assertEqual(<<>>, first_word(<<>>)),
    ok.
 

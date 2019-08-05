-module(p04).
-export([len/1,len_tail/1]).

-include_lib("eunit/include/eunit.hrl").

len([])->
	0;

len([_|T])->
	1+len(T).

%хвостовая рекурсия

len_tail([]) -> 
	0;

len_tail(L) -> 
	len_tail(L,0).

len_tail([_H|T],N) -> 
	len_tail(T, N + 1);

len_tail([], N) -> 
	N.
 


%    Eunit test for len() function       
len_test() ->
    ?assertEqual(0, len([])),
    ?assertEqual(10, len([a,b,c,d,e,f,g,h,v,g])),

    ok.
 
%    Test for len_tail() function       
len_tail_test() ->
    ?assertEqual(0, len_tail([])),
    ?assertEqual(4, len_tail([a,b,c,d])),
    ok.
 

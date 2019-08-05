-module(p06).
-export([is_palindrome/1]).
-include_lib("eunit/include/eunit.hrl").

is_palindrome([])-> 
    false;

is_palindrome([_|[]])-> 
    false;

is_palindrome(List)->
	is_palindrome(List,reverse(List)).

is_palindrome(List, Rlist)->
	 List == Rlist.

%-----------------
	reverse([])->
	    [];
	reverse([H|[]])->
	    [H|[]];

	reverse(List)-> 
		reverse(List,[]).

	reverse([],Acc)->
		Acc;	

	reverse([H|T], Acc)->
	    reverse(T,[H|Acc]).


%    Eunit test for is_palindrome() function        
is_palindrome_test() ->
    ?assertEqual(true, is_palindrome([l,e,v,e,l])),
    ?assertEqual(true, is_palindrome([1,2,3,2,1])),
    ?assertEqual(false, is_palindrome([])),
    ?assertEqual(false, is_palindrome([1,v,3,2,1])),
    ok.



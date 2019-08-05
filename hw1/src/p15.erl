-module(p15).

-export([replicate/2]).



replicate(List,N)->
    replicate(List,[],N).

replicate([H|[]],Acc,N)->
	p07:flatten(p05:reverse([[let_gen:letter_list(N,H)] | Acc]));


replicate([H|T],Acc,N)->
	replicate(T,[[let_gen:letter_list(N,H)]|  Acc],N).

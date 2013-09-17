-module( first_class_functions ).

-export( [task/0] ).

task() ->
	As = [fun math:sin/1, fun math:cos/1, fun cube/1],
	X = 1.0,
	Y = functional_composition( As, X ),
	Bs = [fun cube_inverse/1, fun math:acos/1, fun math:asin/1],
	io:fwrite( "Original: ~p Result ~p~n", [X, functional_composition(Bs, Y)] ).



cube( X ) -> math:pow( X, 2 ).

cube_inverse( X ) -> math:sqrt( X ).

functional_composition( Funs, X ) -> lists:foldl( fun(F, Acc) -> F(Acc) end, X, Funs ).

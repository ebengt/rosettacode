-module( roman_numerals ).

-export( [encode_from_integer/1, decode_from_string/1]).

-record( encode_acc, {n, romans=""} ).

encode_from_integer( N ) when N > 0 ->
	#encode_acc{romans=Romans} = lists:foldl( fun encode_from_integer/2, #encode_acc{n=N}, maps() ),
	Romans.

decode_from_string( Romans ) when Romans =/= "" ->
	Maps = maps(),
	[First_integer | Integers] = [proplists:get_value([X], Maps) || X <- Romans],
	{Last, Sum} = lists:foldl( fun decode_from_integers/2, {First_integer, 0}, Integers ),
	Sum + Last.


encode_from_integer( _Map, #encode_acc{n=0}=Acc ) -> Acc;
encode_from_integer( {_Roman, Value}, #encode_acc{n=N}=Acc ) when N < Value -> Acc;
encode_from_integer( {Roman, Value}, #encode_acc{n=N, romans=Romans} ) ->
	Times = N div Value,
	New_roman = lists:flatten( lists:duplicate( Times, Roman) ),
	#encode_acc{n=N - (Times * Value), romans=Romans ++ New_roman}.

decode_from_integers( Integer, {Previous, Sum} ) when Integer > Previous -> {Integer, Sum - Previous};
decode_from_integers( Integer, {Previous, Sum} ) -> {Integer, Sum + Previous}.

maps() -> [{"M",1000}, {"CM",900}, {"D",500}, {"CD",400}, {"C",100}, {"XC",90}, {"L",50}, {"XL",40}, {"X",10}, {"IX",9}, {"V",5}, {"IV",4}, {"I",1}].

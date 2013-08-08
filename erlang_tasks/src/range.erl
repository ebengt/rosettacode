-module( range ).

-export( [expansion/1, extraction/1, task/0] ).

expansion( String ) ->
	lists:flatten( [expansion_individual(io_lib:fread("~d", X)) || X <- string:tokens(String, ",")] ).

extraction( [H | T] ) when is_integer(H) ->
	Reversed_extracts = extraction_acc( lists:foldl(fun extraction/2, {H, []}, T) ),
	string:join( lists:reverse(Reversed_extracts), "," ).

task() ->
    io:fwrite( "~p~n", [expansion("-6,-3--1,3-5,7-11,14,15,17-20")] ),
    io:fwrite( "~p~n", [extraction([0, 1, 2, 4, 6, 7, 8, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 27, 28, 29, 30, 31, 32, 33, 35, 36, 37, 38, 39])] ).



expansion_individual( {ok, [N], []} ) -> N;
expansion_individual( {ok, [Start], "-" ++ Stop_string} ) -> lists:seq( Start, erlang:list_to_integer(Stop_string) ).

extraction( N, {Start, Acc} ) when N =:= Start + 1 -> {Start, N, Acc};
extraction( N, {Start, Acc} )  -> {N, extraction_acc( {Start, Acc} )};
extraction( N, {Start, Stop, Acc} ) when N =:= Stop + 1 -> {Start, N, Acc};
extraction( N, {Start, Stop, Acc} ) -> {N, extraction_acc( {Start, Stop, Acc} )}.

extraction_acc( {N, Acc} ) -> [erlang:integer_to_list(N) | Acc];
extraction_acc( {Start, Stop, Acc} ) when Stop > Start + 1 -> [erlang:integer_to_list(Start) ++ "-" ++ erlang:integer_to_list(Stop) | Acc];
extraction_acc( {Start, Stop, Acc} ) -> [erlang:integer_to_list(Stop), erlang:integer_to_list(Start) | Acc]. % Reversed

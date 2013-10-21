-module( text_processing_max_licenses ).

-export( [out_dates_from_file/1, task/0] ).

out_dates_from_file( Name ) ->
	{ok, Binary} = file:read_file( Name ),
	Lines = [X || X <- binary:split(Binary, <<"\n">>, [global]), X =/= <<>>],
	{_N, Dict} = lists:foldl( fun out_dates/2, {0, dict:new()}, Lines ),
	[{X, dict:fetch(X, Dict)} || X <- dict:fetch_keys( Dict )].

task() ->
    [{Max, Dates} | _T] = lists:reverse( lists:sort(out_dates_from_file("priv/mlijobs.txt")) ),
    io:fwrite( "Max licenses was ~p at ~p~n", [Max, Dates] ).



out_dates( Line, {N, Dict} ) ->
	[_License, Direction, <<"@">>, Date | _T] = [X || X <- binary:split(Line, <<" ">>, [global]), X =/= <<>>],
	New_n = out_dates_n( N, Direction ),
	New_dict = out_dates_dict( N, New_n, Date, Dict ),
	{New_n, New_dict}.

out_dates_dict( N, New_n, Date, Dict ) when N > New_n -> dict:append( N, Date, Dict );
out_dates_dict( _N, _New_n, _Date, Dict ) -> Dict.

out_dates_n( N, <<"OUT">> ) -> N + 1;
out_dates_n( N, <<"IN">> ) -> N - 1.

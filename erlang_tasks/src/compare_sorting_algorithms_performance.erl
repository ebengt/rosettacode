-module( compare_sorting_algorithms_performance ).

-export( [task/0] ).

task() ->
	N = 5,
	Sort_results = time_sort_functions([bubble_sort, insertion_sort, quick_sort], N ),
	PNG = egd_chart:graph( Sort_results ),
	file:write_file( "compare_sorting_algorithms_performance.png", PNG ).


bubble_sort( List, N ) ->
	{Time, _Result} = timer:tc( fun() -> bubble_sort:list( List ) end ),
	{N, Time}.

time_sort_functions( _Funs, N ) ->
	Ones = [1 || _X <- lists:seq(1, N)],
	Ranges = [X || X <- lists:seq(1, N)],
	Shuffleds = [random:uniform(N) || _X <- lists:seq(1, N)],
	X_Ys = [bubble_sort(X, N) || X <- [Ones, Ranges, Shuffleds]].

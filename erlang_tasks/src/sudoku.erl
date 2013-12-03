-module( sudoku ).

-export( [display/1, start/1, solve/1, task/0] ).

display( Grid ) -> [display_row(Y, Grid) || Y <- lists:seq(1, 9)].
%% A known value is {{Column, Row}, Value}
%% Top left corner is {1, 1}, Bottom right corner is {9,9}
start( Knowns ) -> dict:from_list( Knowns ).

solve( Grid ) ->
	Sure = solve_all_sure( Grid ),
	solve_unsure( potentials(Sure), Sure ).

task() ->
	Simple = [{{1, 1}, 3}, {{2, 1}, 9}, {{3, 1},4}, {{6, 1}, 2}, {{7, 1}, 6}, {{8, 1}, 7},
		{{4, 2}, 3}, {{7, 2}, 4},
		{{1, 3}, 5}, {{4, 3}, 6}, {{5, 3}, 9}, {{8, 3}, 2},
		{{2, 4}, 4}, {{3, 4}, 5}, {{7, 4}, 9},
		{{1, 5}, 6}, {{9, 5}, 7},
		{{3, 6}, 7}, {{7, 6}, 5}, {{8, 6}, 8},
		{{2, 7}, 1}, {{5, 7}, 6}, {{6, 7}, 7}, {{9, 7}, 8},
		{{3, 8}, 9}, {{6, 8}, 8},
		{{2, 9}, 2}, {{3, 9}, 6}, {{4, 9}, 4}, {{7, 9}, 7}, {{8, 9}, 3}, {{9, 9}, 5}],
	task( Simple ),
	Difficult = [{{6, 2}, 3}, {{8, 2}, 8}, {{9, 2}, 5},
		{{3, 3}, 1}, {{5, 3}, 2},
		{{4, 4}, 5}, {{6, 4}, 7},
		{{3, 5}, 4}, {{7, 5}, 1},
		{{2, 6}, 9},
		{{1, 7}, 5}, {{8, 7}, 7}, {{9, 7}, 3},
		{{3, 8}, 2}, {{5, 8}, 1},
		{{5, 9}, 4}, {{9, 9}, 9}],
	task( Difficult ).



display_row( Row, Grid ) ->
	[display_row_group( X, Row, Grid ) || X <- [1, 4, 7]],
	display_row_nl( Row ).

display_row_group( Start, Row, Grid ) ->
	[io:fwrite(" ~c", [display_value(X, Row, Grid)]) || X <- [Start, Start+1, Start+2]],
	io:fwrite( " " ).

display_row_nl( N ) when N =:= 3; N =:= 6; N =:= 9 -> io:nl(), io:nl();
display_row_nl( _N ) -> io:nl().

display_value( X, Y, Grid ) -> display_value( dict:find({X, Y}, Grid ) ).

display_value( error ) -> $.;
display_value( {ok, Value} ) -> Value + $0.

is_out_of_values( {_Position, []} ) -> true;
is_out_of_values( _ ) -> false.

group_positions( {X, Y} ) -> [{Colum, Row} || Colum <- group_positions_close(X), Row <- group_positions_close(Y)].

group_positions_close( N ) when N < 4 -> [1,2,3];
group_positions_close( N ) when N < 7 -> [4,5,6];
group_positions_close( _N ) -> [7,8,9].

potentials( Grid ) ->
	Keys = dict:fetch_keys( Grid ),
	Empties = [{X, Y} || X <- lists:seq(1, 9), Y <- lists:seq(1, 9), not lists:member({X, Y}, Keys)],
	[potentials(X, Grid)  || X <- Empties].

potentials( {X, Y}, Grid ) ->
	Row_positions = [{Row, Y} || Row <- lists:seq(1, 9), Row =/= X],
	Row_values = potentials_values( Row_positions, Grid ),
	Column_positions = [{X, Column} || Column <- lists:seq(1, 9), Column =/= Y],
	Column_values = potentials_values( Column_positions, Grid ),
	Group_positions = lists:delete( {X, Y}, group_positions({X, Y}) ),
	Group_values = potentials_values( Group_positions, Grid ),
	Useds = Row_values ++ Column_values ++ Group_values,
	{{X, Y}, lists:seq(1, 9) -- Useds}.

potentials_values( Keys, Grid ) ->
	Row_values_unfiltered = [dict:find(X, Grid) || X <- Keys],
	[Value || {ok, Value} <- Row_values_unfiltered].

solve_all_sure( Grid ) -> solve_all_sure( solve_all_sure_value(Grid), Grid ).

solve_all_sure( [], Grid ) -> Grid;
solve_all_sure( Sures, Grid ) -> solve_all_sure( lists:foldl(fun solve_all_sure_store/2, Grid, Sures) ).

solve_all_sure_value( Grid ) -> [{Position, Value} || {Position, [Value]} <- potentials(Grid)].

solve_all_sure_store( {Position, Value}, Acc ) -> dict:store( Position, Value, Acc ).

solve_unsure( [], Grid ) -> Grid;
solve_unsure( [{Position, Values} | T], Grid ) -> solve_unsure_potential( Position, Values, T, Grid ).

solve_unsure_potential( Position, [Value], [], Grid ) -> dict:store( Position, Value, Grid );
solve_unsure_potential( Position, [Value | T], Potentials, Grid ) ->
	New_potentials = [{Potential_position, lists:delete(Value, Values)} || {Potential_position, Values} <- Potentials],
	Is_out_of_values = lists:any( fun is_out_of_values/1, New_potentials ),
	solve_unsure_potential_out_of_values( Is_out_of_values, Position, Value, T, Potentials, New_potentials, Grid ).

solve_unsure_potential_out_of_values( true, Position, _Value, Values, Potentials, _New_potentials, Grid ) ->
	solve_unsure_potential( Position, Values, Potentials, Grid );
solve_unsure_potential_out_of_values( false, Position, Value, _Values, _Potentials, [{Next_position, Values} | T], Grid ) -> solve_unsure_potential( Next_position, Values, T, dict:store(Position, Value, Grid) ).

task( Knowns ) ->
	io:fwrite( "Start~n" ),
	Start = start( Knowns ),
	display( Start ),
	io:fwrite( "Solved~n" ),
	Solved = solve( Start ),
	display( Solved ),
	io:nl().

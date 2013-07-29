-module( luhn_test ).

-export( [credit_card/1, task/0] ).

-record( acc, {evens=[], odds=[], position=1} ).

credit_card( Number ) when is_integer(Number) ->
	Acc = lists:foldl( fun split_odds_evens/2, #acc{}, lists:reverse(erlang:integer_to_list(Number)) ),
	Odds_sum = lists:sum( Acc#acc.odds ),
	Evens_sum = evens_sum( Acc#acc.evens ),
	check( Odds_sum + Evens_sum ).

task() -> [io:fwrite("~p: ~p~n", [X, credit_card(X)]) || X <- [49927398716, 49927398717, 1234567812345678, 1234567812345670]].



check( Sum ) when (Sum rem 10) =:= 0 -> valid;
check( _Sum ) -> invalid.

evens_sum( Evens ) -> lists:sum( [evens_sum_single_digit(X * 2) || X <- Evens] ).

evens_sum_single_digit( Number ) when Number > 9 -> (Number div 10) + (Number rem 10);
evens_sum_single_digit(	Number ) -> Number.

split_odds_evens( Char, #acc{odds=Odds, position=P}=Acc ) when P rem 2 =:= 1 -> Acc#acc{odds=[erlang:list_to_integer([Char]) | Odds], position=P+1};
split_odds_evens( Char, #acc{evens=Evens, position=P}=Acc ) when P rem 2 =:= 0 -> Acc#acc{evens=[erlang:list_to_integer([Char]) | Evens], position=P+1}.


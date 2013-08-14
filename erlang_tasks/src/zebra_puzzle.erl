-module( zebra_puzzle ).

-export( [task/0] ).

-record( house, {colour, drink, nationality, number, pet, smoke} ).
-record( sorted_houses, {house_1s=[], house_2s=[], house_3s=[], house_4s=[], house_5s=[]} ).

task() ->
	Houses = [#house{colour=C, drink=D, nationality=N, number=Nr, pet=P, smoke=S} || C <- all_colours(), D <- all_drinks(), N <- all_nationalities(), Nr <- all_numbers(), P <- all_pets(), S <- all_smokes(), is_all_single_house_rules_ok(C, D, N, Nr, P, S)],
	Sorted_houses = lists:foldl( fun house_number_sort/2, #sorted_houses{}, Houses ),
	Streets = [[H1, H2, H3, H4, H5] || H1 <- Sorted_houses#sorted_houses.house_1s, H2 <- Sorted_houses#sorted_houses.house_2s, H3 <- Sorted_houses#sorted_houses.house_3s, H4 <- Sorted_houses#sorted_houses.house_4s, H5 <- Sorted_houses#sorted_houses.house_5s, is_all_multi_house_rules_ok(H1, H2, H3, H4, H5)],
	[Nationality] = [N || #house{nationality=N, pet=zebra} <- lists:flatten(Streets)],
	io:fwrite( "~p owns the zebra~n", [Nationality] ),
	io:fwrite( "All solutions ~p~n", [Streets] ),
	io:fwrite( "Number of solutions: ~p~n", [erlang:length(Streets)] ).



all_colours() -> [blue, green, red, white, yellow].

all_drinks() -> [beer, coffe, milk, tea, water].

all_nationalities() -> [danish, english, german, norveigan, swedish].

all_numbers() -> [1, 2, 3, 4, 5].

all_pets() -> [birds, cats, dog, horse, zebra].

all_smokes() -> [blend, 'blue master', dunhill, 'pall mall', prince].

house_number_sort( #house{number=1}=House, #sorted_houses{house_1s=Houses_1s}=Sorted_houses ) -> Sorted_houses#sorted_houses{house_1s=[House | Houses_1s]};
house_number_sort( #house{number=2}=House, #sorted_houses{house_2s=Houses_2s}=Sorted_houses ) -> Sorted_houses#sorted_houses{house_2s=[House | Houses_2s]};
house_number_sort( #house{number=3}=House, #sorted_houses{house_3s=Houses_3s}=Sorted_houses ) -> Sorted_houses#sorted_houses{house_3s=[House | Houses_3s]};
house_number_sort( #house{number=4}=House, #sorted_houses{house_4s=Houses_4s}=Sorted_houses ) -> Sorted_houses#sorted_houses{house_4s=[House | Houses_4s]};
house_number_sort( #house{number=5}=House, #sorted_houses{house_5s=Houses_5s}=Sorted_houses ) -> Sorted_houses#sorted_houses{house_5s=[House | Houses_5s]}.

is_all_different( [_H] ) -> true;
is_all_different( [H | T] ) -> not lists:member( H, T ) andalso is_all_different( T ).

is_all_multi_house_rules_ok( House1, House2, House3, House4, House5 ) ->
	is_rule_1_ok( House1, House2, House3, House4, House5 )
	andalso is_rule_5_ok( House1, House2, House3, House4, House5 )
	andalso is_rule_11_ok( House1, House2, House3, House4, House5 )
	andalso is_rule_12_ok( House1, House2, House3, House4, House5 )
	andalso is_rule_15_ok( House1, House2, House3, House4, House5 )
	andalso is_rule_16_ok( House1, House2, House3, House4, House5 ).

is_all_single_house_rules_ok( Colour, Drink, Nationality, Number, Pet, Smoke ) ->
	is_rule_2_ok( Nationality, Colour )
	andalso is_rule_3_ok( Nationality, Pet )
	andalso is_rule_4_ok( Nationality, Drink )
	andalso is_rule_6_ok( Drink, Colour )
	andalso is_rule_7_ok( Smoke, Pet )
	andalso is_rule_8_ok( Colour, Smoke )
	andalso is_rule_9_ok( Number, Drink )
	andalso is_rule_10_ok( Nationality, Number )
	andalso is_rule_13_ok( Smoke, Drink )
	andalso is_rule_14_ok( Nationality, Smoke ).

is_rule_1_ok( #house{number=1}=H1,  #house{number=2}=H2,  #house{number=3}=H3,  #house{number=4}=H4,  #house{number=5}=H5  ) ->
	is_all_different( [H1#house.colour, H2#house.colour, H3#house.colour, H4#house.colour, H5#house.colour] )
	andalso is_all_different( [H1#house.drink, H2#house.drink, H3#house.drink, H4#house.drink, H5#house.drink] )
	andalso is_all_different( [H1#house.nationality, H2#house.nationality, H3#house.nationality, H4#house.nationality, H5#house.nationality] )
	andalso is_all_different( [H1#house.pet, H2#house.pet, H3#house.pet, H4#house.pet, H5#house.pet] )
	andalso is_all_different( [H1#house.smoke, H2#house.smoke, H3#house.smoke, H4#house.smoke, H5#house.smoke] );
is_rule_1_ok( _House1,  _House2,  _House3,  _House4,  _House5  ) -> false.

is_rule_2_ok( english, red ) -> true;
is_rule_2_ok( english, _Colour ) -> false;
is_rule_2_ok( _Nationality, red ) -> false;
is_rule_2_ok( _Nationality, _Colour ) -> true.

is_rule_3_ok( swedish, dog ) -> true;
is_rule_3_ok( swedish, _Pet ) -> false;
is_rule_3_ok( _Nationality, dog ) -> false;
is_rule_3_ok( _Nationality, _Pet ) -> true.

is_rule_4_ok( danish, tea ) -> true;
is_rule_4_ok( danish, _Drink ) -> false;
is_rule_4_ok( _Nationality, tea ) -> false;
is_rule_4_ok( _Nationality, _Drink ) -> true.

is_rule_5_ok( #house{colour=green},  #house{colour=white},  _House3,  _House4,  _House5  ) -> true;
is_rule_5_ok( _House1,  #house{colour=green},  #house{colour=white},  _House4,  _House5  ) -> true;
is_rule_5_ok( _House1,  _House2,  #house{colour=green},  #house{colour=white},  _House5  ) -> true;
is_rule_5_ok( _House1,  _House2,  _House3,  #house{colour=green},  #house{colour=white}  ) -> true;
is_rule_5_ok( _House1,  _House2,  _House3,  _House4,  _House5  ) -> false.

is_rule_6_ok( coffe, green ) -> true;
is_rule_6_ok( coffe, _Colour ) -> false;
is_rule_6_ok( _Drink, green ) -> false;
is_rule_6_ok( _Nationality, _Drink ) -> true.

is_rule_7_ok( 'pall mall', birds ) -> true;
is_rule_7_ok( 'pall mall', _Pet ) -> false;
is_rule_7_ok( _Smoke, birds ) -> false;
is_rule_7_ok( _Smoke, _Pet ) -> true.

is_rule_8_ok( yellow, dunhill ) -> true;
is_rule_8_ok( yellow, _Smoke ) -> false;
is_rule_8_ok( _Colour, dunhill ) -> false;
is_rule_8_ok( _Colour, _Smoke ) -> true.

is_rule_9_ok( 3, milk ) -> true;
is_rule_9_ok( 3, _Drink ) -> false;
is_rule_9_ok( _Number, milk ) -> false;
is_rule_9_ok( _Number, _Drink ) -> true.

is_rule_10_ok( norveigan, 1 ) -> true;
is_rule_10_ok( norveigan, _Number ) -> false;
is_rule_10_ok( _Nationality, 1 ) -> false;
is_rule_10_ok( _Nationality, _Number ) -> true.

is_rule_11_ok( #house{smoke=blend},  #house{pet=cats},  _House3,  _House4,  _House5  ) -> true;
is_rule_11_ok( _House1,  #house{smoke=blend},  #house{pet=cats},  _House4,  _House5  ) -> true;
is_rule_11_ok( _House1,  _House2,  #house{smoke=blend},  #house{pet=cats},  _House5  ) -> true;
is_rule_11_ok( _House1,  _House2,  _House3,  #house{smoke=blend},  #house{pet=cats}  ) -> true;
is_rule_11_ok( #house{pet=cats},  #house{smoke=blend},  _House3,  _House4,  _House5  ) -> true;
is_rule_11_ok( _House1,  #house{pet=cats},  #house{smoke=blend},  _House4,  _House5  ) -> true;
is_rule_11_ok( _House1,  _House2,  #house{pet=cats},  #house{smoke=blend},  _House5  ) -> true;
is_rule_11_ok( _House1,  _House2,  _House3,  #house{pet=cats},  #house{smoke=blend}  ) -> true;
is_rule_11_ok( _House1,  _House2,  _House3,  _House4,  _House5  ) -> false.

is_rule_12_ok( #house{smoke=dunhill},  #house{pet=horse},  _House3,  _House4,  _House5  ) -> true;
is_rule_12_ok( _House1,  #house{smoke=dunhill},  #house{pet=horse},  _House4,  _House5  ) -> true;
is_rule_12_ok( _House1,  _House2,  #house{smoke=dunhill},  #house{pet=horse},  _House5  ) -> true;
is_rule_12_ok( _House1,  _House2,  _House3,  #house{smoke=dunhill},  #house{pet=horse}  ) -> true;
is_rule_12_ok( #house{pet=horse},  #house{smoke=dunhill},  _House3,  _House4,  _House5  ) -> true;
is_rule_12_ok( _House1,  #house{pet=horse},  #house{smoke=dunhill},  _House4,  _House5  ) -> true;
is_rule_12_ok( _House1,  _House2,  #house{pet=horse},  #house{smoke=dunhill},  _House5  ) -> true;
is_rule_12_ok( _House1,  _House2,  _House3,  #house{pet=horse},  #house{smoke=dunhill}  ) -> true;
is_rule_12_ok( _House1,  _House2,  _House3,  _House4,  _House5  ) -> false.

is_rule_13_ok( 'blue master', beer ) -> true;
is_rule_13_ok( 'blue master', _Drink ) -> false;
is_rule_13_ok( _Smoke, beer ) -> false;
is_rule_13_ok( _Smoke, _Drink ) -> true.

is_rule_14_ok( german, prince ) -> true;
is_rule_14_ok( german, _Smoke ) -> false;
is_rule_14_ok( _Nationality, beer ) -> false;
is_rule_14_ok( _Nationality, _Smoke ) -> true.

is_rule_15_ok( #house{nationality=norveigan},  #house{colour=blue},  _House3,  _House4,  _House5  ) -> true;
is_rule_15_ok( _House1,  #house{nationality=norveigan},  #house{colour=blue},  _House4,  _House5  ) -> true;
is_rule_15_ok( _House1,  _House2,  #house{nationality=norveigan},  #house{colour=blue},  _House5  ) -> true;
is_rule_15_ok( _House1,  _House2,  _House3,  #house{nationality=norveigan},  #house{colour=blue}  ) -> true;
is_rule_15_ok( #house{colour=blue},  #house{nationality=norveigan},  _House3,  _House4,  _House5  ) -> true;
is_rule_15_ok( _House1,  #house{colour=blue},  #house{nationality=norveigan},  _House4,  _House5  ) -> true;
is_rule_15_ok( _House1,  _House2,  #house{drink=water},  #house{nationality=norveigan},  _House5  ) -> true;
is_rule_15_ok( _House1,  _House2,  _House3,  #house{drink=water},  #house{nationality=norveigan}  ) -> true;
is_rule_15_ok( _House1,  _House2,  _House3,  _House4,  _House5  ) -> false.

is_rule_16_ok( #house{smoke=blend},  #house{drink=water},  _House3,  _House4,  _House5  ) -> true;
is_rule_16_ok( _House1,  #house{smoke=blend},  #house{drink=water},  _House4,  _House5  ) -> true;
is_rule_16_ok( _House1,  _House2,  #house{smoke=blend},  #house{drink=water},  _House5  ) -> true;
is_rule_16_ok( _House1,  _House2,  _House3,  #house{smoke=blend},  #house{drink=water}  ) -> true;
is_rule_16_ok( #house{drink=water},  #house{smoke=blend},  _House3,  _House4,  _House5  ) -> true;
is_rule_16_ok( _House1,  #house{drink=water},  #house{smoke=blend},  _House4,  _House5  ) -> true;
is_rule_16_ok( _House1,  _House2,  #house{drink=water},  #house{smoke=blend},  _House5  ) -> true;
is_rule_16_ok( _House1,  _House2,  _House3,  #house{drink=water},  #house{smoke=blend}  ) -> true;
is_rule_16_ok( _House1,  _House2,  _House3,  _House4,  _House5  ) -> false.


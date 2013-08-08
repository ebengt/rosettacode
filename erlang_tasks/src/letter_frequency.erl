%% Implemented by Arjun Sunel
-module(letter_frequency).
-export([main/0, letter_freq/1]).
main() ->
        case  file:read_file("src/letter_frequency.erl") of
                {ok, FileData} ->
                        letter_freq(binary_to_list(FileData));
                _FileNotExist ->
                        io:format("File do not exist~n")
        end.



letter_freq( Data ) ->
	Dict = lists:foldl( fun (Char, Dict) -> dict:update_counter( Char, 1, Dict ) end, dict:new(), Data ),
	[io:fwrite( "~p	:	~p~n", [[X], dict:fetch(X, Dict)]) || X <- dict:fetch_keys(Dict)].

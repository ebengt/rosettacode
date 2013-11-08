-module( update_configuration_file ).

-export( [read/1, task/0, write/2] ).

disable( Option, Lines ) -> [disable_option(Option, X) || X <- Lines].

read( Name ) ->
	{ok, Binary} = file:read_file( Name ),
	Lines = [binary:bin_to_list(X) || X <- binary:split( Binary, <<"\n">>, [global] )],
	Lines_no_white = [string:strip(X) || X <- Lines],
	Lines_no_control = [strip_control(X) || X <- Lines_no_white],
	Lines_no_consecutive_space = [string:join(string:tokens(X, " "), " ") || X <- Lines_no_control],
	Lines_no_consecutive_semicolon = [strip_semicolon(X) || X <- Lines_no_consecutive_space],
	lists:filter( fun strip_empty/1, Lines_no_consecutive_semicolon ).

task() ->
	Lines = read( "priv/configuration_file2" ),
	write( "configuration_file", Lines ).

write( Name, Lines ) -> file:write_file( Name, binary:list_to_bin(string:join(Lines, "\n")) ).



disable_option( Option, String ) -> String.

is_semicolon( $; ) -> true;
is_semicolon( _C ) -> false.

strip_control( "" ) -> "";
strip_control( ";" ++ _T=String ) -> lists:filter( fun strip_control_codes:is_not_control_code_nor_extended_character/1, String );
strip_control( String ) -> String.

strip_empty( ";" ) -> false;
strip_empty( String ) -> String.


strip_semicolon( ";" ++ _T=String ) -> ";" ++ lists:dropwhile( fun is_semicolon/1, String );
strip_semicolon( String ) -> String.


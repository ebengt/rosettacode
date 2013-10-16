-module( find_bare_lang_tags ).

-export( [in_files/1, task/0] ).

in_files( Files ) ->
	{Dict_file, Dict_count} = lists:foldl( fun count_empty_lang/2, {dict:new(), dict:new()}, Files ),
	[{dict:fetch(X, Dict_count), X, dict:fetch(X, Dict_file)} || X <- dict:fetch_keys(Dict_count)].

task() ->
	Files = filelib:wildcard( "priv/find_bare_lang_tags_?" ),
	Count_lang_files = in_files( Files ),
	io:fwrite( "~p bare language tags.~n", [lists:sum([X || {X, _Y, _Z} <- Count_lang_files])] ),
	[io:fwrite( "~p in ~p ~p~n", [X, Y, Z] ) || {X, Y, Z} <- Count_lang_files].



count_empty_lang( Name, {Dict_file, Dict_count} ) ->
	{ok, Binary} = file:read_file( Name ),
	Lines = string:tokens( erlang:binary_to_list(Binary), "\n" ),
	{_Name, _Lang, New_dict_file, New_dict_count} = lists:foldl( fun count_empty_lang/2, {Name, "no language", Dict_file, Dict_count}, Lines ),
	{New_dict_file, New_dict_count};

count_empty_lang( Line, {File, Lang, Dict_file, Dict_count} ) ->
	New_lang = new_lang( string:str( Line,"=={{header|" ), Line, Lang ),
	Empty_tag = string:str( Line, "<lang>" ),
	{New_dict_file, New_dict_count} = dict_update( Empty_tag, File, Lang, Dict_file, Dict_count ),
	{File, New_lang, New_dict_file, New_dict_count}.

dict_update( 0, _File, _Lang, Dict_file, Dict_count ) -> {Dict_file, Dict_count};
dict_update( _Start, File, Lang, Dict_file, Dict_count ) -> {dict:append( Lang, File, Dict_file ), dict:update_counter( Lang, 1, Dict_count )}.

new_lang( 0, _Line, Lang ) -> Lang;
new_lang( _Start, Line, _Lang ) ->
	Start = string:str( Line, "|" ),
	Stop = string:rstr( Line, "}}==" ),
	string:sub_string( Line, Start+1, Stop-1 ).

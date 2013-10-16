-module( find_unimplemented_tasks ).
-include_lib( "xmerl/include/xmerl.hrl" ).
 
-export( [init/0, per_language/1, rosetta_code_list_of/1, rosetta_code_xmls_from_category/1] ).

init() ->
	application:start( inets ),
	init_ericsson_proxy( is_ericsson() ).

per_language( Language ) ->
	ok = init(),
	Tasks = rosetta_code_list_of( "Programming_Tasks" ),
	Uninplemented = Tasks -- rosetta_code_list_of( Language ),
	io:fwrite( "Unimplemented total: ~p~n", [erlang:length(Uninplemented)] ),
	[io:fwrite("~p~n", [X]) || X <- Uninplemented].

rosetta_code_list_of( Category ) ->
	lists:foldl( fun (XML, Acc) -> Acc ++ xml_selection( "title", XML ) end, [], rosetta_code_xmls_from_category(Category) ).

rosetta_code_xmls_from_category( Category ) ->
	URL = "http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmlimit=500&format=xml&cmtitle=Category:" ++ Category,
	xmls( URL, "", [] ).



init_ericsson_proxy( true ) -> httpc:set_options( [{proxy, {{"www-proxy.ericsson.se", 8080}, []}}] );
init_ericsson_proxy( false ) -> ok.

is_ericsson() ->
	Environments_variables = [H || [H|_] <- [string:tokens(X, "=") || X <- os:getenv()]],
	lists:member( "ARC_RELEASE", Environments_variables ) andalso lists:member( "CCHOME", Environments_variables ).

xmls( URL, Continue, Acc ) ->
	{ok, {{_HTTP,200,"OK"}, _Headers, Body}} = httpc:request( URL ++ Continue ),
	{XML, _} = xmerl_scan:string( Body ),
	New_continue = xmls_url_continue( xml_selection("cmcontinue", XML) ),
	xmls_continue( URL, New_continue, [XML | Acc]  ).

xmls_continue( _URL, "", Acc ) -> lists:reverse( Acc );
xmls_continue( URL, Continue, Acc ) -> xmls( URL, Continue, Acc ).

xmls_url_continue( [] ) -> "";
xmls_url_continue( [Continue | _] ) -> "&cmcontinue=" ++ Continue.

xml_selection( Selection, XML ) -> [lists:map( fun xml_selection_space/1, X) || #xmlAttribute{value=X} <- xmerl_xpath:string("//@" ++ Selection, XML)].

xml_selection_space( 924 ) -> $\s;
xml_selection_space( 1050 ) -> $\s;
xml_selection_space( 1052 ) -> $\s;
xml_selection_space( 8211 ) -> $-;
xml_selection_space( Character ) -> Character.


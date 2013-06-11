-module( find_unimplemented_tasks ).
-include_lib( "xmerl/include/xmerl.hrl" ).
 
-export( [init/0, per_language/1, rosetta_code_list_of/1] ).

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
	URL = "http://rosettacode.org/mw/api.php?action=query&list=categorymembers&cmlimit=500&format=xml&cmtitle=Category:"
	++ Category,
	title_contents( URL, "", [] ).


init_ericsson_proxy( true ) -> httpc:set_options( [{proxy, {{"www-proxy.ericsson.se", 8080}, []}}] );
init_ericsson_proxy( false ) -> ok.

is_ericsson() ->
	Environments_variables = [H || [H|_] <- [string:tokens(X, "=") || X <- os:getenv()]],
	lists:member( "ARC_RELEASE", Environments_variables ) andalso lists:member( "CCHOME", Environments_variables ).

title_contents( URL, Continue, Acc ) ->
	{ok, {{_HTTP,200,"OK"}, _Headers, Body}} = httpc:request( URL ++ Continue ),
	{XML, _} = xmerl_scan:string( Body ),
	News = xml_selection( "title", XML ),
	New_continue = title_contents_url_continue( xml_selection("cmcontinue", XML) ),
	title_contents_continue( URL, New_continue, Acc ++ News ).

title_contents_continue( _URL, "", Acc ) -> Acc;
title_contents_continue( URL, Continue, Acc ) -> title_contents( URL, Continue, Acc ).

title_contents_url_continue( [] ) -> "";
title_contents_url_continue( [Continue | _] ) -> "&cmcontinue=" ++ Continue.

xml_selection( Selection, XML ) ->
	[lists:map( fun xml_8211/1, X) || #xmlAttribute{value=X} <- xmerl_xpath:string("//@" ++ Selection, XML)].

xml_8211( 8211 ) -> $-;
xml_8211( 924 ) -> $\s;
xml_8211( 1050 ) -> $\s;
xml_8211( 1052 ) -> $\s;
xml_8211( Character ) -> Character.


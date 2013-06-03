-module( case_sensitivity_of_identifiers ).

-export( [task/0] ).

task() ->
	catch dog = "Benjamin", % Will badmatch without catch
	Dog = "Samba",
	DOG = "Bernie",
	io:fwrite( "The three dogs are named ~s, ~s and ~s~n", [dog, Dog, DOG] ).

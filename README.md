rosettacode
===========

Files used for http://rosettacode.org

To build Erlang modules you first need an Erlang system (see http://www.erlang.org/download.html if one is not present on your machine).
Then run "./rebar compile" in erlang_tasks/ . This will download etop and build it, followed by building the Rosettacode modules.
Finally, start the Erlang shell from erlang_tasks/ with "erl -pa ebin -pa deps/etop/ebin". Now you can run the Erlang Rosetta code tasks I have written.
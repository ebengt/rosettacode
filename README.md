rosettacode
===========

Files here are my contribution to http://rosettacode.org,
or ones that where already present there and that I use after small changes.
Content is available under GNU Free Documentation License 1.2.

To build Erlang modules you first need an Erlang system (see http://www.erlang.org/download.html if one is not present on your machine).
Then run "./rebar compile" in erlang_tasks/ . This will download etop and build it, followed by building the Rosettacode modules.
Finally, start the Erlang shell from erlang_tasks/ with "erl -pa ebin -pa deps/etop/ebin". Now you can run the Erlang Rosetta code tasks I have written.

Content is available under GNU Free Documentation License 1.2.

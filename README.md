Easy-Mid
======

Easy-Mid is a small, easy to use SSL Socket Server

Goals
-----

Easy-Mid aims to provide following advantages:

* **Easy** to use.
* Almost **fast**.
* **Hot Code Change**: protocol handlers are replaceable without shutdown your server.

The server is currently in early development. Comments and suggestions are
more than welcome. To contribute, either open bug reports, or fork the project
and send us pull requests with new or improved functionality. You should
discuss your plans with us before doing any serious work, though, to avoid
duplicating efforts.

Quick start
-----------

* make
* cd ebin
* erl -boot start_sasl

Then in the erlang shell, start the application

* application:start(ss_server).



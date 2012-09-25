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
``` erlang
make
cd ebin
erl -boot start_sasl
```
Then in the erlang shell, start the application

``` erlang
application:start(ss_server).
```


### Choose the custom listen port(default is 2222) ###

#### custom parameter in command line ####

``` erlang
erl -boot start_sasl -listen_port 12345
```

#### change the default parameter in app resource file ####

Edit the ss_server.app file in ebin directory.Find out the line

{env, []}

change it to

{env, [{listen_port,12345}]}

then the default listen_port is changed to 12345

#### python process_node example ####

###start python process node###

python process_node.py -n ss1@127.0.0.1 -c 123456

this python process node is just echo the data it received

###start easy-mid process node###

/usr/local/bin/erl -boot start_sasl -listen_port 8243  -env ERL_NO_VFORK 1 -name ss@127.0.0.1 -setcookie 123456








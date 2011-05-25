.SUFFIXES: .erl .beam

vpath %.erl src

ERL_OBJ = $(patsubst src/%.erl,ebin/%.beam, $(wildcard src/*erl))

all: main

main: ${ERL_OBJ}

ebin/%.beam: %.erl 
	erlc +debug_info -W -o ebin $<

clean: 
	rm -rf ebin/*.beam ebin/*.dump


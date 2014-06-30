#!/bin/sh
erl -pa ebin deps/*/ebin -s erlife \
	-eval "io:format(\"Open http://localhost:8085 in your browser~n\")."
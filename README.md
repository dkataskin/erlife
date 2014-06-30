erlife
======

Erlife is an implementation of Conway's Game of Life. Implemented as a client-server app. Server is http server on WebSockets written in Erlang (cowboy, bullet) and client is HTML5\javascript (canvas). Although some optimizations were applied this is NOT a HashLife implementation. 

Runs on R16B03-1, works in Chrome. Wasn't tested with other runtimes and browsers.

Navigate to sources, type the following command:
```
$ ./rebar get-deps compile
```
You can then start the app with the following commands:
```
$ erl -pa ebin deps/*/ebin

Erlang R16B03-1 (erts-5.10.4) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V5.10.4  (abort with ^G)
1> erlife:start().
```
or

```
$ ./start.sh
```

Then point your browser to http://localhost:8085.


css styles and layout were taken from http://pmav.eu/stuff/javascript-game-of-life-v3.1.1/

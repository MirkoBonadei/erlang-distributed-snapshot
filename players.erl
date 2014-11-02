-module(players).
-export([output/0]).

output() -> io:fwrite("hello, world\n").

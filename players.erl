-module(players).
-export([setup/0, output/0]).

output() -> receive
                M -> io:fwrite(M)
            end.

setup() -> spawn(players, output, []).

-module(players).
-export([setup/0, output/0, decrementing_player/1]).

output() -> receive
                M -> io:fwrite(M)
            end.

decrementing_player(Targets) -> receive 
    {target, T} -> decrementing_player(lists:append(Targets, [T]))
end.

setup() ->
    O = spawn(players, output, []),
    register(output, O),
    P1 = spawn(players, decrementing_player, [[O]]),
    register(entry, P1),
    P2 = spawn(players, decrementing_player, [[O]]),
    P3 = spawn(players, decrementing_player, [[O]]),
    P1 ! {target, P2},
    P2 ! {target, P3},
    P3 ! {target, P1}
    .

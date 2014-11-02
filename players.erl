-module(players).
-export([setup/0, snapshot/0, output/0, decrementing_player/1]).

output() -> receive
                M -> io:fwrite("~w~n", [M]),
                output()
            end.

decrementing_player(Targets) -> decrementing_player(Targets, {working}). 
decrementing_player(Targets, State) -> receive 
    {target, Target} -> decrementing_player(lists:append(Targets, [Target]), State);
    {token, Value} ->
        Decremented = Value - 1,
        if
            Decremented > 0 ->
                lists:map(fun(Target) -> Target ! {token, Decremented} end, Targets);
            true -> true
        end,
        decrementing_player(Targets, State);
    {take_snapshot} -> 
        lists:map(fun(Target) -> Target ! {marker} end, Targets),
        decrementing_player(Targets, {taking_snapshot, []});
    {marker} -> 
        if 
            element(1, State) == took_snapshot ->
                decrementing_player(Targets, {working});
            element(1, State) == taking_snapshot ->
                lists:map(fun(Target) -> Target ! {marker} end, Targets),
                decrementing_player(Targets, {took_snapshot, []});
            true ->
                lists:map(fun(Target) -> Target ! {marker} end, Targets),
                decrementing_player(Targets, {taking_snapshot, []})
        end
end.

setup() ->
    O = spawn(players, output, []),
    register(output_pid, O),
    P1 = spawn(players, decrementing_player, [[O]]),
    register(entry_pid, P1),
    P2 = spawn(players, decrementing_player, [[O]]),
    P3 = spawn(players, decrementing_player, [[O]]),
    P1 ! {target, P2},
    P2 ! {target, P3},
    P3 ! {target, P1}
    .

snapshot() ->
    whereis(entry_pid) ! {take_snapshot}.

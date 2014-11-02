-module(players).
-export([setup/0, snapshot/0, output/0, decrementing_player/1, start/1]).

output() -> receive
                M -> io:fwrite("~w~n", [M]),
                output()
            end.

decrementing_player(Targets) -> decrementing_player(Targets, undefined, {working}). 
decrementing_player(Targets, LastSeen, State) -> receive 
    {target, Target} -> decrementing_player(
                          lists:append(Targets, [Target]),
                          LastSeen,
                          State
                        );
    {token, Value} ->
        Decremented = Value - 1,
        if
            Decremented > 0 ->
                lists:map(fun(Target) -> Target ! {token, Decremented} end, Targets);
            true -> true
        end,
        if 
            element(1, State) == taking_snapshot ->
                decrementing_player(
                  Targets, 
                  Value,
                  {
                   taking_snapshot,
                   lists:append(element(2, State), [{token, Value}])
                  }
                 );
            true ->
                decrementing_player(Targets, Value, State)
        end;
    {take_snapshot} -> 
        lists:map(fun(Target) -> Target ! {marker} end, Targets),
        decrementing_player(
          Targets,
          LastSeen,
          {taking_snapshot, []}
        );
    {marker} -> 
        if 
            element(1, State) == took_snapshot ->
                decrementing_player(
                  Targets,
                  LastSeen,
                  {working}
                );
            element(1, State) == taking_snapshot ->
                lists:map(fun(Target) -> Target ! {marker} end, Targets),
                Snapshot = {snapshot, element(2, State)},
                [OutputProcess, _] = Targets,
                OutputProcess ! Snapshot,
                decrementing_player(
                  Targets,
                  LastSeen,
                  {took_snapshot}
                );
            element(1, State) == working ->
                lists:map(fun(Target) -> Target ! {marker} end, Targets),
                decrementing_player(
                  Targets,
                  LastSeen,
                  {taking_snapshot, [LastSeen]}
                )
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
start([NAsString]) ->
    {N, ""} = string:to_integer(NAsString),
    io:fwrite("~w~n", [N]),
    whereis(entry_pid) ! {token, N}.

snapshot() ->
    whereis(entry_pid) ! {take_snapshot}.

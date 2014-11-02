-module(players).
-export([setup/0, snapshot/0, decrementing_player/0, decrementing_player/1, start/1, funnel/1]).

decrementing_player() -> decrementing_player([]). 
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
    {marker, Funnel} -> 
        if 
            element(1, State) == took_snapshot ->
                decrementing_player(
                  Targets,
                  LastSeen,
                  {working}
                );
            element(1, State) == taking_snapshot ->
                lists:map(fun(Target) -> Target ! {marker, Funnel} end, Targets),
                Snapshot = {snapshot, element(2, State)},
                Funnel ! Snapshot,
                decrementing_player(
                  Targets,
                  LastSeen,
                  {took_snapshot}
                );
            element(1, State) == working ->
                lists:map(fun(Target) -> Target ! {marker, Funnel} end, Targets),
                decrementing_player(
                  Targets,
                  LastSeen,
                  {taking_snapshot, [LastSeen]}
                )
        end
end.

setup() ->
    P1 = spawn(players, decrementing_player, []),
    register(entry_pid, P1),
    P2 = spawn(players, decrementing_player, []),
    P3 = spawn(players, decrementing_player, []),
    P1 ! {target, P2},
    P2 ! {target, P3},
    P3 ! {target, P1}
    .
start([NAsString]) ->
    {N, ""} = string:to_integer(NAsString),
    io:fwrite("~w~n", [N]),
    whereis(entry_pid) ! {token, N}.

snapshot() ->
    Funnel = spawn(players, funnel, [3]),
    whereis(entry_pid) ! {marker, Funnel}.

funnel(NumberOfProcesses) ->
    receive 
        Message -> io:fwrite("Snapshot[~w]: ~w~n", [self(), Message])
    end,
    funnel(NumberOfProcesses - 1).



    

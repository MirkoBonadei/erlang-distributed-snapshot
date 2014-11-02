-module(players).
-export([setup/0, snapshot/0, decrementing_player/0, decrementing_player/1, start/1, funnel/1]).

decrementing_player() -> decrementing_player([]). 
decrementing_player(Targets) -> decrementing_player(Targets, 100, {working}). 
decrementing_player(Targets, LastSeen, State) -> 
    io:fwrite("State of ~w: ~w~n", [self(), State]),
    receive 
    {target, Target} -> decrementing_player(
                          lists:append(Targets, [Target]),
                          LastSeen,
                          State
                        );
    {token, Value, Sender} ->
        Decremented = Value - 1,
        if
            Decremented > 0 ->
                [Next|_] = Targets,
                Next ! {token, Decremented, self()};
            true -> true
        end,
        if 
            element(1, State) == taking_snapshot ->
                decrementing_player(
                  Targets, 
                  Value,
                  {
                   taking_snapshot,
                   lists:append(element(2, State), [{token, Value, Sender}]),
                   element(3, State)
                  }
                 );
            true ->
                decrementing_player(Targets, Value, State)
        end;
    {marker, Funnel, Sender} -> 
        if 
            element(1, State) == taking_snapshot ->
                Snapshot = element(2, State),
                Remaining = lists:filter(
                    fun(Target) -> Target /= Sender end,
                    element(3, State)
                ),
                io:fwrite("Remaining: ~w~n", [Remaining]),
                if 
                    length(Remaining) == 0 -> 
                        Funnel ! {snapshot_terminated};
                    true -> true
                end,
                decrementing_player(
                  Targets,
                  LastSeen,
                  {taking_snapshot, Snapshot, Remaining}
                );
    
            element(1, State) == working ->
                lists:map(fun(Target) -> Target ! {marker, Funnel, self()} end, Targets),
                Remaining = Targets,
                decrementing_player(
                  Targets,
                  LastSeen,
                  {taking_snapshot, [LastSeen], Remaining}
                )
        end;
    {tell_snapshot, Funnel} ->
        % TODO: check State is taking_snapshot
        Snapshot = element(2, State),
        Funnel ! {snapshot, Snapshot},
        decrementing_player(
          Targets,
          LastSeen,
          {working, [LastSeen]}
        )
end.

setup() ->
    P1 = spawn(players, decrementing_player, []),
    register(entry_pid1, P1),
    P2 = spawn(players, decrementing_player, []),
    register(entry_pid2, P2),
    P3 = spawn(players, decrementing_player, []),
    register(entry_pid3, P3),
    P1 ! {target, P2},
    P1 ! {target, P3},
    P2 ! {target, P3},
    P2 ! {target, P1},
    P3 ! {target, P1},
    P3 ! {target, P2}.

start([NAsString]) ->
    {N, ""} = string:to_integer(NAsString),
    io:fwrite("Starting with: ~w~n", [N]),
    whereis(entry_pid1) ! {token, N, self()}.

snapshot() ->
    Funnel = spawn(players, funnel, [[whereis(entry_pid1), whereis(entry_pid2), whereis(entry_pid3)]]),
    whereis(entry_pid1) ! {marker, Funnel, start}.

funnel(Processes) ->
    receive 
            {snapshot_terminated} -> 
                lists:map(fun(P) -> P ! {tell_snapshot, self()} end, Processes),
                collect(Processes)
    end.

collect(Processes) ->  collect(Processes, []).
collect(Processes, CollectedSnapshots) -> 
    if 
        length(CollectedSnapshots) < length(Processes) ->
            receive 
                {snapshot, Snapshot} -> 
                    io:fwrite("~w~n", [Snapshot]),
                    collect(Processes, lists:append(CollectedSnapshots, [Snapshot]))
            end;
        true ->
            Termination = termination(CollectedSnapshots),
            if 
                Termination -> true;
                true -> snapshot()
            end
    end.

termination(CollectedSnapshots) -> 
    length(
      lists:filter(fun(Snapshot) -> [State|_Messages] = Snapshot, State == 1 end, CollectedSnapshots)
     ) > 0.

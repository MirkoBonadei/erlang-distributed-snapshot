Implementation of Chandy-Lamport algorithm for the detection of stable properties in a distributed system.
Works by computing a distributed snapshot whose:
- detected global state is reachable by the starting global state.
- detected global state can lead to the ending global state.
So if the stable global property is true at the computed S*, it must also be true at the ending global state present in the system when the algorithm has terminated.
If the stable property is false at the computed S*, we can only say it was false at the starting global state.

== Specific example
- Three processes pass around a token in a ring, and each one of them decrements it before sending it to the others
- A funnel process is notified of the snapshot termination, and it asks the 3 processes their snapshot result (state + incoming messages).
- The funnel process may detect termination and stop if one of the processes has as state "1", since it means the token has been decremented enough times. If instead this is not yet true, it initiates another snapshot round.

== Limitations
# FIFO message delivery is only guaranteed on a single machine by Erlang (currently, may be worse in the future).

== References 
[http://en.wikipedia.org/wiki/Snapshot_algorithm]

Implementation of Chandy-Lamport algorithm for the detection of stable properties in a distributed system.
Works by computing a distributed snapshot whose:
- detected global state is reachable by the starting global state.
- detected global state can lead to the ending global state.
So if the stable global property is true at the computed S*, it must also be true at the ending global state present in the system when the algorithm has terminated.
If the stable property is false at the computed S*, we can only say it was false at the starting global state.

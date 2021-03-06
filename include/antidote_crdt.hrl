-type crdt() :: term().
-type update() :: {atom(), term()}.
-type effect() :: term().
-type value() ::  term().
-type reason() :: term().

-type pncounter() :: integer().
-type pncounter_update() :: {increment, integer()} |
                            {decrement, integer()}.
-type pncounter_effect() :: integer().
-type pncounter_value() :: integer().


-export_type([ crdt/0,
               update/0,
               effect/0,
               value/0,
               pncounter/0,
               pncounter_update/0,
               pncounter_effect/0,
               pncounter_value/0
             ]).

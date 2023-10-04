type state
(**[state] is the current state of the game*)

val play : state -> unit
(**[play] is the game loop.*)

val update : state -> state
(**[update] is the update game state from the old game state*)

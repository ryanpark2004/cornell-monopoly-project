open Board
open Player
(**Module type Utility specifies various utilities in Monopoly*)
type chances
(** [chances] is a variant representing difference cards*)

val rollDice : unit -> int
(**[rollDice] is a random integer from 0(inclusive) to bound(exclusive)*)

val pullChance : unit -> chances
(**[pullChance] is a randomly selected chance variant.*)

val pullChest : unit -> chances
(**[pullChance] is a randomly selected community chest variant.*)

val tile_action : tile -> player -> player list -> int -> player list
(**[tile_action] defines the different effects each tile has when 
    a player lands on them. Returns the new player after everything has 
    changed*)


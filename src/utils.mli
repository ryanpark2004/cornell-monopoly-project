(**Module type [Utility] specifies various utilities in Monopoly*)
module type Utility = sig
  type chances
  (** [chances] is a variant representing difference cards*)

  val rollDice : unit -> int
  (**[rollDice] is a random integer from 0(inclusive) to bound(exclusive)*)

  val pullChance : unit -> chances
  (**[pullChance] is a randomly selected chance variant*)
end

module MyUtil : Utility

(**Module type [Utility] specifies various utilities in Monopoly*)
module type Utility = sig
  (** [chances] is a variant representing difference cards*)
  type chances = ToStart | ToJail | Money | LoseMoney

  val rollDice : int -> int
  (**[rollDice] is a random integer from 0(inclusive) to bound(exclusive)*)

  val pullChance : int -> chances
  (**[pullChance] is a randomly selected chance variant. 
      Requires that bound is 1 + number of chances*)
end

module MyUtil : Utility

module type Utility = sig
  type chances = ToStart | ToJail | Money | LoseMoney

  val rollDice : int -> int
  val pullChance : int -> chances
end

module MyUtil : Utility = struct
  type chances = ToStart | ToJail | Money | LoseMoney

  let rollDice (bound : int) : int = Random.int bound

  let pullChance (bound : int) : chances =
    let n = Random.int bound in
    match n with
    | 0 -> ToStart
    | 1 -> ToJail
    | 2 -> Money
    | 3 -> LoseMoney
    | _ -> failwith "Unreachable"
end

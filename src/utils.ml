type chances = ToStart | ToJail | GainMoney | LoseMoney

let chance_list = (4, [ ToStart; ToJail; GainMoney; LoseMoney ])
let dice_bound = 6
let rollDice () : int = Random.int dice_bound

let pullChance () =
  let length, lst = chance_list in
  let n = Random.int length in
  List.nth lst n

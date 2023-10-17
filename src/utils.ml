open Player
open Board

type chances = ToStart | ToJail | GainMoney of int | LoseMoney of int

let chance_list =
  ( 6,
    [ ToStart; ToJail; GainMoney 40; LoseMoney 20; GainMoney 100; LoseMoney 50 ]
  )

let dice_bound = 6
let rollDice () : int = Random.int dice_bound

let pullChance () =
  let length, lst = chance_list in
  let n = Random.int length in
  List.nth lst n

let tile_action tile player =
  match tile with
  | Start ->
      print_endline "Passed Go: Collect $200";
      { player with money = player.money + 200 }
  | Tax x ->
      print_endline ("Oh No! You were taxed $" ^ string_of_int x);
      { player with money = player.money - x }
  | Chance -> (
      print_endline "You pulled a chance card!";
      let card = pullChance () in
      match card with
      | ToStart ->
          print_endline "CHANCE: Advance to Go, Collect $200";
          { player with position = 0; money = player.money + 200 }
      | ToJail ->
          print_endline "CHANCE: Go to Jail. Do not collect $200.";
          { player with in_jail = true }
      | GainMoney x ->
          print_endline ("CHANCE: You are lucky! Collect $" ^ string_of_int x);
          { player with money = player.money + x }
      | LoseMoney x ->
          print_endline ("CHANCE: Unlucky! Pay $" ^ string_of_int x);
          { player with money = player.money - x })
  | Parking ->
      print_endline "Free Parking";
      player
  | Jail ->
      print_endline "Go to Jail. Do not collect $200.";
      { player with in_jail = true }
  | _ -> player

open Player
open Board

type chances =
  | ToStart
  | ToJail
  | GainMoney of int
  | LoseMoney of int

let chance_list =
  ( 6,
    [ ToStart; ToJail; GainMoney 40; LoseMoney 20; GainMoney 100; LoseMoney 50 ]
  )

let chest_list =
  (5, [ GainMoney 30; LoseMoney 100; GainMoney 150; LoseMoney 25; GainMoney 50 ])

let dice_bound = 6
let rollDice () : int = 1 + Random.int dice_bound

let pullChance () =
  let length, lst = chance_list in
  let n = Random.int length in
  List.nth lst n

let pullChest () =
  let length, lst = chest_list in
  let n = Random.int length in
  List.nth lst n

let tile_action tile player =
  match tile with
  | Start ->
      print_endline "You landed back on Go";
      player
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
          print_endline "CHANCE: Go directly to Jail. Do not collect $200.";
          { player with in_jail = 3; position = 4 }
      | GainMoney x ->
          print_endline ("CHANCE: You are lucky! Collect $" ^ string_of_int x);
          { player with money = player.money + x }
      | LoseMoney x ->
          print_endline ("CHANCE: Unlucky! Pay $" ^ string_of_int x);
          { player with money = player.money - x })
  | Chest -> (
      print_endline "You pulled a Community Chest card!";
      let card = pullChest () in
      match card with
      | GainMoney x ->
          print_endline
            ("COMMUNITY CHEST: You are lucky! Collect $" ^ string_of_int x);
          { player with money = player.money + x }
      | LoseMoney x ->
          print_endline ("COMMUNITY CHEST: Unlucky! Pay $" ^ string_of_int x);
          { player with money = player.money - x }
      | _ -> player)
  | Parking ->
      print_endline "Free Parking";
      player
  | Jail ->
      print_endline "Go to Jail.";
      { player with in_jail = 3 }
  | _ -> player

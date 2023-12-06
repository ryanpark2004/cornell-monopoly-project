open Player
open Board
open Exceptions

type chances =
  | ToStart
  | ToJail
  | GainMoney of int
  | LoseMoney of int

module HashedProperty : Hashtbl.HashedType with type t = property = struct
  type t = property

  let equal p1 p2 = String.equal (property_to_string p1) (property_to_string p2)
  let hash p1 = String.hash (property_to_string p1)
end

module PropertyManager = Hashtbl.Make (HashedProperty)

let properties = PropertyManager.create (length new_board)

let owner (prop : property) : player option =
  PropertyManager.find_opt properties prop

let claim_property loc buyer =
  Printf.printf "You landed on %s. Buy:[B] with %i | Skip:[Enter]"
    (property_to_string loc) (calculated_rent loc);
  match read_line () with
  | "B" ->
      PropertyManager.add properties loc buyer;
      (*Warning: PropertyManager captures this specific version of buyer. 
       * This needs to be fixed*)
      [ buy_property loc buyer ]
  | _ -> [ buyer ]

let rec purchase (loc : property) (buyer : player) (seller : player) =
  let rent = calculated_rent loc in
  Printf.printf "You landed on %s. Currently owned by %s."
    (property_to_string loc) seller.name;
  Printf.printf "Buy: [B] with %i | Skip: [Enter]" rent;
  match read_line () with
  | "B" ->
      [ buy_property loc buyer; { seller with money = seller.money + rent } ]
  | _ -> [ buyer ]

let chance_list =
  ( 6,
    [ ToStart; ToJail; GainMoney 40; LoseMoney 20; GainMoney 100; LoseMoney 50 ]
  )

let chest_list =
  (5, [ GainMoney 30; LoseMoney 100; GainMoney 150; LoseMoney 25; GainMoney 50 ])

let dice_bound = 1
let rollDice () : int = 1 + Random.int dice_bound

let pullChance () =
  let length, lst = chance_list in
  let n = Random.int length in
  List.nth lst n

let pullChest () =
  let length, lst = chest_list in
  let n = Random.int length in
  List.nth lst n

let tile_action tile player : player list =
  if player.in_jail > 0 then [ player ]
  else
    match tile with
    | Start ->
        print_endline "You landed back on Go";
        [ player ]
    | Tax x ->
        print_endline ("Oh No! You were taxed $" ^ string_of_int x);
        [ { player with money = player.money - x } ]
    | Chance -> (
        print_endline "You pulled a chance card!";
        let card = pullChance () in
        match card with
        | ToStart ->
            print_endline "CHANCE: Advance to Go, Collect $200";
            [ { player with position = 0; money = player.money + 200 } ]
        | ToJail ->
            print_endline "CHANCE: Go directly to Jail. Do not collect $200.";
            [ { player with in_jail = 3; position = pos_of_tile Jail } ]
        | GainMoney x ->
            print_endline ("CHANCE: You are lucky! Collect $" ^ string_of_int x);
            [ { player with money = player.money + x } ]
        | LoseMoney x ->
            print_endline ("CHANCE: Unlucky! Pay $" ^ string_of_int x);
            [ { player with money = player.money - x } ])
    | Chest -> (
        print_endline "You pulled a Community Chest card!";
        let card = pullChest () in
        match card with
        | GainMoney x ->
            print_endline
              ("COMMUNITY CHEST: You are lucky! Collect $" ^ string_of_int x);
            [ { player with money = player.money + x } ]
        | LoseMoney x ->
            print_endline ("COMMUNITY CHEST: Unlucky! Pay $" ^ string_of_int x);
            [ { player with money = player.money - x } ]
        | _ -> [ player ])
    | Parking ->
        print_endline "Free Parking";
        [ player ]
    | Jail ->
        print_endline "Go to Jail.";
        [ { player with in_jail = 3 } ]
    | Property loc -> (
        match owner loc with
        | Some seller -> purchase loc player seller
        | None -> claim_property loc player)
    | _ -> raise No_Such_Tile

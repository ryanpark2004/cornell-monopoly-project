open Player
open Board
open Exceptions

type chances =
  | ToStart
  | ToJail
  | GainMoney of int
  | LoseMoney of int
(*
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
     | _ -> [ buyer ] *)

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

let rec tile_action tile player plist : player list =
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
    | Property prop -> property_action prop player plist
    | _ -> raise No_Such_Tile

and owner_opt (prop : property) (plist : player list) : player option =
  match plist with
  | [] -> None
  | h :: t -> if List.mem prop h.properties then Some h else owner_opt prop t

and pay_rent prop buyer (seller : player) plist =
  let rent = calculated_rent prop plist in
  let new_buyer = { buyer with money = buyer.money - rent } in
  let new_seller = { seller with money = seller.money + rent } in
  if buyer = seller then print_endline "You landed on your own property!"
  else
    Printf.printf "You landed on %s owned by %s. You paid %i"
      (property_to_string prop) seller.name rent;
  [ new_buyer; new_seller ]

and property_action (prop : property) (player : player) (plist : player list) :
    player list =
  match owner_opt prop plist with
  | None -> ask_buy prop player plist
  | Some s -> pay_rent prop player s plist

and ask_buy (prop : property) player plist =
  Printf.printf "You landed on %s. Buy:[B]  | Skip: [Enter]"
    (property_to_string prop);
  match read_line () with
  | "B" | "b" ->
      [
        {
          player with
          properties = prop :: player.properties;
          money = player.money - calculated_rent prop plist;
        };
      ]
  | _ -> [ player ]

and calculated_rent (prop : property) (plist : player list) : int =
  match prop with
  | Location x -> x.price
  | Tcat_station t -> tcat_rent t plist
  | Utility u -> utility_rent u plist

and tcat_rent tcat (plist : player list) : int = failwith "todo"
and utility_rent util plist = failwith "todo"

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
let rollDice () : int = 1 + Random.int dice_bound + (1 + Random.int dice_bound)

let pullChance () =
  let length, lst = chance_list in
  let n = Random.int length in
  List.nth lst n

let pullChest () =
  let length, lst = chest_list in
  let n = Random.int length in
  List.nth lst n

let rec tile_action tile player plist n : player list =
  if player.in_jail > 0 then [ player ]
  else
    match tile with
    | Start _ -> (
        print_endline "You landed back on Go!\nPress anything to continue > ";
        match read_line () with
        | _ -> [ player ])
    | Tax x -> (
        print_endline
          ("Oh No! You were taxed $" ^ string_of_int x
         ^ ".\nPress anything to continue > ");
        match read_line () with
        | _ -> [ { player with money = player.money - x } ])
    | Chance _ -> (
        print_endline "You pulled a chance card!";
        let card = pullChance () in
        match card with
        | ToStart -> (
            print_endline
              "CHANCE: Advance to Go, Collect $200.\n\
               Press anything to continue > ";
            match read_line () with
            | _ -> [ { player with position = 0; money = player.money + 200 } ])
        | ToJail -> (
            print_endline
              "CHANCE: Go directly to Jail. Do not collect $200\n\
               Press anything to continue > ";
            match read_line () with
            | _ ->
                [ { player with in_jail = 3; position = pos_of_tile (Jail 1) } ]
            )
        | GainMoney x -> (
            print_endline
              ("CHANCE: You are lucky! Collect $" ^ string_of_int x
             ^ ".\nPress anything to continue > ");
            match read_line () with
            | _ -> [ { player with money = player.money + x } ])
        | LoseMoney x -> (
            print_endline
              ("CHANCE: Unlucky! Pay $" ^ string_of_int x
             ^ ".\nPress anything to continue > ");
            match read_line () with
            | _ -> [ { player with money = player.money - x } ]))
    | Chest _ -> (
        print_endline "You pulled a Community Chest card!";
        let card = pullChest () in
        match card with
        | GainMoney x -> (
            print_endline
              ("COMMUNITY CHEST: You are lucky! Collect $" ^ string_of_int x
             ^ ".\nPress anything to continue > ");
            match read_line () with
            | _ -> [ { player with money = player.money + x } ])
        | LoseMoney x ->
            print_endline
              ("COMMUNITY CHEST: Unlucky! Pay $" ^ string_of_int x
             ^ ".\nPress anything to continue > ");
            [ { player with money = player.money - x } ]
        | _ -> [ player ])
    | Parking _ -> (
        print_endline
          "You landed on Free Parking. \nPress anything to continue >";
        match read_line () with
        | _ -> [ player ])
    | Jail _ -> (
        print_endline "Go to Jail.\nPress anything to continue >";
        match read_line () with
        | _ -> [ { player with in_jail = 3 } ])
    | Property prop -> property_action prop player plist n

and owner_opt (prop : property) (plist : player list) : player option =
  match plist with
  | [] -> None
  | h :: t -> if List.mem prop h.properties then Some h else owner_opt prop t

and pay_rent prop buyer (seller : player) plist n =
  let rent = calculated_rent prop plist n in
  let new_buyer = { buyer with money = buyer.money - rent } in
  let new_seller = { seller with money = seller.money + rent } in
  if buyer = seller then
    print_endline
      "You landed on your own property!\nPress anything to continue > "
  else
    Printf.printf
      "You landed on %s owned by %s. You paid $%i.\n\
      \ Press anything to continue >" (property_to_string prop) seller.name rent;

  match read_line () with
  | _ -> [ new_buyer; new_seller ]

and property_action (prop : property) (player : player) (plist : player list) n
    : player list =
  match owner_opt prop plist with
  | None -> ask_buy prop player plist n
  | Some s -> pay_rent prop player s plist n

and ask_buy (prop : property) player plist n =
  let check =
    match prop with
    | Location l -> player.money >= l.price
    | Tcat_station t -> player.money >= t.price
    | Utility u -> player.money >= u.price
  in
  if check then (
    Printf.printf
      "You landed on %s. Press [B] to buy, or Press [ENTER] to skip > "
      (property_to_string prop);
    match read_line () with
    | "B" | "b" -> (
        print_endline
          "\n\
           You bought the property. Now you can start collecting rent!.\n\
           Press anything to continue >";
        match read_line () with
        | _ ->
            [
              {
                player with
                properties = prop :: player.properties;
                money =
                  (player.money
                  -
                  match prop with
                  | Location x -> x.price
                  | Tcat_station x -> x.price
                  | Utility x -> x.price);
              };
            ])
    | _ -> [ player ])
  else (
    Printf.printf
      "You landed on %s. You don't have enough money to buy it.\n\
      \ Press anything to continue > " (property_to_string prop);
    match read_line () with
    | _ -> [ player ])

and calculated_rent (prop : property) (plist : player list) n : int =
  match prop with
  | Location x -> x.rent
  | Tcat_station t -> tcat_rent t plist
  | Utility u -> utility_rent u plist n

and tcat_rent tcat (plist : player list) : int =
  let rec find_owner tcat plist =
    match plist with
    | [] -> failwith "no owners"
    | h :: t ->
        if List.mem (Tcat_station tcat) h.properties then h
        else find_owner tcat t
  in
  let rec num_stations acc props =
    match props with
    | [] -> acc
    | Tcat_station h :: t -> num_stations (acc + 1) t
    | _ :: t -> num_stations acc t
  in
  50 * num_stations 0 (find_owner tcat plist).properties

and utility_rent util plist n =
  let rec find_owner util plist =
    match plist with
    | [] -> failwith "no owners"
    | h :: t ->
        if List.mem (Utility util) h.properties then h else find_owner util t
  in
  let rec num_utils acc props =
    match props with
    | [] -> acc
    | Utility u :: t -> num_utils (acc + 1) t
    | _ :: t -> num_utils acc t
  in
  n * 4 * num_utils 0 (find_owner util plist).properties

open Player
open Board
open Exceptions

let debug = false

(**************Debug functions****************)
let debug_brokenness x : int =
  if debug then (
    Printf.printf "\ncalculate_brokenness returned: %i\n" x;
    x)
  else x

let debug_selection before after =
  if debug then (
    List.iter (Printf.printf "|%s|")
      ("\nBEFORE:" :: List.map property_to_string before);
    List.iter (Printf.printf "|%s|")
      ("\nAFTER: " :: List.map property_to_string after);
    after)
  else after

(*********************************************)

type chances =
  | ToStart
  | ToJail
  | GainMoney of int
  | LoseMoney of int

(* A list of the chance cards in the current deck. Each card has
   equal chance of being pulled.*)
let chance_list =
  ( 6,
    [ ToStart; ToJail; GainMoney 40; LoseMoney 20; GainMoney 100; LoseMoney 50 ]
  )

(* A list of community chest cards in the current deck. Each card has equal
   chance of being pulled. *)
let chest_list =
  (5, [ GainMoney 30; LoseMoney 100; GainMoney 150; LoseMoney 25; GainMoney 50 ])

(* *)
let dice_bound = 6
let rollDice () : int = 1 + Random.int dice_bound + Random.int dice_bound + 1

let pullChance chance_list =
  let length, lst = chance_list in
  let n = Random.int length in
  List.nth lst n

let pullChest chest_list =
  let length, lst = chest_list in
  let n = Random.int length in
  List.nth lst n

let rec tile_action tile player plist n debug : player list =
  if player.in_jail > 0 then [ player ]
  else
    match tile with
    | Start _ ->
        if debug == false then (
          print_endline "You landed back on Go!\nPress anything to continue > ";
          match read_line () with
          | _ -> [ player ])
        else [ player ]
    | Tax x ->
        if debug == false then (
          print_endline
            ("Oh No! You were taxed $" ^ string_of_int x
           ^ ".\nPress anything to continue > ");
          match read_line () with
          | _ -> [ { player with money = player.money - x } ])
        else [ { player with money = player.money - x } ]
    | Chance _ -> (
        print_endline "You pulled a chance card!";
        let card = pullChance chance_list in
        match card with
        | ToStart -> (
            print_endline
              "CHANCE: Advance to Go, Collect $100.\n\
               Press anything to continue > ";
            match read_line () with
            | _ -> [ { player with position = 0; money = player.money + 100 } ])
        | ToJail -> (
            print_endline
              "CHANCE: Go directly to Jail. Do not collect $100\n\
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
        let card = pullChest chest_list in
        match card with
        | GainMoney x -> (
            print_endline
              ("COMMUNITY CHEST: You are lucky! Collect $" ^ string_of_int x
             ^ ".\nPress anything to continue > ");
            match read_line () with
            | _ -> [ { player with money = player.money + x } ])
        | LoseMoney x -> (
            print_endline
              ("COMMUNITY CHEST: Unlucky! Pay $" ^ string_of_int x
             ^ ".\nPress anything to continue > ");
            match read_line () with
            | _ -> [ { player with money = player.money - x } ])
        | _ -> [ player ])
    | Parking _ ->
        if debug == false then (
          print_endline
            "You landed on Free Parking. \nPress anything to continue >";
          match read_line () with
          | _ -> [ player ])
        else [ player ]
    | Jail _ ->
        if debug = false then (
          print_endline "Go to Jail.\nPress anything to continue >";
          match read_line () with
          | _ -> [ { player with in_jail = 3 } ])
        else [ { player with in_jail = 3 } ]
    | Property prop -> property_action prop player plist n

and owner_opt (prop : property) (plist : player list) : player option =
  match plist with
  | [] -> None
  | h :: t -> if List.mem prop h.properties then Some h else owner_opt prop t

and pay_rent prop buyer (seller : player) plist n debug =
  let rent = calculated_rent prop plist n in
  let new_buyer = { buyer with money = buyer.money - rent } in
  let new_seller = { seller with money = seller.money + rent } in
  if buyer.name = seller.name then
    if debug = false then begin
      print_endline
        "You landed on your own property!\nPress anything to continue > ";
      match read_line () with
      | _ -> [ buyer ]
    end
    else [ buyer ]
  else if debug = false then begin
    Printf.printf
      "You landed on %s owned by %s. You paid $%i.\n\
       Press anything to continue >" (property_to_string prop) seller.name rent;
    match read_line () with
    | _ -> [ new_buyer; new_seller ]
  end
  else [ new_buyer; new_seller ]

and property_action (prop : property) (player : player) (plist : player list) n
    : player list =
  match owner_opt prop plist with
  | None -> ask_buy prop player debug
  | Some s -> pay_rent prop player s plist n false

and ask_buy (prop : property) player debug =
  let check =
    match prop with
    | Location l -> player.money >= l.price
    | Tcat_station t -> player.money >= t.price
    | Utility u -> player.money >= u.price
  in
  if check then
    if debug = false then (
      Printf.printf
        "You landed on %s, which costs $%i and rents for %s.\n\
         Press [B] to buy, or Press [ENTER] to skip > "
        (property_to_string prop)
        (property_buying_value prop)
        (rent_text prop);
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
    else
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
      ]
  else if debug = false then (
    Printf.printf
      "You landed on %s. You don't have enough money to buy it.\n\
       Press anything to continue > " (property_to_string prop);
    match read_line () with
    | _ -> [ player ])
  else [ player ]

and rent_text prop =
  match prop with
  | Location x -> "$" ^ string_of_int x.rent
  | Tcat_station _ -> "\n$50 * the # of TCAT stations you own"
  | Utility _ -> "\n$4 * Value of Dice Roll * the # of Utilities you own"

and calculated_rent (prop : property) (plist : player list) n : int =
  match prop with
  | Location x -> x.rent
  | Tcat_station t -> tcat_rent t plist
  | Utility u -> utility_rent u plist n

and tcat_rent tcat (plist : player list) : int =
  let rec find_owner tcat plist =
    match plist with
    | [] -> raise (Unreachable "utils/tcat_rent received an empty player list")
    | h :: t ->
        if List.mem (Tcat_station tcat) h.properties then h
        else find_owner tcat t
  in
  let rec num_stations acc props =
    match props with
    | [] -> acc
    | Tcat_station _ :: t -> num_stations (acc + 1) t
    | _ :: t -> num_stations acc t
  in
  50 * num_stations 0 (find_owner tcat plist).properties

and utility_rent util plist n =
  let rec find_owner util plist =
    match plist with
    | [] ->
        raise (Unreachable "utils/utility_rent received an empty player list")
    | h :: t ->
        if List.mem (Utility util) h.properties then h else find_owner util t
  in
  let rec num_utils acc props =
    match props with
    | [] -> acc
    | Utility _ :: t -> num_utils (acc + 1) t
    | _ :: t -> num_utils acc t
  in
  n * 4 * num_utils 0 (find_owner util plist).properties

let rec check_broke (p : player) (players : player list) debug : player list =
  if p.money < 0 then mortgage_action p players debug else players

and calculate_brokeness (p : player) : int = debug_brokenness p.money

and mortgage_action (p : player) (plst : player list) debug =
  if debug = false then (
    let deficit = calculate_brokeness p in
    Printf.printf
      "\n\
       Uh oh, %s, you are broke! \n\
       You need $%i to recover. \n\
       If you have any properties to mortgage, you can sell them now. \n"
      p.name ~-deficit;
    let props =
      debug_selection p.properties (select_property p.properties [])
    in
    if deficit + sum_values props >= 0 then
      let new_p =
        {
          p with
          properties = remove_props p.properties props;
          money = p.money + sum_values props;
        }
      in
      List.map (fun (e : player) -> if e.name = p.name then new_p else e) plst
    else kill_player p plst debug)
  else
    let deficit = calculate_brokeness p in
    let props =
      debug_selection p.properties (select_property p.properties [])
    in
    if deficit + sum_values props >= 0 then
      let new_p =
        {
          p with
          properties = remove_props p.properties props;
          money = p.money + sum_values props;
        }
      in
      List.map (fun (e : player) -> if e.name = p.name then new_p else e) plst
    else kill_player p plst debug

and kill_player p plst debug =
  if debug = false then (
    Printf.printf
      "\n\
       %s could not recover from their deficit. They lost!\n\
       Press anything to continue > " p.name;
    match read_line () with
    | _ -> List.filter (fun (e : player) -> e.name <> p.name) plst)
  else List.filter (fun (e : player) -> e.name <> p.name) plst

and select_property (props : property list) (acc : property list) :
    property list =
  match props with
  | [] -> acc
  | h :: t -> (
      Printf.printf "\nSell %s for $%i: [Y] | [N]. Current sum: %i > "
        (property_to_string h) (property_selling_value h) (sum_values acc);
      match read_line () with
      | "Y" | "y" -> select_property t (h :: acc)
      | "N" | "n" -> select_property t acc
      | _ -> select_property (h :: t) acc)

and sum_values (props : property list) : int =
  List.fold_left ( + ) 0 (List.map property_selling_value props)

and remove_props (props : property list) (target : property list) =
  match props with
  | [] -> target
  | h :: t -> remove_props t (List.filter (fun p -> p <> h) target)

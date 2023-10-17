(* board.ml *)
type location = {
  name : string;
  price : int;
  color : string;
  base_rent : int;
  house_rent_multipliers : int list;
  build_cost : int;
  num_houses : int;
  mortgage : int;
}

type tcat_station = { name : string; price : int; rent : int; mortgage : int }

type utility = {
  name : string;
  price : int;
  rent_multipliers : int * int;
  mortgage : int;
}

type property =
  | Location of location
  | Tcat_station of tcat_station
  | Utility of utility

type tile =
  | Start
  | Property of property
  | Tax of int
  | Chance
  | Chest
  | Parking
  | Jail

type board = tile list

let calculated_rent (prop : property) : int =
  match prop with
  | Location x ->
      let total_rent =
        x.base_rent * List.nth x.house_rent_multipliers x.num_houses
      in
      total_rent
  | Tcat_station x -> x.rent (* fix *)
  | Utility x -> ( match x.rent_multipliers with h, _ -> h * 1 (* fix *))

let property_to_string (p : property) : string =
  match p with
  | Location l -> l.name
  | Tcat_station t -> t.name
  | Utility u -> u.name

let to_string (t : tile) : string =
  match t with
  | Start -> "Start"
  | Property p -> property_to_string p
  | Tax _ -> "Tax"
  | Chance -> "Chance"
  | Chest -> "Chest"
  | Parking -> "Parking"
  | Jail -> "Jail"

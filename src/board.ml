open Exceptions

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

type tcat_station = {
  name : string;
  price : int;
  rent : int;
  mortgage : int;
}

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

type board = (tile * int) list
(**[board] is a list of tiles, marked by a number that represents order*)

let tlist : tile list = [ Start; Tax 50; Chance; Parking; Chest; Tax 20; Jail ]

let new_board : board =
  let rec indices (n : int) (lst : tile list) : int list =
    match lst with
    | [] -> []
    | _ :: t -> n :: indices (n + 1) t
  in
  List.combine tlist (indices 0 tlist)

let rec length (b : board) : int =
  match b with
  | [] -> 0
  | _ :: t -> 1 + length t

let pos_of_tile (t : tile) : int =
  let rec helper (lst : board) (t : tile) : int =
    match lst with
    | [] -> raise No_Such_Tile
    | (tile, idx) :: tl -> if tile = t then idx else helper tl t
  in
  helper new_board t

let rec tile_of_pos (b : board) (n : int) : tile =
  match b with
  | [] -> raise (Invalid_argument "the given position is invalid")
  | (tile, n') :: t -> if n = n' then tile else tile_of_pos t n

let calculated_rent (prop : property) : int =
  match prop with
  | Location x ->
      let total_rent =
        x.base_rent * List.nth x.house_rent_multipliers x.num_houses
      in
      total_rent
  | Tcat_station x -> x.rent (* fix *)
  | Utility x -> (
      match x.rent_multipliers with
      | h, _ -> h * 1 (* fix *))

let property_to_string (p : property) : string =
  match p with
  | Location l -> l.name
  | Tcat_station t -> t.name
  | Utility u -> u.name

let to_string (t : tile) : string =
  match t with
  | Start -> "\027[32mGo\027[0m"
  | Property p -> "\027[33m" ^ property_to_string p ^ "\027[0m"
  | Tax x -> "\027[38;5;214mTax ($" ^ string_of_int x ^ ")\027[0m"
  | Chance -> "\027[35mChance\027[0m"
  | Chest -> "\027[34mChest\027[0m"
  | Parking -> "\027[36mFree Parking\027[0m"
  | Jail -> "\027[31mJail\027[0m"

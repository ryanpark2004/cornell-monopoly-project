open Exceptions

let debug = true

(* board.ml *)
type location = {
  name : string;
  price : int;
  rent : int;
  mortgage : int;
}

type tcat_station = {
  name : string;
  price : int;
  mortgage : int;
}

type utility = {
  util_name : string;
  price : int;
  mortgage : int;
}

type property =
  | Location of location
  | Tcat_station of tcat_station
  | Utility of utility

type tile =
  | Start of int
  | Property of property
  | Tax of int
  | Chance of int
  | Chest of int
  | Parking of int
  | Jail of int

(* A list of normal location properties on the board.*)
let locations =
  Array.of_list
    [
      {
        name = "\027[38;5;137mHigh Rise 5 [!]\027[0m";
        price = 40;
        rent = 16;
        mortgage = 10;
      };
      {
        name = "\027[38;5;137mThe Gothics [!]\027[0m";
        price = 50;
        rent = 24;
        mortgage = 15;
      };
      {
        name = "\027[38;5;22mOkenshields [!!]\027[0m";
        price = 120;
        rent = 60;
        mortgage = 30;
      };
      {
        name = "\027[38;5;22mMorrison [!!]\027[0m";
        price = 140;
        rent = 80;
        mortgage = 35;
      };
      {
        name = "\027[38;5;93mDuffield Hall [!!!]\027[0m";
        price = 280;
        rent = 150;
        mortgage = 70;
      };
      {
        name = "\027[38;5;93mStatler Hotel [!!!]\027[0m";
        price = 300;
        rent = 190;
        mortgage = 75;
      };
      {
        name = "\027[38;5;214mMann Library [!!!!]\027[0m";
        price = 400;
        rent = 230;
        mortgage = 100;
      };
      {
        name = "\027[38;5;214mThe Clocktower [!!!!]\027[0m";
        price = 450;
        rent = 250;
        mortgage = 100;
      };
    ]

(* A list of TCAT station properties on the board*)
let stations =
  Array.of_list
    [
      { name = "Commons Station [🚌]"; price = 200; mortgage = 100 };
      { name = "Collegetown Station [🚌]"; price = 200; mortgage = 100 };
      { name = "North Campus Station [🚌]"; price = 200; mortgage = 100 };
      { name = "Central Station [🚌]"; price = 200; mortgage = 100 };
    ]

(* A list of utility properties on the board. *)
let utilities =
  Array.of_list
    [
      {
        util_name = "\027[38;5;250mCornell Hydroelectric [🚰]\027[0m";
        price = 150;
        mortgage = 50;
      };
      {
        util_name = "\027[38;5;250mEduroam Station [🚰]\027[0m";
        price = 170;
        mortgage = 85;
      };
    ]

type board = (tile * int) list

(* The tile list that is used to create the initial board*)
let tlist : tile list =
  [
    Start 1;
    Property (Location locations.(0));
    (* Chance 1;
       Property (Tcat_station stations.(0));
       Property (Location locations.(1));
       Chest 1;
       Parking 1;
       Property (Utility utilities.(0));
       Tax 80;
       Property (Tcat_station stations.(1));
       Property (Location locations.(2));
       Property (Location locations.(3));
       Jail 1;
       Chance 2;
       Property (Location locations.(4));
       Property (Tcat_station stations.(2));
       Property (Location locations.(5));
       Chest 2;
       Parking 2;
       Property (Utility utilities.(1));
       Tax 150;
       Property (Tcat_station stations.(3));
       Property (Location locations.(6));
       Property (Location locations.(7)); *)
  ]

let debug_board = [ Start 0; Property (Location locations.(1)); Tax 1000 ]

let rec indices (n : int) (lst : tile list) : int list =
  match lst with
  | [] -> []
  | _ :: t -> n :: indices (n + 1) t

let new_board : board =
  if debug then List.combine debug_board (indices 0 debug_board)
  else List.combine tlist (indices 0 tlist)

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
  | [] ->
      raise (Invalid_argument (Printf.sprintf "%i is an invalid position" n))
  | (tile, n') :: t -> if n = n' then tile else tile_of_pos t n

let property_selling_value (p : property) : int =
  match p with
  | Location l -> l.mortgage
  | Tcat_station t -> t.mortgage
  | Utility u -> u.mortgage

let property_buying_value (p : property) : int =
  match p with
  | Location l -> l.price
  | Tcat_station t -> t.price
  | Utility u -> u.price

let property_to_string (p : property) : string =
  match p with
  | Location l -> l.name
  | Tcat_station t -> t.name
  | Utility u -> u.util_name

let to_string (t : tile) : string =
  match t with
  | Start _ -> "\027[32mGo\027[0m"
  | Property p -> "\027[33m" ^ property_to_string p ^ "\027[0m"
  | Tax x -> "\027[38;5;202mTax ($" ^ string_of_int x ^ ")\027[0m"
  | Chance _ -> "\027[35mChance\027[0m"
  | Chest _ -> "\027[34mChest\027[0m"
  | Parking _ -> "\027[36mFree Parking\027[0m"
  | Jail _ -> "\027[31mJail\027[0m"

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
  owner : Player.player option;
}

type tcat_station = {
  name : string;
  price : int;
  rent : int;
  mortgage : int;
  owner : Player.player option;
}

type utility = {
  name : string;
  price : int;
  rent_multipliers : int * int;
  mortgage : int;
  owner : Player.player option;
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

type board = tile list * Player.player list

let action tile =
  match tile with
  | Start -> None
  | Property p -> None
  | Tax x -> None
  | Chance -> None
  | Chest -> None
  | Parking -> None
  | Jail -> None

let calculated_rent (prop : property) : int =
  match prop with
  | Location x ->
      let total_rent =
        x.base_rent * List.nth x.house_rent_multipliers x.num_houses
      in
      total_rent
  | Tcat_station x -> x.rent (* fix *)
  | Utility x -> ( match x.rent_multipliers with h, t -> h * 1 (* fix *))

let rec print_board board =
  match board with
  | [], _ -> ()
  | h :: t, x ->
      print_endline h.name;
      print_board (t, x)

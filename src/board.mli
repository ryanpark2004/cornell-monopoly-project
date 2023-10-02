(**************************************************************************)
(*                                                                        *)
(*  Module: board                                                         *)
(*                                                                        *)
(*  Description: This module provides basic functionality for the board   *)
(*    and its pieces, including properties and tiles.                     *)
(*                                                                        *)
(*  Author: Ethan Baker                                                   *)
(*  Last Updated: 10/2/23                                                 *)
(**************************************************************************)

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

(* Defines what happens when the player lands on the tile.*)
val action : tile -> unit
val buy_property : property -> unit
val sell_property : property -> unit
val mortgage_property : property -> unit
val build : location -> unit
val calculated_rent : property -> int
val print_board : board -> unit

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
}
(** Represents a non-utility and non-TCAT station property on the gameboard. 
    -[name]: The name of the location (e.g. Clocktower).
    -[price]: The purchase price of the location.
    -[color]: The color group of the location (e.g. Red).
    -[base_rent]: The rent charged when no houses are built.
    -[house_rent_multipliers]: A list of rent multipliers for 1 to 5 houses, 
    where [base_rent] * mutliplier = total rent. Must have len = 5.
    -[build_cost]: The cost to build a house on this location.
    -[num_houses]: The current number of houses built on this location.
    -[mortgage]: The mortgage value of the location.*)

type tcat_station = {
  name : string;
  price : int;
  rent : int;
  mortgage : int;
}
(** Represents a TCAT station property on the gameboard, which works similarly 
    to railroad properties in classic Monopoly.
      -[name]: The name of the station (e.g. Ithaca Commons Station).
      -[price]: The purchase price of the station.
      -[rent]: The rent charged when a player owns only one station. Total rent
      is calculated by [rent] * 2^n, where n is the number of TCAT stations 
      owned by the player.
      -[mortgage]: The mortagage value of the station.*)

type utility = {
  name : string;
  price : int;
  rent_multipliers : int * int;
  mortgage : int;
}
(** Represents a utility property on the gameboard.
    -[name]: The name of the utility (e.g. Cornell Water Turbine).
    -[price]: The purchase price of the utility.
    -[rent_multipliers]: The first element is the rent multiplier when the player
    owns only one utility, and the second is the rent multiplier when the 
    player owns both utilities. Total rent is calculated by multiplier * 
    dice roll value.
    -[mortgage]: The mortgage value of the utility.
    -[owner]: The current owner of the utility, or [None] if unowned.*)

(** Represents a property tile on the game board. A property can be a standard 
    location, a railroad-like tcat_station, or a utility.*)
type property =
  | Location of location  (** Represents a standard property on the board.*)
  | Tcat_station of tcat_station
      (** Represents a TCAT station, which is like a railroad in traditional 
          monopoly.*)
  | Utility of utility  (** Represents a utility property on the board.*)

(** Represents a tile on the game board. Tiles can either be start tiles, 
    properties, taxes, chance, community chest, free parking, or jail tiles. *)
type tile =
  | Start
      (** A start tile, where the players start and collect $200 when passing 
          through it each round.*)
  | Property of property  (** A standard property tile. *)
  | Tax of int  (** A tile which requires the player to pay a tax.*)
  | Chance  (** A tile which allows the player to pick a chance card.*)
  | Chest  (** A tile which allows the player to pick a community chest card.*)
  | Parking
      (** The free parking tile, which does not do anything in particular. This 
          tile is also used when a player is 'just passing through' jail.*)
  | Jail
      (** A tile which sends players to jail, where they must wait or pay 
      their way out to continue playing.*)

type board = (tile * int) list
(** Represents the overall game board, which keeps track of all tile and 
    player status.*)

val calculated_rent : property -> int
(** Calculates the rent due to visiting players based off of specifics for 
    the different property types. 
    -[location]: [base rent] * current house multiplier.
    -[tcat_station]: [rent] * 2^n where n is the number of tcat_station 
    properties the player owns.
    -[utility]: [rent] * current dice role value.*)

val to_string : tile -> string
(**converts a tile to string*)

val new_board : board
(**A fresh new board*)

val pos_of_tile : tile -> int
(**Returns the integer position of the tile. Starts at 0*)

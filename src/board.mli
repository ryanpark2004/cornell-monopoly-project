(*                                                                        *)
(*  Module: board                                                         *)
(*                                                                        *)
(*  Description: This module provides basic functionality for the board   *)
(*  and its pieces, including properties and tiles.                       *)
(*                                                                        *)
(*  Authors: Ethan Baker, Bill Park, Ryan Park                            *)
(*  Date: December 10th 2023                                              *)
(**************************************************************************)
type location = {
  name : string;
  price : int;
  rent : int;
  mortgage : int;
}
(** Represents a non-utility and non-TCAT station property on the gameboard. 
    -[name]: The name of the location (e.g. Clocktower).
    -[price]: The purchase price of the location.
    -[rent]: The rent that is charged when another player lands on it.
    -[mortgage]: The mortgage value of the location.*)

type tcat_station = {
  name : string;
  price : int;
  mortgage : int;
}
(** Represents a TCAT station property on the gameboard, which works similarly 
    to railroad properties in classic Monopoly.
      -[name]: The name of the station (e.g. Ithaca Commons Station).
      -[price]: The purchase price of the station.
      -[mortgage]: The mortagage value of the station.*)

type utility = {
  util_name : string;
  price : int;
  mortgage : int;
}
(** Represents a utility property on the gameboard.
    -[util_name]: The name of the utility (e.g. Cornell Water Turbine).
    -[price]: The purchase price of the utility.
    -[mortgage]: The mortgage value of the utility.*)

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
  | Start of int
      (** A start tile, where the players start and collect $200 when passing 
          through it each round.*)
  | Property of property  (** A standard property tile. *)
  | Tax of int  (** A tile which requires the player to pay a tax.*)
  | Chance of int  (** A tile which allows the player to pick a chance card.*)
  | Chest of int
      (** A tile which allows the player to pick a community chest card.*)
  | Parking of int
      (** The free parking tile, which does not do anything in particular. This 
          tile is also used when a player is 'just passing through' jail.*)
  | Jail of int
      (** A tile which sends players to jail, where they must wait or pay 
      their way out to continue playing.*)

type board = (tile * int) list
(** Represents the overall game board, which keeps track of where tiles are 
    located.*)

val new_board : board
(** Creates a new board using a list of tiles.*)

val length : board -> int
(** Calculates the length of the current game board. 
    An empty board has length 0.*)

val pos_of_tile : tile -> int
(** Returns the integer position of the tile. Starts at 0 *)

val tile_of_pos : board -> int -> tile
(** Returns the tile at the position n in the inputted board. 
    Raises Invalid_Tile exception when tiles does not exist at the position.*)

val property_to_string : property -> string
(* Converts a property to a string by returning out the name of property. *)

val to_string : tile -> string
(** Converts a tile to string, using ASCII codes to change the color of 
    different tiles.*)

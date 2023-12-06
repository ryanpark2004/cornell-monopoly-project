open Board

(**************************************************************************)
(*                                                                        *)
(*  Module: Player                                                        *)
(*                                                                        *)
(*  Description: This module provides basic functionality for the player  *)
(*    and any associated functions with the player                        *)
(*                                                                        *)
(*  Author: Ryan, Bill, Ethan                                             *)
(**************************************************************************)
type player = {
  name : string;
  money : int;
  properties : int list;
  position : int;
  in_jail : int;
}
(** Type player represents the attributes of a player.
      This includes their name, money, properties owned, their position, and 
      the amount of time where they are in jail**)

val create_player : string -> player
(** The function create_player initializes a player with a given name 
      to the start of the board with money and no properties.**)

val get_name : player -> string
(** Returns the name of the specified player.**)

val buy_property : property -> player -> player
(** The function buy_property checks to see if a player can buy a property
       If so, the amount of money is subtracted from their account and the 
       property is added to the players account. If not, an invalid_arg
       is raised. **)

val receive_money : player -> int -> player
(** The function receive_money adds any amount of money earned or subtracted
       from their account. **)

val move_player : player -> int -> player
(**Returns a new player with updated position*)

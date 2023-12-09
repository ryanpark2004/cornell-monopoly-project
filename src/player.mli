open Board

(**************************************************************************)
(*                                                                        *)
(*  Module: Player                                                        *)
(*                                                                        *)
(*  Description: This module provides basic functionality for the player  *)
(*  and any associated functions with the player                          *)
(*                                                                        *)
(*  Author: Ryan, Bill, Ethan                                             *)
(*  Date: December 10th, 2023                                             *)
(**************************************************************************)
type player = {
  name : string;
  mutable money : int;
  mutable properties : property list;
  position : int;
  in_jail : int;
}
(** Type player represents the attributes of a player. This includes their name,
    money, properties owned, their position, and the amount of time where 
    they are in jail.*)

val create_player : string -> player
(** The function create_player initializes a player with a given name to the 
    start of the board with some intitial money and no properties.*)

val get_name : player -> string
(** Returns the name of the specified player.*)

val receive_money : player -> int -> player
(** The function receive_money adds any amount of money earned or subtracted
       from their account. *)

val move_player : player -> int -> bool -> player
(** Takes in a [player] and an integer [n] and returns a new player with the new
    position [n] + old position. If the new position is greater than the length 
    of the board, [move_player] returns the modulus of the calculated position 
    and board length.*)

(*                                                                        *)
(*  Module: Exceptions                                                    *)
(*                                                                        *)
(*  Description: This module provides custom exceptions to be used in     *)
(*  instances of tile and money related errors.                           *)
(*                                                                        *)
(*  Authors: Ethan Baker, Bill Park, Ryan Park                            *)
(*  Date: December 10th 2023                                              *)
(**************************************************************************)

exception No_Such_Tile
(** Raises an error when attempting to call or use a tile that does not 
   exist in the current game board. *)

exception Not_Enough_Money of string
(** Raises an exception when the player attempts to buy a property, gets taxed, 
    pays rent, or pays out of jail without suffient funds.*)

exception Unreachable of string

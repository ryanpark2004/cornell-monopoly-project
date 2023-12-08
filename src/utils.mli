(*                                                                        *)
(*  Module: Utils                                                         *)
(*                                                                        *)
(*  Description: This module provides functions that help the main script *)
(*  control the flow of the game. It implements functionality for rolling *)
(*  the dice, l anding on and interacting with tiles, paying and charging *)
(*  rent, and buying and mortgaging properties.                           *)
(*                                                                        *)
(*  Authors: Ethan Baker, Bill Park, Ryan Park                            *)
(*  Date: December 10th 2023                                              *)
(**************************************************************************)
open Board
open Player

type chances
(** [chances] is a variant representing different chance cards. ToStart cards 
    transport the player to the Go tile, ToJail cards place the player in jail, 
    GainMoney awards the player a monetary prize, and LoseMoney penalizes the 
    player by removing some of their money. *)

val rollDice : unit -> int
(** [rollDice] is a random integer from 2 to 2 x bound. In normal playing 
    conditions, the probability of getting an value by calling [rollDice] is 
    the same as getting that output when rolling two physical dice.  *)

val pullChance : unit -> chances
(**[pullChance] is a uniformly randomly selected chance variant.*)

val pullChest : unit -> chances
(**[pullChance] is a uniformly randomly selected community chest variant.*)

val tile_action : tile -> player -> player list -> int -> player list
(** [tile_action] defines the different effects each tile has when 
    a player lands on them. Returns the new player after everything has 
 changed. The actions for each type of tile are defined below:
    Start: Nothing. Retuns the player as it is
    Tax x: Returns the player with x-less money.
    Chance: Returns the player after adjusting money and positon, based off 
    of the instructions of the chance card that is selected.
    Chest: Returns the player after adjusting money, based off of the 
    instructions of the community chest card that is selected.
    Parking: Nothing. Retuns the player as it is.
    Jail: Sets the player's in_jail status to 3, and returns.*)

val owner_opt : property -> player list -> player option
(** Returns [Some player] if player owns the input property. Else [None].*)

val pay_rent : property -> player -> player -> player list -> int -> player list
(** [pay_rent] orchestrates the transaction between the owner and renter on 
    when a rented lands on the owned property. Returns a list of the owner with 
    the added rent money, and the rented without the rent money.*)

val check_broke : player -> player list -> player list
(**[check_broke] checks if player is broke. Returns the same player list as
    the argument if the player is not broke. Otherwise, it goes through
    mortgage process.*)

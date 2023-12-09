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

val tile_action : tile -> player -> player list -> int -> bool -> player list
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

val pay_rent :
  property -> player -> player -> player list -> int -> bool -> player list
(** [pay_rent] orchestrates the transaction between the owner and renter on 
    when a rented lands on the owned property. Returns a list of the owner with 
    the added rent money, and the rented without the rent money.*)

val property_action : property -> player -> player list -> int -> player list
(** Checks if a property is owned. If not, asks a player if they would like to 
    buy it. If so, charges rent to the player.*)

val ask_buy : property -> player -> bool -> player list
(** Allows the player to choose to buy a property. If so, it returns a new 
    player list with the updated player with less the money it cost and 
    with the property in its properties inventory. If no, it returns
    the player list as it is. *)

val calculated_rent : property -> player list -> int -> int
(** Returns the rent of the property, taking into account the other properties 
    owned by the players, and if necessary, the dice roll value. *)

val tcat_rent : tcat_station -> player list -> int
(** Returns the current rent of the TCAT station, which is 50 * the number of 
    TCAT stations the owner owns. *)

val utility_rent : utility -> player list -> int -> int
(** Returns the current rent of the utility property, which is equal to the
    dice roll value * the number of utilities owned by the owner * 4. *)

val check_broke : player -> player list -> player list
(**[check_broke] checks if player is broke. Returns the same player list as
    the argument if the player is not broke. Otherwise, it goes through
    mortgage process.*)

val calculate_brokeness : player -> int
(** Used to debug broke player conditions. Returns player.money *)

val mortgage_action : player -> player list -> player list
(** Prompts players to mortgage thier propeties when they run out of money. 
    Gives players the option to sell off any properties they may own, then 'kills'
    them if they still do not have enough money. *)

val kill_player : player -> player list -> player list
(** Displays a game over message for the player who ran out of money, then 
    removes them from the player list and returns the updated list.*)

val select_property : property list -> property list -> property list
(** Allows a player to choose which properties they mortgage, then adds the 
    mortgage value to their balance.*)

val sum_values : property list -> int
(** Returns the total mortage value of all of the properties in the input list.*)

val remove_props : property list -> property list -> property list
(** Removes properties from the first list from the second list, and 
    returns the result.*)

val rent_text : property -> string
(** Returns the string representing how much rent is for the property.*)

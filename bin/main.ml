open Monopoly
open Board
open Player
open Utils
open Printf

(*******************************Helpers**************************************)
type state = {
  board : board;
  players : Player.player list;
  max_run : int;
  current_run : int;
}

let new_state : state =
  { board = Board.new_board; players = []; max_run = 10; current_run = 0 }

(**[add_players] is a new state with new players added. 
    The number of players must be less than or equal to 4,
     or invalid argument exception is raised. The name of each 
     player must be nonempty and 10 character long or less*)
let initialize_players (s : state) : state =
  print_endline "How many players?: \n";
  let num_players = int_of_string (read_line ()) in
  if num_players <= 0 || num_players > 4 then invalid_arg "Too many players"
  else
    let rec helper n acc =
      match n with
      | 0 -> acc
      | x ->
          let () = print_endline ("Name of player " ^ string_of_int x ^ ":") in
          let player_name = read_line () in
          let name_length = String.length player_name in
          if name_length <= 0 || name_length > 10 then
            invalid_arg "Name is too long"
          else
            let player = create_player player_name in
            helper (n - 1) (player :: acc)
    in
    {
      board = s.board;
      players = helper num_players [];
      max_run = s.max_run;
      current_run = s.current_run;
    }

(******************************************************************************)

(*********************************THE LOOP*************************************)
let rec play (n : int) = failwith "unimplemneted"

(**[update_player] is a new player whose status is updated using the
    "dummy player" [info]. This serves as an helper function for
    updated_players. 
    Soft Requirement: [name] is the name of [p], otherwise [p] is returned*)
let update_player (name : string) (info : player) (p : player) : player =
  if get_name p = name then
    let new_player =
      {
        name = p.name;
        money = p.money + info.money;
        properties = p.properties @ info.properties;
        position = p.position + info.position;
        in_jail = info.in_jail;
      }
    in
    new_player
  else p

(**[update_players] is a [plst] except that player [p] is updated based on 
some [info]. [info] is a "dummy player" that contains the new informations 
based on the actions taken by the player [p]*)
let updated_players (s : state) (p : player) (info : player) : state =
  let new_plst = List.map (update_player (get_name p) info) s.players in
  { s with players = new_plst }

(**[perform_action] returns a "dummy player" that stores all the information 
about the player's actions. In other words, it is the change that is to be 
applied to the player. This function is to be used in conjunction with 
update_players

As of 10/18/2023, this function only moves the player with a random dice*)
let perform_action (p : player) : player =
  let n = rollDice () in
  { p with money = 0; properties = []; position = n; in_jail = false }

let update (s : state) : state = failwith "unimplemented"

(******************************************************************************)

(******************************** PRINTS **************************************)

(*TODO: modify [p] to player list so that the resultant string list contains all
  tiles with player names next to them.*)
let rec attach_player (tlst : tile list) (p : player) : string list =
  let player_tile = string_of_int p.position in
  match tlst with
  | [] -> []
  | h :: t ->
      let this_tile = to_string h in
      if this_tile = player_tile then
        let combined = this_tile ^ "---" ^ p.name in
        combined :: attach_player t p
      else attach_player t p

(*TODO: modify [p] in [print] to accommodate all players*)
let print state =
  let b = state.board in
  let p =
    match state.players with
    | [ x ] -> x
    | h :: _ -> h
    | _ -> invalid_arg "no players"
  in
  let strings = attach_player b p in
  List.iter (printf "%s \n") strings

(******************************************************************************)

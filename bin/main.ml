open Monopoly
open Player
open Board
open Utils
open Printf

(*******************************Game Loop***************************************)
type state = { board : board; players : Player.player list }

let new_state : state = { board = Board.new_board; players = [] }

(**[add_players] is a new state with new players added. 
    The number of players must be less than or equal to 4,
     or invalid argument exception is raised. The name of each 
     player must be nonempty and 10 character long or less*)
let add_players (s : state) : state =
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
    { board = s.board; players = helper num_players [] }

let rec play (n : int) = failwith "unimplemneted"
let update = failwith "Unimplemented"

(******************************************************************************)
(******************************** PRINTS **************************************)

(*TODO: modify [p] to player list so that the resultant string list contains
   all tiles with player names next to them.*)
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

(* Recursive Function for rolling constantly *)
let rec roll_roll (player : player) : unit =
  print_endline "Roll the dice by hitting pressing Enter!";
  print_string "> ";
  let input = read_line () in
  match input with
  | "" ->
      let dice_number = rollDice () in
      let dice_string = string_of_int dice_number in
      let combined_string = "You moved " ^ dice_string ^ " spaces!" in
      print_endline combined_string;
      let board_state = [ Start; Tax 100; Chance; Jail; Parking; Parking ] in
      let player_1 = move_player player dice_number in
      let tile_position = player_1.position mod 6 in
      let player_action = List.nth board_state tile_position in
      let final_player = tile_action player_action player in
      roll_roll final_player
  | _ -> failwith "That wasn't the Enter key!"

(* Command Line *)
let () =
  print_endline "\n\nWelcome to Cornell Monopoly!\n";
  print_endline "Please input the number of players. (1 for now!)";
  print_string "> ";
  let player_number = read_line () in
  print_endline "Generating players...";
  match int_of_string player_number with
  | 1 ->
      print_endline "What is Player 1's name?";
      print_string "> ";
      let player_name = read_line () in
      let player_1 = create_player player_name in
      roll_roll player_1
  | _ -> failwith "That is not a valid amount of players!"

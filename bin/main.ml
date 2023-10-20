open Monopoly
open Board
open Player
open Utils

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
    let rec helper n =
      match n with
      | 0 -> []
      | x ->
          let () = print_endline ("Name of player " ^ string_of_int x ^ ":") in
          let player_name = read_line () in
          let name_length = String.length player_name in
          if name_length <= 0 || name_length > 10 then
            invalid_arg "Name is too long"
          else
            let player = create_player player_name in
            player :: helper (n - 1)
    in
    {
      board = s.board;
      players = helper num_players;
      max_run = s.max_run;
      current_run = s.current_run;
    }

(******************************************************************************)

(******************************** PRINTS **************************************)
(*
let rec print_board (b : board) : unit =
  match b with
  | [] -> print_endline ""
  | h :: t ->
      let tile, _ = h in
      printf "%s \n" (to_string tile);
      print_board t
*)

let rec check_occupancy (tl : tile * int) (plst : player list) : player option =
  match plst with
  | [] -> None
  | h :: t ->
      let tile, pos = tl in
      if h.position = pos then Some h else check_occupancy tl t

let rec pretty_board (b : board) (plst : player list) :
    (tile * player option) list =
  match b with
  | [] -> []
  | h :: t -> (
      let occupancy = check_occupancy h plst in
      let tl, pos = h in
      match occupancy with
      | None -> (tl, None) :: pretty_board t plst
      | Some p -> (tl, Some p) :: pretty_board t plst)

let print_state (s : state) : unit =
  let pb = pretty_board s.board s.players in
  let rec print_pb (pb : (tile * player option) list) : unit =
    match pb with
    | [] -> print_endline "---------------\n"
    | h :: t ->
        (let tile, player = h in
         match player with
         | None -> print_endline (to_string tile)
         | Some p -> print_endline (to_string tile ^ " --- " ^ p.name));
        print_pb t
  in
  print_pb pb
(******************************************************************************)

(*********************************THE LOOP*************************************)

(**[action] is a dummy player that contains all the changes to be applied to the actual player
    As of now, the action only contains changes in player position*)
let action : player =
  let n = rollDice () in
  print_endline ("Rolled dice: " ^ string_of_int n);
  { name = "info"; money = 0; properties = []; position = n; in_jail = false }

(**[update_player] is the new player whose fields have changed using [info]*)
let update_player (p : player) (info : player) : player =
  {
    p with
    money = p.money + info.money;
    properties = p.properties @ info.properties;
    position = p.position + info.position;
    in_jail = info.in_jail;
  }

(**[single_turn] is an updated state after every player performed an action *)
let single_turn (s : state) : state =
  let rec helper (plst : player list) : player list =
    match plst with
    | [] ->
        print_endline ("Turn #" ^ string_of_int s.current_run ^ " ended");
        []
    | h :: t -> update_player h action :: helper t
  in
  { s with players = helper s.players; current_run = s.current_run + 1 }

(******************************************************************************)
(********************************MAIN APP**************************************)
(*eval -> print loop*)
let rec loop (s : state) (eval : state -> state) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | _ ->
      let s2 = s |> eval in
      print_state s2;
      loop s2 eval

let () =
  print_endline "\n\n Welcome to Monopoly. \n";
  let start = initialize_players new_state in
  loop start single_turn

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

let occupants (tl : tile * int) (plst : player list) : tile * player list =
  let tile, pos = tl in
  let rec helper (plst2 : player list) : player list =
    match plst2 with
    | [] -> []
    | h :: t -> if pos = h.position then h :: helper t else helper t
  in
  (tile, helper plst)

(**[pretty_board] is a board where each tile is associated with all the players standing on it*)
let rec pretty_board (plst : player list) (b : board) :
    (tile * player list) list =
  (*for each tile, check what players are standing on it*)
  match b with
  | [] -> []
  | h :: t -> occupants h plst :: pretty_board plst t

let rec print_board (pb : (tile * player list) list) : unit =
  match pb with
  | [] -> print_endline "------------------\n"
  | h :: t ->
      let tile, plst = h in
      let name_list = List.map get_name plst in
      print_string (to_string tile);
      if not (name_list = []) then print_string "---" else print_string "";
      List.iter print_string name_list;
      print_endline "\n";
      print_board t

let print_state (s : state) = s.board |> pretty_board s.players |> print_board

(******************************************************************************)

(*********************************THE LOOP*************************************)

(**[action] is a dummy player that contains all the changes to be applied to the actual player
    As of now, the action only contains changes in player position*)
let action : player =
  print_string "hi";
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
    | h :: t ->
        let change = action in
        let new_player = update_player h change in
        new_player :: helper t
  in
  { s with players = helper s.players; current_run = s.current_run + 1 }

(*eval -> print loop*)
let rec loop (s : state) (eval : state -> state) : unit =
  if s.current_run > s.max_run then print_endline "Max run reached"
  else begin
    print_string "Press Enter > ";
    let input = read_line () in
    match input with
    | _ ->
        let s2 = s |> eval in
        print_state s2;
        loop s2 eval
  end
(******************************************************************************)
(********************************MAIN APP**************************************)

let () =
  print_endline "\n\n Welcome to Monopoly. \n";
  let start = initialize_players new_state in
  loop start single_turn

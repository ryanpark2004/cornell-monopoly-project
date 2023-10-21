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
  { board = Board.new_board; players = []; max_run = 9; current_run = 0 }

(**[add_players] is a new state with new players added. 
    The number of players must be less than or equal to 4,
     or invalid argument exception is raised. The name of each 
     player must be nonempty and 10 character long or less*)
let initialize_players (s : state) : state =
  print_string "How many players? > ";
  let num_players = try int_of_string (read_line ()) with _ -> 0 in
  if num_players <= 0 || num_players > 1 then begin
    print_endline "Invalid player amount (Max 1).";
    exit 0
  end
  else
    let rec helper n =
      match n with
      | 0 -> []
      | x ->
          let () =
            print_string ("\nName of Player " ^ string_of_int x ^ " > ")
          in
          let player_name = read_line () in
          let name_length = String.length player_name in
          if name_length <= 0 || name_length > 10 then begin
            print_endline "Invalid name (0 < Length <= 10).";
            exit 0
          end
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
    | h :: t -> if pos = h.position + 1 then h :: helper t else helper t
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
  | [] -> print_endline "-----------------------------\n"
  | (h1, h2) :: t ->
      let tile, plst = (h1, h2) in
      print_string ("| " ^ to_string tile);
      if not (h2 = []) then print_string " --- " else print_string "";
      let rec print_players name_list =
        match name_list with
        | [] -> ()
        | h :: t ->
            print_string h.name;
            if h.in_jail then print_string " \027[31m[IN JAIL]\027[0m";
            print_string (" ($" ^ string_of_int h.money ^ ")");
            print_players t
      in
      print_players h2;
      print_endline "\n|";
      print_board t

let print_board_frame pb =
  print_endline "------- UPDATED BOARD ------- \n|";
  print_board pb

let print_state (s : state) =
  s.board |> pretty_board s.players |> print_board_frame

(******************************************************************************)

(*********************************THE LOOP*************************************)

(**[action] is a dummy player that contains all the changes to be applied to the actual player
    As of now, the action only contains changes in player position*)
let action player : player =
  let n = rollDice () in
  print_endline ("You rolled a " ^ string_of_int n ^ "!\n");
  { name = "info"; money = 0; properties = []; position = n; in_jail = false }

(**[update_player] is the new player whose fields have changed using [info]*)
let update_player (p : player) (info : player) : player =
  {
    p with
    money =
      (if p.position + info.position >= 6 then (
         print_endline "Passed Go: Collect $200";
         p.money + info.money + 200)
       else p.money + info.money);
    properties = p.properties @ info.properties;
    position = (p.position + info.position) mod 6;
    in_jail = info.in_jail;
  }

(**[single_turn] is an updated state after every player performed an action *)
let single_turn (s : state) : state =
  let rec helper (plst : player list) : player list =
    match plst with
    | [] ->
        print_endline
          ("Turn #" ^ string_of_int (s.current_run + 1) ^ " ended. \n");
        []
    | h :: t ->
        let change = action h in
        let changed_player = update_player h change in
        let new_player =
          match List.nth s.board changed_player.position with
          | tile, n -> tile_action tile changed_player
        in
        new_player :: helper t
  in
  { s with players = helper s.players; current_run = s.current_run + 1 }

(*eval -> print loop*)
let rec loop (s : state) (eval : state -> state) : unit =
  if s.current_run > s.max_run then
    print_endline "Max run reached. Thanks for playing!"
  else begin
    print_string "\nPress [ENTER] to roll the dice > ";
    let input = read_line () in
    match input with
    | "" ->
        let s2 = s |> eval in
        print_state s2;
        loop s2 eval
    | _ -> print_string "Exiting game. Thanks for playing!"
  end
(******************************************************************************)
(********************************MAIN APP**************************************)

let () =
  print_endline
    "\n\n\n\027[32m         ___                      _ _               ";
  print_endline "        / __\\___  _ __ _ __   ___| | |              ";
  print_endline "       / /  / _ \\| '__| '_ \\ / _ \\ | |              ";
  print_endline "      / /__| (_) | |  | | | |  __/ | |              ";
  print_endline "      \\____/\\___/|_|  |_| |_|\\___|_|_|              ";
  print_endline "                                              ";
  print_endline "                                      _       ";
  print_endline "  /\\/\\   ___  _ __   ___  _ __   ___ | |_   _ ";
  print_endline " /    \\ / _ \\| '_ \\ / _ \\| '_ \\ / _ \\| | | | |";
  print_endline "/ /\\/\\ \\ (_) | | | | (_) | |_) | (_) | | |_| |";
  print_endline "\\/    \\/\\___/|_| |_|\\___/| .__/ \\___/|_|\\__, |";
  print_endline "                         |_|            |___/ \027[0m\n\n";
  print_endline
    "Welcome to Cornell Monopoly! \n\
     For more details about how to play, look at the README.md file.\n\
     To begin playing, answer the prompts below.\n\n";
  Random.self_init ();
  let start = initialize_players new_state in
  loop start single_turn

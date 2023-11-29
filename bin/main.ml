open Monopoly
open Board
open Player
open Utils

(*******************************Helpers**************************************)
type state = {
  mutable board : board;
  mutable players : player list;
  max_run : int;
  current_run : int;
}

let new_state : state =
  { board = Board.new_board; players = []; max_run = 5; current_run = 0 }

let name_to_players (nlst : string list) : player list =
  List.map (fun n -> create_player n) nlst

(**[add_players] is a new state with new players added. 
    The number of players must be less than or equal to 4,
     or invalid argument exception is raised. The name of each 
     player must be nonempty and 10 character long or less*)
let initialize_players (s : state) : state =
  print_string "How many players? > ";
  let num_players = try int_of_string (read_line ()) with _ -> 0 in
  if num_players <= 0 || num_players > 4 then begin
    print_endline "Invalid player amount (Max 4).";
    exit 0
  end
  else
    let rec helper (n : int) (acc : string list) : player list =
      if n = 0 then name_to_players acc
      else begin
        Printf.printf "\n Name of Player %d: " n;
        let name = read_line () in
        if String.length name <= 0 || String.length name > 10 then (
          print_endline "\nInvalid name (0 < length <= 10)";
          helper n acc)
        else if List.mem name acc then (
          Printf.printf "\nPlayer %s already exists" name;
          helper n acc)
        else helper (n - 1) (acc @ [ name ])
      end
    in
    { s with players = helper num_players [] }
(******************************************************************************)

(******************************** PRINTS **************************************)

(**[occupants] is a list of players occupying tile [t] on current state [s]. Returns [None] is no player on [t]*)
let occupants (s : state) (t : tile) : player list option =
  match List.filter (fun p -> p.position = pos_of_tile t) s.players with
  | [] -> None
  | h :: t -> Some (h :: t)

type pretty_tile = {
  tile : tile;
  pos : int;
  plst : player list option;
}

(**[pretty_board] is a list of pretty_tile in given state [s]*)
let pretty_board (s : state) : pretty_tile list =
  List.map (fun (t, n) -> { tile = t; pos = n; plst = occupants s t }) s.board

(**[print_board] prints the board in these steps:
  * 1. generate pretty_board
  * 2. iterate through pretty board and convert occupants into string
  * 3. for each iteration, print |tile --- names*)
let print_board (s : state) : unit =
  let concat (plst : player list option) : string =
    match plst with
    | None -> ""
    | Some lst ->
        List.map (fun p -> p.name ^ ", ") lst |> List.fold_left ( ^ ) ""
  in
  let iter_helper (pt : pretty_tile) : unit =
    match pt.plst with
    | None -> Printf.printf "| %s \n" (to_string pt.tile)
    | Some _ ->
        Printf.printf "| %s --- %s \n" (to_string pt.tile) (concat pt.plst)
  in
  print_endline "\n----------BOARD--------\n";
  List.iter iter_helper (pretty_board s)

(**[print_status] prints the current status of the players. It displays
  * their money and properties.*)
let print_status (s : state) : unit =
  let iter_helper (p : player) : unit =
    if p.in_jail > 0 then
      Printf.printf "|%s   $%d   %d \n" p.name p.money p.in_jail
    else Printf.printf "|%s   $%d \n" p.name p.money
  in
  print_endline "\n-------STATUS-------\n";
  List.iter iter_helper s.players

let print_state (s : state) : unit =
  print_board s;
  print_status s

(******************************************************************************)
(*********************************THE LOOP*************************************)

let loop = failwith "unimplemented"
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
  loop start

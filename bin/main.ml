open Monopoly
open Board
open Player
open Utils

type state = {
  board : board;
  mutable players : player list;
  max_run : int;
  current_run : int;
}

let new_state : state =
  { board = Board.new_board; players = []; max_run = 20; current_run = 1 }

(***************************Debug********************************************)

let debug = true

let print_player (p : player) : unit =
  Printf.printf "Name: %s | Money: %i\n" p.name p.money

let print_players (s : state) : unit =
  if debug = true then List.iter print_player s.players else ()

(****************************************************************************)

(*******************************Helpers**************************************)
let name_to_players (nlst : string list) : player list =
  List.map (fun n -> create_player n) nlst

(**[add_players] is a new state with new players added. 
    The number of players must be less than or equal to 4,
     or invalid argument exception is raised. The name of each 
     player must be nonempty and 10 character long or less*)
let rec initialize_players (s : state) : state =
  print_string "How many players? > ";
  let num_players = try int_of_string (read_line ()) with _ -> 0 in
  if num_players <= 0 || num_players > 4 then begin
    print_endline "Invalid player amount (Max 4).";
    initialize_players s
  end
  else
    let rec helper (n : int) (acc : string list) : player list =
      if n = 0 then name_to_players acc
      else begin
        Printf.printf "\nName of Player %d: " (num_players - n + 1);
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

(*Takes out the contents of the reference lists*)
let deref lst = List.map (fun p -> !p) lst

(**[occupants] is a list of players occupying tile [t] on current state [s]. Returns [None] is no player on [t]*)
let occupants (s : state) (t : tile) : player list option =
  match List.filter (fun p -> p.position = pos_of_tile t) s.players with
  | [] -> None
  | lst -> Some lst

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
        String.concat ", "
          (List.map
             (fun p ->
               p.name
               ^ if p.in_jail > 0 then " \027[31m[IN JAIL]\027[0m" else "")
             lst)
  in
  let iter_helper (pt : pretty_tile) : unit =
    match pt.plst with
    | None -> Printf.printf "| %s \n" (to_string pt.tile)
    | Some _ ->
        Printf.printf "| %s --- %s \n" (to_string pt.tile) (concat pt.plst)
  in
  print_endline "\n\n------------BOARD-----------";
  List.iter iter_helper (pretty_board s)

(**[print_status] prints the current status of the players. It displays
  * their money and properties.*)
let print_status (s : state) : unit =
  print_endline "\n-----------STATUS-----------";
  let iter_helper (p : player) : unit =
    if p.in_jail > 0 then
      Printf.printf "| %s  \027[32m $%d \027[0m \027[31m%d \027[0m" p.name
        p.money p.in_jail
    else Printf.printf "| %s  \027[32m $%d \027[0m" p.name p.money
  in
  List.iter iter_helper s.players;
  print_string "| \n"

let print_state (s : state) : unit =
  print_board s;
  print_status s

(******************************************************************************)
(*********************************THE LOOP*************************************)
let rec roll (p : player) : player =
  Printf.printf "%s's turn: [Enter] : Roll | [I]: Info | [Q]: Quit\n" p.name;
  match read_line () with
  | "" ->
      let n = rollDice () in
      Printf.printf "%s rolled %i\n" p.name n;
      move_player p n
  | "I" | "i" -> failwith "Todo"
  | "Q" | "q" -> begin
      print_endline "Quitting...";
      exit 0
    end
  | _ ->
      print_endline "Invalid Input";
      roll p

(*Before each turn, the state is printed. After each turn, state is updated*)

(**[replace] replaces the instance of [p] in [lst] with [p]
  This is used to update the state of the game each turn as opposed to each round.*)
let replace (lst : player list) (p : player) =
  List.map (fun e -> if e.name = p.name then p else e) lst

(**[turn] returns a player after rolling dice and performing an action.
    It also mutates the state to reflect the change.
    a jailed player goes through [jailed_turn] instead.*)
let rec turn (s : state) (p : player) : player =
  print_state s;
  if p.in_jail > 0 then jailed_turn s p
  else begin
    let rolled = roll p in
    let tile = tile_of_pos new_board rolled.position in
    let new_p = tile_action tile rolled in
    match new_p with
    | [ p ] ->
        let replaced = replace s.players p in
        s.players <- replaced;
        p
    | _ -> failwith "Unreachable"
  end

and jailed_turn (s : state) (p : player) : player =
  Printf.printf "%s is in jail. Remaining turns: %i/%i\n" p.name p.in_jail 3;
  Printf.printf "Press [Y] to pay $200 and roll the dice. [N] to stay in jail: ";
  match read_line () with
  | "Y" | "y" -> turn s { p with money = p.money - 200; in_jail = 0 }
  | "N" | "n" -> { p with in_jail = p.in_jail - 1 }
  | _ ->
      Printf.printf "Invalid input \n";
      jailed_turn s p

let round (s : state) : player list = List.map (turn s) s.players

let rec loop (s : state) : unit =
  if s.current_run > s.max_run then begin
    print_state s;
    Printf.printf "\n\n Thank you for playing!\n\n"
  end
  else begin
    Printf.printf "\n\nRun #%i / %i" s.current_run s.max_run;
    loop { s with players = round s; current_run = s.current_run + 1 }
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
  loop start

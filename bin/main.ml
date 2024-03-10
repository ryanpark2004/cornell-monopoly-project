open Monopoly
open Board
open Player
open Utils

(*                                                                        *)
(*  Module: Main                                                          *)
(*                                                                        *)
(*  Description: This module implments the main script of the Cornell     *)
(*  Monopoly game. It keeps track of the game state, which is made up     *)
(*  of the board, players, and game status. It also implements the logic  *)
(*  for the main game loop, which is structured into rounds and turns for *)
(*  players. Finally, it provides detailed print functionality for the    *)
(*  game board and simple and detailed information.                       *)
(*                                                                        *)
(*  Authors: Ethan Baker, Bill Park, Ryan Park                            *)
(*  Date: December 10th 2023                                              *)
(**************************************************************************)
let debug = false

type state = {
  board : board;
  mutable players : player list;
}
(** State holds all of the current game information, notably the board and its 
    tiles, and the players with their current attributes.*)

(** [new_state] is the initial state at the beginning of the game*)
let new_state : state = { board = Board.new_board; players = [] }

(*******************************Helpers**************************************)

(** Maps the player names that are inputted to the user to players
      1, 2, 3, and 4, in that order. *)
let name_to_players (nlst : string list) : player list =
  List.map (fun n -> create_player n) nlst

(**[initialize_players] is a new state with new players added. 
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

(**[occupants] is a list of players occupying tile [t] on current state [s].
    Returns [None] is no player on [t]*)
let occupants (s : state) (t : tile) : player list option =
  match List.filter (fun p -> p.position = pos_of_tile t) s.players with
  | [] -> None
  | lst -> Some lst

type pretty_tile = {
  tile : tile;
  plst : player list option;
}
(** Representa a tile element, with extra information about the , 
     current occupants on it.*)

(**[pretty_board] is a list of [pretty_tile]s in given state [s]. *)
let pretty_board (s : state) : pretty_tile list =
  List.map (fun (t, _) -> { tile = t; plst = occupants s t }) s.board

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
    | None -> Printf.printf "| %s\n" (to_string pt.tile)
    | Some _ ->
        Printf.printf "| %s --- %s \n" (to_string pt.tile) (concat pt.plst)
  in
  print_endline "\n------------BOARD-----------";
  List.iter iter_helper (pretty_board s)

(**[print_status] prints the current status of the players. It displays
   their money and properties.*)
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

(** Roll controls the flow of the game when it is a players turn to roll. 
    It prompts the player for an input, then either rolls the dice and returns 
    the updated player, displays information, or quits the game. [roll] is 
    recursively called after displaying information or recieving an invalid 
    input.*)
let rec roll s (p : player) n : player =
  Printf.printf
    "\n\
     %s's turn: Press [Enter] to roll the dice, Press [I] for more information,\n\
     or Press [Q] to quit> " p.name;
  match read_line () with
  | "" ->
      Printf.printf "\n\n%s rolled a %i!\n" p.name n;
      move_player p n false
  | "I" | "i" ->
      print_endline (get_detailed_information s);
      roll s p n
  | "Q" | "q" -> begin
      print_endline "Quitting...";
      exit 0
    end
  | _ ->
      print_endline "Invalid Input";
      roll s p n

(** [get_detailed_information] returns a structured string containing a 
          list of players current information using the game state. 
          It shows the players' names, their balances, their in jail status, 
          and the properties that they currently own.*)
and get_detailed_information state =
  let intro =
    "\n------------------------DETAILED INFORMATION------------------------"
  in
  let rec player_info acc plist full_list =
    match plist with
    | [] -> acc
    | h :: t ->
        player_info
          (acc ^ "| "
          ^ n_spaces (longest_name full_list 0 - String.length h.name) ""
          ^ h.name ^ " - \027[32mBalance: $" ^ string_of_int h.money
          ^ "\027[0m, \027[31mJail Status: " ^ string_of_int h.in_jail
          ^ " turns\027[0m \n     "
          ^ n_spaces (String.length h.name) ""
          ^ n_spaces (longest_name full_list 0 - String.length h.name) ""
          ^ "\027[33mProperties: " ^ list_props h.properties "" ^ "\027[0m\n\n"
          )
          t full_list
  in
  intro ^ "\n" ^ player_info "" state.players state.players

(** [list_props] returns a string representation of the list of properties a 
    current player [p] owns.*)
and list_props p acc =
  match p with
  | [] -> if acc = "" then "None" else acc
  | h :: t ->
      let name =
        match h with
        | Location l -> l.name
        | Tcat_station t -> t.name
        | Utility u -> u.util_name
      in
      list_props t (if acc = "" then name else acc ^ ", " ^ name)

(** Returns a string with [n] spaces. Used to format detailed 
    information print statements.*)
and n_spaces n acc =
  match n with
  | 0 -> acc
  | _ -> n_spaces (n - 1) (acc ^ " ")

(** Returns the length of the longest player's name. Used in conjunction with 
    [n_spaces] to format the detailed information string.*)
and longest_name plist acc =
  match plist with
  | [] -> acc
  | h :: t ->
      if String.length h.name > acc then longest_name t (String.length h.name)
      else longest_name t acc

(**[replace_once] replaces the instance of [p] in [lst] with [p]
  This is used to update the state of the game each turn as opposed to each round.*)
let replace_once (lst : player list) (p : player) =
  List.map (fun e -> if e.name = p.name then p else e) lst

(** Replaces all players in the current list [lst] with new players of 
      the same name in [pl]. Players in the initial list that are not in 
      [pl] remain the same. *)
let rec replace_all (lst : player list) (pl : player list) =
  match pl with
  | [] -> lst
  | h :: t -> replace_all (replace_once lst h) t

(**[turn] returns a player and the new state after rolling dice and performing an action.
    It also mutates the state to reflect the change.
    a jailed player goes through [jailed_turn] instead.*)
let rec turn (s : state) (p : player) : state =
  if p.in_jail > 0 then jailed_turn s p
  else begin
    let n = rollDice () in
    let rolled = roll s p n in
    let tile = tile_of_pos new_board rolled.position in
    let plst = tile_action tile rolled s.players n false in
    s.players <- replace_all s.players plst;
    let plst2 = check_broke (List.hd plst) s.players debug in
    s.players <- plst2;
    print_state s;
    if List.length s.players = 1 then win_game s else s
  end

(** Displays a Winner message after a player has won the game, 
    and exits the program.*)
and win_game (s : state) : state =
  begin
    let winner = List.hd s.players in
    print_endline "\027[38;5;214m  _    _ _                       _";
    print_endline " | |  | (_)                     | |";
    print_endline " | |  | |_ _ __  _ __   ___ _ __| |";
    print_endline " | |/\\| | | '_ \\| '_ \\ / _ \\ '__| |";
    print_endline " | /  / | | | | | | | \\| _/_| | |_|";
    print_endline "  \\/  \\/|_|_| |_|_| |_|\\___||_| (_)";
    Printf.printf "\n\n Congratulations %s, you won! \n\n\027[0m" winner.name;
    exit 0
  end

(** Asks a player [p] if they would like to pay their way out of jail, if they 
    can afford it. If the player pays their way out of jail, they lose $200 and 
    reset their in_jail status, while playing their next turn normally. If they 
    do not, the player's in_jail status is reduced by one, and their turn is 
    over. Invalid inputs recursively call the function to ask the player again.*)
and jailed_turn (s : state) (p : player) : state =
  Printf.printf "%s is in jail. Remaining turns: %i/%i\n" p.name p.in_jail 3;
  if p.money >= 200 then (
    Printf.printf
      "Press [Y] to pay $200 and roll the dice, or press [N] to stay in jail> ";
    match read_line () with
    | "Y" | "y" -> turn s { p with money = p.money - 200; in_jail = 0 }
    | "N" | "n" ->
        s.players <- replace_once s.players { p with in_jail = p.in_jail - 1 };
        s
    | _ ->
        Printf.printf "Invalid input \n";
        jailed_turn s p)
  else (
    Printf.printf
      "You cannot afford your bail.\n\
       Press anything to wait your turn in jail > ";
    match read_line () with
    | _ ->
        s.players <- replace_once s.players { p with in_jail = p.in_jail - 1 };
        s)

(** Iterates over the players in the game, asking them to each play a turn. 
    State is updated in between every turn.*)
let round (s : state) : state =
  let st = ref s in
  let n = List.length !st.players in
  for i = 0 to n - 1 do
    let arr = Array.of_list !st.players in
    let new_s = turn !st arr.(i) in
    st := new_s
  done;
  !st

(** Calls round recursively until the game ends by the players quitting 
    or winning. Updates the state in between each round.*)
let rec loop (s : state) : unit =
  begin
    loop (round s)
  end

(******************************************************************************)
(********************************MAIN APP**************************************)

(** The main script for the Cornell Monopoly game. It first displays an ASCII
     art welcome message, then prompts the player to begin the game.*)
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
     To begin playing, answer the prompts below.\n\n";
  Random.self_init ();
  let start = initialize_players new_state in
  print_state start;
  loop start

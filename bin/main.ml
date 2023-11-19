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

  print_endline "\n------------------------\n";
  List.iter iter_helper (pretty_board s)

(**[print_status] prints the current status of the players. It displays
  * their money and properties.*)
let print_status (s : state) : unit =
  print_endline "\n-------STATUS-------\n";
  let iter_helper (p : player) : unit =
    if p.in_jail > 0 then
      Printf.printf "|%s   $%d   %d" p.name p.money p.in_jail
    else Printf.printf "|%s   $%d" p.name p.money
  in
  List.iter iter_helper s.players

(* match b with
     | [] -> print_endline "-----------------------------\n"
     | (h1, h2) :: t ->
         let tile, _ = (h1, h2) in
         print_string ("| " ^ to_string tile);
         if not (h2 = []) then print_string " --- " else print_string "";
         let rec print_players name_list =
           match name_list with
           | [] -> ()
           | h :: t ->
               print_string h.name;
               if h.in_jail > 0 then print_string " \027[31m[IN JAIL]\027[0m";
               print_string (" ($" ^ string_of_int h.money ^ ")");
               print_players t
         in
         print_players h2;
         print_endline "\n|";
         print_board t

   let print_board_frame pb =
     print_endline "------- UPDATED BOARD ------- \n|";
     print_board pb *)

let print_state (s : state) =
  print_board s;
  print_status s

(******************************************************************************)

(*********************************THE LOOP*************************************)
let debug = false

let print_player p =
  if not debug then p
  else (
    Printf.printf "\nDEBUG: name: %s | money: %d | pos: %d | in_jail: %d\n"
      p.name p.money p.position p.in_jail;
    p)

type info = {
  money : int;
  properties : int list;
  position : int;
  in_jail : int;
}
(**[info] is a record that has same fields as [player] but is mutable.*)

(**[update_player] is the new player whose fields have changed using [info]*)
let update_player (p : player) (info : info) : player =
  let new_pos = (p.position + info.position) mod 6 in
  {
    p with
    money = p.money + info.money;
    properties = p.properties @ info.properties;
    position = new_pos;
    in_jail =
      (if p.in_jail <= 0 && new_pos = 4 then 3 else p.in_jail + info.in_jail);
    (*if player was not in jail*)
  }

(**[normal_turn] returns an [info] of change based on decisions made in a usual 
    turn*)
let normal_turn (player : player) : info =
  print_string "\nPress [ENTER] to roll the dice > ";
  match read_line () with
  | _ ->
      let n = rollDice () in
      Printf.printf "\n%s rolled %d" player.name n;
      { money = 0; properties = []; position = n; in_jail = 0 }
(*add more actions here*)

(**[jailed_turn] returns an [info] based on the decisions the jailed
   player makes*)
let jailed_turn (player : player) : info =
  Printf.printf
    "\n\
     The player is in jail. Turns remaining: %d. \n\
     Press [Y] to pay $300 and roll dice" (player.in_jail - 1);
  match read_line () with
  | "Y" ->
      let new_info =
        {
          money = -300;
          properties = [];
          position = 0;
          in_jail = -1 * player.in_jail;
        }
      in
      let updated = update_player player new_info in
      normal_turn updated
  | _ -> { money = 0; properties = []; position = 0; in_jail = -1 }
(*still in jail*)

(**[turn] combines [normal_turn] and [jailed_turn] by checking
    the [p]'s [in_jail] count*)
let turn (p : player) : player =
  let info = if p.in_jail > 0 then jailed_turn p else normal_turn p in
  print_player (update_player (print_player p) info)

(**[loop] is the main game loop.
  * First, it checks if the game reached max number of runs.
  * If not, then it runs a secondary loop [sub_loop], iterating
  * over the player list [plst]. *)
let rec loop (s : state) : unit =
  if s.current_run > s.max_run then
    print_endline "\nMax run reached. Thanks for playing!"
  else
    let rec sub_loop (plst : player list) (s : state) =
      match plst with
      | [] ->
          Printf.printf "\nRun %d/%d ended." s.current_run s.max_run;
          loop { s with current_run = s.current_run + 1 }
      | h :: t ->
          Printf.printf "\nPlayer %s's turn." h.name;
          let new_plst =
            List.map (fun x -> if x.name = h.name then turn h else x) s.players
          in
          let new_state = { s with players = new_plst } in
          print_state new_state;
          sub_loop t new_state
    in
    sub_loop s.players s

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

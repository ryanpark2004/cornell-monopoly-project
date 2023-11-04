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
  if num_players <= 0 || num_players > 4 then begin
    print_endline "Invalid player amount (Max 4).";
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

(**[pretty_board] is a board where each tile is associated with all the players 
    standing on it*)
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
  print_board pb

let print_state (s : state) =
  s.board |> pretty_board s.players |> print_board_frame

(******************************************************************************)

(*********************************THE LOOP*************************************)

type info = {
  mutable money : int;
  mutable properties : int list;
  mutable position : int;
  mutable in_jail : int;
}
(**[info] is a record that has same fields as [player] but is mutable.*)

(**[update_player] is the new player whose fields have changed using [info]*)
let update_player (p : player) (info : info) : player =
  {
    p with
    money =
      (if p.position + info.position >= 6 then (
         print_endline "Passed Go: Collect $200";
         p.money + info.money + 200)
       else p.money + info.money);
    properties = p.properties @ info.properties;
    position = (p.position + info.position) mod 6;
    in_jail = p.in_jail + info.in_jail;
  }

(**[normal_turn] returns an [info] of change based on decisions made in a usual 
    turn*)
let normal_turn (player : player) : info =
  let player_info : info =
    {
      money = player.money;
      properties = player.properties;
      position = player.position;
      in_jail = player.in_jail;
    }
  in
  print_string "\nPress [ENTER] to roll the dice > ";
  match read_line () with
  | _ ->
      let n = rollDice () in
      Printf.printf "You rolled %d" n;
      player_info.position <- player_info.position + n;
      player_info (*add more actions here*)

(**[jailed_turn] returns an [info] based on the decisions the jailed
   player makes*)
let jailed_turn (player : player) : info =
  let player_info : info =
    {
      money = player.money;
      properties = player.properties;
      position = player.position;
      in_jail = player.in_jail;
    }
  in
  Printf.printf
    "The player is in jail. Turns remaining: %d. Press [Y] to pay $300 and \
     roll dice"
    (player_info.in_jail - 1);
  match read_line () with
  | "Y" ->
      player_info.money <- player_info.money - 300;
      player_info.in_jail <- 0;
      let updated = update_player player player_info in
      normal_turn updated
  | _ ->
      player_info.in_jail <- player_info.in_jail - 1;
      player_info

(**[turn] combines [normal_turn] and [jailed_turn] by checking
    the [p]'s [in_jail] count*)
let turn (p : player) : player =
  let info = if p.in_jail > 0 then jailed_turn p else normal_turn p in
  update_player p info

(**[loop] is the main game loop.
  * First, it checks if the game reached max number of runs.
  * If not, then it runs a secondary loop [sub_loop], iterating
  * over the player list [plst]. *)
let rec loop (s : state) : unit =
  if s.current_run > s.max_run then
    print_endline "Max run reached. Thanks for playing!"
  else
    let rec sub_loop (plst : player list) =
      match plst with
      | [] ->
          Printf.printf "Run %d/%d ended." s.current_run s.max_run;
          loop { s with current_run = s.current_run + 1 }
      | h :: t ->
          Printf.printf "Player %s's turn." h.name;
          let new_plst =
            List.map (fun x -> if x.name = h.name then turn h else x) s.players
          in
          let new_state = { s with players = new_plst } in
          print_state new_state;
          sub_loop t
    in
    sub_loop s.players

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

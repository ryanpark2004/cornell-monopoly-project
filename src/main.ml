open Board
open Player
open Printf

type state = { board : Board.board; players : Player.player list }

let play = failwith "Unimplemented"
let update = failwith "Unimplemented"
let print_state state = failwith "Unimplemented"

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

open Board

let debug = true

type player = {
  name : string;
  mutable money : int;
  mutable properties : property list;
  position : int;
  in_jail : int;
}

let debug_player =
  { name = "DEBUG"; money = 100; properties = []; position = 0; in_jail = 0 }

let create_player (name : string) : player =
  if debug then { debug_player with name = debug_player.name ^ ": " ^ name }
  else { name; money = 1000; properties = []; position = 0; in_jail = 0 }

let get_name player = player.name

let receive_money player (bonus : int) : player =
  { player with money = player.money + bonus }

let move_player (player : player) (n : int) : player =
  let new_pos = player.position + n in
  if new_pos < length new_board then
    { player with position = (player.position + n) mod length new_board }
  else (
    print_endline "You passed Go, collect $100!";
    {
      player with
      position = (player.position + n) mod length new_board;
      money = player.money + 100;
    })

open Exceptions
open Printf
open Board

type player = {
  name : string;
  mutable money : int;
  mutable properties : property list;
  position : int;
  in_jail : int;
}

let create_player (name : string) : player =
  { name; money = 5000; properties = []; position = 0; in_jail = 0 }

let get_name player = player.name

let buy_property prop player : player =
  let rent = calculated_rent prop in
  if player.money >= rent then { player with money = player.money - rent }
  else
    raise
      (Not_Enough_Money
         (Printf.sprintf "Needed %i but owns %i" rent player.money))

let receive_money player (bonus : int) : player =
  { player with money = player.money + bonus }

let move_player (player : player) (n : int) : player =
  { player with position = (player.position + n) mod length new_board }

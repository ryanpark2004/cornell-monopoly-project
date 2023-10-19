type player = {
  name : string;
  money : int;
  properties : int list;
  position : int;
  in_jail : bool;
}

let create_player (title : string) : player =
  { name = title; money = 5000; properties = []; position = 0; in_jail = false }

let get_name player = player.name

let move_player player steps =
  { player with position = player.position + steps }

let buy_property player rent : player =
  if player.money >= rent then
    {
      player with
      money = player.money - rent;
      properties = player.position :: player.properties;
    }
  else invalid_arg "Player does not have sufficient funds"

let receive_money player (bonus : int) : player =
  { player with money = player.money + bonus }

let jail_send player = { player with in_jail = true; position = 15 }

let jail_escape player =
  if player.in_jail then { player with in_jail = false }
  else invalid_arg "Player is not in jail"

let move_player (player : player) (n : int) : player =
  { player with position = player.position + n }

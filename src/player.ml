type player = {
  name : string;
  money : int;
  properties : int list;
  position : int;
  in_jail : int;
}

let create_player (name : string) : player =
  { name; money = 5000; properties = []; position = 0; in_jail = 0 }

let get_name player = player.name

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

let move_player (player : player) (n : int) : player =
  { player with position = player.position + n }

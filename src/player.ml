open Board

module type PlayerType = sig
  type player

  val create_player : string -> player
  val move_player : player -> int -> player
  val buy_property : player -> location -> player
  val receive_money : player -> int -> player
  val jail_send : player -> player
  val jail_escape : player -> player
end

module MyPlayer : PlayerType = struct
  type player = {
    name : string;
    money : int;
    properties : location list;
    position : int;
    in_jail : bool;
  }

  let create_player (title : string) : player =
    {
      name = title;
      money = 5000;
      properties = [];
      position = 0;
      in_jail = false;
    }

  let move_player player steps =
    { player with position = player.position + steps }

  let buy_property player property : player =
    if player.money >= property.base_rent then
      {
        player with
        money = player.money - property.base_rent;
        properties = property :: player.properties;
      }
    else invalid_arg "Player does not have sufficient funds"

  let receive_money player (bonus : int) : player =
    { player with money = player.money + bonus }

  let jail_send player = { player with in_jail = true; position = 15 }

  let jail_escape player =
    if player.in_jail then { player with in_jail = false }
    else invalid_arg "Player is not in jail"
end

open OUnit2
open Monopoly
open Board
open Player
open Exceptions
open Utils
open List

(********************************************************************
     (* Test Plan   *)
 ********************************************************************)
(* This is the test suite for our board game: Cornell Monopoly.
   TO DOOOO *)

(********************************************************************
     (* Board Test Cases *)
 ********************************************************************)

let locations =
  Array.of_list
    [ { name = "LocationTest"; price = 40; rent = 8; mortgage = 20 } ]

let stations =
  Array.of_list [ { name = "StationTest"; price = 200; mortgage = 100 } ]

let utilities =
  Array.of_list [ { util_name = "UtilityTest"; price = 150; mortgage = 50 } ]

let test_board_0 = []
let test_board_1_help = [ Start ]

let test_board_1 =
  let rec indices (n : int) (lst : tile list) : int list =
    match lst with
    | [] -> []
    | _ :: t -> n :: indices (n + 1) t
  in
  List.combine test_board_1_help (indices 0 test_board_1_help)

let test_board_2_help =
  [
    Start;
    Tax 100;
    Chance;
    Chest;
    Parking;
    Jail;
    Property (Location locations.(0));
  ]

let test_board_2 =
  let rec indices (n : int) (lst : tile list) : int list =
    match lst with
    | [] -> []
    | _ :: t -> n :: indices (n + 1) t
  in
  List.combine test_board_2_help (indices 0 test_board_2_help)

let pos_of_tile (t : tile) : int =
  let rec helper (lst : board) (t : tile) : int =
    match lst with
    | [] -> raise No_Such_Tile
    | (tile, idx) :: tl -> if tile = t then idx else helper tl t
  in
  helper test_board_2 t

let pos_of_tile2 (t : tile) : int =
  let rec helper (lst : board) (t : tile) : int =
    match lst with
    | [] -> raise No_Such_Tile
    | (tile, idx) :: tl -> if tile = t then idx else helper tl t
  in
  helper test_board_1 t

(*Helper Function: Testing if board length is valid.*)
let test_length name out in1 = name >:: fun _ -> assert_equal out (length in1)

let board_suite =
  [
    (*Board Test Cases: length*)
    test_length "length of 0" 0 test_board_0;
    test_length "length of 1" 1 test_board_1;
    test_length "length of >1" 7 test_board_2;
    (*Board Test Cases: pos_of_tile *)
    ("pos_of_tile Start" >:: fun _ -> assert_equal 0 (pos_of_tile Start));
    ("pos_of_tile Tax" >:: fun _ -> assert_equal 1 (pos_of_tile (Tax 100)));
    ("pos_of_tile Chance" >:: fun _ -> assert_equal 2 (pos_of_tile Chance));
    ("pos_of_tile Chest" >:: fun _ -> assert_equal 3 (pos_of_tile Chest));
    ("pos_of_tile Parking" >:: fun _ -> assert_equal 4 (pos_of_tile Parking));
    ("pos_of_tile Jail" >:: fun _ -> assert_equal 5 (pos_of_tile Jail));
    (*Board Test Cases: pos_of_tile_fail *)
    ( "pos_of_tile Fail: No Tax" >:: fun _ ->
      let exn = No_Such_Tile in
      assert_raises exn (fun () ->
          try pos_of_tile2 (Tax 100) with exn -> raise exn) );
    ( "pos_of_tile Fail: No Chance" >:: fun _ ->
      let exn = No_Such_Tile in
      assert_raises exn (fun () ->
          try pos_of_tile2 Chance with exn -> raise exn) );
    ( "pos_of_tile Fail: No Chest" >:: fun _ ->
      let exn = No_Such_Tile in
      assert_raises exn (fun () ->
          try pos_of_tile2 Chest with exn -> raise exn) );
    ( "pos_of_tile Fail: No Parking" >:: fun _ ->
      let exn = No_Such_Tile in
      assert_raises exn (fun () ->
          try pos_of_tile2 Parking with exn -> raise exn) );
    ( "pos_of_tile Fail: No Jail" >:: fun _ ->
      let exn = No_Such_Tile in
      assert_raises exn (fun () ->
          try pos_of_tile2 Jail with exn -> raise exn) );
    (*Board Test Cases: title_of_pos *)
    ( "title_of_pos Start" >:: fun _ ->
      assert_equal Start (tile_of_pos test_board_2 0) );
    ( "title_of_pos Tax" >:: fun _ ->
      assert_equal (Tax 100) (tile_of_pos test_board_2 1) );
    ( "title_of_pos Chance" >:: fun _ ->
      assert_equal Chance (tile_of_pos test_board_2 2) );
    ( "title_of_pos Chest" >:: fun _ ->
      assert_equal Chest (tile_of_pos test_board_2 3) );
    ( "title_of_pos Parking" >:: fun _ ->
      assert_equal Parking (tile_of_pos test_board_2 4) );
    ( "title_of_pos Jail" >:: fun _ ->
      assert_equal Jail (tile_of_pos test_board_2 5) );
    (*Board Test Cases: property_to_string *)
    ( "property_to_string Location" >:: fun _ ->
      assert_equal
        ~printer:(fun string -> string)
        "LocationTest"
        (property_to_string (Location locations.(0))) );
    ( "property_to_string Station" >:: fun _ ->
      assert_equal
        ~printer:(fun string -> string)
        "StationTest"
        (property_to_string (Tcat_station stations.(0))) );
    ( "property_to_string Utility" >:: fun _ ->
      assert_equal
        ~printer:(fun string -> string)
        "UtilityTest"
        (property_to_string (Utility utilities.(0))) )
    (*Board Test Cases: to_string *);
    ( "to_string Go" >:: fun _ ->
      assert_equal
        ~printer:(fun string -> string)
        "\027[32mGo\027[0m"
        (to_string (List.nth test_board_2_help 0)) );
    ( "to_string Tax" >:: fun _ ->
      assert_equal
        ~printer:(fun string -> string)
        ("\027[38;5;202mTax ($" ^ string_of_int 100 ^ ")\027[0m")
        (to_string (List.nth test_board_2_help 1)) );
    ( "to_string Chance" >:: fun _ ->
      assert_equal
        ~printer:(fun string -> string)
        "\027[35mChance\027[0m"
        (to_string (List.nth test_board_2_help 2)) );
    ( "to_string Chest" >:: fun _ ->
      assert_equal
        ~printer:(fun string -> string)
        "\027[34mChest\027[0m"
        (to_string (List.nth test_board_2_help 3)) );
    ( "to_string Parking" >:: fun _ ->
      assert_equal
        ~printer:(fun string -> string)
        "\027[36mFree Parking\027[0m"
        (to_string (List.nth test_board_2_help 4)) );
    ( "to_string Jail" >:: fun _ ->
      assert_equal
        ~printer:(fun string -> string)
        "\027[31mJail\027[0m"
        (to_string (List.nth test_board_2_help 5)) );
    ( "to_string Property" >:: fun _ ->
      assert_equal
        ~printer:(fun string -> string)
        ("\027[33m" ^ property_to_string (Location locations.(0)) ^ "\027[0m")
        (to_string (List.nth test_board_2_help 6)) );
  ]

(********************************************************************
     (* Player Test Cases *)
 ********************************************************************)

let p1 = create_player "p1"
let p2 = create_player "p2"
let p3 = create_player "p3"
let p4 = create_player "p4"

let move_player (player : player) (n : int) : player =
  { player with position = (player.position + n) mod length test_board_2 }

let player_suite =
  [
    (*Player Test Cases: Player Attributes *)
    ("create_player money check" >:: fun _ -> assert_equal 5000 p1.money);
    ("create_player jail check" >:: fun _ -> assert_equal 0 p1.in_jail);
    ("create_player properties check" >:: fun _ -> assert_equal [] p1.properties);
    ("create_player position check" >:: fun _ -> assert_equal 0 p1.position);
    (*Player Test Cases: get_name *)
    ("get_name player 1" >:: fun _ -> assert_equal "p1" (get_name p1));
    ("get_name player 2" >:: fun _ -> assert_equal "p2" (get_name p2));
    ("get_name player 3" >:: fun _ -> assert_equal "p3" (get_name p3));
    ("get_name player 4" >:: fun _ -> assert_equal "p4" (get_name p4));
    (*Player Test Cases: receive_money  *)
    ("receie_money $0" >:: fun _ -> assert_equal 5000 (receive_money p1 0).money);
    ( "receie_money <$0" >:: fun _ ->
      assert_equal 4900 (receive_money p1 (-100)).money );
    ( "receie_money >$0" >:: fun _ ->
      assert_equal 5500 (receive_money p1 500).money );
    (*Player Test Cases: move_player  *)
    ( "move_player 0 spaces" >:: fun _ ->
      assert_equal 0 (move_player p1 0).position );
    ( "move_player 1 spaces" >:: fun _ ->
      assert_equal 1 (move_player p1 1).position );
    ( "move_player >1 spaces" >:: fun _ ->
      assert_equal 3 (move_player p1 3).position );
    ( "move_player > board.length spaces" >:: fun _ ->
      assert_equal 1 (move_player p1 8).position );
  ]

(********************************************************************
     (* Uitility Test Cases *)
 ********************************************************************)
let dice_bound = 3
let rollDice () : int = 1 + Random.int dice_bound

let utils_suite =
  [
    (*Utility Test Cases: roll_move  *)
    ( "roll_move test" >:: fun _ ->
      assert_bool "Error"
        (let x = rollDice () in
         x >= 1 && x <= 4) );
    (*Utility Test Cases: pullChance  *)
    (*Utility Test Cases: pullChest  *)
    (*Utility Test Cases: tileAction  *)
  ]
(********************************************************)

let tests =
  "monopoly suite" >::: List.flatten [ board_suite; player_suite; utils_suite ]

let _ = run_test_tt_main tests

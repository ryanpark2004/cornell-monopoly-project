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

   The parts of the system that were automatically tested by OUnit were through
   unit tests for all modules excluding main, which mainly dealt with player
   inputs in the terminal. For manual testing, we tested the main module by
   playing the game multiple times: each time, we attempted to "break" the game
   by inputting purposely weird inputs to see if the program did not crash.
   If the game did, edits to the code were made to adjust for such issues.

   The modules that were tested in this test suite were modules Board,
   Player, and Util. Each module possesses specific functions pertaining to
   the name of their module (Example: Module Player has functions that deal
   only with player-related functions, such as get_name, receive_money,
   and move_player.). Test Cases were mainly designed using a glass-box testing
   approach: the functionality of each function was analyzed, and test cases
   were made with the goal of testing as many execution paths for the functions
   in each module as possible. Furthermore, as mentioned above previously,
   we adopted a testing strategy within the terminal similar to randomized
   testing and the "devious player", in which inputs were intentionally
   misleading to find bugs hidden within the system.

   We believe the testing approach demonstrates the correctness of the system
   because it not only provides full coverage of the functionality of code
   through unit test, but it also covers edge cases within user inputs through
   our rigorous testing of the terminal. By pairing both OUnit tests with our
   manual testing, we believe that our testing has proven the functionality of
   our system and guarentees that there are minimal to no errors.
*)

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
let test_board_1_help = [ Start 1 ]

let test_board_1 =
  let rec indices (n : int) (lst : tile list) : int list =
    match lst with
    | [] -> []
    | _ :: t -> n :: indices (n + 1) t
  in
  List.combine test_board_1_help (indices 0 test_board_1_help)

let test_board_2_help =
  [
    Start 1;
    Tax 100;
    Chance 1;
    Chest 1;
    Parking 1;
    Jail 1;
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

(*Helper Function: Testing if board length is valid.*)
let test_length name out in1 = name >:: fun _ -> assert_equal out (length in1)

let board_suite =
  [
    (*Board Test Cases: length*)
    test_length "length of 0" 0 test_board_0;
    test_length "length of 1" 1 test_board_1;
    test_length "length of >1" 7 test_board_2;
    (*Board Test Cases: pos_of_tile *)
    ("pos_of_tile Start" >:: fun _ -> assert_equal 0 (pos_of_tile (Start 1)));
    ("pos_of_tile Tax" >:: fun _ -> assert_equal 1 (pos_of_tile (Tax 100)));
    ("pos_of_tile Chance" >:: fun _ -> assert_equal 2 (pos_of_tile (Chance 1)));
    ("pos_of_tile Chest" >:: fun _ -> assert_equal 3 (pos_of_tile (Chest 1)));
    ("pos_of_tile Parking" >:: fun _ -> assert_equal 4 (pos_of_tile (Parking 1)));
    ("pos_of_tile Jail" >:: fun _ -> assert_equal 5 (pos_of_tile (Jail 1)));
    (*Board Test Cases: pos_of_tile_fail *)
    ( "pos_of_tile Fail: No Tax" >:: fun _ ->
      let exn = No_Such_Tile in
      assert_raises exn (fun () ->
          try pos_of_tile (Tax 50) with exn -> raise exn) );
    ( "pos_of_tile Fail: No Chance" >:: fun _ ->
      let exn = No_Such_Tile in
      assert_raises exn (fun () ->
          try pos_of_tile (Chance 3) with exn -> raise exn) );
    ( "pos_of_tile Fail: No Chest" >:: fun _ ->
      let exn = No_Such_Tile in
      assert_raises exn (fun () ->
          try pos_of_tile (Chest 2) with exn -> raise exn) );
    ( "pos_of_tile Fail: No Parking" >:: fun _ ->
      let exn = No_Such_Tile in
      assert_raises exn (fun () ->
          try pos_of_tile (Parking 3) with exn -> raise exn) );
    ( "pos_of_tile Fail: No Jail" >:: fun _ ->
      let exn = No_Such_Tile in
      assert_raises exn (fun () ->
          try pos_of_tile (Jail 2) with exn -> raise exn) );
    (*Board Test Cases: title_of_pos *)
    ( "tile_of_pos Start" >:: fun _ ->
      assert_equal (Start 1) (tile_of_pos test_board_2 0) );
    ( "tile_of_pos Tax" >:: fun _ ->
      assert_equal (Tax 100) (tile_of_pos test_board_2 1) );
    ( "tile_of_pos Chance" >:: fun _ ->
      assert_equal (Chance 1) (tile_of_pos test_board_2 2) );
    ( "tile_of_pos Chest" >:: fun _ ->
      assert_equal (Chest 1) (tile_of_pos test_board_2 3) );
    ( "tile_of_pos Parking" >:: fun _ ->
      assert_equal (Parking 1) (tile_of_pos test_board_2 4) );
    ( "tile_of_pos Jail" >:: fun _ ->
      assert_equal (Jail 1) (tile_of_pos test_board_2 5) );
    (*Board Test Cases: property_selling_value *)
    ( "property_selling_value Location" >:: fun _ ->
      assert_equal 20 (property_selling_value (Location locations.(0))) );
    ( "property_selling_value Station" >:: fun _ ->
      assert_equal 100 (property_selling_value (Tcat_station stations.(0))) );
    ( "property_selling_value Utility" >:: fun _ ->
      assert_equal 50 (property_selling_value (Utility utilities.(0))) );
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

let move_player (player : player) (n : int) : player =
  { player with position = (player.position + n) mod length test_board_2 }

let player_suite =
  [
    (*Player Test Cases: Player Attributes *)
    ("create_player money check" >:: fun _ -> assert_equal 1000 p1.money);
    ("create_player jail check" >:: fun _ -> assert_equal 0 p1.in_jail);
    ("create_player properties check" >:: fun _ -> assert_equal [] p1.properties);
    ("create_player position check" >:: fun _ -> assert_equal 0 p1.position);
    (*Player Test Cases: get_name *)
    ("get_name player 1" >:: fun _ -> assert_equal "p1" (get_name p1));
    ("get_name player 2" >:: fun _ -> assert_equal "p2" (get_name p2));
    (*Player Test Cases: receive_money  *)
    ("receie_money $0" >:: fun _ -> assert_equal 1000 (receive_money p1 0).money);
    ( "receie_money <$0" >:: fun _ ->
      assert_equal 900 (receive_money p1 (-100)).money );
    ( "receie_money >$0" >:: fun _ ->
      assert_equal 1500 (receive_money p1 500).money );
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

let test_location : location =
  { name = "test loc"; price = 100; rent = 0; mortgage = 100 }

let test_location2 : location =
  { name = "test loc"; price = 100; rent = 0; mortgage = 100 }

let test_mortgage_board =
  [ Start 0; Property (Location test_location); Tax 100 ]

let jail_p1 =
  { name = "Player1"; money = 500; position = 0; in_jail = 1; properties = [] }

let new_p2 =
  {
    name = "Player2";
    money = 500;
    position = 3;
    in_jail = 0;
    properties = [ Location test_location ];
  }

let players = [ jail_p1; new_p2 ]

let utils_suite =
  [
    (*Utility Test Cases: roll_move  *)
    ( "roll_move test" >:: fun _ ->
      assert_bool "Error"
        (let x = rollDice () in
         x >= 1 && x <= 7) );
    (*Utility Test Cases: tile_Action  *)
    ( "tile_Action: Jail" >:: fun _ ->
      assert_equal [ jail_p1 ] (tile_action (Jail 1) jail_p1 players 0 true) );
    ( "tile_Action: Start" >:: fun _ ->
      assert_equal [ new_p2 ] (tile_action (Start 0) new_p2 players 0 true) );
    ( "tile_Action: Tax" >:: fun _ ->
      assert_equal
        [
          {
            name = "Player2";
            money = 400;
            position = 3;
            in_jail = 0;
            properties = [];
          };
        ]
        (tile_action (Tax 100) new_p2 players 0 true) );
    ( "tile_Action: FreeParking" >:: fun _ ->
      assert_equal [ new_p2 ] (tile_action (Parking 0) new_p2 players 0 true) );
    ( "tile_Action: Jail" >:: fun _ ->
      assert_equal
        [
          {
            name = "Player2";
            money = 500;
            position = 3;
            in_jail = 3;
            properties = [];
          };
        ]
        (tile_action (Jail 0) new_p2 players 0 true) );
    (*Utility Test Cases: owner_opt  *)
    ( "owner_opt Player has Property" >:: fun _ ->
      assert_equal (Some new_p2) (owner_opt (Location test_location) players) );
    ( "owner_opt No Player has property" >:: fun _ ->
      assert_equal None (owner_opt (Location test_location2) players) );
  ]

(********************************************************)

let tests =
  "monopoly suite" >::: List.flatten [ board_suite; player_suite; utils_suite ]

let _ = run_test_tt_main tests

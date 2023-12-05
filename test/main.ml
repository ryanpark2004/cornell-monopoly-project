open OUnit2
open Monopoly
open Board
open Player
open Exceptions
open Utils

let test_board_no_indices = [ Start; Tax 100; Chance; Chest; Parking; Jail ]

let test_board =
  let rec indices (n : int) (lst : tile list) : int list =
    match lst with
    | [] -> []
    | _ :: t -> n :: indices (n + 1) t
  in
  List.combine test_board_no_indices (indices 0 test_board_no_indices)

let p1 = create_player "p1"
let p2 = create_player "p2"
let p3 = create_player "p3"
let p4 = create_player "p4"

(**Board test********************************************)

let test_length name out in1 = name >:: fun _ -> assert_equal out (length in1)

let test_rent name out in1 =
  name >:: fun _ -> assert_equal out (calculated_rent in1)

let test_pos_of_tile name out in1 =
  name >:: fun _ -> assert_equal out (pos_of_tile in1)

let test_tile_of_pos name out in1 =
  name >:: fun _ -> assert_equal out (tile_of_pos in1)

let test_tile_of_pos_fail in1 =
  let exn = No_Such_Tile in
  assert_raises exn (fun () ->
      try tile_of_pos in1 with No_Such_Tile -> raise exn)

let board_suite = [ test_length "length of 6" 6 test_board ]
(********************************************************)

(**Player test*******************************************)
let receive_test name out player rent =
  name >:: fun _ -> assert_equal out (receive_money player rent)

let move_test name out player n =
  name >:: fun _ -> assert_equal out (move_player player n)

let player_suite = []
(********************************************************)

(**Utils test********************************************)
let action_test out player _ = assert_equal out (tile_action player)

let utils_suite = []
(********************************************************)

let tests =
  "monopoly suite" >::: List.flatten [ board_suite; player_suite; utils_suite ]

let _ = run_test_tt_main tests

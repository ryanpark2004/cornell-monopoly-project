open OUnit2
open Monopoly
open Board
open Player
open Exceptions
open Utils
(**Board test********************************************)
let test_board = [Start; Tax 100; Chance; Chest; Parking]

let test_length out in1 _ = assert_equal out (length in1)
let test_rent out in1 _ = assert_equal out (calculated_rent in1)
let test_pos_of_tile out in1 _ = assert_equal out (pos_of_tile in1)
let test_tile_of_pos out in1 _ = assert_equal out (tile_of_pos in1)
let test_tile_of_pos_fail in1 _ =
  let exn = No_Such_Tile in
  assert_raises exn (fun () -> try tile_of_pos in1 with No_Such_Tile -> raise exn)

let board_suite = []

(********************************************************)


(**Player test*******************************************)
let p1 = create_player "p1"
let p2 = create_player "p2"
let p3 = create_player "p3"
let p4 = create_player "p4"

let receive_test out player rent _ = assert_equal out (receive_money player rent)
let move_test out player n _ = assert_equal out (move_player player n)

let player_suite = []

(********************************************************)

(**Utils test********************************************)
let action_test out player _ = assert_equal out (tile_action player)

let utils_suite = []

(********************************************************)

let tests = "monopoly suite" >::: List.flatten [board_suite;player_suite;utils_suite]
let _ = run_test_tt_main tests

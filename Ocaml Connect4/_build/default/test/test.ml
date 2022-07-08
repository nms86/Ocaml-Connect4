(* Test Plan:

   This OUnit test file contains tests for the following functionality:
   adding a chip, testing game outcomes, testing popping out a chip, and
   testing the computer player including its functions to check if it
   can win in the next turn and where to move next, testing setting and
   getting player names, and testing setting and getting cpu status.

   Manual testing was used to test entire game run-throughs and the user
   interface at the start, including asking for player names and
   win/loss messages. Automatic OUnit testing was used to test specific
   functionality like game state.

   Modules tested in OUnit include State and Interface and were
   developed with glass box testing.

   This testing approach demonstrates correctness of the system as it
   tests all functional features of the game with extensive test cases.
   By using glass box testing, the code was able to be thoroughly tested
   without missing important test cases by only doing black box testing.
   There are extensive cases for end cases and typical inputs for all
   important features. *)

open OUnit2
open Con4
open State

let init_test (name : string) (expected_output : int list list) =
  name >:: fun _ -> assert_equal expected_output (init |> get_board)

let addchip_test
    (name : string)
    (state : State.t)
    (col : int)
    (chip : chip)
    (expected_output : int list list) =
  name >:: fun _ ->
  assert_equal expected_output (state |> add_chip chip col |> get_board)

let addchip_exc_test
    (name : string)
    (state : State.t)
    (col : int)
    (chip : chip)
    (expected_output : string) =
  name >:: fun _ ->
  assert_raises (Failure expected_output) (fun _ ->
      state |> add_chip chip col)

let checkoutcome_test
    (name : string)
    (state : State.t)
    (expected_output : State.outcome) =
  name >:: fun _ -> assert_equal expected_output (state |> check_outcome)

let checkoutcome_win_test
    (name : string)
    (state : State.t)
    (expected_output : int) =
  name >:: fun _ ->
  match state |> check_outcome with
  | Win i -> assert_equal expected_output i
  | NoOutcome -> assert_bool "NoOutcome" false
  | Tie -> assert_bool "Tie" false

let popout_test
    (name : string)
    (state : State.t)
    (popout_i : int)
    (popout_j : int)
    (expected_output : int list list) =
  name >:: fun _ ->
  assert_equal expected_output
    (get_board (popout popout_i popout_j state))

let win_moves_test
    (name : string)
    (state : State.t)
    (expected_output : (int * chip) list) =
  name >:: fun _ -> assert_equal expected_output (win_moves state)

let rec string_of_intlist = function
  | [] -> ""
  | h :: t -> string_of_int h ^ string_of_intlist t

let high_mid_move_test
    (name : string)
    (state : State.t)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal expected_output (high_mid_move state)
    ~printer:string_of_intlist

let opp_wins_test
    (name : string)
    (state : State.t)
    (expected_output : int list) =
  name >:: fun _ ->
  assert_equal expected_output
    (opp_wins state |> List.sort compare)
    ~printer:string_of_intlist

let rec apply_n f n x = if n = 0 then x else apply_n f (n - 1) (f x)

let player_name_test
    (name : string)
    (pl_id : int)
    (state : State.t)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output (state |> get_pl_name pl_id)

(**[col_chip_sort] sorts an int*chip list by lowest int first, tie
   broken by Normal first, then Anvil, then Bomb.
   [Requires: no duplicates]*)
let col_chip_sort =
  List.sort (fun (col1, chip1) (col2, chip2) ->
      if col1 <> col2 then col1 - col2
      else
        match chip1 with
        | Normal -> -1
        | Anvil -> (
            match chip2 with
            | Normal -> 1
            | Anvil -> failwith "duplicate in col_chip_sort"
            | Bomb -> -1)
        | Bomb -> 1)

let rec int_chip_tostring = function
  | [] -> ""
  | (i, c) :: t ->
      string_of_int i
      ^ (match c with
        | Normal -> "Normal"
        | Anvil -> "Anvil"
        | Bomb -> "Bomb")
      ^ " " ^ int_chip_tostring t

let three_move_test
    (name : string)
    (state : State.t)
    (expected_output : (int * State.chip) list) =
  name >:: fun _ ->
  assert_equal
    (expected_output |> col_chip_sort)
    (state |> three_moves |> col_chip_sort)
    ~printer:int_chip_tostring

let is_cpu_test
    (name : string)
    (state : State.t)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (state |> is_cpu)

let state_tests =
  [
    init_test
      "the board of init is a list of 6 int lists, each with 7 zero's"
      [
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    addchip_test
      "the board of init then adding chip to col 1 is empty board \
       except 1st element of 1st list is 1"
      init 1 Normal
      [
        [ 1; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    addchip_test
      "the board of init then adding chip to col 7 is empty board \
       except 1st element of 7th list is 1"
      init 7 Normal
      [
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 1; 0; 0; 0; 0; 0 ];
      ];
    addchip_test
      "the board of init, add chip to col 7, add chip to col 1 is \
       empty board except 1st element of 7th list is 1, 1st element of \
       1st list is 2"
      (init |> add_chip Normal 7)
      1 Normal
      [
        [ 2; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 1; 0; 0; 0; 0; 0 ];
      ];
    addchip_test
      "the board of init, add chip to col 7, add chip to col 1, add \
       chip to col 1 is empty board except 1st element of 7th list is \
       1, 1st element of 1st list is 2, 2nd element of 1st list is 1"
      (init |> add_chip Normal 7 |> add_chip Normal 1)
      1 Normal
      [
        [ 2; 1; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 1; 0; 0; 0; 0; 0 ];
      ];
    addchip_test
      "the board of init, add chip 1 six times is empty board except \
       1st list is [1;2;1;2;1;2]"
      (init |> add_chip Normal 1 |> add_chip Normal 1
     |> add_chip Normal 1 |> add_chip Normal 1 |> add_chip Normal 1)
      1 Normal
      [
        [ 1; 2; 1; 2; 1; 2 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    addchip_exc_test
      "the board of init, add chip 1 seven times raises Failure \
       ColumnFull"
      (init |> add_chip Normal 1 |> add_chip Normal 1
     |> add_chip Normal 1 |> add_chip Normal 1 |> add_chip Normal 1
     |> add_chip Normal 1)
      1 Normal "ColumnFull";
    addchip_exc_test
      "the board of init, add chip 0 raises Failure InvalidColumn" init
      0 Normal "InvalidColumn";
    addchip_exc_test
      "the board of init, add chip 8 raises Failure InvalidColumn" init
      8 Normal "InvalidColumn";
    checkoutcome_test "outcome of init is NoOutcome" init NoOutcome;
    checkoutcome_test "outcome of init then add chip at 1 is NoOutcome"
      (init |> add_chip Normal 1)
      NoOutcome;
    checkoutcome_win_test
      "Hor Win test: outcome of add_chip 1, 1, 2, 2, 3, 3, 4 \n\
      \      |  0  0  0  0  0  0  0  |\n\
      \      |  0  0  0  0  0  0  0  |\n\
      \      |  0  0  0  0  0  0  0  |\n\
      \      |  0  0  0  0  0  0  0  |\n\
      \      |  2  2  2  0  0  0  0  |\n\
      \      |  1  1  1  1  0  0  0  |is Win for Pl 1"
      (init |> add_chip Normal 1 |> add_chip Normal 1
     |> add_chip Normal 2 |> add_chip Normal 2 |> add_chip Normal 3
     |> add_chip Normal 3 |> add_chip Normal 4)
      1;
    checkoutcome_win_test
      "Vert Hor Win test: outcome of add_chip 1, 2, 1, 2, 1, 2, 1 is \
       ______|  0  0  0  0  0  0  0  |\n\
      \      |  0  0  0  0  0  0  0  |\n\
      \      |  1  0  0  0  0  0  0  |\n\
      \      |  1  2  0  0  0  0  0  |\n\
      \      |  1  2  0  0  0  0  0  |\n\
      \      |  1  2  0  0  0  0  0  | Win for Pl 1"
      (init |> add_chip Normal 1 |> add_chip Normal 2
     |> add_chip Normal 1 |> add_chip Normal 2 |> add_chip Normal 1
     |> add_chip Normal 2 |> add_chip Normal 1)
      1;
    checkoutcome_win_test
      "Hor Win test Pl 2: outcome of add_chip 7, 1, 1, 2, 2, 3, 3, 4 \
       ______|  0  0  0  0  0  0  0  |\n\
      \      |  0  0  0  0  0  0  0  |\n\
      \      |  0  0  0  0  0  0  0  |\n\
      \      |  0  0  0  0  0  0  0  |\n\
      \      |  1  1  1  0  0  0  0  |\n\
      \      |  2  2  2  2  0  0  1  | is Win for Pl 2"
      (init |> add_chip Normal 7 |> add_chip Normal 1
     |> add_chip Normal 1 |> add_chip Normal 2 |> add_chip Normal 2
     |> add_chip Normal 3 |> add_chip Normal 3 |> add_chip Normal 4)
      2;
    checkoutcome_win_test
      "Vert Hor Win test Pl 2: outcome of add_chip 7, 1, 2, 1, 2, 1, \
       2, 1 \n\
      \      |  0  0  0  0  0  0  0  |\n\
      \      |  0  0  0  0  0  0  0  |\n\
      \      |  2  0  0  0  0  0  0  |\n\
      \      |  2  1  0  0  0  0  0  |\n\
      \      |  2  1  0  0  0  0  0  |\n\
      \      |  2  1  0  0  0  0  1  |is Win for Pl 1"
      (init |> add_chip Normal 7 |> add_chip Normal 1
     |> add_chip Normal 2 |> add_chip Normal 1 |> add_chip Normal 2
     |> add_chip Normal 1 |> add_chip Normal 2 |> add_chip Normal 1)
      2;
    checkoutcome_win_test
      "Diag topright Win test: outcome of add_chip 1, 2, 2, 3, 3, 4, \
       3, 4, 4, 5, 4 \n\
      \       |  0  0  0  0  0  0  0  |\n\
      \       |  0  0  0  0  0  0  0  |\n\
      \       |  0  0  0  1  0  0  0  |\n\
      \       |  0  0  1  1  0  0  0  |\n\
      \       |  0  1  1  2  0  0  0  |\n\
      \       |  1  2  2  2  0  2  0  | is Win for Pl 1"
      (init |> add_chip Normal 1 |> add_chip Normal 2
     |> add_chip Normal 2 |> add_chip Normal 3 |> add_chip Normal 3
     |> add_chip Normal 4 |> add_chip Normal 3 |> add_chip Normal 4
     |> add_chip Normal 4 |> add_chip Normal 6 |> add_chip Normal 4)
      1;
    checkoutcome_win_test
      "Diag topright Win test Pl 2: outcome of add_chip 7, 1, 2, 2, 3, \
       3, 4, 3, 4, 4, 5, 4 \n\
      \       |  0  0  0  0  0  0  0  |\n\
      \       |  0  0  0  0  0  0  0  |\n\
      \       |  0  0  0  2  0  0  0  |\n\
      \       |  0  0  2  2  0  0  0  |\n\
      \       |  0  2  2  1  0  0  0  |\n\
      \       |  2  1  1  1  0  1  1  | is Win for Pl 1"
      (init |> add_chip Normal 7 |> add_chip Normal 1
     |> add_chip Normal 2 |> add_chip Normal 2 |> add_chip Normal 3
     |> add_chip Normal 3 |> add_chip Normal 4 |> add_chip Normal 3
     |> add_chip Normal 4 |> add_chip Normal 4 |> add_chip Normal 6
     |> add_chip Normal 4)
      2;
    checkoutcome_win_test
      "Diag botright Win test: outcome of add_chip 7, 6, 6, 5, 5, 4, \
       5, 4, 4, 3, 4 \n\
      \       |  0  0  0  0  0  0  0  |\n\
      \       |  0  0  0  0  0  0  0  |\n\
      \       |  0  0  0  1  0  0  0  |\n\
      \       |  0  0  0  1  1  0  0  |\n\
      \       |  0  0  0  2  1  1  0  |\n\
      \       |  0  2  0  2  2  2  1  | is Win for Pl 1"
      (init |> add_chip Normal 7 |> add_chip Normal 6
     |> add_chip Normal 6 |> add_chip Normal 5 |> add_chip Normal 5
     |> add_chip Normal 4 |> add_chip Normal 5 |> add_chip Normal 4
     |> add_chip Normal 4 |> add_chip Normal 2 |> add_chip Normal 4)
      1;
    checkoutcome_win_test
      "Diag botright Win test Pl 2: outcome of add_chip 1, 7, 6, 6, 5, \
       5, 4, 5, 4, 4, 3, 4 is \n\
      \       |  0  0  0  0  0  0  0  |\n\
      \       |  0  0  0  0  0  0  0  |\n\
      \       |  0  0  0  2  0  0  0  |\n\
      \       |  0  0  0  2  2  0  0  |\n\
      \       |  0  0  0  1  2  2  0  |\n\
      \       |  1  1  0  1  1  1  2  | Win for Pl 1"
      (init |> add_chip Normal 1 |> add_chip Normal 7
     |> add_chip Normal 6 |> add_chip Normal 6 |> add_chip Normal 5
     |> add_chip Normal 5 |> add_chip Normal 4 |> add_chip Normal 5
     |> add_chip Normal 4 |> add_chip Normal 4 |> add_chip Normal 2
     |> add_chip Normal 4)
      2;
    checkoutcome_test
      "outcome of filling board up with no 4 in rows is Tie\n\
      \ |  2  1  2  1  2  1  2  |\n\
       _|  2  1  2  1  2  1  1  |\n\
       _|  2  1  2  1  2  1  2  |\n\
       _|  1  2  1  2  1  2  1  |\n\
       _|  1  2  1  2  1  2  2  |\n\
       _|  1  2  1  2  1  2  1  |"
      (init
      |> apply_n
           (fun x -> x |> add_chip Normal 1 |> add_chip Normal 2)
           3
      |> apply_n
           (fun x -> x |> add_chip Normal 2 |> add_chip Normal 1)
           3
      |> apply_n
           (fun x -> x |> add_chip Normal 3 |> add_chip Normal 4)
           3
      |> apply_n
           (fun x -> x |> add_chip Normal 4 |> add_chip Normal 3)
           3
      |> apply_n
           (fun x -> x |> add_chip Normal 5 |> add_chip Normal 6)
           3
      |> apply_n
           (fun x -> x |> add_chip Normal 6 |> add_chip Normal 5)
           3
      |> apply_n
           (fun x -> x |> add_chip Normal 7 |> add_chip Normal 7)
           3)
      Tie;
    addchip_test
      "the board of init, add chip to col 1, add ANVIL chip to col 1, \
       is empty board except 1st element of 1st list is 2"
      (init |> add_chip Normal 1)
      1 Anvil
      [
        [ 2; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    addchip_test
      "the board of init, add chip to col 1 five times, add ANVIL chip \
       to col 1, is empty board except 1st element of 1st list is 2"
      (init |> apply_n (add_chip Normal 1) 5)
      1 Anvil
      [
        [ 2; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    addchip_exc_test
      "the board of init, add chip to col 1 six times, then add ANVIL \
       chip to col 1 raises Failure ColumnFull"
      (init |> apply_n (add_chip Normal 1) 6)
      1 Anvil "ColumnFull";
    addchip_test
      "the board of init, add ANVIl chip to col 1 is empty board \
       except 1st element of 1st list is 1"
      init 1 Anvil
      [
        [ 1; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    addchip_exc_test
      "adding Anvil chip twice for player 1 raises Failure \
       NotEnoughChips"
      (init |> add_chip Anvil 1 |> add_chip Normal 1)
      1 Anvil "NotEnoughChips";
    addchip_exc_test
      "the board of init, add ANVIL chip 0 raises Failure InvalidColumn"
      init 0 Anvil "InvalidColumn";
    (* win moves and high_mid_move tests *)
    win_moves_test "empty board has no win moves" init [];
    win_moves_test "board with one piece has no win moves"
      (init |> add_chip Normal 3)
      [];
    win_moves_test "board with one piece has no win moves"
      (init |> add_chip Normal 3)
      [];
    win_moves_test
      "board with columns [1; 1; 1; 0; 0; 0] and [2; 2; 2; 0; 0; 0] \
       has win move in column 3"
      (init |> add_chip Normal 3 |> add_chip Normal 4
     |> add_chip Normal 3 |> add_chip Normal 4 |> add_chip Normal 3
     |> add_chip Normal 4)
      [ (3, Normal) ];
    win_moves_test
      "board with columns [1; 1; 1; 0; 0; 0] and [2; 2; 2; 0; 0; 0] \
       has win move in column 4"
      (init |> add_chip Normal 4 |> add_chip Normal 3
     |> add_chip Normal 4 |> add_chip Normal 3 |> add_chip Normal 4
     |> add_chip Normal 3)
      [ (4, Normal) ];
    win_moves_test
      "board with columns [1; 1; 1; 0; 0; 0] and [2; 2; 2; 0; 0; 0] \
       twice has win move in column 1 and 4"
      (init |> add_chip Normal 4 |> add_chip Normal 3
     |> add_chip Normal 4 |> add_chip Normal 3 |> add_chip Normal 4
     |> add_chip Normal 3 |> add_chip Normal 1 |> add_chip Normal 2
     |> add_chip Normal 1 |> add_chip Normal 2 |> add_chip Normal 1
     |> add_chip Normal 2)
      [ (1, Normal); (4, Normal) ];
    high_mid_move_test "empty board has no win moves" init
      [ 4; 3; 5; 2; 6; 1; 7 ];
    high_mid_move_test
      "board with two pieces in col 3 and 4 has best next move in col 4"
      (init |> add_chip Normal 3 |> add_chip Normal 4)
      [ 4; 3; 5; 2; 6; 1; 7 ];
    high_mid_move_test
      "board with piece in col 1 has best next move in col 1"
      (init |> add_chip Normal 1)
      [ 1; 4; 3; 5; 2; 6; 7 ];
    high_mid_move_test
      "board with three pieces in col 1, 2, and 3 has best next move \
       in col 3"
      (init |> add_chip Normal 1 |> add_chip Normal 2
     |> add_chip Normal 3)
      [ 3; 2; 1; 4; 5; 6; 7 ];
    high_mid_move_test
      "board with three pieces in col 2 has best next move in col 2"
      (init |> add_chip Normal 2 |> add_chip Normal 2
     |> add_chip Normal 2)
      [ 2; 4; 3; 5; 6; 1; 7 ];
    high_mid_move_test
      "board with three pieces in col 7 and other shorter stacks has \
       best next move in col 7"
      (init |> add_chip Normal 7 |> add_chip Normal 7
     |> add_chip Normal 7 |> add_chip Normal 1 |> add_chip Normal 3
     |> add_chip Normal 5)
      [ 7; 3; 5; 1; 4; 2; 6 ];
    high_mid_move_test
      "board with column 2 filed up has best move column 4"
      (init |> add_chip Normal 2 |> add_chip Normal 2
     |> add_chip Normal 2 |> add_chip Normal 2 |> add_chip Normal 2
     |> add_chip Normal 2)
      [ 4; 3; 5; 6; 1; 7 ];
    high_mid_move_test
      "board with column 2 filed up and piece in column 5 has best \
       move column 5"
      (init |> add_chip Normal 2 |> add_chip Normal 2
     |> add_chip Normal 2 |> add_chip Normal 2 |> add_chip Normal 2
     |> add_chip Normal 2 |> add_chip Normal 5)
      [ 5; 4; 3; 6; 1; 7 ];
    high_mid_move_test
      "high mid move after Anvil 2, Normal 2, is 2 (testing that \
       doesn't crash after using up all Anvil chips"
      (init |> add_chip Anvil 2 |> add_chip Normal 2)
      [ 2; 4; 3; 5; 6; 1; 7 ];
    (* popout testing: *)
    popout_test
      "the empty board with column 1 equal to [1;2;1;2;1;2] with the \
       chip in (1, 1) popped out"
      (init |> add_chip Normal 1 |> add_chip Normal 1
     |> add_chip Normal 1 |> add_chip Normal 1 |> add_chip Normal 1
     |> add_chip Normal 1)
      1 1
      [
        [ 2; 1; 2; 1; 2; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    popout_test
      "the board of a chip in column 7 and column 1 with the chip in \
       column 7 popped out"
      (init |> add_chip Normal 7 |> add_chip Normal 1)
      1 7
      [
        [ 2; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    popout_test
      "the board with a chip in column 7 and then column 1 with the \
       chip in column 1 popped out"
      (init |> add_chip Normal 7 |> add_chip Normal 1)
      1 1
      [
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 1; 0; 0; 0; 0; 0 ];
      ];
    popout_test
      "the board of init, add chip 1 six times, 1st list is \
       [1;2;1;2;1;2] with the first 2 popped out"
      (init |> add_chip Normal 1 |> add_chip Normal 1
     |> add_chip Normal 1 |> add_chip Normal 1 |> add_chip Normal 1)
      2 1
      [
        [ 1; 1; 2; 1; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    addchip_test
      "the board of init, add chip to 1, 2, 3, then Bomb to 2 is the \
       empty board but with 2 in col 2"
      (init |> add_chip Normal 1 |> add_chip Normal 2
     |> add_chip Normal 3)
      2 Bomb
      [
        [ 0; 0; 0; 0; 0; 0 ];
        [ 2; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    addchip_test
      "the board of init, add chip to 1, 2, 3, 1, 2, 3 then Bomb to 2 \
       is the empty board but with 1 in col 1, 2 1 in col 2, 1 in col \
       3"
      (init |> add_chip Normal 1 |> add_chip Normal 2
     |> add_chip Normal 3 |> add_chip Normal 1 |> add_chip Normal 2
     |> add_chip Normal 3)
      2 Bomb
      [
        [ 1; 0; 0; 0; 0; 0 ];
        [ 2; 1; 0; 0; 0; 0 ];
        [ 1; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    addchip_test
      "the board of init, add chip to 1, 2, 3, then Bomb to 2 is the \
       empty board but with 2 in col 1"
      (init |> add_chip Normal 1 |> add_chip Normal 2
     |> add_chip Normal 3)
      1 Bomb
      [
        [ 2; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 1; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    addchip_test
      "the board of init, add chip to 1, 2, 3, 1, 2, 3 then Bomb to 1 \
       is the empty board but with 1 1 in col 1, 2 in col 2, 1 2 in \
       col 3"
      (init |> add_chip Normal 1 |> add_chip Normal 2
     |> add_chip Normal 3 |> add_chip Normal 1 |> add_chip Normal 2
     |> add_chip Normal 3)
      1 Bomb
      [
        [ 1; 1; 0; 0; 0; 0 ];
        [ 2; 0; 0; 0; 0; 0 ];
        [ 1; 2; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    addchip_test
      "the board of init, add chip to 5, 6, 7, then Bomb to 6 is the \
       empty board but with 2 in col 6"
      (init |> add_chip Normal 5 |> add_chip Normal 6
     |> add_chip Normal 7)
      6 Bomb
      [
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 2; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    addchip_test
      "the board of init, add chip to 5, 6, 7, 5, 6, 7 then Bomb to 6 \
       is the empty board but with 1 in col 5, 2 1 in col 6, 1 in col \
       7"
      (init |> add_chip Normal 5 |> add_chip Normal 6
     |> add_chip Normal 7 |> add_chip Normal 5 |> add_chip Normal 6
     |> add_chip Normal 7)
      6 Bomb
      [
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 1; 0; 0; 0; 0; 0 ];
        [ 2; 1; 0; 0; 0; 0 ];
        [ 1; 0; 0; 0; 0; 0 ];
      ];
    addchip_test
      "the board of init, add chip to 5, 6, 7, 5, 6, 7 then Bomb to 6 \
       is the empty board but with 1 in col 5, 2 1 in col 6, 1 in col \
       7"
      (init |> add_chip Normal 5 |> add_chip Normal 6
     |> add_chip Normal 7 |> add_chip Normal 5 |> add_chip Normal 6
     |> add_chip Normal 7)
      7 Bomb
      [
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 1; 2; 0; 0; 0; 0 ];
        [ 2; 0; 0; 0; 0; 0 ];
        [ 1; 1; 0; 0; 0; 0 ];
      ];
    player_name_test "Name of player 1 at init is Bob" 1 init "Bob";
    player_name_test "Name of player 2 at init is Joe" 2 init "Joe";
    player_name_test
      "Name of player 1 after setting player 1 name to Sam is Sam" 1
      (init |> set_pl_name 1 "Sam")
      "Sam";
    player_name_test
      "Name of player 2 after setting player 1 name to Sam is Joe" 2
      (init |> set_pl_name 1 "Sam")
      "Joe";
    three_move_test "[] creates a three in a row for empty board" init
      [];
    three_move_test
      "[(1, Normal); (4, Normal)] creates a three in a row for board \
       with chips for pl 1 at 2, 3"
      (init |> add_chip Normal 2 |> add_chip Normal 2
     |> add_chip Normal 3 |> add_chip Normal 3)
      [
        (1, Normal);
        (1, Anvil);
        (1, Bomb);
        (4, Normal);
        (4, Anvil);
        (4, Bomb);
        (5, Normal);
        (5, Anvil);
        (5, Bomb);
      ];
    addchip_test "add chip Bomb 4 is a board with pl 1 chip at 4" init 4
      Bomb
      [
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 1; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
        [ 0; 0; 0; 0; 0; 0 ];
      ];
    three_move_test
      "The three in a row for board of add chip at 1, 1, 2, 2, 1, 3 is \
       [(3, Anvil)]"
      (init |> add_chip Normal 1 |> add_chip Normal 1
     |> add_chip Normal 2 |> add_chip Normal 2 |> add_chip Normal 1
     |> add_chip Normal 3)
      [ (3, Anvil) ];
    three_move_test
      "The three in a row for board of add chip Anvil 1, add chip \
       Anvil 1 is [], and we check that no [NotEnoughChips] exception \
       is raised"
      (init |> add_chip Anvil 1 |> add_chip Anvil 1)
      [];
    three_move_test
      "The three in a row for board of add chip 1, 2, 2, 4, 4, 4 is \
       [(1, Normal); (4, Normal)]"
      (init |> add_chip Normal 1 |> add_chip Normal 2
     |> add_chip Normal 2 |> add_chip Normal 4 |> add_chip Normal 4
     |> add_chip Normal 4)
      [ (1, Normal); (4, Normal) ];
    three_move_test
      "The three in a row for board of add chip at 1, 1, 2, 2, Anvil \
       7, 3 is []. (We test that it cannot reccomend a power chip move \
       if no power chips are left)."
      (init |> add_chip Normal 1 |> add_chip Normal 1
     |> add_chip Normal 2 |> add_chip Normal 2 |> add_chip Anvil 7
     |> add_chip Normal 3)
      [];
    three_move_test
      "The three in a row for board of add chip at 1 six times is []. \
       (We test that no exception for [Failure ColumnFull] is \
       raised))."
      (init |> apply_n (add_chip Normal 1) 6)
      [];
    is_cpu_test "pl 1 is not cpu" init false;
    is_cpu_test "pl 2 is not cpu" (init |> add_chip Normal 1) false;
    is_cpu_test "pl 1 is cpu after using set_to_cpu 1"
      (init |> set_to_cpu 1)
      true;
    is_cpu_test "pl 2 is not cpu after using set_to_cpu 1"
      (init |> set_to_cpu 1 |> add_chip Normal 1)
      false;
    opp_wins_test "hor three in a row"
      (init |> add_chip Normal 4 |> add_chip Normal 4
     |> add_chip Normal 5 |> add_chip Normal 5 |> add_chip Normal 3)
      [ 2; 6 ];
  ]

let suite = "test suite for final" >::: List.flatten [ state_tests ]
let _ = run_test_tt_main suite

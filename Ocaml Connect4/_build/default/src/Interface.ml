open Printf
open State

let rec print_last_element (index : int) (column : int list) =
  print_string (string_of_int (List.nth column index) ^ "  ")

let rec print_last_for_all (board : int list list) (current_row : int) =
  match board with
  | [] -> ()
  | h :: t ->
      print_last_element (current_row - 1) h;
      print_last_for_all t current_row

let rec print_columns
    (board : int list list)
    (current_row : int)
    (acc : int) =
  match acc with
  | 1 -> ()
  | _ ->
      print_string "|  ";
      print_last_for_all board current_row;
      print_endline "|";
      print_columns board (current_row - 1) (acc - 1)

let print_board (board : State.t) =
  print_columns (State.get_board board) 6 7
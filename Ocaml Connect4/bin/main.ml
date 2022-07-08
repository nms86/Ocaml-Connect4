open Con4
open Interface

(* asks for the users at the start of the game and stores that
   information *)
let set_user read_line player st message =
  print_endline (message ^ string_of_int player ^ ":\n");
  print_string "> ";
  match read_line () with
  | name ->
      print_endline ("\n Welcome " ^ name ^ "!\n");
      if name = "cpu" then
        st |> State.set_pl_name 2 name |> State.set_to_cpu 2
      else st |> State.set_pl_name player name

(**[print_gamestate st] prints that it's the current player in [st]'s
   turn, then prints board, then prints how many power chips they have
   left.*)
let print_gamestate st =
  let cur_pl_id = st |> State.get_curr_player in
  print_endline
    ((st |> State.get_pl_name cur_pl_id) ^ ", it's your turn\n");
  print_board st;
  print_endline (State.powerchips_to_str cur_pl_id st)

(**[input_chiptype st] prints prompt for chip type and returns
   corresponding chiptype. If inputted string doesn't match any chip
   type, calls itself again.*)
let rec input_chiptype st =
  print_endline
    "\nEnter the chip type you'd like: normal, anvil, or bomb";
  let chip_str = read_line () |> String.lowercase_ascii in
  if chip_str = "normal" then State.Normal
  else if chip_str = "bomb" && State.get_chip_count State.Bomb st > 0
  then State.Bomb
  else if chip_str = "anvil" && State.get_chip_count State.Anvil st > 0
  then State.Anvil
  else (
    print_endline "\nInvalid choice, try again!\n >";
    input_chiptype st)

(**[input_col st chip_type] prints prompt for col num and returns
   corresponding [st] updated with [add_chip] using the col num and
   [chip_type]. If inputted col num results in an exception (like column
   full), calls itself again.*)
let rec input_col st chip_type =
  print_endline
    "\n\
     Enter the number of the column where you'd like to enter your tile";
  let col_num = read_line () in
  match State.add_chip chip_type (int_of_string col_num) st with
  | new_st -> new_st
  | exception _ ->
      print_endline "\nInvalid Column, try again\n >";
      input_col st chip_type

(* executes a user's turn: either a cpu turn or a real user's turn*)
let rec user_turn game =
  print_gamestate game;
  if State.is_cpu game then exec_cpu_move game
  else
    let chip_type = input_chiptype game in
    let new_game = input_col game chip_type in
    process_result new_game

(**[exec_cpu_move game] returns [game] with cpu move doing add chip,
   assuming that the current player is a cpu.*)
and exec_cpu_move game =
  let cpu_move = State.cpu_move game in
  let new_game = game |> State.add_chip (snd cpu_move) (fst cpu_move) in
  process_result new_game

(* prints the result of the game once it has ended (win/loss/tie) *)
and process_result game =
  match Con4.State.check_outcome game with
  | Win winner ->
      print_board game;
      let winner_name = game |> State.get_pl_name winner in
      print_string
        ("\nCongratulations! " ^ winner_name ^ " has won the game\n")
  | Tie -> print_string "\n The game has ended in a TIE!\n"
  | NoOutcome -> user_turn game

(* starts the entire game and welcomes the user *)
let main () =
  let game = Con4.State.init in
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Connect 4!\n";
  let set_pl1 =
    set_user read_line 1 game "Please enter the name of player"
  in
  let set_pl2 =
    set_user read_line 2 set_pl1
      "Please enter the name of player 2 or 'cpu' for a computer player"
  in
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nLet's start this game!\n";

  ANSITerminal.erase Screen;

  user_turn set_pl2

(* Execute the game engine. *)
let () = main ()
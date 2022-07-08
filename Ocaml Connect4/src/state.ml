(* the int is the player id of player*)
type chip_count = {
  anvil : int;
  bomb : int;
}

type player = {
  player_id : int;
  player_name : string;
  chip_count : chip_count;
  is_cpu : bool;
}

type t = {
  board : int list list;
  (* int list is the columns, int list list is the list of columns*)
  (* each int list is a column, with the first element being the bottom
     row, last element being top row*)
  (*the first int list is the left-most column, the last int list is the
    right most column*)
  (* 0 is no chips, 1 is player 1's chip, etc*)
  current_pl : int;
  player_list : player list;
}

type chip =
  | Normal
  | Anvil
  | Bomb

let set_pl_name pl_id name st =
  let new_pl_list =
    st.player_list
    |> List.map (fun x ->
           if x.player_id = pl_id then { x with player_name = name }
           else x)
  in
  { st with player_list = new_pl_list }

let get_pl_name pl_id st =
  st.player_list |> List.find (fun x -> x.player_id = pl_id) |> fun x ->
  x.player_name

(**[make_list n elem] is a list of [n] occurences of [elem].
   [Requires: n >= 0].*)
let rec make_list n elem =
  if n = 0 then [] else elem :: make_list (n - 1) elem

(**[make_board nrow ncol] is a list of ncol columns, where each column
   is a list of nrow 0's*)
let make_board nrow ncol =
  let row_list = make_list nrow 0 in
  make_list ncol row_list

(**[apply_n f n x] applies [f] to [x] [n] times. [Requires: n >= 0]*)
let rec apply_n f n x = if n = 0 then x else apply_n f (n - 1) (f x)

let get_board state = state.board

let init =
  {
    board = make_board 6 7;
    current_pl = 1;
    player_list =
      [
        {
          player_id = 1;
          player_name = "Bob";
          chip_count = { anvil = 1; bomb = 1 };
          is_cpu = false;
        };
        {
          player_id = 2;
          player_name = "Joe";
          chip_count = { anvil = 1; bomb = 1 };
          is_cpu = false;
        };
      ];
  }

type outcome =
  | Win of int
  (*the int is the player id of the winner, 1 for player 1, etc*)
  | Tie
  | NoOutcome

(**[all_eq x1 x2 x3 x4] is x1 if all args are equal, else 0*)
let all_eq x1 x2 x3 x4 = if x1 = x2 && x2 = x3 && x3 = x4 then x1 else 0

(**[index board i j] is the element in the ith row, jth column of board.
   bottom row is the 1st row, left col is the 1st col*)
let index board (i, j) = List.(nth (nth board (j - 1)) (i - 1))

(**[pair_sequence (n, m)] is the list \[(n,m); (n-1,m)...(0,m)].
   [Requires: n >= 0] *)
let rec pair_sequence (n, m) =
  if n = 0 then [] else (n, m) :: pair_sequence (n - 1, m)

(**[sequence n] is the list \[n; n-1...1]. [Requires: n >= 0]*)
let rec sequence n = if n = 0 then [] else n :: sequence (n - 1)

(**[index_list nrow ncol] is the list of all index pairs for nrow rows
   and ncol columns. Requires: nrow and ncol >= 0*)
let index_list nrow ncol =
  let col_maker x = pair_sequence (nrow, x) in
  List.map (fun x -> col_maker x) (sequence ncol) |> List.flatten

(**[winner_aux checker board ij f] is the player id with 4 chips that
   pass [checker] at ij, [f] ij, [f] [f] ij, and [f] [f][f] ij, is 0
   otherwise, where [ij] is tuple (i, j) representing the ith row and
   jth column index*)
let winner_aux checker board ij f =
  let index_board = index board in
  match
    checker (index_board ij)
      (index_board (ij |> f))
      (index_board (ij |> f |> f))
      (index_board (ij |> f |> f |> f))
  with
  | c -> c
  | exception _ -> 0

(**[check_board_winner checker f board] is the player id with 4 chips
   that pass [checker] at ij, [f] ij, [f] [f] ij, and [f] [f][f] ij for
   all ij in board, 0 otherwise*)
let check_board_winner checker f board =
  let ncol = List.length board in
  let nrow = List.length (List.nth board 0) in
  List.map
    (fun ij -> winner_aux checker board ij f)
    (index_list nrow ncol)
  |> List.filter (fun x -> x <> 0)
  |> fun x -> if List.length x = 0 then 0 else List.hd x

(**[check_X_win checker board] returns the player id with a match
   according to [checker] in the X direction, 0 otherwise*)
let check_hor_win checker board =
  check_board_winner checker (fun (i, j) -> (i, j + 1)) board

let check_vert_win checker board =
  check_board_winner checker (fun (i, j) -> (i + 1, j)) board

let check_diag_topright_win checker board =
  check_board_winner checker (fun (i, j) -> (i + 1, j + 1)) board

let check_diag_botright_win checker board =
  check_board_winner checker (fun (i, j) -> (i - 1, j + 1)) board

(**[check_win checker board] is the player id of the player with a win
   in board according to [checker], else 0*)
let check_win checker board =
  let win_cond =
    [
      check_hor_win;
      check_vert_win;
      check_diag_topright_win;
      check_diag_botright_win;
    ]
  in
  List.map (fun win_cond -> win_cond checker board) win_cond
  |> List.fold_left max 0

(**[check_full board] is true if board is full of non-zeros, else false*)
let check_full board =
  board |> List.flatten |> List.filter (fun x -> x = 0) = []

let check_outcome state =
  let win_id = check_win all_eq (get_board state) in
  if win_id <> 0 then Win win_id
  else
    let tie_bool = check_full (get_board state) in
    if tie_bool then Tie else NoOutcome

(**[player_mod num_pl pl_id] is the correct player id for an incremented
   player id (returns to player one if going over the number of players)*)
let player_mod num_pl pl_id =
  match pl_id mod (num_pl + 1) with
  | 0 -> 1
  | c -> c

(**[get_curr_player state] is the player id of the player currently
   playing*)
let get_curr_player state = state.current_pl

(**[next_player state] is the Player who goes next in state*)
let next_player state =
  state |> get_curr_player |> ( + ) 1 |> player_mod 2

(**[add_chip_aux pl lst] is lst with the chip for player id [pl] added.
   Raises [Failure "ColumnFull"] if column is already full*)
let rec add_chip_aux pl = function
  | 0 :: t -> pl :: t
  | h :: t -> h :: add_chip_aux pl t
  | [] -> failwith "ColumnFull"

(**[replace_element elem n lst] is lst with the nth element replaced
   with elem (first index = 1). Raises [Failure "ReplaceError"] if n >
   length of lst*)
let rec replace_element elem n = function
  | h :: t ->
      if n = 1 then elem :: t else h :: replace_element elem (n - 1) t
  | _ -> failwith "ReplaceError"

let get_chip_count chip st =
  let curr_pl =
    st.player_list
    |> List.filter (fun x -> x.player_id = get_curr_player st)
    |> List.hd
  in
  match chip with
  | Normal -> 999
  | Anvil -> curr_pl.chip_count.anvil
  | Bomb -> curr_pl.chip_count.bomb

(**[player_chip_dec curr_pl chip] is [curr_pl] with the chip count of
   type [chip] decremented by one.*)
let player_chip_dec curr_pl chip =
  {
    curr_pl with
    chip_count =
      {
        anvil =
          (if chip = Anvil then curr_pl.chip_count.anvil - 1
          else curr_pl.chip_count.anvil);
        bomb =
          (if chip = Bomb then curr_pl.chip_count.bomb - 1
          else curr_pl.chip_count.bomb);
      };
  }

(**[dec_chip_count chip st] is player_list of [st] with number of chips
   of type [chip] for the current player in [st] decremented by 1*)
let dec_chip_count chip st =
  let curr_pl =
    st.player_list
    |> List.filter (fun x -> x.player_id = get_curr_player st)
    |> List.hd
  in
  let new_pl =
    match chip with
    | Normal -> curr_pl
    | Anvil -> player_chip_dec curr_pl Anvil
    | Bomb -> player_chip_dec curr_pl Bomb
  in
  st.player_list
  |> List.map (fun x ->
         if x.player_id = get_curr_player st then new_pl else x)

(**[height lst] is the number of non-zero elements in lst*)
let height lst = lst |> List.filter (fun x -> x <> 0) |> List.length

(**[popout_helper col i] pops out the element i in the column and
   returns the new column*)
let rec popout_helper (col : int list) (i : int) =
  match i with
  | 5 -> replace_element 0 (i + 1) col
  | _ ->
      popout_helper
        (replace_element (List.nth col (i + 1)) (i + 1) col)
        (i + 1)

(** [popout_cons_columns board new_col new_col_num] combines the new
    column in the int list list*)
let popout_cons_columns
    (board : int list list)
    (new_col : int list)
    (new_col_num : int) =
  replace_element new_col (new_col_num + 1) board

let popout i j st =
  let new_board =
    popout_cons_columns (get_board st)
      (popout_helper (List.nth (get_board st) (j - 1)) (i - 1))
      (j - 1)
  in
  { st with board = new_board }

(**[chip_adder_state chip col select_col st] is [st] with the correct
   pre-adding chip adjustments based on [chip], [col], [select_col]
   (mainly popping out chips). Raises [Failure "NotEnoughChips"] if
   current player in [st] has zero chips of type [chip].*)
let chip_pre_state chip col select_col st =
  if get_chip_count chip st = 0 then failwith "NotEnoughChips"
  else if chip = Anvil then
    st |> apply_n (popout 1 col) (height select_col)
  else if chip = Bomb then
    let bomb_height = height select_col in
    if bomb_height = 0 then st
    else
      let blowup_left =
        match List.nth (get_board st) (col - 2) with
        | lst -> popout bomb_height (col - 1) st
        | exception _ -> st
      in
      let blowup_right =
        match List.nth (get_board st) col with
        | lst -> popout bomb_height (col + 1) blowup_left
        | exception _ -> blowup_left
      in
      popout bomb_height col blowup_right
  else st

(**[chip_adder col chip st] adds chip of type [chip] to [st] once it has
   been prepared with pre-adding by [chip_adder_state]*)
let chip_adder col chip st =
  let new_col =
    add_chip_aux (get_curr_player st)
      (List.nth (get_board st) (col - 1))
  in
  let new_pl_list = dec_chip_count chip st in
  let new_board = replace_element new_col col (get_board st) in
  {
    board = new_board;
    current_pl = next_player st;
    player_list = new_pl_list;
  }

let rec add_chip chip col state =
  let select_col =
    match List.nth (get_board state) (col - 1) with
    | a -> a
    | exception _ -> failwith "InvalidColumn"
  in
  (*checks that column not already full*)
  let _ = add_chip_aux (get_curr_player state) select_col in
  match chip with
  | Normal -> chip_adder col Normal state
  | Anvil ->
      let anvil_pre_state = chip_pre_state Anvil col select_col state in
      chip_adder col Anvil anvil_pre_state
  | Bomb ->
      let bomb_pre_state = chip_pre_state Bomb col select_col state in
      chip_adder col Bomb bomb_pre_state

(**[check_win_future st chip col] is true if [st] has outcome [Win]
   after adding chip type [chip] to column [col], else false*)
let check_win_future (st : t) (chip : chip) (col : int) =
  match check_win all_eq (get_board (add_chip chip col st)) with
  | exception _ -> false
  | 0 -> false
  | _ -> true

(**[check_cols st chip col] is the list of int*chip combinations from
   ([col],[chip]) to (8,[chip]) that result in a win.*)
let rec check_cols (st : t) (chip : chip) (col : int) :
    (int * chip) list =
  if col >= 8 then []
  else if check_win_future st chip col then
    (col, chip) :: check_cols st chip (col + 1)
  else check_cols st chip (col + 1)

let win_moves st =
  check_cols st Normal 1 @ check_cols st Anvil 1 @ check_cols st Bomb 1

(**[lst_eq lst] is true if all elements of lst are equal.*)
let rec lst_eq = function
  | [] | [ _ ] -> true
  | a :: (b :: t as tt) -> if a = b then lst_eq tt else false

(**[three_eq x1 x2 x3 x4] is the non-zero element if three elements are
   equal and not zero, and the other is 0, else is 0*)
let three_eq x1 x2 x3 x4 =
  let x_list = [ x1; x2; x3; x4 ] in
  let x_not_z = x_list |> List.filter (fun x -> x <> 0) in
  if x_not_z |> List.length = 3 then
    if x_not_z |> lst_eq then List.hd x_not_z else 0
  else 0

(**[moves] is the list of all possible moves in column number*chip type
   format*)
let moves =
  let rows = [ 1; 2; 3; 4; 5; 6; 7 ] in
  let norm = rows |> List.map (fun x -> (x, Normal)) in
  let bomb = rows |> List.map (fun x -> (x, Bomb)) in
  let anvil = rows |> List.map (fun x -> (x, Anvil)) in
  let _ =
    (*check to see if have all chip types*)
    match Normal with
    | Normal -> ()
    | Anvil -> ()
    | Bomb -> ()
  in
  norm @ anvil @ bomb

let three_moves st =
  let cur_pl = st |> get_curr_player in
  moves
  |> List.filter (fun (col, chip) ->
         match
           check_win three_eq (st |> add_chip chip col |> get_board)
         with
         | c -> c = cur_pl
         | exception _ -> false)

(**[high_mid_comp a b] is negative if a is higher priority, according to
   high_mid_move, else is positive.*)
let high_mid_comp (high_a, mid_a, left_a) (high_b, mid_b, left_b) =
  if high_a > high_b then -1
  else if high_b > high_a then 1
  else if mid_a < mid_b then -1
  else if mid_b < mid_a then 1
  else compare left_a left_b

let cols = [ 1; 2; 3; 4; 5; 6; 7 ]

let high_mid_move st =
  let highest = get_board st |> List.map height in
  let mid_most = cols |> List.map (fun x -> abs (x - 4)) in
  cols
  |> List.map (fun x ->
         (List.nth highest (x - 1), List.nth mid_most (x - 1), x))
  |> List.sort high_mid_comp
  |> List.map (fun (_, _, x) -> x)
  |> List.filter (fun x ->
         match add_chip Normal x st |> check_outcome with
         | exception _ -> false
         | _ -> true)

(**[prohibited_moves st] is the list of moves that will set up an
   immediate win for the next player in [st]*)
let prohibited_moves st =
  moves
  |> List.filter (fun (col, chip) ->
         match win_moves (st |> add_chip chip col) with
         | h :: t -> true
         | [] -> false
         | exception _ -> false)

let opp_wins st =
  let next_pl = next_player st in
  let next_pl_st = { st with current_pl = next_pl } in
  cols
  |> List.filter (fun x ->
         match add_chip Normal x next_pl_st |> check_outcome with
         | Win next_pl -> true
         | _ -> false)

let cpu_move st =
  let winmoves = win_moves st in
  if winmoves <> [] then winmoves |> List.hd
  else
    let opp_winmoves = opp_wins st in
    if opp_winmoves <> [] then (opp_winmoves |> List.hd, Normal)
    else
      let prohibited = prohibited_moves st in
      let three_moves_filt =
        st |> three_moves
        |> List.filter (fun x -> List.mem x prohibited = false)
      in
      if three_moves_filt <> [] then three_moves_filt |> List.hd
      else (high_mid_move st |> List.hd, Normal)

let is_cpu st =
  let curr_pl = get_curr_player st in
  st.player_list |> List.find (fun x -> x.player_id = curr_pl)
  |> fun x -> x.is_cpu

let set_to_cpu pl_id st =
  let new_pl_list =
    st.player_list
    |> List.map (fun x ->
           if x.player_id = pl_id then { x with is_cpu = true } else x)
  in
  { st with player_list = new_pl_list }

let powerchips_to_str pl_id st =
  let chips =
    st.player_list |> List.find (fun x -> x.player_id = pl_id)
    |> fun x -> x.chip_count
  in
  get_pl_name pl_id st ^ " has "
  ^ string_of_int chips.anvil
  ^ " Anvil chips, "
  ^ string_of_int chips.bomb
  ^ " Bomb chips"

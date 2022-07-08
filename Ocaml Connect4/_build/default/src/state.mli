(** Representation of a game state of Connect Four, with functions and
    types that be used to manipulate the game state, and get information
    about specific aspects of the game state.*)

type t
(**t represents the current game state, which includes a board with
   pieces and the player with next turn*)

type chip =
  | Normal
  | Anvil
  | Bomb
      (**[chip] is the type of chip used. [Anvil] destroys all chips
         under it. [Bomb] destroys the chips directly to its bottom
         left, bottom right, and bottom. *)

val init : t
(**[init] is the empty 6 row, 7 column board with player 1 having
   current turn, each player have 1 Bomb and 1 Anvil chip, player 1
   having default name Bob and player 2 having default name Joe*)

val add_chip : chip -> int -> t -> t
(**[add_chip chip col state] is [state] with a chip of type [chip]
   placed at column [col] by the player with current turn in [state] and
   the new next player decided and the chip count for power chips
   decremented. Raises [Failure "ColumnFull"] if column is already full.
   Raises [Failure "InvalidColumn"] if col < 0 or > number of columns in
   state's board. Raises [Failure "NotEnoughChips"] if current player
   has zero chips of type [chip].*)

(**outcome reprsents the outcome of each turn*)
type outcome =
  | Win of int
  | Tie
  | NoOutcome

val check_outcome : t -> outcome
(**[check_outcome state] is the outcome of state*)

val get_board : t -> int list list
(**[get_board state] is the board of state as an int list list. Each int
   list is a column, with the first element being the bottom row, last
   element being top row. The first int list is the left-most column,
   the last int list is the right most column. 0 is no chips, 1 is
   player 1's chip, etc*)

val get_curr_player : t -> int
(**[get_curr_player t] is the id of the player who is currently playing
   at the moment*)

val popout : int -> int -> t -> t
(**[popout i j st] is [st] with the [i] row [j] column chip popped out.
   A popped chipped disappears and all chips above fall down and fill
   in. Bottom row is 1st row, leftmost column is 1st column*)

val set_pl_name : int -> string -> t -> t
(**[set_pl_name pl_id name st] is [st] with the player name of player id
   [pl_id] set to [name]. [Requires: pl_id is a valid player id]*)

val get_pl_name : int -> t -> string
(**[get_pl_name pl_id st] is player name of the player with player id
   [pl_id] in [st]. [Requires: pl_id is a valid player id]*)

val win_moves : t -> (int * chip) list
(**[win_moves st] is the list of all the immediate winning moves for the
   current player in [st]. List is unordered. Should return every
   winning column int and chip type combination.*)

val three_moves : t -> (int * chip) list
(**[three_moves st] is the list of all the moves which will immediately
   set up a three in a row for the current player in [st]. Three in a
   rows include chip layouts in which the missing chip is the 2nd or 3rd
   in a 4 in a row. List is unordered. Should return every three in a
   row column int and chip type combination.*)

val high_mid_move : t -> int list
(**[high_mid_mov st] is the list of column ints for move with preference
   for the highest height, tie-breaking by closest to the middle,
   tie-breaking by the left most. Moves that will raise an error are
   removed.*)

val opp_wins : t -> int list
(**[opp_wins st] is list of all columns that will give next player is
   [st] a win with a normal chip.*)

val cpu_move : t -> int * chip
(**[cpu_move st] is the column int, chip type pair that the cpu will
   chose as their move based off [st], assuming that the cpu is the
   current player in [st]. First the cpu prefers immediate winning
   moves, then prohibits moves which set up immediate winning moves for
   opponent, then prefers moves which set up three in a row, then
   prefers the highest move, tie-broken by most middle.*)

val is_cpu : t -> bool
(**[is_cpu st] is true if the current player in [st] is a cpu*)

val set_to_cpu : int -> t -> t
(**[set_to_cpu pl_id st] is [st] with the player of id [pl_id] set to
   cpu.*)

val powerchips_to_str : int -> t -> string
(**[powerchips_to_str pl_id st] is a string representation of the
   powerchips left for the player of id [pl_id] in [st].*)

val get_chip_count : chip -> t -> int
(**[get_chip_count chip st] is the number of chips of type [chip] left
   for the current player in [st]. Returns 999 for [Normal] since there
   are infinite [Normal] chips.*)
(** The type [t] represents an abstract pocamel which will be used in a battle.
 * A pocamel has a name, types, level, hp, max_hp, attack, defense, speed, 
 * accracy evasion, buffs, moves, item.*)
type t

(** The [move] type represents a move that a pocamel can do, with limited
 * stock.*)
type move 

(** The type [stat] represents a particular statistic*)
type stat = int

exception IllegalMove

val empty : t

(**[from_json json] returns a pocamel object from a json file denoting
   name and stats*)
val from_json : Yojson.Basic.json -> t

(**[get_stat_list t] returns the current stats of the pocamel, in the following
   order: Level, HP, Max HP, Attack, Defense, Speed, Accuracy, Evasion*)
val get_stat_list : t -> int list

(**[get_type t] returns a 2-tuple of strings denoting the type of the pocamel
   If the pokemon only has 1 type, then the  *)
val get_type : t -> string * (string option)

(**[get_buffs t] returns the current amount of buffs in the same order
   as the stat list, with 0's meaning no buffs. *)
val get_buffs : t -> int list

(**
 * [attack pk1 pk2 moveList move] returns a pocamel after another pocamel has
 * made a move on it
 * Effects: None
 * @param moveList list of moves
 * @param move the name of the move to be executed
 * @param pk1 the move maker pocamel
 * @param pk2 the recipient of the move
 * @param whether its a real event or not
 * @return the modified pocamel
*)
val attack : t -> t -> Move.t list -> string -> bool -> t

(**
 * [poke_rep pk] returns a string representing pk's name and hp
 * Effects: None
 * @param pk the pocamel to print
 * @return the string representing the pk's name and hp
*)
val poke_rep : t -> string

(**
 * [formatPocamel pk] returns a string of the pk's name
 * Effects: None
 * @param pk the pocamel to get the move from
 * @return the string representing the pk;s name
*)
val name : t -> string

(**
 * [hp pk] returns the hp
 * Effects: None
 * @param pk the pocamel to get the hp from
 * @return an int for the hp
*)
val hp : t -> int

(**
 * [formatPocamel pk] returns a string representing pk
 * Effects: None
 * @param pk the pocamel to print
 * @return the string representing the pk
*)
val use_item : t -> Item.t -> t

(**
 * [use_elixir pk item] returns a pk which has the elixir used on it
 * Effects: None
 * @param pk the pocamel to use the elixir on
 * @param item the item to use
 * @return a new pk which has the elixir used on it
*)
val use_elixir : t -> Item.t -> t 

(**
 * [move_list pk] returns the moves of pk
 * Effects: None
 * @param pk the pocamel whose moves to print
 * @return a list of moves of the pocamel
*)
val move_list : t -> move list

(**
 * [formatMove pk] returns a string representing move
 * Effects: None
 * @param moveList the list of Moves.t to use
 * @param move the move to format
 * @param pk the pocamel to whom the move belongs
 * @return the string representing the move
*)
val formatMove : t -> Move.t list -> move -> string

(**
 * [formatPocamel pk] returns a string representing pk
 * Effects: None
 * @param pk the pocamel to print
 * @return the string representing the pk
*)
val formatPocamel : t -> string

(**
 * [stock mv] returns the stock of mv
 * Effects: None
 * @param mv the move to get the stock of
 * @return the stock of the move
*)
val stock : move -> int

(**
 * [move_name mv] returns the name of mv
 * Effects: None
 * @param mv the move to get the name of
*)
val move_name : move -> string

(**
 * [get_stock pk mv] returns the stock of the move of pk 
 * whose name matches the mv
 * Effects: None
 * @param mv the name of the move to get the stock of
 * @param pk the pocamel to whom the move belongs
 * @return the stock of the move
*)
val get_stock : t -> string -> int

(**
 * [decrement_stock_helper pk1 move] decrements the stock of a certain move
 * Effects: None
 * @param pk1 the pocamel whose moves to dock
 * @param move the name of hte move to dock
 * @return a new pocamel
*)
val decrement_stock : t -> string -> t

(**
 * [get_speed pk] returns the speed of pk
 * Effects: None
 * @param pk the pocamel to get the speed of
 * @return the speed of the pocamel
*)
val get_speed : t -> int

(**
 * [get_type pk] returns the type of pk
 * Effects: None
 * @param pk the pocamel to get the type of
 * @return the type of the pocamel
*)
val get_type : t -> string

(**
 * [level pk] returns the level of pk
 * Effects: None
 * @param pk the pocamel to get the level of
 * @return the level of the pocamel
*)
val level : t -> int

(**
 * [get_type pk] returns the max hp of pk
 * Effects: None
 * @param pk the pocamel to get the max hp of
 * @return the max hp of the pocamel
*)
val max_hp : t-> int

(**
 * [catch item pk] returns a tuple of a unit and boolean based off of whether or
 * not the target pocamel pk was caught based off of all the criteria that are
 * standard to the pokemon game including ball type, pocamel health, and of
 * course luck!
 * Effects: Prints the result of the attempted catch
 * @param item the ball that is being used
 * @param pk the target pocamel being caught
 * @return a tuple of the result print and a boolean of whether or not it was
 * caught
*)
val catch : Item.t -> t -> bool

(**
 * [vary pk] takes in a pocamel and returns the same exact pocamel except the 
 * level which is recalculated to be some amount higher.
 * Effects: none
 * @param pk the pocamel who's level is changed
 * @return the pocamel with changed level
*)
val vary : t -> t
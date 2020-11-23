(** The type [t] represents a move which is an action that a pocamel can do.
 * For the pupose of calculation, a move has a name, power, accuracy, desc,
 * stock, and a movetype.
*)
type t

(**
 * [move_from_json json] parses a move type froma Yojson file
 * representation.
 * Effects: None
 * @param json the Yojson representation of the json doc
 * @return the parsed move type
*)
val move_from_json : Yojson.Basic.json -> t

(**
 * [power move] returns the power of a move.
 * Effects: None
 * @param move the move whose power will be returned
 * @return the power
*)
val power : t -> int

(**
 * [move_name move] returns the name of a move.
 * Effects: None
 * @param move the move whose name will be returned
 * @return the name
*)
val move_name : t -> string

(**
 * [move_type move] returns the type of a move.
 * Effects: None
 * @param move the move whose type will be returned
 * @return the type
*)
val move_type : t -> string

(**
 * [move_stock move] returns the stock of a move.
 * Effects: None
 * @param move the move whose stock will be returned
 * @return the stock
*)
val move_stock : t -> int

(**
 * [move_acc move] returns the acc of a move.
 * Effects: None
 * @param move the move whose acc will be returned
 * @return the ac
*)
val move_acc : t -> int
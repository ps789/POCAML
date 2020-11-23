open Yojson.Basic.Util

(** The type [t] represents a move which is an action that a pocamel can do.
 * For the pupose of calculation, a move has a name, power, accuracy, desc,
 * stock, and a movetype.
*)
type t = {
  name : string;
  power : int;
  acc : int;
  desc : string;
  stock : int;
  move_type : string;
}

(**
 * [move_from_json json] parses a move type froma Yojson file
 * representation.
 * Effects: None
 * @param json the Yojson representation of the json doc
 * @return the parsed move type
*)
let move_from_json json = {
  name = json |> member "name" |> to_string;
  power = json |> member "power" |> to_int;
  acc = json |> member "acc" |> to_int;
  desc = json |> member "desc" |> to_string;
  stock = json |> member "stock" |> to_int;
  move_type = json |> member "type" |> to_string;
}

(**
 * [move_type move] returns the type of a move.
 * Effects: None
 * @param move the move whose type will be returned
 * @return the type
*)
let move_type move = move.move_type

(**
 * [power move] returns the power of a move.
 * Effects: None
 * @param move the move whose power will be returned
 * @return the power
*)
let power move = move.power

(**
 * [move_name move] returns the name of a move.
 * Effects: None
 * @param move the move whose name will be returned
 * @return the name
*)
let move_name move = move.name

(**
 * [move_stock move] returns the stock of a move.
 * Effects: None
 * @param move the move whose stock will be returned
 * @return the stock
*)
let move_stock move = move.stock

(**
 * [move_acc move] returns the acc of a move.
 * Effects: None
 * @param move the move whose acc will be returned
 * @return the ac
 *)
 let move_acc move = move.acc
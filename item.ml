open Yojson.Basic.Util

(** The type [t] represents an item with a certain id and a count of its amount
 * and a int option representing, if it's an elixer, the heal amount.*)
type t = {
  id : string;
  count : int;
  elixir : int option;
}

(** 
 * [zero_eq z] checks to see if z is equal to 0.
 * Effects: None
 * @requires z is an integer
 * @param z the number compared
 * @return boolean equality to zero.
*)
let zero_eq z = z = 0

(** 
 * [item_from_json z] parses in a type item from a Yojson representation.
 * Effects: None
 * @requires json is a valid Yojson representation.
 * @param json the Yojson representation.
 * @return the parses item object
*)
let item_from_json json = {
  id = json |> member "id" |> to_string;
  count = json |> member "count" |> to_int;
  elixir = if(json |> member "elixir" |> to_int |> zero_eq) then
      None
    else Some (json |> member "elixir" |> to_int);
}

(** 
 * [formatItem item] returns a formatted toString of item.
 * Effects: None
 * @param  item the item to be formatted.
 * @return the string representation of the item.
*)
let formatItem item = String.concat "\n" [item.id; string_of_int item.count]

(** 
 * [name item] returns the name of an item.
 * Effects: None
 * @param  item the item whose name is returned.
 * @return name of the item
*)
let name item = item.id

(** 
 * [elixer item] returns the elixer of an item.
 * Effects: None
 * @param item the item whose elixer is returned.
 * @return elixer of an item
*)
let elixir item = item.elixir

(** 
 * [noItem item] checks to see if the item id is None
 * Effects: None
 * @param item the item whose id is being compared
 * @return whether or not the id of the item is None.
*)
let noItem item = item.id = "None"

(** 
 * [countZero item] returns whether or not the amount of the item is 0.
 * Effects: None
 * @param item the item that is being checked for count.
 * @return whether or not that item has count 0
*)
let countZero item = item.count = 0

(** 
 * [decrCount item] decrements the amount of an item that the person will have.
 * Effects: None
 * @param item the item whose amount is decreased
 * @return the modified item type
*)
let decrCount item = {
  item with count = item.count-1
}
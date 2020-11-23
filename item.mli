(** The type [t] represents an item with a certain id and a count of its amount
 * and a int option representing, if it's an elixer, the heal amount.*)
type t

(** 
 * [item_from_json z] parses in a type item from a Yojson representation.
 * Effects: None
 * @requires json is a valid Yojson representation.
 * @param json the Yojson representation.
 * @return the parses item object
*)
val item_from_json : Yojson.Basic.json -> t

(** 
 * [noItem item] checks to see if the item id is None
 * Effects: None
 * @param item the item whose id is being compared
 * @return whether or not the id of the item is None.
*)
val noItem : t -> bool

(** 
 * [name item] returns the name of an item.
 * Effects: None
 * @param  item the item whose name is returned.
 * @return name of the item
*)
val name : t -> string

(** 
 * [elixer item] returns the elixer of an item.
 * Effects: None
 * @param item the item whose elixer is returned.
 * @return elixer of an item
*)
val elixir : t -> int option

(** 
 * [formatItem item] returns a formatted toString of item.
 * Effects: None
 * @param  item the item to be formatted.
 * @return the string representation of the item.
*)
val formatItem : t -> string

(** 
 * [decrCount item] decrements the amount of an item that the person will have.
 * Effects: None
 * @param item the item whose amount is decreased
 * @return the modified item type
*)
val decrCount : t -> t

(** 
 * [countZero item] returns whether or not the amount of the item is 0.
 * Effects: None
 * @param item the item that is being checked for count.
 * @return whether or not that item has count 0
*)
val countZero : t -> bool
(** The type [t] represets a location which is a coordinate tuple int,int.
*)
type t

(**
 * [location_from_json json] parses a location type froma Yojson file
 * representation.
 * Effects: None
 * @param json the Yojson representation of the json doc
 * @return the parsed location type
*)
val location_from_json : Yojson.Basic.json -> t

(**
 * [get_x loc] get the x coordinate of a loc
 * Effects: none
 * @param loc the location
 * @return the x coord of the loc
*)
val get_x : t -> int

(**
 * [get_y loc] get the y coordinate of a loc
 * Effects: none
 * @param loc the location
 * @return the y coord of the loc
*)
val get_y : t -> int

(**
 * [new_loc x y] returns a new location with the x coordinate x and the y
 * coordinate y.
 * Effects: none
 * @param x the x coordinate
 * @param y the y coordinate
 * @return the new coordinate (x,y)
*)
val new_loc : int -> int -> t
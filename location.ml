open Yojson.Basic.Util

(** The type [t] represets a location which is a coordinate tuple int,int.
*)
type t = int*int

(**
 * [location_from_json json] parses a location type froma Yojson file
 * representation.
 * Effects: None
 * @param json the Yojson representation of the json doc
 * @return the parsed location type
*)
let location_from_json json = (json |>  member "x" |> to_int,
                               json |> member "y" |> to_int)

(**
 * [get_x loc] get the x coordinate of a loc
 * Effects: none
 * @param loc the location
 * @return the x coord of the loc
*)
let get_x loc = fst loc

(**
 * [get_y loc] get the y coordinate of a loc
 * Effects: none
 * @param loc the location
 * @return the y coord of the loc
*)
let get_y loc = snd loc

(**
 * [new_loc x y] returns a new location with the x coordinate x and the y
 * coordinate y.
 * Effects: none
 * @param x the x coordinate
 * @param y the y coordinate
 * @return the new coordinate (x,y)
*)
let new_loc (x: int) (y: int)  : t = (x, y)
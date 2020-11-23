(** The type [t] represents a trainer who will be commanding their pocamels.
 * Multiple trainers participate in a battle.
*)
type t

exception IllegalPocamel
exception IllegalItem
exception IllegalMove
exception TooManyPocamels


type orientation = North | South | East | West

(**
 * [trainer_from_json json] parses a trainer type from a Yojson file
 * representation.
 * Effects: None
 * @param json the Yojson representation of the json doc
 * @return the parsed trainer type
*)
val trainer_from_json : Yojson.Basic.json -> t

(**
 * [get_items trainer] returns a list of items that the trainer has
 * representation.
 * Effects: None
 * @param trainer the owner of the items
 * @return the trainer's items
*)
val get_items : t -> Item.t list

(**
 * [get_pocamel trainer poke] returns the pocamel and all of its information
 * based off of the pokemon that the trainer has. Throws IllegalPocamel if the
 * trainer doesn't have that pocamel.
 * Effects: None
 * @param trainer who is being accessed for the pocamel
 * @return the target pocamel
*)
val get_pocamel : t -> string -> Pocamel.t

(**
 * [pocamels trainer] returns all of the pocamels that a trainer has.
 * Effects: None
 * @param trainer th owner of the pocamels
 * @return the list of pocamels a trainer has
*)
val pocamels : t -> Pocamel.t list

(**
 * [go trainer] returns the tail of all the pocamels a trainer has.
 * Effects: None
 * @param trainer the owner of the pocamels
 * @return the tail of the list of pocamels.
*)
val go : t -> t

(**
 * [use_item trainer pk item] returns a trainer with the trainer having used an
 * item on a pocamel. Throws IllegalPocamel if the pocamel can't be found, or 
 * IllegalItem if the item can't be used.
 * Effects: None
 * @param trainer the owner of the pocamels
 * @param pk the target pocamel to use an item on
 * @param item the item to be used
 * @return the tail of the list of pocamels.
*)
val use_item : t -> string -> string -> t

(**
 * [replace_trainer_pocamels] returns a trainer with one of its pocamels
 * being modified, speicifically the one with the same name as pk
 * Effects: None
 * @param pk the target pocamel to change in the Trainer's pocamel list
 * @param trainer the trainer to be modified
 * @return trainer with the modified pocamels
*)
val replace_trainer_pocamels : Pocamel.t -> t -> t

(**
 * [get_lor_or trainer] gets a tuple of the location and an orientation of 
 * a trainer
 * Effects: None
 * @param trainer the trainer whose information is returned
 * @return the location orientation tuple of a trainer
*)
val get_loc_or : t -> Location.t * orientation

(**
 * [use_pocaball tr1 tr2 pk item] simulates the event of tr1 throwing a po-ball
 * at tr2 active pocamon. Catch rates and mechanics are completely determined
 * using pokemon rules, and it is dependant on health, kind of ball, and luck.
 * This method returns the resultant trainers after the attempted catch.
 * Throws exceptions if tr1 either has 6 pocamels, doesn't have the pocamball
 * which is used, or if the target pocamel isn't the one who is out.
 * Effects: Prints information about the catch
 * @param tr1 the trainer throwing the pocamball
 * @param tr2 the trainer whose pocamball is being caught
 * @param pk the target pk to be caught
 * @param item the pocamball being thrown
 * @return the resultant trainers after the attempted catch
*)
val use_pocaball : t -> t -> Pocamel.t -> string -> t array * bool

(**
 * [name t] returns the name of a trainer
 * Effects: None
 * @param trainer the trainer name is returned
 * @return the name of the trainer
*)
val name : t -> string

(**
 * [vision_tile trainer] gets a tuple of the location in front of a trainer
 * based off of their location and the orientation they are standing in.
 * Effects: None
 * @param trainer the trainer whose adjacent tile is returned
 * @return the location of the tile in front of the trainer
*)
val vision_tile : t -> Location.t

(**
 * [set_name trainer name] returns the same trainer with name changed to name
 * Effects: None
 * @param trainer the trainer whose name is changed
 * @param name the name to be changed to
 * @return the new trainer
*)
val set_name : t -> string -> t

(**
 * [turn trainer ori] returns the same trainer with orientaiton changed to ori
 * Effects: None
 * @param trainer the trainer whose orientation is changed
 * @param ori the orientation to be changed to
 * @return trainer with new orientation
*)
val turn : t -> orientation -> t

(**
 * [set_location trainer newLoc] returns the same trainer but in a differen
 * location specified by newLoc
 * Effects: None
 * @param trainer the trainer whose location is changed.
 * @param newLoc the location to be changed to
 * @return the trainer with a new location
*)
val set_location : t -> Location.t -> t

(**
 * [wild pocamel] returns the wild trainer with a target pocamel as its only
 * pocamel. This is how pocamels exist naturally in the wild, under a psuedo
 * trainer.
 * @param pocamel
 * @return the wild trainer with just target pocamel
*)
val wild : Pocamel.t -> t
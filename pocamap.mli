type map_object = Grass | Tree

exception Out_of_bounds

(** The type t represents a pocamap where the player will traverse in the game*)
type t 

(**
 * [from_json json map ul lr] parses in the map object from the json file
 * in the required Yojson format. A map has a plane, the wild pocamels, and 
 * rate.
 * Effects: None
 * @param json the valid Yojson type representing the game
 * @return the parsed in map
*)
val from_json : Yojson.Basic.json -> t

(**
 * [print map] prints the map based off of its dimensions and the objects inside
 * of it.
 * Effects: prints the pocamap
 * @param map the hashtable of the objects and all their coordinates
 * @return unit 
*)
val print : t -> Trainer.t -> unit

(**
 * [updateTrainers map trainerList] updates the pocamap based off of the trainer
 * list in a game.
 * Effects: None
 * @param map the hashtable of the objects and all their coordinates
 * @param trainerList the list of trainers in the game.
 * @return the map with all of the trainers updated
*)
val updateTrainers : t -> Trainer.t list -> t

(**
 * [mapTile map Loc] gets the hash table value correlated with the key of the 
 * location with x coordinate x and y coordinate y
 * Effects: none
 * @param map the hashtable of the objects and all their coordinates
 * @param loc the location
 * @return unit 
*)
val mapTile : t -> Location.t -> map_object option * Trainer.t option

(**
 * [wild_pocamels map] returns a list of all possible wild pocamels in the map
 * Effects: 
 * @param map the hashtable of the objects and all their coordinates
 * @return the list of all wild pocamels in the Game
*)
val wild_pocamels : t -> Pocamel.t list

(**
 * [encounter_rate map] returns the encounter rate of a map
 * Effects: none
 * @param map the hashtable of the objects and all their coordinates
 * @return float reprsentation of the rate
*)
val encounter_rate : t -> float

val clear_map : int -> int -> unit

val mapRows : t -> int

val mapColumns : t -> int
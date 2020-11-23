(** 
 * The type [t] represents the arbitrary game type, which has certain 
 * boundaries, a player, opponenets, moves, items, and the current battle.*)
type t

(** 
 * The type [location] is a certain location inluding all of the details 
 * provided by Location module.*)
type location

(**
 * [from_json j] is the game that [j] represents.
 * Requires: [j] is a valid JSON game representation.
 *
 * @param Yojson.Basic.json a read in json file representation
 * @return an game representation of the Yjson.Basic.jsonobject
*)
val from_json : Yojson.Basic.json -> t

(** 
 * [moves game] returns all of the moves that a certain game allows, a sort
 * a kind of lexicon.
 * Effects: none
 * @param game the game that a battle occurs in
 * @return all of the moves in the game lexicon
*)
val moves : t -> Move.t list

(** 
 * [get_player t] gets the main player from the game.
 * Effects: none
 * @param game the game state
 * @return the main player from a game
*)
val get_player : t -> Trainer.t

(** 
 * [get_battle game] gets the battle if it exists, and otherwise throws Not_
 * Found error.
 * Effects: none
 * @param game the game state
 * @return the battle of a game state
*)
val get_battle : t -> Battle.t

(** 
 * [set_battle game currBattle] sets the battle of the game to the current
 * battle that is passed in.
 * Effects: none
 * @param game the game state
 * @currBattle the current Battle of the game
 * @return a game with intiialized current battle.
*)
val set_battle : t -> Battle.t -> t

(** 
 * [autoBattle game] sets up a game based off of the default conditions from
 * the json.
 * Effects: none
 * @param game the game state
 * @return a game with intiialized opponent.
*)
val autoBattle : t -> t

(**
 * [start_battle opponent game] starts the battle for game game with some
 * opponent.
 * Effects: none
 * @param opponent the Trainer type that will be the opponent in the battle
 * @param game the game state
 * @return a game with intiialized opponent.
*)
val start_battle : Trainer.t -> t -> string -> t

(**
 * [get_map game] returns the current map of a game
 * Effects: none
 * @param game the game state
 * @return the map of a game
*)
val get_map : t -> Pocamap.t

(**
 * [update_map game] returns the current map of a game weith nothing changed
 * except it updates the trainers.
 * Effects: none
 * @param game the game state
 * @return the updated map of the game
*)
val update_map : t -> t

(**
 * [opponents game] returns the opponent trainers that exist in a game.
 * Effects: none
 * @param game the game state
 * @return the opponents of the game
*)
val opponents : t -> Trainer.t list

(**
 * [is_in_battle game] returns whether or not the current game state is in 
 * battle or not
 * Effects: none
 * @param game the game state
 * @return the battle condition of the game
*)
val is_in_battle : t -> bool

(**
 * [move game dir] moves the player in the direction dir in the game state
 * that the player is currently in.
 * Effects: none
 * @param game the game state
 * @param dir the direction that the player needs to move in
 * @return resultant game from moving in that dir 
*)
val move : t -> Trainer.orientation -> t

(**
 * [get_front game] returns the Pocamap tile in front of the current player.
 * Effects: none
 * @param game the game state
 * @return the tile in front of the player.
*)
val get_front : t -> Pocamap.map_object option * Trainer.t option

(**
 * [toggle_battle game] toggles the battle from its current state to the inverse
 * of the current state. Ie: form in to out and vice versa.
 * Effects: none
 * @param game the game state
 * @return game with battle state inverted
*)
val toggle_battle : t -> t

(**
 * [update_player game] updates the game with a current state battle to take
 * the Battle's player and sets that for the game.
 * Effects: none
 * @param game the game state
 * @return the game with the player updated
*)
val update_player : t -> t

(**
 * [calc_encounter game] returns whether or not there is an encounter with a 
 * wild pocamel
 * Precondition: trainer is not in tree our out of bounds
 * Effects: none
 * @param game the game state
 * @return whether or not there is an encounter
*)
val calc_encounter : t -> bool

(**
 * [encounter game] based on whether or not there should be a wild encounter
 * based off of the random calculator defined above, either sets up a wild
 * encounter or just keeps the game state.
 * Effects: none
 * @param game the game state
 * @return either a wild battle interaction or just the game
*)
val encounter : t -> t 

(**
 * [get_starting_pocamel game] returns the starting pocamel of a certain game
 * state
 * Effects: none
 * @param game the game state
 * @return the pocamel that is stating for the current trainer
*)
val get_starting_pocamel : t -> string

(**
 * [set_starting_pocamel game pocamel] sets the starting pocamel of the trainer 
 * to pocamel
 * Effects: none
 * @param game the game state
 * @param pocamel the pocamel to set to
 * @return whether the game state with the starting pocamel changed
*)
val set_starting_pocamel : t -> string -> t

(**
 * [starting_pocamel_dead game] returns whether or not the starting pocamel of 
 * the trainer is dead
 * Effects: none
 * @param game the game state
 * @return whether or not the trainers starting pocamel has fainted
*)
val starting_pocamel_dead : t -> bool
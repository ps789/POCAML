(** 
 * The represents the abstract data type of a battle, which involves two 
 * trainers, and their two pocamels 
*)
type t

(** 
 * A result is whether or not an action in a battle is possible 
*)
type result = Legal of t | Illegal

(**
 * [attack isTrainer battle moveList move] triggers an attack from either 
 * the trainer's pocamel if isTrainer is true or the enemies pocamel if false.
 * This is based off of the game-wide move database, and an illegal move
 * excpetion is thrown if the move doesn't exist or can't be called.
 * Side Effects: None
 * @param isTrainer whether or not its the trainers move
 * @param battle the current battle state
 * @param moveList the moveList of the game
 * @param move the move that the current user is trying to attack with
 * @return the pocamel that is occurs as a result of being attacks by target
*)
val attack : bool -> t -> Move.t list -> string -> result

(**
 * [switch isTrainer battle pocamelName] switches the currently out pocamel with 
 * name pocamelName with another pocamel in the users party. Throws exception
 * if you try to switch with a fainted pokemon or pokemon not in the party.
 * Side Effects: None
 * @param isTrainer whether or not its the trainers move
 * @param battle the current battle state
 * @param pocamelName the pocamel to be switched to
 * @return the resultant battle state with pocamels switched
*)
val switch : bool -> t -> string -> result

(**
 * [use isTrainer battle pk itemName] uses the target item itemName on the 
 * target pocamel pk. If the item is an elixer, then it goes towards their 
 * health, otherwise the pocamel will hold the item. Returns illegal if
 * either the target pokemon doesn't exist in the party or the item doesn't.
 * Side Effects: None
 * @param isTrainer whether or not its the trainers move
 * @param battle the current battle state
 * @param pk the pocamel to use the item on
 * @param itemName the item to be used
 * @return the resultant battle state with item used
*)
val use : bool -> t -> string -> string -> result

(**
 * [init_battle t1 t2] initiates a battle between trainer t1 t2 and their
 * pocamels
 * Side Effects: None
 * @param t1 trainer 1
 * @param t2 trainer 2
 * @return the intiated battle state
*)
val init_battle : string -> Trainer.t -> Trainer.t -> t

(**
 * [player_pocamel battle] returns the pocamel of the player in the battle
 * Side Effects: None
 * @param battle the current state of the battle
 * @return the pocamel of the player
*)
val player_pocamel : t -> Pocamel.t

(**
 * [opponent_pocamel battle] returns the pocamel of the opponent in the battle
 * Side Effects: None
 * @param battle the current state of the battle
 * @return the pocamel of the opponent
*)
val opponent_pocamel : t -> Pocamel.t

(**
 * [player battle] returns the player in the battle
 * Side Effects: None
 * @param battle the current state of the battle
 * @return the player
*)
val player : t -> Trainer.t

(**
 * [opponent battle] returns opponent in the battle
 * Side Effects: None
 * @param battle the current state of the battle
 * @return the opponent
*)
val opponent : t -> Trainer.t

(**
 * [isFinished battle isPlayer] checks if the battle is finished (all pocamels
 * dead on the side indicated by isPlayer)
 * Side Effects: None
 * @param battle the current state of the battle
 * @param isPlayer is the side to be checked the player side
 * @return boolean indicating if the trainer is done
*)
val isFinished : t -> bool -> bool

(**
 * [currPocamelFaint battle isPlayer] checks if the current Pocamel is
 * fainted (all pocamels
 * dead on the side indicated by isPlayer)
 * Side Effects: Prints if the pocamel has fainted or not
 * @param battle the current state of the battle
 * @param isPlayer is the side to be checked the player side
 * @return boolean indicating if pocamel is fained
*)
val currPocamelFaint : t-> bool -> bool

(**
 * [playerFirst battle] checks if the player goes first. If speed is equal,
 * in the player's favor
 * Side Effects: None
 * @param battle the current state of the battle
 * @return boolean indicating if player gets to go first
*)
val playerFirst : t -> bool

(**
 * [use_pocaball battle itemName] uses a pocamball with name Item name from
 * your inventory on the enemy pocamel.
 * Side Effects: None
 * @param battle the current state of the battle
 * @param itemName the name of the pocamball being used
 * @return the state with the pocambal used
*)
val use_pocaball : t -> string -> result

(**
 * [is_wild battle] determines whether or not the battle is with a wild pocamel.
 * Side Effects: None
 * @param battle the current state of the battle
 * @return whethe or not the battle is a wild one
*)
val is_wild : t -> bool
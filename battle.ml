open Command
open Pocamel
open Trainer

(**
 * The represents the abstract data type of a battle, which involves two
 * trainers, and their two pocamels
*)
type t = {
  player_pocamel : Pocamel.t;
  player : Trainer.t;
  opponent_pocamel : Pocamel.t;
  opponent : Trainer.t;
}

(**
 * A result is whether or not an action in a battle is possible
*)
type result = Legal of t | Illegal

(**
 * [init_battle t1 t2] initiates a battle between trainer t1 t2 and their
 * pocamels
 * Side Effects: None
 * @param t1 trainer 1
 * @param t2 trainer 2
 * @return the intiated battle state
*)
let init_battle starting_pocamel t1 t2 = let pocamels1 = Trainer.pocamels t1 in
  let pocamels2 = Trainer.pocamels t2 in
  {
    player_pocamel =
      List.find (fun x -> Pocamel.name x = starting_pocamel) pocamels1;
    player = t1;
    opponent_pocamel = List.hd pocamels2;
    opponent = t2;
  }

(**
 * [attack isTrainer battle moveList move] triggers an attack from either
 * the trainer's pocamel if isTrainer is true or the enemies pocamel if false.
 * This is based off of the game-wide move database, and an illegal move
 * excpetion is thrown if the move doesn't exist or can't be called.
 * Side Effects: Prints any critical hits, effectivity, etc
 * @param isTrainer whether or not its the trainers move
 * @param battle the current battle state
 * @param moveList the moveList of the game
 * @param move the move that the current user is trying to attack with
 * @return the pocamel that is occurs as a result of being attacks by target
*)
let attack isTrainer battle moveList move =
  try
    let attacked_pocamel = if isTrainer then
        (Pocamel.attack battle.player_pocamel
           battle.opponent_pocamel moveList move true)
      else (Pocamel.attack battle.opponent_pocamel
              battle.player_pocamel moveList move true) in
    let playerPocamel =
      if isTrainer then
        (if Pocamel.get_stock battle.player_pocamel move < 1
         then raise IllegalMove else
           (Pocamel.decrement_stock battle.player_pocamel move))
      else attacked_pocamel in
    let opponentPocamel = if isTrainer then attacked_pocamel
      else
        (if Pocamel.get_stock battle.opponent_pocamel move < 1
         then raise IllegalMove else
           Pocamel.decrement_stock battle.opponent_pocamel move) in
    Legal {
      player_pocamel = playerPocamel;
      player = battle.player
               |> Trainer.replace_trainer_pocamels playerPocamel;
      opponent_pocamel = opponentPocamel;
      opponent = battle.opponent
                 |> Trainer.replace_trainer_pocamels opponentPocamel;
    }
  with
    Pocamel.IllegalMove -> Illegal

(**
 * [switch isTrainer battle pocamelName] switches the currently out pocamel with
 * name pocamelName with another pocamel in the users party. Throws exception
 * if you try to switch with a fainted pocamel or pocamel not in the party.
 * Side Effects: Prints come back and go
 * @param isTrainer whether or not its the trainers move
 * @param battle the current battle state
 * @param pocamelName the pocamel to be switched to
 * @return the resultant battle state with pocamels switched
*)
let switch isTrainer battle pocamelName =
  try
    let target_pocamel =
      if isTrainer then Trainer.get_pocamel battle.player pocamelName
      else Trainer.get_pocamel battle.opponent pocamelName in
    if Pocamel.hp target_pocamel < 1 then raise Trainer.IllegalPocamel

    else
      ANSITerminal.(print_string [yellow]
                      ((if(isTrainer)then "\nYou yell: "
                        else "\nYour opponent shouts: ")));
    if(Trainer.name battle.opponent <> "wild" || isTrainer)  then
      (ANSITerminal.(print_string [red] 
                       ("\n\nCome back, "^ 
                        (Pocamel.name (if (isTrainer) 
                                       then battle.player_pocamel else
                                         battle.opponent_pocamel)) ^ "\n"));
       ANSITerminal.(print_string [green]
                       ("Go, " ^ (Pocamel.name target_pocamel) ^ "!\n\n")))
    else (
      ANSITerminal.(print_string [green]
                      ("Go, " ^ (Pocamel.name target_pocamel) ^ "!\n\n")));
    Legal {
      player_pocamel =
        if isTrainer then target_pocamel
        else battle.player_pocamel;
      player = battle.player;
      opponent_pocamel = if isTrainer then battle.opponent_pocamel
        else target_pocamel;
      opponent = battle.opponent;
    }
  with
    Trainer.IllegalPocamel -> Illegal

(**
 * [use isTrainer battle pk itemName] uses the target item itemName on the
 * target pocamel pk. If the item is an elixer, then it goes towards their
 * health, otherwise the pocamel will hold the item. Returns illegal if
 * either the target pocamel doesn't exist in the party or the item doesn't.
 * Side Effects: None
 * @param isTrainer whether or not its the trainers move
 * @param battle the current battle state
 * @param pk the pocamel to use the item on
 * @param itemName the item to be used
 * @return the resultant battle state with item used
*)
let use isTrainer battle pk itemName =
  try
    let new_trainer = if isTrainer
      then Trainer.use_item battle.player pk itemName
      else Trainer.use_item battle.opponent pk itemName in
    Legal {
      player_pocamel = if isTrainer then Trainer.get_pocamel new_trainer
            (battle.player_pocamel |> Pocamel.name) else battle.player_pocamel;
      player = if isTrainer then new_trainer else battle.player;
      opponent_pocamel = if isTrainer then battle.opponent_pocamel
        else Trainer.get_pocamel new_trainer pk;
      opponent = if isTrainer then battle.opponent else new_trainer;
    }
  with
  | Trainer.IllegalPocamel -> Illegal
  | Trainer.IllegalItem -> Illegal

(**
 * [player_pocamel battle] returns the pocamel of the player in the battle
 * Side Effects: None
 * @param battle the current state of the battle
 * @return the pocamel of the player
*)
let player_pocamel battle = battle.player_pocamel

(**
 * [opponent_pocamel battle] returns the pocamel of the opponent in the battle
 * Side Effects: None
 * @param battle the current state of the battle
 * @return the pocamel of the opponent
*)
let opponent_pocamel battle = battle.opponent_pocamel

(**
 * [player battle] returns the player in the battle
 * Side Effects: None
 * @param battle the current state of the battle
 * @return the player
*)
let player battle = battle.player

(**
 * [opponent battle] returns opponent in the battle
 * Side Effects: None
 * @param battle the current state of the battle
 * @return the opponent
*)
let opponent battle = battle.opponent

(**
 * [isFinished battle isPlayer] checks if the battle is finished (all pocamels
 * dead on the side indicated by isPlayer)
 * Side Effects: None
 * @param battle the current state of the battle
 * @param isPlayer is the side to be checked the player side
 * @return boolean indicating if the trainer is done
*)
let isFinished battle isPlayer = let trainer = if(isPlayer) then battle.player
                                   else battle.opponent in
  List.fold_left
    (fun acc x -> acc && Pocamel.hp x = 0) true (Trainer.pocamels trainer)

(**
 * [currPocamelFaint battle isPlayer] checks if the current Pocamel is
 * fainted (all pocamels
 * dead on the side indicated by isPlayer)
 * Side Effects: Prints if the pocamel has fainted or not
 * @param battle the current state of the battle
 * @param isPlayer is the side to be checked the player side
 * @return boolean indicating if pocamel is fained
*)
let currPocamelFaint battle isPlayer =
  let pocamel = if(isPlayer) then battle.player_pocamel
    else battle.opponent_pocamel in
  if(Pocamel.hp pocamel = 0 && Pocamel.name pocamel <> "NONE") then
    (ANSITerminal.(print_string [red]
                     ("\nYour " ^ (if(isPlayer) then "" else "Opponent's ") ^
                      (Pocamel.name pocamel) ^ " has fainted!\n\n"));
     true)
  else false

(**
 * [playerFirst battle] checks if the player goes first. If speed is equal,
 * in the player's favor
 * Side Effects: None
 * @param battle the current state of the battle
 * @return boolean indicating if player gets to go first
*)
let playerFirst battle = Pocamel.get_speed battle.player_pocamel
                         >= Pocamel.get_speed battle.opponent_pocamel
(**
 * [use_pocaball battle itemName] uses a pocamball with name Item name from
 * your inventory on the enemy pocamel.
 * Side Effects: None
 * @param battle the current state of the battle
 * @param itemName the name of the pocamball being used
 * @return the state with the pocambal used
*)
let use_pocaball battle itemName =
  try
    let new_trainers =
      Trainer.use_pocaball
        battle.player battle.opponent battle.opponent_pocamel itemName in
    let (new_trainer1:Trainer.t) = Array.get (fst new_trainers) 0 in
    let (new_trainer2:Trainer.t) = Array.get (fst new_trainers) 1 in

    match new_trainers with
    | (_,true) -> Legal {
        battle with player = new_trainer1;
                    opponent = new_trainer2;
                    opponent_pocamel = Pocamel.empty;
      }
    | (_,false) -> Legal {
        battle with player = new_trainer1;
      }
  with
  | Trainer.IllegalItem -> Illegal
  | Trainer.IllegalMove -> Illegal

(**
 * [is_wild battle] determines whether or not the battle is with a wild pocamel.
 * Side Effects: None
 * @param battle the current state of the battle
 * @return whethe or not the battle is a wild one
*)
let is_wild battle = (Trainer.name battle.opponent) = "wild"

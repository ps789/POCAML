open Pocamel
open Trainer
open Battle
open Yojson.Basic.Util
open Item
open Move
open Pocamap

(** The type [itemtype] represents an item with a certain id and a tool-tip
 * which describes it.*)
type itemtype ={
  id : string;
  tool_tip : string;
}

(** The type [move] is a certain possible move inluding all of the details
 * provided by Move module.*)
type move = Move.t

(**
 * The type [pocamel] is a certain possible pocamel inluding all of the details
 * provided by Pocamel module.*)
type pocamel = Pocamel.t

(**
 * The type [trainer] is a trainer inluding all of the details
 * provided by Trainer module.*)
type trainer = Trainer.t

(**
 * The type [itemtype] is a possible battle inluding all of the details
 * provided by Battle module.*)
type battle = Battle.t option

(**
 * The type [location] is a certain location inluding all of the details
 * provided by Location module.*)
type location = Location.t

(**
 * The type [t] represents the arbitrary game type, which has certain
 * boundaries, a player, opponenets, moves, items, and the current battle.*)
type t ={
  starting_pocamel : string;
  player : trainer;
  opponents : trainer list;
  moves : move list;
  items : itemtype list;
  battle : battle;
  map: Pocamap.t;
  in_battle : bool
}

(**
 * [item_type_from_json json] extracts a item_type type from a json
 * Effects: none
 * @param json the json representation
 * @return an item type
*)
let item_type_from_json json = {
  id = json |> member "id" |> to_string;
  tool_tip = json |> member "tool_tip" |> to_string;
}

(**
 * [from_json json] extracts a game type from a json and uses subsequent
 * from_json methods from other modules to extract their types.
 * Effects: none
 * @param json the json representation
 * @return an game type
*)
let from_json json = {
  starting_pocamel = json |> member "starting pocamel" |> to_string;
  player = json |> member "player" |> Trainer.trainer_from_json;
  opponents = json |> member "opponents" |> to_list 
              |> List.map Trainer.trainer_from_json;
  moves = json |> member "moves" |> to_list |> List.map move_from_json;
  items = json |> member "items" |> to_list |> List.map item_type_from_json;
  battle = None;
  map  = json |> member "map" |> Pocamap.from_json;
  in_battle = false;
}

(**
 * [moves game] returns all of the moves that a certain game allows, a sort
 * a kind of lexicon.
 * Effects: none
 * @param game the game that a battle occurs in
 * @return all of the moves in the game lexicon
*)
let moves (game : t) = game.moves

(**
 * [get_move game move] pulls out the complete move type from an game
 * lexicon based off of its name.
 * Effects: none
 * @requires game must be in the game
 * @param game the game to pull the move from
 * @param move the move to be pulled
 * @return a move type that was queried
*)
let get_move game move = List.find 
    (fun x -> String.lowercase_ascii (Move.move_name x) = 
              String.lowercase_ascii(move)) game.moves

(**
 * [get_player t] gets the main player from the game.
 * Effects: none
 * @param game the game state
 * @return the main player from a game
*)
let get_player t = t.player

(**
 * [start_battle opponent game] starts the battle for game game with some
 * opponent.
 * Effects: none
 * @param opponent the Trainer type that will be the opponent in the battle
 * @param game the game state
 * @return a game with intiialized opponent.
*)
let start_battle opponent game name = 
  if((Trainer.name opponent) <> "wild") 
  then ANSITerminal.
         (print_string [yellow] 
            ("You have challenged Pocamel Trainer " ^ 
             (Trainer.name opponent) ^ " to a 
  Pocamel battle!\n\n"))
  else ();
  if name = "" then {
    game with battle = 
                Some (Battle.init_battle game.starting_pocamel 
                        game.player opponent);
              in_battle = true
  }
  else
    {
      game with battle = Some (Battle.init_battle game.starting_pocamel 
                                 (Trainer.set_name game.player name) opponent);
                in_battle = true
    }


(**
 * [get_battle game] gets the battle if it exists, and otherwise throws Not_
 * Found error.
 * Effects: none
 * @param game the game state
 * @return the battle of a game state
*)
let get_battle game = match(game.battle) with
  |Some b -> b
  |None -> raise Not_found

(**
 * [set_battle game currBattle] sets the battle of the game to the current
 * battle that is passed in.
 * Effects: none
 * @param game the game state
 * @currBattle the current Battle of the game
 * @return a game with intiialized current battle.
*)
let set_battle game currBattle = {
  game with battle = Some currBattle
}

(**
 * [opponents game] returns the opponent trainers that exist in a game.
 * Effects: none
 * @param game the game state
 * @return the opponents of the game
*)
let opponents game = game.opponents

(**
 * [get_map game] returns the current map of a game
 * Effects: none
 * @param game the game state
 * @return the map of a game
*)
let get_map game = game.map

(**
 * [update_map game] returns the current map of a game weith nothing changed
 * except it updates the trainers.
 * Effects: none
 * @param game the game state
 * @return the updated map of the game
*)
let update_map (game: t) = {
  game with map = 
              Pocamap.updateTrainers game.map (game.player :: game.opponents)
}

(**
 * [get_front game] returns the Pocamap tile in front of the current player.
 * Effects: none
 * @param game the game state
 * @return the tile in front of the player.
*)
let get_front game = 
  game.player |> Trainer.vision_tile |> Pocamap.mapTile game.map

(**
 * [is_in_battle game] returns whether or not the current game state is in 
 * battle or not
 * Effects: none
 * @param game the game state
 * @return the battle condition of the game
*)
let is_in_battle game = game.in_battle

(**
 * [move game dir] moves the player in the direction dir in the game state
 * that the player is currently in.
 * Effects: none
 * @param game the game state
 * @param dir the direction that the player needs to move in
 * @return resultant game from moving in that dir 
*)
let move game dir = let turnedGame = {
    game with player = (game.player |> Trainer.turn) dir;
  } in
  try(
    match (turnedGame |> get_front) with
    |(Some Tree, _) -> turnedGame
    |(_, Some _) -> turnedGame
    |_ -> {
        turnedGame with player = Trainer.set_location (turnedGame.player) 
                            (Trainer.vision_tile turnedGame.player)
      }
  )with Pocamap.Out_of_bounds -> turnedGame

(**
 * [toggle_battle game] toggles the battle from its current state to the inverse
 * of the current state. Ie: form in to out and vice versa.
 * Effects: none
 * @param game the game state
 * @return game with battle state inverted
*)
let toggle_battle game = ANSITerminal.erase Screen;{
    game with in_battle = not game.in_battle
  }

(**
 * [autoBattle game] sets up a game based off of the default conditions from
 * the json.
 * Effects: none
 * @param game the game state
 * @return a game with intiialized opponent.
*)
let autoBattle game =  
  toggle_battle(start_battle (List.hd game.opponents) game "")

(**
 * [update_player game] updates the game with a current state battle to take
 * the Battle's player and sets that for the game.
 * Effects: none
 * @param game the game state
 * @return the game with the player updated
*)
let update_player game = 
  match(game.battle) with
  |Some t -> {
      game with player = Battle.player t
    }
  |None -> game

(**
 * [wild_battle game] starts a wild encounter battle in the game, based off
 * random spawn rate from a list of pre-selected possible wild pocamels.
 * Effects: none
 * @param game the game state
 * @return the game with the wild_battle intiated
*)
let wild_battle game = 
  let wildList = (Pocamap.wild_pocamels game.map) in
  let wild_pocamel = (List.nth  wildList (Random.int (List.length wildList))) in
  Pocamap.clear_map (Pocamap.mapRows game.map) (Pocamap.mapColumns game.map);
  ANSITerminal.erase Screen;
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH (Unix.tcgetattr Unix.stdin);
  ANSITerminal.(print_string [red] 
                  ("You have encountered a wild "^
                   (Pocamel.name wild_pocamel)^"!\n\n"));
  start_battle (Trainer.wild wild_pocamel) game ""

(**
 * [calc_encounter game] returns whether or not there is an encounter with a 
 * wild pocamel
 * Precondition: trainer is not in tree our out of bounds
 * Effects: none
 * @param game the game state
 * @return whether or not there is an encounter
*)
let calc_encounter game = Random.float 1.0 < (Pocamap.encounter_rate game.map)

(**
 * [encounter game] based on whether or not there should be a wild encounter
 * based off of the random calculator defined above, either sets up a wild
 * encounter or just keeps the game state.
 * Effects: none
 * @param game the game state
 * @return either a wild battle interaction or just the game
*)
let encounter game = 
  match fst (Pocamap.mapTile game.map 
               (fst (Trainer.get_loc_or game.player))) with
  |Some t -> wild_battle game
  |None -> game

(**
 * [get_starting_pocamel game] returns the starting pocamel of a certain game
 * state
 * Effects: none
 * @param game the game state
 * @return the pocamel that is stating for the current trainer
*)
let get_starting_pocamel game = game.starting_pocamel

(**
 * [set_starting_pocamel game pocamel] sets the starting pocamel of the trainer 
 * to pocamel
 * Effects: none
 * @param game the game state
 * @param pocamel the pocamel to set to
 * @return whether the game state with the starting pocamel changed
*)
let set_starting_pocamel game pocamel = {
  game with starting_pocamel = pocamel
}

(**
 * [starting_pocamel_dead game] returns whether or not the starting pocamel of 
 * the trainer is dead
 * Effects: none
 * @param game the game state
 * @return whether or not the trainers starting pocamel has fainted
*)
let starting_pocamel_dead game = 
  (Trainer.get_pocamel game.player game.starting_pocamel) |> Pocamel.hp = 0

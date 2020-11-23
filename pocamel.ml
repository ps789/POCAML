open Yojson.Basic.Util
open Item
open Move

exception IllegalMove 

(** The [move] type represents a move that a pocamel can do, with limited
 * stock.*)
type move = {
  name : string;
  stock : int;
}

(** The [buff] type represents a record which holds all of the bonuses
 * to be add to the base stats of a pocamel.*)
type buff = {
  hp_buff : int;
  max_hp_buff : int;
  attack_buff : int;
  defense_buff : int;
  speed_buff : int;
  accuracy_buff : int;
  evasion_buff : int;
}

(** The type [t] represents an abstract pocamel which will be used in a battle.
 * A pocamel has a name, types, level, hp, max_hp, attack, defense, speed, 
 * accracy evasion, buffs, moves, item.*)
type t = {
  name : string;
  type_combo : string * (string option);
  level : int;
  hp : int;
  max_hp : int;
  attack : int;
  defense : int;
  speed : int;
  accuracy : int;
  evasion : int;
  buffs : buff;
  moves : move list;
  item : Item.t option;
}

(** The type [stat] represents a particular statistic*)
type stat = int

let empty_buff = {
  hp_buff = 0;
  max_hp_buff = 0;
  attack_buff = 0;
  defense_buff = 0;
  speed_buff = 0;
  accuracy_buff = 0;
  evasion_buff = 0;
}

(**
 * [empty] represents the null pocamel, which is used as a placeholder when 
 * that pocamel is taken in an interaction. 
*)
let empty = {
  name = "NONE";
  type_combo = ("normal",None);
  level = 0;
  hp = 0;
  max_hp = 0;
  attack = 0;
  defense = 0;
  speed = 0;
  accuracy = 0;
  evasion = 0;
  buffs = empty_buff;
  moves = [];
  item = None;
}

(**
 * [get_stat_list t] returns the current stats of the pocamel, in the following
   order: Level, HP, Max HP, Attack, Defense, Speed, Accuracy, Evasion
 * Effects: none
 * @param poke the pocamel whose stats are parsed
 * @return the parsed stat list 
*)
let get_stat_list poke = [poke.level;poke.hp;poke.max_hp;poke.attack;
                          poke.defense;poke.speed;poke.accuracy;poke.evasion]

(**
 * [get_type t] returns a 2-tuple of strings denoting the type of the pocamel
 * If the pocamel only has 1 type, then just the first. 
 * Effects: None
 * @param poke the pocamel to get the type
 * @return the pocamels type
*)
let get_type poke = poke.type_combo

(**
 * [get_buffs t] returns the current amount of buffs in the same order
 * as the stat list, with 0's meaning no buffs.
 * Effects: None
 * @param poke the pocamel to get the buffs from
 * @return the pocamels buffs
*)
let get_buffs poke = let buffs = 
                       poke.buffs in [buffs.hp_buff;
                                      buffs.max_hp_buff; 
                                      buffs.attack_buff; 
                                      buffs.defense_buff; 
                                      buffs.speed_buff;
                                      buffs.accuracy_buff; 
                                      buffs.evasion_buff]

(**
 *[from_json json] returns a pocamel object from a json file denoting
 * name and stats
 * Effects: None
 * @param json a valid Yojson representation
 * @return the parsed move
*)
let move_from_json json = {
  name = json |> member "name" |> to_string;
  stock = json |> member "stock" |> to_int;
}

(**
 * [from_json json] returns a pocamel object from a json file denoting
 * name and stats. All Buffs start at zero to start. 
 * Effects: none
 * @param json Yojson representation
 * @return the parsed in pocamel
*)
let from_json json = {
  name = json |> member "name" |> to_string;
  type_combo = json |> member "type" |> to_list |> 
               begin
                 function 
                 | [] -> ("Normal", None)
                 | h::[] -> (to_string h,None) 
                 | h::t::_ -> (to_string h,Some (to_string t)) end;
  level = json |> member "level" |> to_int;
  hp = json |> member "hp" |> to_int;
  max_hp = json |> member "max_hp" |> to_int;
  attack = json |> member "attack" |> to_int;
  defense = json |> member "defense" |> to_int;
  speed = json |> member "speed" |> to_int;
  accuracy = json |> member "accuracy" |> to_int;
  evasion = json |> member "evasion" |> to_int;
  buffs = {hp_buff = 0;
           max_hp_buff = 0;
           attack_buff = 0;
           defense_buff = 0;
           speed_buff = 0;
           accuracy_buff = 0;
           evasion_buff = 0};
  moves = json |> member "moves" |> to_list |> List.map move_from_json;
  item = if(json |> member "item" |> Item.item_from_json |> Item.noItem) then
      None
    else Some (json |> member "item" |> Item.item_from_json)
}

let typeList = [("fairy", ["poison"; "steel"; "fire"], 
                 ["fighting"; "dragon"; "dark"], []);
                ("dark", ["fighting"; "dark"; "fairy"], 
                 ["ghost"; "psychic"], []);
                ("dragon", ["steel"], ["dragon"], ["fairy"]);
                ("ice", ["steel";"fire"; "water"; "ice"], 
                 ["flying"; "ground"; "grass"; "dragon"], []);
                ("psychic", ["steel"; "psychic"], 
                 ["fighting"; "poison"; "dark"], []);
                ("electric", ["grass";"electric"; "dragon"], 
                 ["flying"; "ground"; "water"], ["ground"]);
                ("grass", 
                 ["flying";"poison"; "bug"; "steel"; "fire"; "grass"; "dragon"], 
                 ["ground"; "rock"; "water"], []);
                ("water", ["water";"grass"; "dragon"], 
                 ["ground"; "rock"; "fire"], []);
                ("fire", ["rock";"fire"; "water"; "dragon"], 
                 ["bug"; "ice"; "grass"; "steel"], []);
                ("steel", ["steel";"fire"; "water"; "electric"], 
                 ["rock"; "ice"; "fairy"], []);
                ("ghost", ["dark"], ["ghost"; "psychic"], ["normal"]);
                ("bug",["fighting";"flying"; "poison"; "ghost"; "steel"; "fire"; 
                        "fairy"], ["psychic"; "grass"; "dark"], []);
                ("rock", ["fighting";"ground"; "steel"], ["flying"; "bug"; 
                                                          "fire"; "ice"], []);
                ("ground", ["bug";"grass"], ["poison"; "rock"; "steel"; "fire"; 
                                             "electric"], ["flying"]);
                ("poison", ["poison";"ground"; "rock"; "ghost"], 
                 ["grass"; "fairy"], ["steel"]);
                ("flying", ["steel";"fire"; "water"; "ice"], 
                 ["flying"; "ground"; "grass"; "dragon"], []);
                ("fighting", ["flying";"poison"; "bug"; "psychic"; "fairy"], 
                 ["normal"; "rock"; "steel"; "ice"; "dark"], ["ghost"]);
                ("normal", ["rock"], [], ["ghost"])
               ]

(**
 * [calculate_type_effectiveness type1 typ2] calculates the type bonus 
 * of effectiveness based on the standard Pocamel type comparator.
 * Effects: None
 * @param type1 the type of a move
 * @param type2 the type of a target pocamel
 * @return the multiplicative effectivenesss modifier
*)
let calculate_type_effectiveness type1 type2 = 
  let elementList = 
    List.find 
      (fun (x, _, _, _) ->  x = type1) 
      typeList in
  match(elementList) with
  |(_, not, super, null) -> 
    if (List.mem type2 not) then 
      0.5
    else if(List.mem type2 super) 
    then 1.5
    else if(List.mem type2 null)then 0.0
    else 1.0

(**
 * [get_move pk move moveList] gets the move from a pocamel based on a pocamel's
 * move set and queries the move lexicon.
 * Effects: None
 * @param pk the pocamel to get the move from
 * @param move the move to get
 * @param moveList the movelist from where to get the move
 * @return the move type
*)
let get_move (pk : t) move moveList = try(
  List.find (fun (x : Move.t) -> String.lowercase_ascii move = 
                                 String.lowercase_ascii(Move.move_name x))
    moveList
) with Not_found -> raise (IllegalMove)

(**
 * [calculate_mod pk1 pk2 move] calculates the modifier bonus based on many 
 * factors like type bonus, critical bonus, random modifier, and typeMove bonus 
 * based on Pocamel standard calculation.
 * Effects: None
 * @param pk1 the attacking pocamel
 * @param pk2 the attacked pocamel
 * @move the move that pk1 is using
 * @return the multiplicative modifier to the damage
*)
let calculate_mod pk1 pk2 (move:Move.t) isReal = 
  try
    if (List.exists (fun (x:move) -> 
        String.lowercase_ascii x.name = 
        String.lowercase_ascii(Move.move_name move)) pk1.moves) 
    then () 
    else raise Not_found;
    let randNumOne = Random.float 0.15 +. 0.85 in
    let randNumTwo = Random.int 512 in
    let critical = 
      if(randNumTwo > pk1.speed) then 1.0 
      else (if isReal then (
          ANSITerminal.(print_string [red] 
                          "Critical hit!\n\n")) else (); 1.5) in
    let type_bonus = match pk1.type_combo with
      | (h,None) -> if(h = Move.move_type move) then 1.5 else 1.0
      | (h,Some t) -> if((h = Move.move_type move) || 
                         (t = Move.move_type move)) then 1.5
        else 1.0 in
    let type_effectiveness = match pk2.type_combo with
      | (h,None) -> calculate_type_effectiveness (Move.move_type move) h
      | (h,Some t) -> calculate_type_effectiveness (Move.move_type move) h *.
                      calculate_type_effectiveness (Move.move_type move) t in
    if(isReal) then (if (type_effectiveness > 1.0) then 
                       (ANSITerminal.(print_string [red] 
                                        "It's super effective!\n"))
                     else if (type_effectiveness = 0.)
                     then (ANSITerminal.(print_string [white] 
                                           "It did nothing...\n"))
                     else if (type_effectiveness < 1.) then
                       (ANSITerminal.(print_string [blue] 
                                        "It was not very effective...\n")))
    else ();
    randNumOne *. critical *. type_bonus *. type_effectiveness
  with
    Not_found -> raise IllegalMove
(**
 * [decrement_stock_helper pk1 moves move agr] helps decrement stock through
 * and aggegator
 * Effects: None
 * @param pk1 the pocamel whose moves to dock
 * @param moves the list of remaining moves of the pocamel
 * @param move the name of the move
 * @param agr the new move list
 * @return a new move list
*)
let rec decrement_stock_helper (pk1:t) (moves:move list) (move:string) 
    (agr:move list) =
  match moves with
  | [] -> agr
  | h::t -> if (String.lowercase_ascii h.name = String.lowercase_ascii move) 
    then 
      decrement_stock_helper pk1 t move 
        ({name = h.name; stock = h.stock - 1}::agr)
    else decrement_stock_helper pk1 t move (h::agr)

(**
 * [decrement_stock_helper pk1 move] decrements the stock of a certain move
 * Effects: None
 * @param pk1 the pocamel whose moves to dock
 * @param move the name of hte move to dock
 * @return a new pocamel
*)
let decrement_stock pk1 move : t =
  let altered_moves = decrement_stock_helper pk1 pk1.moves move [] in
  {
    name = pk1.name;
    type_combo = pk1.type_combo;
    level = pk1.level;
    hp = pk1.hp;
    max_hp = pk1.max_hp;
    attack = pk1.attack;
    defense = pk1.defense;
    speed = pk1.speed;
    accuracy = pk1.accuracy;
    evasion = pk1.evasion;
    buffs = {hp_buff = 0;
             max_hp_buff = 0;
             attack_buff = 0;
             defense_buff = 0;
             speed_buff = 0;
             accuracy_buff = 0;
             evasion_buff = 0};
    moves = altered_moves;
    item = pk1.item;
  }

(**
 * [attack pk1 pk2 moveList move] returns a pocamel after another pocamel has
 * made a move on it
 * Effects: None
 * @param moveList list of moves
 * @param move the name of the move to be executed
 * @param pk1 the move maker pocamel
 * @param pk2 the recipient of the move
 * @return the modified pocamel
*)
let attack (pk1: t) (pk2 : t) (moveList : Move.t list) (move : string) isReal=  
  try 
    let mv = get_move pk1 move moveList in
    if(isReal) then
      ANSITerminal.(print_string [yellow] 
                      (pk1.name ^ " used " ^ move ^ "!\n"))
    else 
      ();
    let damage = if(Random.float 100. > 
                    ((float_of_int pk1.accuracy) *. 
                     (Move.move_acc mv |> float_of_int) /. 
                     (float_of_int pk2.evasion))) then
        if(isReal) then 
          (ANSITerminal.
             (print_string [red] (pk1.name ^ "'s move missed!\n")); 0.0) 
        else 0.0
      else
        ((float_of_int (2 * pk1.level)/.5.0 +. 2.0) 
         *. (float_of_int (Move.power mv) *. 
             float_of_int (pk1.attack / pk2.defense))/.50.0 +. 2.0) 
        *. (calculate_mod pk1 pk2 mv isReal) in
    {
      name = pk2.name;
      type_combo = pk2.type_combo;
      level = pk2.level;
      hp = if pk2.hp - int_of_float damage < 0 then 0 
        else pk2.hp - int_of_float damage;
      max_hp = pk2.max_hp;
      attack = pk2.attack;
      defense = pk2.defense;
      speed = pk2.speed;
      accuracy = pk2.accuracy;
      evasion = pk2.evasion;
      buffs = {hp_buff = 0;
               max_hp_buff = 0;
               attack_buff = 0;
               defense_buff = 0;
               speed_buff = 0;
               accuracy_buff = 0;
               evasion_buff = 0};
      moves = pk2.moves;
      item = pk2.item;
    }
  with
    IllegalMove -> raise IllegalMove

(**
 * [poke_rep pk] returns a string representing pk's name and hp
 * Effects: None
 * @param pk the pocamel to print
 * @return the string representing the pk's name and hp
*)
let poke_rep (poke : t)= poke.name ^ " " ^ string_of_int poke.hp

(**
 * [formatPocamel pk] returns a string of the pk's name
 * Effects: None
 * @param pk the pocamel to get the move from
 * @return the string representing the pk;s name
*)
let name pk = pk.name

(**
 * [hp pk] returns the hp
 * Effects: None
 * @param pk the pocamel to get the hp from
 * @return an int for the hp
*)
let hp pk = pk.hp

(**
 * [formatPocamel pk] returns a string representing pk
 * Effects: None
 * @param pk the pocamel to print
 * @return the string representing the pk
*)
let use_item pk item = {
  name =  pk.name;
  type_combo = pk.type_combo;
  level = pk.level;
  hp = pk.hp;
  max_hp = pk.max_hp;
  attack = pk.attack;
  defense = pk.defense;
  speed = pk.speed;
  accuracy = pk.accuracy;
  evasion = pk.evasion;
  buffs = pk.buffs;
  moves = pk.moves;
  item = Some item;
}

(**
 * [use_elixir pk item] returns a pk which has the elixir used on it
 * Effects: None
 * @param pk the pocamel to use the elixir on
 * @param item the item to use
 * @return a new pk which has the elixir used on it
*)
let use_elixir pk item = 

  let to_heal = match Item.elixir item with
    | Some n -> n
    | None -> 0 in 

  {
    name =  pk.name;
    type_combo = pk.type_combo;
    level = pk.level;
    hp = if pk.hp + to_heal > pk.max_hp then pk.max_hp else pk.hp + to_heal;
    max_hp = pk.max_hp;
    attack = pk.attack;
    defense = pk.defense;
    speed = pk.speed;
    accuracy = pk.accuracy;
    evasion = pk.evasion;
    buffs = pk.buffs;
    moves = pk.moves;
    item = pk.item;
  }

(**
 * [move_list pk] returns the moves of pk
 * Effects: None
 * @param pk the pocamel whose moves to print
 * @return a list of moves of the pocamel
*)
let move_list pk = pk.moves


(**
 * [formatPocamel pk] returns a string representing move
 * Effects: None
 * @param moveList the list of Moves.t to use
 * @param move the move to format
 * @param pk the pocamel to whom the move belongs
 * @return the string representing the move
*)
let formatMove pk moveList (move: move) = 
  let moveDescr = get_move pk move.name moveList in
  String.concat "\n" 
    [Move.move_name moveDescr; 
     String.concat "" [string_of_int move.stock; "/";
                       string_of_int (Move.move_stock moveDescr); "pp"]; 
     Move.move_type moveDescr]

(**
 * [formatPocamel pk] returns a string representing pk
 * Effects: None
 * @param pk the pocamel to print
 * @return the string representing the pk
*)
let formatPocamel pk = 
  let typing = match (pk.type_combo) with
    |(x, Some y) -> x^", "^y
    |(x, None) -> x
  in
  String.concat "\n" [pk.name; "lv "^ (string_of_int pk.level); 
                      (string_of_int pk.hp)^"/"^(string_of_int pk.max_hp);
                      typing]

(**
 * [get_stock pk mv] returns the stock of the move of pk 
 * whose name matches the mv
 * Effects: None
 * @param mv the name of the move to get the stock of
 * @param pk the pocamel to whom the move belongs
 * @return the stock of the move
*)
let get_stock (pk:t) (move:string) =
  let my_move = List.find (fun (x:move) -> 
      String.lowercase_ascii x.name = String.lowercase_ascii move) pk.moves in
  my_move.stock

(**
 * [stock mv] returns the stock of mv
 * Effects: None
 * @param mv the move to get the stock of
 * @return the stock of the move
*)
let stock mv = mv.stock

(**
 * [move_name mv] returns the name of mv
 * Effects: None
 * @param mv the move to get the name of
*)
let move_name (mv:move) = mv.name

(**
 * [get_speed pk] returns the speed of pk
 * Effects: None
 * @param pk the pocamel to get the speed of
 * @return the speed of the pocamel
*)
let get_speed pk = pk.speed + pk.buffs.speed_buff

(**
 * [level pk] returns the level of pk
 * Effects: None
 * @param pk the pocamel to get the level of
 * @return the level of the pocamel
*)
let level pk = pk.level

(**
 * [get_type pk] returns the type of pk
 * Effects: None
 * @param pk the pocamel to get the type of
 * @return the type of the pocamel
*)
let get_type pk = match pk.type_combo with
  | (h, None) -> h
  | (h, Some t) -> h ^ ", " ^ t

(**
 * [get_type pk] returns the max hp of pk
 * Effects: None
 * @param pk the pocamel to get the max hp of
 * @return the max hp of the pocamel
*)
let max_hp pk = pk.max_hp+pk.buffs.max_hp_buff

(**
 * [shakeBall x] shake a ball x number of times by printing to the console
 * Effects: prints to the console and delays execution by 1 second with sleep
 * @param x the number of times to shake
 * @return unit
*)
let rec shakeBall (x : int) : unit =  if(x <= 0) then () else 
    (ANSITerminal.(print_string [yellow] "*the ball shakes*..");
     print_endline "";
     Unix.sleep 1;
     shakeBall (x-1))
(**
 * [shake x] ball f] determine the amount of times that a pocamball should shake
 * based of a defined constant f and the type of ball. Can shake from 1 to 3
 * times.
 * Effects: none
 * @param ball the kind of ball
 * @param f the predefined f constant
 * @return the number of times to shake the ball
*)
let shake ball f = 
  let ball_count = if ball = "poca" 
    then 255.0 else if ball = "great" then 200.0 else 0.0 in
  let d = 5000.0 /. ball_count in
  let x = (d *. f)/.255.0 in
  if(x < 10.0) then ()
  else if(x < 30.0) then shakeBall 1
  else if(x < 30.0) then shakeBall 2
  else shakeBall 3

(**
 * [catch item pk] returns a tuple of a unit and boolean based off of whether or
 * not the target pocamel pk was caught based off of all the criteria that are
 * standard to the pokemon game including ball type, pocamel health, and of
 * course luck!
 * Effects: Prints the result of the attempted catch
 * @param item the ball that is being used
 * @param pk the target pocamel being caught
 * @return a tuple of the result print and a boolean of whether or not it was
 * caught
*)
let catch item pk =
  ANSITerminal.erase Screen;
  if(Item.name item = "poca-ball") then
    let rand = Random.float(255.0) in
    let f = float_of_int (pk.max_hp * 255 * 4)/. float_of_int(pk.hp * 12) in
    if(f >= rand) 
    then (
      shakeBall 3;
      ANSITerminal.
        (print_string [green] ("The " ^ pk.name ^ " was caught!\n\n"));
      true)
    else
      (shake "poca" f;
       ANSITerminal.(print_string [red] ("The " ^ pk.name ^ " escaped!\n\n"));
       false)
  else if(Item.name item = "great-ball") then
    let rand = Random.float(200.0) in
    let f = float_of_int (pk.max_hp * 255 * 4)/. float_of_int(pk.hp * 12) in
    if(f >= rand) 
    then (
      shakeBall 3;
      ANSITerminal.
        (print_string [green] ("The " ^ pk.name ^ " was caught!\n\n"));
      true)
    else
      (shake "poca" f;
       ANSITerminal.(print_string [red] ("The " ^ pk.name ^ " escaped!\n\n"));
       false)
  else (
    shakeBall 3;
    ANSITerminal.(print_string [green] ("The " ^ pk.name ^ " was caught!\n\n"));
    true)

(**
 * [vary pk] takes in a pocamel and returns the same exact pocamel except the 
 * level which is recalculated to be some amount higher.
 * Effects: none
 * @param pk the pocamel who's level is changed
 * @return the pocamel with changed level
*)
let vary pk = {
  pk with level = pk.level + Random.int 10 - 5
}
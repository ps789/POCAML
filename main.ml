(**
 * [print string color] print the string in ANSIT terminal in the specified 
 * color 
 * Effects: prints the string
 * @param string the string to be printed
 * @param color the color for the string to be printed in
 * @return unit 
*)
let print string color = 
  ANSITerminal.(print_string [color] string)

(** [cleanSpaceRevers acc list] cleans out the extra spaces
 * @param list of words to remove extra spaces
 * @return list of words in reverse order
*)
let rec cleanSpaceReverse acc = function
  |[] -> acc
  | h::t -> 
    if(h= "")then cleanSpaceReverse acc t else (cleanSpaceReverse (h::acc) t)

(** [cleanSpace words] takes in a series of words and then returns the words
 * with extra spaces removed.
 * @param words with which to remove spaces
 * @return string of words with extra spaces removed
*)
let cleanSpace words =
  String.concat " " (List.rev 
                       (cleanSpaceReverse [] (String.split_on_char ' ' words)))

(** [printList param theList] takes in a function and and a list to print 
 * the results of that function on the elements of the list
 * @param param the function with which to print
 * @param theList = the list to enact
*)
let rec printList param theList =
  match(theList) with
  |[] -> print "\r\n" ANSITerminal.yellow
  |h::t ->  print
              (String.concat "" [(param h); if(t = []) then "" else "\n \n"]) 
              ANSITerminal.yellow; 
    printList param t

(** [finishCheck battle] checks if the battle is finished
 * Side Effects: exit if finished
 * @param battle the battle to check
 * @return nothing
*)
let finishCheck battle : bool= 
  if(Battle.isFinished battle false) then 
    (print "You have won! [Press enter to continue]" ANSITerminal.green;
     let _ = read_line () in true)
  else if (Battle.isFinished battle true) then
    (print "You have lost... \n" ANSITerminal.blue;
     let _ = read_line () in true)
  else false


(** [decideBot game] enacts a move based on the current game state
 * Side Effects: None
 * @param game the game to analyze
 * @return modified game
*)
let decideBot game = 
  let battle = Game.get_battle game in 
  if(Battle.isFinished battle false || Battle.isFinished battle true) 
  then game
  else
    match(Bot.determine_move battle game) with
    |("switch", pocamelName) -> 
      if(Battle.currPocamelFaint battle false)then () else (); 
      (match(Battle.switch false battle pocamelName) with
       | Battle.Legal x -> Game.set_battle game x
       | Battle.Illegal -> game)
    |("attack", moveName) -> 
      (match(Battle.attack false battle (Game.moves game) moveName) with
       | Battle.Legal x -> Game.set_battle game x
       | Battle.Illegal ->  game)
    |_ -> game

(** [deadSwitch game] prompts the player to switch pocamels if the pocamel
 * is dead
 * Side Effects: None
 * @param game the current state of the game
 * @return the modified game state
*)
let rec deadSwitch game = let battle = Game.get_battle game in
  ANSITerminal.(print_string [cyan] 
                  "Choose a pocamel to switch to: \n\n");
  printList (Pocamel.formatPocamel) (Battle.player battle |> Trainer.pocamels);

  ANSITerminal.(print_string [white] 
                  "\nSwitch to: ");
  let pocamelName = read_line () in
  match(Battle.switch true battle pocamelName) with
  |Battle.Legal x -> ANSITerminal.erase Screen; Game.set_battle game x
  |Battle.Illegal -> 
    (ANSITerminal.erase Screen;
     ANSITerminal.(print_string [red] 
                     "\n\n You can't switch to that pocamel!\n\n");
     deadSwitch game)

(** [deadSwitchOpponent game] switches opponent's pocamel if dead
 * Side Effects: None
 * @param game current game state
 * @return altered game state
*)
let deadSwitchOpponent game = 
  let battle = Game.get_battle game in
  match(Bot.determine_move battle game) with
  |("switch", pocamelName) -> 
    (match(Battle.switch false battle pocamelName) with
     | Legal x -> Game.set_battle game x
     | Illegal -> finishCheck battle; game)
  |_ -> game

(** [get_pokemon_desc tr] returns the small graphical representation of 
 * the amount of dead and alive pocamels in a user's party.
 * Side Effects: prints the red and white pocamels according to their health
 * @param tr the trainer to be represented
 * @return unit
*)
let get_pokemon_desc tr : unit =
  let pocamels = Trainer.pocamels tr in
  let pocamel_array = Array.of_list pocamels in
  let pocamel_length = List.length pocamels in
  let red_balls = ref 0 in
  let gray_balls = ref 0 in
  for i = 0 to pocamel_length - 1 do
    if(Pocamel.hp (Array.get pocamel_array i) > 0) then
      red_balls := !red_balls + 1
    else
      gray_balls := !gray_balls + 1
  done;
  for i = 0 to !red_balls - 1 do
    ANSITerminal.
      (print_string [red] 
         "o");
  done;
  for i = 0 to !gray_balls - 1 do
    ANSITerminal.
      (print_string [white] 
         "o");
  done;
  ANSITerminal.
    (print_string [red] 
       "\n")

(** [print_hp hp max] returns the color an opponents health should be
 * Side Effects: none
 * @param hp the current hp of a pocamel
 * @param max the max hp of a pocamel
 * @return the necessary color
*)
let print_hp hp max  = 
  (print (string_of_int (hp)^"/"^string_of_int (max) ^ "\n")
     (if (float_of_int hp) /. (float_of_int max)  > 0.5 then
        ANSITerminal.green
      else if 
        ((float_of_int hp) /. (float_of_int max)  > 0.25) then
        ANSITerminal.yellow
      else
        ANSITerminal.red))

(** [handle_escape] helper method for Unix interactions
 * Side Effects: none
 * @return the respective string based on input
*)
let handle_escape () =
  match input_char stdin with
  (* ESC *)
  | '[' -> (match input_char stdin with
      | 'A' -> "North"
      | 'B' -> "South"
      | 'C' -> "East"
      | 'D' -> "West"
      | _ -> "Help"
    )
  | _ -> "Help"


(* Handle an input key. *)
(** [handle_key] helper method for Unix interactions which handles the input key
 * This provides all of our functionality for interacting in map mode.
 * Side Effects: none
 * @return the respective decision based on key input
*)
let rec handle_key () =
  match input_char stdin with
  (* ESC *)
  | '\027' -> handle_escape ()
  (* All other characters *)
  |' ' -> "Interact"
  |'e' -> "Menu"
  |'q' -> "Quit"
  | _ -> "Help"

(* Handle an input key. *)
(** [resetCanon termio] turns the keyboard funcationality from providing player
 * direction to allowing for keyboard input. 
 * Side Effects: none
 * @param the terminal object
 * @return the respective decision based on key input
*)
let resetCanon termio = 
  Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH termio

(** [moveDir dir] turns the keyboard input into a direction for the player to 
 * move in
 * Side Effects: none
 * @param dir the direction to go to
 * @return the respective Trainer direction
*)
let moveDir = function
  |"North" -> Trainer.North
  |"East" -> Trainer.East
  |"South" -> Trainer.South
  |"West" -> Trainer.West
  | string -> raise Not_found

(** [bag game] enacts bag command
*)

(** [bag game] allows the player to look inside of their bag in the game. The
 * player will either be able to use a potion, pocamball, item.
 * Side effects : prints information about the player's items
 * @returns the new game play 
*)
let rec bag game = let battle = Game.get_battle game in
  ANSITerminal.(print_string [yellow] "Your items: \n\n");
  printList Item.formatItem (battle |> Battle.player |> Trainer.get_items);

  ANSITerminal.
    (print_string [white] "\nUse item [type back to return to menu]: ");

  let itemName = read_line () in
  let pokeSplit = String.split_on_char '-' itemName in
  let get_ball = match pokeSplit with
    | [] -> ""
    | h::[] -> h
    | h::t -> match t with
      | [] -> ""
      | s::[] -> s
      | h::s -> h in


  if(String.lowercase_ascii itemName = "back") then 
    (ANSITerminal.erase Screen;
     playMain game)
  else if(String.lowercase_ascii get_ball = "ball") then
    match (itemName |> (game |> Game.get_battle |> Battle.use_pocaball)) with 
    |Battle.Legal x -> 
      let resultGame = decideBot (Game.set_battle game x) in
      if(Battle.currPocamelFaint (Game.get_battle resultGame) true) then 
        playMain (deadSwitch resultGame)
      else (playMain resultGame)
    |Battle.Illegal -> 
      (ANSITerminal.erase Screen;
       ANSITerminal.
         (print_string [red] "You can't catch a pocamel right now!.\n");
       playMain game)

  else
    ANSITerminal.(print_string [white] "\n\nYour pocamel: \n\n");

  printList (Pocamel.formatPocamel) (Battle.player battle |> Trainer.pocamels);

  ANSITerminal.
    (print_string [white] 
       "\nUse item on which pocamel [type back to return to menu]: ");

  let pocamelName = read_line() in

  if(String.lowercase_ascii pocamelName = "back") then 
    (ANSITerminal.erase Screen;
     playMain game)
  else
    match (itemName |> (game |> Game.get_battle |> Battle.use true) 
             pocamelName) with 
    |Battle.Legal x -> 
      ANSITerminal.erase Screen;
      ANSITerminal.(print_string [green] 
                      ("\nYou used " ^ itemName 
                       ^ " on " ^ pocamelName ^ "!\n"));
      let resultGame = decideBot (Game.set_battle game x) in
      if(Battle.currPocamelFaint (Game.get_battle resultGame) true) then 
        (
          playMain (deadSwitch resultGame)) else (playMain resultGame)
    |Battle.Illegal -> 
      (ANSITerminal.erase Screen;
       ANSITerminal.(print_string [yellow] "You can't do that Rip.\n");
       playMain game)

(** [bagMap game] this is identical in function to bag except it is used when 
 * the player is in map mode.
 * Effects: prints information about the player
 * @param game the current game state
 * @return the new game play
*)
and bagMap game = 
  ANSITerminal.(print_string [yellow] "Your items: \n\n");
  printList Item.formatItem (Game.get_player game |> Trainer.get_items);

  ANSITerminal.(print_string [white] 
                  "\nUse item [type back to return to menu]: ");

  let itemName = read_line () in
  let pokeSplit = String.split_on_char '-' itemName in
  let get_ball = match pokeSplit with
    | [] -> ""
    | h::[] -> h
    | h::t -> match t with
      | [] -> ""
      | s::[] -> s
      | h::s -> h in

  if(String.lowercase_ascii itemName = "back") then 
    (ANSITerminal.erase Screen;
     playMain game)
  else if(String.lowercase_ascii get_ball = "ball") then
    (ANSITerminal.(print_string [red] "You can't do that!\n");
     let _ = read_line () in ();
     ANSITerminal.erase Screen;
     playMain game)

  else
    ANSITerminal.(print_string [white] "\n\nYour pocamel: \n\n");

  printList (Pocamel.formatPocamel) (Game.get_player game |> Trainer.pocamels);

  ANSITerminal.
    (print_string [white] 
       "\nUse item on which pocamel [type back to return to menu]: ");

  let pocamelName = read_line() in

  if(String.lowercase_ascii pocamelName = "back") then 
    (ANSITerminal.erase Screen;
     playMain game)
  else
    match (itemName |> (game |> Game.get_battle |> Battle.use true) 
             pocamelName) with 
    |Battle.Legal x -> 
      ANSITerminal.erase Screen;
      ANSITerminal.(print_string [green] 
                      ("\nYou used " ^ itemName ^ " on " ^ 
                       pocamelName ^ "!\n"));
      playMain (Game.update_player (Game.set_battle game x))
    |Battle.Illegal -> 
      (ANSITerminal.erase Screen;
       ANSITerminal.(print_string [yellow] "You can't do that Rip.\n");
       playMain game)

(** [run game] runs away
 * Side effects : exits game
 * @returns unit
*)
and run game = 
  ANSITerminal.erase Screen;
  if Battle.is_wild (Game.get_battle game) then
    (ANSITerminal.(print_string [green] 
                     "You fled away! The horror The horror!\n\n");
     playMain (Game.update_player (Game.toggle_battle game)))
  else
    ANSITerminal.(print_string [red] "You can't escape!!!!\n");
  playMain game;


  (** [help game] provide the help text for a game.
     * Side effects : prints the help text
     * @returns unit
  *)
and help game = 
  if (Game.is_in_battle game) 
  then ANSITerminal.(print_string [yellow] 
                       ("NO HELP FOR YOU!!!! HAHA. \n"^
                        "But anyway, bag to use items, run to flee,\n"^
                        "fight to use moves, and switch to switch pocamels.\n"))
  else (ANSITerminal.(print_string [yellow] 
                        ("To move, use the arrow keys.\n"^
                         "To talk to trainers, use the space bar.\n"^
                         "To exit the game, press q.\n"^
                         "To access the menu, press e.\n"^
                         "Press enter to continue: ")); 
        let _ = read_line () in ());
  playMain game
(** [fight game] enacts fight command allows for battle interaction between
 * player and pocamels or other trainers
 * Effects: Prints information about the battle
 * @param game the current game state
 * @return the new game play
*)
and fight game = let battle = Game.get_battle game in
  ANSITerminal.(print_string [yellow] 
                  "Your moves: \n\n");

  printList (Pocamel.formatMove 
               (battle |> Battle.player_pocamel) 
               (Game.moves game)) 
    (Pocamel.move_list (battle |> Battle.player_pocamel));

  ANSITerminal.(print_string [white] 
                  ("\nYour attack [type back to return to menu]: "));

  let move = read_line () in
  if(String.lowercase_ascii move = "back") then 
    (ANSITerminal.erase Screen;
     playMain game)
  else
    ANSITerminal.erase Screen; 
  ANSITerminal.(print_string [default] ("\n"));
  if(Battle.playerFirst battle) then 
    (match (Battle.attack true battle (Game.moves game) move) with
     |Legal x -> 
       playMain (decideBot (Game.set_battle game x))
     |Illegal -> ANSITerminal.erase Screen;
       ANSITerminal.(print_string [yellow] 
                       "That move is not available!\n\n");
       playMain game) 
  else let newGame = decideBot game in
    if (finishCheck (Game.get_battle newGame))
    then playMain (Game.update_player (Game.toggle_battle game))
    else
    if(Battle.currPocamelFaint (Game.get_battle newGame) true) then 
      playMain (deadSwitch newGame)
    else match (Battle.attack true (Game.get_battle newGame) 
                  (Game.moves game) move) with
    |Legal x ->  
      playMain (deadSwitchOpponent (Game.set_battle newGame x))
    |Illegal -> ANSITerminal.(print_string [yellow] 
                                "That move is not available!\n\n");
      playMain game
(** [switch game] enacts switch command between a player and their pocamel
 * Effects: Prints information about the trainer's team and the resultant
 * switches
 * @param game the current game state
 * @return the new game play
*)
and switch game = let battle = Game.get_battle game in
  ANSITerminal.(print_string [cyan] 
                  "Your Pocamel Party: \n\n");

  printList (Pocamel.formatPocamel) (Battle.player battle |> Trainer.pocamels);

  ANSITerminal.
    (print_string [white] "\nSwitch with [type back to return to menu]: ");
  let pocamelName = read_line () in
  if(String.lowercase_ascii pocamelName = "back") then 
    (ANSITerminal.erase Screen;
     playMain game)
  else
    (ANSITerminal.erase Screen;
     match(Battle.switch true battle pocamelName) with
     |Legal x -> 
       let resultGame = decideBot (Game.set_battle game x) in
       if(Battle.currPocamelFaint (Game.get_battle resultGame) true) then 
         playMain (deadSwitch resultGame) else playMain resultGame
     |Illegal ->
       ANSITerminal.(print_string [yellow] 
                       "You can't switch to that pocamel!\n\n"));
  playMain game
(** [switch Map game] enacts switch map command which allows you to switch
 * your starting pocamel outside of combat
 * Effects: Prints information about the trainer's team
 * @param game the current game state
 * @return the new game play 
*)
and switchMap game = 

  ANSITerminal.(print_string [green] 
                  ("\nYour Starting Pocamel: " ^ Game.get_starting_pocamel game 
                   ^ " | "));
  ANSITerminal.(print_string [cyan] 
                  "Your Pocamel Party: \n\n");

  printList (Pocamel.formatPocamel) (Game.get_player game |> Trainer.pocamels);

  ANSITerminal.(print_string [white] 
                  "\nSet Starting pocamel [type back to return to menu]: ");
  let pocamelName = read_line () in
  if(String.lowercase_ascii pocamelName = "back") then 
    (ANSITerminal.erase Screen;
     playMain game)
  else
    (ANSITerminal.erase Screen;
     match(Battle.switch true (Game.get_battle game) pocamelName) with
     |Legal x -> 
       playMain (Game.set_starting_pocamel game pocamelName)
     |Illegal ->
       ANSITerminal.(print_string [yellow] 
                       "You can't start with that pocamel!\n\n"));
  playMain game

(** [playGame game] plays the current game when in battle.
 * Effects: Prints all information about the battle, can all cause the program 
 * to be delayed with sleeps.
 * @param game the current game state
 * @return the new game play
*)
and playGame game = 
  let battle = Game.get_battle game in
  let opponent = Battle.opponent battle in
  let player = Battle.player battle in
  let player_pocamel = Battle.player_pocamel battle in
  let opponent_pocamel = Battle.opponent_pocamel battle in
  if (finishCheck battle) 
  then playMain (Game.update_player (Game.toggle_battle game))
  else if(Battle.currPocamelFaint battle true)then
    playMain (deadSwitch game)
  else
  if(Trainer.name opponent <> "wild") then
    (ANSITerminal.(print_string [yellow] 
                     ("\n\nTrainer " ^ (Trainer.name opponent) ^ " | "));
     (get_pokemon_desc opponent))
  else
    ();
  ANSITerminal.(print_string [red] 
                  ("\n"^(Pocamel.name opponent_pocamel)));

  ANSITerminal.(print_string [cyan] " HP: ");

  let (opponent_hp, opponent_max, player_hp, player_max) = 
    (Pocamel.hp (opponent_pocamel), 
     Pocamel.max_hp (opponent_pocamel),
     Pocamel.hp (player_pocamel),
     Pocamel.max_hp (player_pocamel)) in
  print_hp opponent_hp opponent_max;
  ANSITerminal.(print_string [green] ("  Level: "));
  ANSITerminal.
    (print_string [white] 
       (string_of_int (Pocamel.level (opponent_pocamel))));
  ANSITerminal.(print_string [green] ("  Type: "));
  ANSITerminal.
    (print_string [white] 
       (Pocamel.get_type (opponent_pocamel) ^ "\n"));

  ANSITerminal.(print_string [yellow] 
                  ("\n\nTrainer " ^ (Trainer.name player) ^ " | "));
  (get_pokemon_desc player);
  ANSITerminal.(print_string [blue] 
                  (Pocamel.name (player_pocamel)));
  ANSITerminal.(print_string [cyan] " HP: ");
  print_hp player_hp player_max;

  ANSITerminal.(print_string [green] ("  Level: "));
  ANSITerminal.
    (print_string [white] 
       (string_of_int (Pocamel.level (player_pocamel))));
  ANSITerminal.(print_string [green] ("  Type: "));
  ANSITerminal.
    (print_string [white] 
       (Pocamel.get_type (player_pocamel) ^ "\n"));

  ANSITerminal.(print_string [red] ("\n [Fight] ")); 
  ANSITerminal.(print_string [magenta] (" [Bag] ")); 
  ANSITerminal.(print_string [cyan] (" [Switch] ")); 
  ANSITerminal.(print_string [yellow] (" [Run]\n\n")); 

  ANSITerminal.(print_string [white] ("Your move : "));

  match read_line () with
  | exception End_of_file -> ()
  | command -> try(
    ANSITerminal.(print_string [default] ("\n"));
    match (Command.parse command) with
    |Command.Bag -> bag game
    |Command.Run -> run game
    |Command.Fight -> fight game
    |Command.Help -> help game
    |Command.Switch -> switch game
  ) with
  |Command.Malformed ->
    ANSITerminal.erase Screen; 
    ANSITerminal.(print_string [red] 
                    "That's not a valid thing to do!\n");
    playMain game
  |Command.Empty ->
    ANSITerminal.erase Screen; 
    ANSITerminal.(print_string [red] "I CAN'T HEAR YOUUUUUU!!!!!\n");
    playMain game

(** [playMap game] plays the current map when in when out of battle.
 * Effects: Prints all information about the trainer
 * @param game the current game state
 * @return the new game play
*)
and playMap game = let termio = Unix.tcgetattr Unix.stdin in
  ANSITerminal.erase Screen; 
  if(Game.starting_pocamel_dead game) 
  then (ANSITerminal.(print_string [red] 
                        "Your starting pocamel has fainted! Switch!\n");
        switchMap game)
  else (
    (game |> Game.update_map |> Game.get_map |> Pocamap.print)
      (Game.get_player game);

    let newtermio = 
      {termio with c_icanon = false; c_vmin = 1; c_vtime = 0; c_echo = false} in
    Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH newtermio;

    match handle_key () with
    |"Interact" -> resetCanon termio; (match snd (Game.get_front game) with 
        | None -> playMain game
        | Some t -> playMain (Game.start_battle t (Game.toggle_battle game) ""))
    |"Menu" -> 
      (ANSITerminal.erase Above;
       ANSITerminal.
         (print_string [green] 
            ("Would you like to access you bag" ^ 
             "switch your starting pocamel? : \n\n"));
       ANSITerminal.(print_string [blue] "  [bag] ");
       ANSITerminal.(print_string [cyan] "  [switch]\n\n");
       ANSITerminal.(print_string [white] "Your move: ");
       resetCanon termio; match (String.lowercase_ascii (read_line ())) with
       |"bag" -> bagMap game
       |"switch" -> switchMap game
       |_ -> ANSITerminal.(print_string [red] "That's not allowed!\n\n"));
      playMain game
    |"Quit" -> (resetCanon termio;
                ANSITerminal.
                  (print_string [green] 
                     "Make sure to give us a 3/3 on the scope store!\n\n");
                exit 0)
    |"Help" -> resetCanon termio; help game
    |c -> resetCanon termio; moveMap game c)

(** [playGame game] plays the current game when in battle.
 * Effects: Prints all information about the battle, can all cause the program 
 * to be delayed with sleeps.
 * @param game the current game state
 * @return the new game play
*)
and moveMap game dir = let moveGame = (Game.move game (moveDir dir)) in 
  if(Game.calc_encounter moveGame) then
    playMain (Game.encounter moveGame) else
    playMain moveGame

(** [playMain game] plays the game based on whether or not the player is
 * currently in battle.
 * Effects: Prints all information about the battle, can all cause the program 
 * to be delayed with sleeps.
 * @param game the current game state
 * @return the new game play
*)
and playMain game = if(Game.is_in_battle game) then playGame game
  else playMap game

(**
 * [play_game f] starts the adventure in file [f] and reads it into a Yojson
 * object, which is then turned into a valid adventure representation. If the
 * user inputs an invalid or unrecognized json then an error message is thrown.
 * This method creates an adventure state and then evokeStates on it.
 *
 * @param f which is a file that contains a json format for an adventure
 * to be parsed from
 * @return type unit that we get from playin the adventure game. play_game
 * either will return an error message to the user to reinput the file or start
 * the adventure.
*)
let play_game f name =
  match(List.rev (String.split_on_char '.' f)) with
  | [] ->
    ANSITerminal.(print_string [red]
                    "Uhhhhhh this is not a JSON. We need a JSON. Period.\n");
    exit 0
  | h :: t ->
    if (h = "json")
    then ( try(
        ANSITerminal.(print_string [yellow] 
                        "Welcome to your first battle! Type 'help' to get help
                        \n");
        let j = Yojson.Basic.from_file f in
        let game = Game.from_json j in
        playMain (Game.autoBattle game)
      )with e -> 
        ANSITerminal.(print_string [red]
                        "You gave us nothing and said it was a json file.\n");
      )
    else
      ANSITerminal.
        (print_string [red]
           "Uhhhhhh this is not a JSON. We need a JSON. Period.\n"); exit 0


(**
 * [main ()] prompts for the game to play, then starts it. If the line is
 * empty, then an [Exception End_of_fie] is thrown. Otherwise, it plays the game
 * with the user input as a [file_name]
 *
 * @return either returns an exception or plays the adventure game, which
 * returns a series of unit play and error messages.
*)
let main () =
  ANSITerminal.erase Screen;
  ANSITerminal.(print_string [green] 
                  "\n\n                             Welcome to Pocamel!\n\n"); 
  ANSITerminal.
    (print_string [yellow]
       "         The game in which you move around a world and capture pocamels! 
                 Note: this is definitely not a ripoff of Po*emon.\n\n");

  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> 
    print_endline "";
    print_endline 
      "What is your name bold adventurer? [hit enter for default]: \n";
    print_string  "> ";
    match read_line () with
    | exception End_of_file -> ()
    | name -> play_game file_name name

(* Execute the game engine. *)
let () = main ()

open Pocamel
open Battle
open Trainer
open Game
(**
 * [determine_move battle] determines the next move that a AI bot should make
 * based off of the following protocol given a battle.
 *
 * 1. If the current pokemon of the bot isn't dead, then the bot picks a move
 *  at random to attack with. This will only choose moves with > 0 stock.
 * 2. If the current pokemon is dead, then switch with another pokemon that has
 * greater than 0 health to battle with.
 * 
 * The result of the bot can be one of two things: switch ... or attack ...
 * 
 * Side effects: None
 * @param battle the current state of the battle
 * @return the string result that the bot will choose to do.
*)
let determine_move battle game =
  let my_pocamel = Battle.opponent_pocamel battle in
  let opponent_pocamel = Battle.player_pocamel battle in
  let me = Battle.opponent battle in
  let health_percentage x = 
    float_of_int (Pocamel.hp (x))/.float_of_int(Pocamel.max_hp (x)) in
  let non_killed_pocamels = 
    List.filter (fun x -> health_percentage x <> 0.0) (Trainer.pocamels me) in
  let non_hurt_pocamels = 
    List.filter (fun x -> health_percentage x > 0.15) (Trainer.pocamels me) in
  let highest_poca = 
    (if(List.length non_hurt_pocamels > 0) then
       List.fold_left 
         (fun x y -> 
            if (health_percentage x) > (health_percentage y) then x else y)  
         my_pocamel non_hurt_pocamels
     else
       List.fold_left 
         (fun x y -> 
            if (health_percentage x) > (health_percentage y) then x else y) 
         my_pocamel non_killed_pocamels)
  in
  if(Pocamel.hp my_pocamel = 0) then
    ("switch", Pocamel.name highest_poca)
  else if(health_percentage my_pocamel < 0.25 && my_pocamel <> highest_poca)then
    ("switch", Pocamel.name highest_poca)
  else
    let available_moves = Array.of_list (List.filter (fun (x:Pocamel.move) -> 
        Pocamel.stock x > 0) 
        (Pocamel.move_list (Battle.opponent_pocamel battle))) in
    if(Array.length available_moves = 0) then
      if(List.length non_hurt_pocamels > 0 || 
         List.length non_killed_pocamels > 0) then
        ("switch", Pocamel.name highest_poca)
      else
        ("attack", "") 
    else
      let max_damage_move = List.fold_left (fun x y -> 
          if(Pocamel.hp (Pocamel.attack my_pocamel opponent_pocamel 
                           (Game.moves game) (Pocamel.move_name x) false) 
             <  Pocamel.hp (Pocamel.attack my_pocamel opponent_pocamel 
                              (Game.moves game) (Pocamel.move_name y) false)) 
          then x else y) 
          (Array.get available_moves 0) (Pocamel.move_list my_pocamel) in
      ("attack", Pocamel.move_name max_damage_move)


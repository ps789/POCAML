open Pocamel
open Yojson.Basic.Util
open Location
open Item

exception IllegalPocamel
exception IllegalItem
exception IllegalMove
exception TooManyPocamels

type location = Location.t

type item = Item.t

type orientation = North | South | East | West

(** The type [t] represents a trainer who will be commanding their pocamels.
 * Multiple trainers participate in a battle.
*)
type t = {
  name : string;
  pocamels : Pocamel.t list;
  items : item list;
  loc : location;
  direction : orientation;
}

(**
 * [ori_from_int dir] gets a direction based on the orientation given as 
 * a string.
 * Effects: None
 * @param the dir given as string
 * @return the direction
*)
let ori_from_string dir =
  if(dir = "North") then North
  else if(dir = "South") then South
  else if(dir = "East") then East
  else West

(**
 * [trainer_from_json json] parses a trainer type from a Yojson file
 * representation.
 * Effects: None
 * @param json the Yojson representation of the json doc
 * @return the parsed trainer type
*)
let trainer_from_json json = {
  name = json |> member "name" |> to_string;
  pocamels = json |> member "pocamels" |> to_list |> List.map Pocamel.from_json;
  items = json |> member "items" |> to_list |> List.map Item.item_from_json;
  loc = json |>  member "location" |> Location.location_from_json;
  direction = json |> member "direction" |> to_string |> ori_from_string;
}

(**
 * [get_items trainer] returns a list of items that the trainer has
 * representation.
 * Effects: None
 * @param trainer the owner of the items
 * @return the trainer's items
*)
let get_items trainer = 
  trainer.items

(**
 * [get_pocamel trainer poke] returns the pocamel and all of its information
 * based off of the pokemon that the trainer has. Throws IllegalPocamel if the
 * trainer doesn't have that pocamel.
 * Effects: None
 * @param trainer who is being accessed for the pocamel
 * @return the target pocamel
*)
let get_pocamel trainer poke = 
  try
    List.find (fun x -> Pocamel.name x = poke) trainer.pocamels 
  with
    Not_found -> raise IllegalPocamel

(**
 * [pocamels trainer] returns all of the pocamels that a trainer has.
 * Effects: None
 * @param trainer th owner of the pocamels
 * @return the list of pocamels a trainer has
*)
let pocamels trainer = 
  trainer.pocamels

(**
 * [go trainer] returns the tail of all the pocamels a trainer has.
 * Effects: None
 * @param trainer the owner of the pocamels
 * @return the tail of the list of pocamels.
*)
let go trainer = {
  trainer with pocamels = List.tl trainer.pocamels;
}

(**
 * [replace_pocamels] returns the list of the Trainer's pocamels with one of
 * them being modified, specifically the one with the same name as the target
 * pocamel.
 * Effects: None
 * @param pk1 the target pocamel to change in the Trainer's pocamel list
 * @param pkList the pocamel list that the Trainer has
 * @param pkAgg the accumulator for pocamels
 * @return list of the Trainer's pocamels with a modified pocamel.
*)
let rec replace_pocamels pk1 pkList pkAgg = 
  match pkList with
  | [] -> pkAgg
  | h::t -> if(Pocamel.name h = Pocamel.name pk1) 
    then replace_pocamels pk1 t (pk1::pkAgg)
    else replace_pocamels pk1 t (h::pkAgg)

(**
 * [replace_trainer_pocamels pk trainer] returns a trainer with one of its 
 * pocamels being modified, speicifically the one with the same name as pk
 * Effects: None
 * @param pk the target pocamel to change in the Trainer's pocamel list
 * @param trainer the trainer to be modified
 * @return trainer with the modified pocamels
*)
let replace_trainer_pocamels pk trainer = {
  trainer with pocamels = replace_pocamels pk trainer.pocamels []
}

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
let use_item (trainer : t) (pk : string) (item : string) = 
  try
    let my_item = Item.decrCount 
        (List.find (fun (x : item) -> Item.name x = item) trainer.items) in
    let target_pk = get_pocamel trainer pk in
    let modified_pk = match Item.elixir my_item with
      | None -> Pocamel.use_item target_pk my_item
      | Some n -> Pocamel.use_elixir target_pk my_item in
    let new_bag = List.filter (fun x -> not (Item.countZero x))
        (my_item::(List.filter (fun (x: item) -> (Item.name x <> item)) 
                     trainer.items)) in
    let new_pocamels = replace_pocamels modified_pk trainer.pocamels [] in

    {
      trainer with pocamels = new_pocamels; 
                   items = new_bag;
    }
  with
  | Not_found -> raise IllegalItem
  | IllegalPocamel -> raise IllegalPocamel

(**
 * [get_lor_or trainer] gets a tuple of the location and an orientation of 
 * a trainer
 * Effects: None
 * @param trainer the trainer whose information is returned
 * @return the location orientation tuple of a trainer
*)
let get_loc_or trainer = (trainer.loc, trainer.direction)

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
let use_pocaball tr1 tr2 pk item = 
  try 
    let my_item = Item.decrCount 
        (List.find (fun (x : item) -> Item.name x = item) tr1.items) in
    let new_bag = List.filter (fun x -> not (Item.countZero x))
        (my_item::(List.filter (fun (x: item) -> (Item.name x <> item)) 
                     tr1.items)) in
    if(List.length tr1.pocamels > 5) then raise TooManyPocamels else
    if(tr2.name = "wild") then
      if(Pocamel.catch my_item pk) then
        (
          Array.of_list 
            [{
              tr1 with pocamels = (pk::tr1.pocamels);
                       items = new_bag;
            };
             {
               tr2 with pocamels = 
                          List.filter 
                            (fun x -> Pocamel.name x <> Pocamel.name pk) 
                            tr2.pocamels; 
             }], true)
      else
        (Array.of_list 
           [{tr1 with items = new_bag};
            tr2], false)
    else 
      raise IllegalMove
  with
  | Not_found -> raise IllegalItem
  | TooManyPocamels -> raise IllegalMove

(**
 * [name t] returns the name of a trainer
 * Effects: None
 * @param trainer the trainer name is returned
 * @return the name of the trainer
*)
let name t = t.name

(**
 * [vision_tile trainer] gets a tuple of the location in front of a trainer
 * based off of their location and the orientation they are standing in.
 * Effects: None
 * @param trainer the trainer whose adjacent tile is returned
 * @return the location of the tile in front of the trainer
*)
let vision_tile trainer  =
  let trainerx = Location.get_x trainer.loc
  and trainery = Location.get_y trainer.loc
  in
  match trainer.direction with
  | North -> Location.new_loc trainerx (trainery-1)
  | South -> Location.new_loc trainerx (trainery+1)
  | East -> Location.new_loc (trainerx+1) trainery
  | West -> Location.new_loc (trainerx-1) trainery

(**
 * [set_name trainer name] returns the same trainer with name changed to name
 * Effects: None
 * @param trainer the trainer whose name is changed
 * @param name the name to be changed to
 * @return the new trainer
*)
let set_name trainer name =
  {
    trainer with name = name;
  }

(**
 * [turn trainer ori] returns the same trainer with orientaiton changed to ori
 * Effects: None
 * @param trainer the trainer whose orientation is changed
 * @param ori the orientation to be changed to
 * @return trainer with new orientation
*)
let turn trainer ori = {
  trainer with direction = ori;
}

(**
 * [set_location trainer newLoc] returns the same trainer but in a differen
 * location specified by newLoc
 * Effects: None
 * @param trainer the trainer whose location is changed.
 * @param newLoc the location to be changed to
 * @return the trainer with a new location
*)
let set_location trainer newLoc = {
  trainer with loc = newLoc;
}

(**
 * [wild pocamel] returns the wild trainer with a target pocamel as its only
 * pocamel. This is how pocamels exist naturally in the wild, under a psuedo
 * trainer.
 * @param pocamel
 * @return the wild trainer with just target pocamel
*)
let wild pocamel = {
  name = "wild";
  pocamels = [Pocamel.vary pocamel];
  items = [];
  loc = Location.new_loc 0 0;
  direction = North;
}
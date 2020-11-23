open Trainer
open Yojson.Basic.Util
open Pocamel
type map_object = Grass | Tree

(** The type t represents a pocamap where the player will traverse in the game*)
type t = {
  plane : Location.t * 
          ((Location.t,map_object option * Trainer.t option) Hashtbl.t);
  wild : Pocamel.t list;
  rate : float 
}

exception Out_of_bounds

(**
 * [mapRows map] returns the number of rows in a map.
 * Effects: None
 * @param map the map to be checked
 * @return the number of rows in a map
*)
let mapRows (map : t) = fst map.plane |> Location.get_y

(**
 * [mapColumns map] returns the number of columns in a map.
 * Effects: None
 * @param map the map to be checked
 * @return the number of columns in a map
*)
let mapColumns (map : t) = fst map.plane |> Location.get_x

(**
 * [mapObject map] returns the map object from the pocamap typle
 * Effects: None
 * @param the map to be extract from 
 * @return the map object
*)
let mapObject (map : t) = snd map.plane

(**
 * [ullr_from_json json] parses from a json document the upper left and lower
 * right bound for some construction which will exist in our map.
 * Effects: None
 * @param json the valid Yojson object to parse from 
 * @return the tuple with the parsed ul lr construction
*)
let ullr_from_json json = (json |> member "ul" |> Location.location_from_json,
                           json |> member "lr" |> Location.location_from_json)

(**
 * [initMap map x y boundCoord] initializes the map based off of the intial x
 * y coordinate of the player, the map which contains all of our important 
 * map elements, and the bounding coordinates of the map. 
 * Effects: None
 * @param map the map which is hashtable for the objects and their coords
 * @param x the current x coordinate of the player
 * @param y the current y coordinate of the player
 * @param boundCoord the bounding coordinates of the map
 * @return the initialized map based off of the hashmap
*)
let rec initMap map x y boundCoord  :  
  ((Location.t,map_object option * Trainer.t option) Hashtbl.t) = 
  if y > Location.get_y boundCoord then map
  else if x > Location.get_x boundCoord then initMap map 1 (y+1) boundCoord
  else begin
    Hashtbl.replace map (Location.new_loc x y) (None, None);
    initMap map (x+1) y boundCoord
  end

(**
 * [eraseTrainersHelper map x y boundCoord] removes the old place where the
 * trainer used to be in the map for the purpose of testing.
 * Effects: None
 * @param map the hashtable of the objects and all their coordinates
 * @param x the current x position of the player
 * @param y the current y position of the player
 * @param  boundCoord the bounding coordinates of the map
 * @return the map with the old player position erased
*)
let rec eraseTrainersHelper map x y boundCoord : 
  ((Location.t,map_object option * Trainer.t option) Hashtbl.t) = 
  if y > Location.get_y boundCoord then map
  else if x > Location.get_x boundCoord 
  then eraseTrainersHelper map 1 (y+1) boundCoord
  else begin
    Hashtbl.replace map (Location.new_loc x y) 
      (fst (Hashtbl.find map (Location.new_loc x y)), None);
    eraseTrainersHelper map (x+1) y boundCoord
  end

(**
 * [addTrainers map trainerList] adds all of the trainers to the map for the 
 * purpose of printing based on their individual hash-mapped stored positions.
 * Effects: None
 * @param map the hashtable of the objects and all their coordinates
 * @param x the current x position of the player
 * @param trainerList the list of trainers that exist in the game
 * @return the map with the trainers placed in it
*)
let rec addTrainers map trainerList = match(trainerList) with
  |[] -> map
  |h::t -> Hashtbl.replace map (fst (Trainer.get_loc_or h)) 
             (fst (Hashtbl.find map (fst (Trainer.get_loc_or h))), Some h);
    addTrainers map t

(**
 * [updateTrainers map trainerList] updates the pocamap based off of the trainer
 * list in a game.
 * Effects: None
 * @param map the hashtable of the objects and all their coordinates
 * @param trainerList the list of trainers in the game.
 * @return the map with all of the trainers updated
*)
let updateTrainers map trainerList = {
  map with plane = (fst map.plane, ((eraseTrainersHelper 
                                       (snd map.plane) 1 1 (fst map.plane))
                                    |> addTrainers) trainerList)
}

(**
 * [addPatchHelper obj map ul lr] sets patches of grass or trees in the map
 * by taking their bounding corner coordinates, the current map, and targeting
 * inbetween those bounds.
 * Effects: None
 * @param map the hashtable of the objects and all their coordinates
 * @param obj the abstract map object of the game state
 * @param ul the upper left bound of the grass or tree patch
 * @param lr the same but opposit of the upper left
 * @return the map with the patch placed in
*)
let rec addPatchHelper (obj : map_object)
    (map : (Location.t,map_object option * 
                       Trainer.t option) Hashtbl.t) 
    (ul: Location.t) (lr : Location.t) = 
  let rec patchRow map (l: int) (r: int) (row: int) = 
    (if(l > r) then map
     else (Hashtbl.replace map (Location.new_loc l row) 
             (Some obj, None); patchRow map (l+1) r row))
  in if(Location.get_y ul > Location.get_y lr) then map
  else addPatchHelper obj
      (patchRow map (Location.get_x ul) (Location.get_x lr) (Location.get_y ul))
      (Location.new_loc (Location.get_x ul) (Location.get_y ul +1)) lr

(**
 * [addPatches obj map ul lr] uses the add patch helper to add the patches to
 * the actual map.
 * Effects: None
 * @param map the hashtable of the objects and all their coordinates
 * @param obj the abstract map object of the game state
 * @param patchList
 * @return the map with the patch placed on
*)
let rec addPatches (obj : map_object) (map : t) 
    (patchList : (Location.t * Location.t) list) = 
  match (patchList) with
  |[] -> map
  |h::t -> addPatches obj {
      map with plane = 
                 (fst map.plane, 
                  (addPatchHelper obj (snd map.plane) (fst h) (snd h)))
    } t

(**
 * [from_json json map ul lr] parses in the map object from the json file
 * in the required Yojson format. A map has a plane, the wild pocamels, and 
 * rate.
 * Effects: None
 * @param json the valid Yojson type representing the game
 * @return the parsed in map
*)
let from_json json = let bounds = 
                       json |> member "lr" |> Location.location_from_json in 
  let basicMap = 
    { plane = 
        (bounds,initMap (Hashtbl.create 
                           (Location.get_x bounds * Location.get_y bounds)) 
           1 1 bounds
        );
      wild = 
        json |> 
        member "pocamels" |> to_list |> List.map Pocamel.from_json;
      rate = json |> member "encounter rate" |> to_float
    } in (addPatches Grass basicMap 
            (json |> member "grass" |> to_list 
             |> List.map ullr_from_json)
          |> addPatches Tree) (json |> member "trees" |> to_list 
                               |> List.map ullr_from_json)


(**
 * [printMap x y rows columns map] recursively prints the map out followed by
 * the helper prints for the menu. This is intended to be used as a helper 
 * method for print.
 * Effects: Prints the map to the console
 * @param x the x counter for iterating through the map
 * @param y the y counter for iterating through the map
 * @param rows the number of columns in the map
 * @param columns the number of columns in the a map
 * @param map the hashtable of the objects and all their coordinates
 * @return unit
*)
let rec printMap x y rows columns (map : t)  player =
  if y > rows then (ANSITerminal.(print_string [blue] 
                                    "\n[e | Menu] ");
                    ANSITerminal.(print_string [yellow] 
                                    "[Space | Challenge] ");
                    ANSITerminal.(print_string [red] 
                                    "[q | Quit]\n");
                    print_endline "")
  else if x > columns 
  then let _ = print_endline "" in printMap 1 (y+1) rows columns map player
  else
    begin
      let _ = match Hashtbl.find (snd map.plane) (Location.new_loc x y) 
        with 
        |(_, Some t) -> 
          ANSITerminal.(print_string (if(t = player) 
                                      then [white] else [magenta])
                          (match snd (Trainer.get_loc_or t) with
                           |Trainer.North -> "^"
                           |Trainer.East -> ">"
                           |Trainer.South -> "v"
                           |Trainer.West -> "<")
                       )
        |(Some Tree, _) -> ANSITerminal.(print_string [red] "T")
        |(Some Grass, _) -> ANSITerminal.(print_string [green] "w")
        |(None, _) -> ANSITerminal.(print_string [black] " ")
      in printMap (x+1) y rows columns map player
    end

(**
 * [print map] prints the map based off of its dimensions and the objects inside
 * of it.
 * Effects: prints the pocamap
 * @param map the hashtable of the objects and all their coordinates
 * @return unit 
*)
let print (map : t) player =
  (printMap 1 1 (Location.get_y (fst map.plane))
     (Location.get_x (fst map.plane)) map player)

(**
 * [mapTile map Loc] gets the hash table value correlated with the key of the 
 * location with x coordinate x and y coordinate y
 * Effects: none
 * @param map the hashtable of the objects and all their coordinates
 * @param loc the location
 * @return unit 
*)
let mapTile map loc =
  let x = Location.get_x loc
  and y = Location.get_y loc in
  if(x<1 || x > mapColumns map || y < 1 || y > mapRows map) then
    raise Out_of_bounds
  else
    Hashtbl.find (snd map.plane) (Location.new_loc x y)

(**
 * [wild_pocamels map] returns a list of all possible wild pocamels in the map
 * Effects: 
 * @param map the hashtable of the objects and all their coordinates
 * @return the list of all wild pocamels in the Game
*)
let wild_pocamels map =
  map.wild

(**
 * [encounter_rate map] returns the encounter rate of a map
 * Effects: none
 * @param map the hashtable of the objects and all their coordinates
 * @return float reprsentation of the rate
*)
let encounter_rate map =
  map.rate

let clear_map rows columns = 
  for i = 0 to rows - 1 do
    for j = 0 to columns - 1 do
      (ANSITerminal.(print_string [white] "█"));
    done;
    print_endline "";
    Unix.sleepf 0.2;
  done;
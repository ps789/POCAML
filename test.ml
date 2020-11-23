open OUnit2
open Yojson.Basic
open Item
open Move
open Pocamel
open Command
open Battle
open Trainer
open Game
open Bot

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and
   [pp_list] to get helpful output from OUnit. *)
let cmp_demo =
  [
    "order is irrelevant" >:: (fun _ ->
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "bar"] ["bar"; "foo"]);
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ ->
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          ["foo"; "foo"] ["foo"]);
    *)
  ]

(********************************************************************
   End helper functions.
 ********************************************************************)
let myGame = Game.from_json (Yojson.Basic.from_file "a.json")
let game_json = Yojson.Basic.from_file "a.json"
let trainer = get_player myGame
let playerTrainer = Game.autoBattle myGame |> Game.get_battle |> Battle.player
let player_items = get_items playerTrainer
let opponentTrainer = Game.autoBattle myGame |> Game.get_battle |> Battle.opponent
let opponent_items = get_items opponentTrainer
let trainer_items = get_items trainer
let player_pocamels = pocamels playerTrainer
let opponent_pocamels = pocamels opponentTrainer

let item_tests = [
  "test_item1" >::
  (fun _ -> assert_equal (Item.name (List.hd trainer_items)) ("Hype Potion"));
  "test_item2" >::
  (fun _ -> assert_equal (Item.name (List.nth trainer_items 1)) ("Stone Plate"));
  "test_item3" >::
  (fun _ -> assert_equal (Item.name (List.nth trainer_items 2)) ("poca-ball"));
  "test_item4" >::
  (fun _ -> assert_equal (Item.name (List.nth trainer_items 3)) ("great-ball"));
  "test_item5" >::
  (fun _ -> assert_equal (Item.name (List.nth trainer_items 4)) ("master-ball"));
]

let pocamel_tests = [
  "test_pocamel1" >::
  (fun _ -> assert_equal (Pocamel.name (List.hd player_pocamels)) ("Medapod"));
  "test_pocamel2" >::
  (fun _ -> assert_equal (Pocamel.name (List.nth player_pocamels 1)) ("Majikarp"));
  "test_pocamel3" >::
  (fun _ -> assert_equal (Pocamel.name (List.nth player_pocamels 2)) ("Hipcamdon"));
  "test_pocamel4" >::
  (fun _ -> assert_equal (Pocamel.name (List.nth player_pocamels 3)) ("Prelimkia"));
  "test_pocamel5" >::
  (fun _ -> assert_equal (Pocamel.name (List.hd opponent_pocamels)) ("Majikarp"));
  "test_pocamel6" >::
  (fun _ -> assert_equal (Pocamel.name (List.nth opponent_pocamels 1)) ("Majikarp2"));
  "test_pocamel7" >::
  (fun _ -> assert_equal (Pocamel.name (List.nth opponent_pocamels 2)) ("Laptopcrop"));
  "test_pocamel8" >::
  (fun _ -> assert_equal (Pocamel.name (List.nth opponent_pocamels 3)) ("Typachacka"));
  "test_pocamel9" >::
  (fun _ -> assert_equal (Pocamel.name (List.nth opponent_pocamels 4)) ("Medapod"));
]


let command_tests = [
  "test_parse1" >::
  (fun _ -> assert_equal (parse "Bag") (Bag));
  "test_parse2" >::
  (fun _ -> assert_equal (parse "Run") (Run));
  "test_parse3" >::
  (fun _ -> assert_equal (parse "run") (Run));
  "test_parse4" >::
  (fun _ -> assert_equal (parse "bag") (Bag));
  "test_parse5" >::
  (fun _ -> assert_equal (parse "fight") (Fight));
  "test_parse6" >::
  (fun _ -> assert_equal (parse "Fight") (Fight));
  "test_parse7" >::
  (fun _ -> assert_equal (parse "switch") (Switch));
  "test_parse8" >::
  (fun _ -> assert_equal (parse "help") (Help));
  "test_parse9" >::
  (fun _ -> assert_equal (parse "Help") (Help));
  "test_parse10" >::
  (fun _ -> assert_equal (parse " Help ") (Help));
  "test_parse11" >::
  (fun _ -> assert_equal (parse " switch ") (Switch));
]

let game_tests = [
  "test1" >::
  (fun _ -> assert_equal (get_items playerTrainer) (player_items));
  "test2" >::
  (fun _ -> assert_equal (get_items opponentTrainer) (opponent_items));
]

let trainer_tests = [
"test1" >::
(fun _ -> assert_equal (get_player myGame) (trainer));
"test2" >::
(fun _ -> assert_equal (Trainer.get_items trainer) (trainer_items));
"test3" >::
(fun _ -> assert_equal (Trainer.get_items opponentTrainer) (opponent_items));
"test4" >::
(fun _ -> assert_equal (Trainer.get_items opponentTrainer) (opponent_items));
"test5" >::
(fun _ -> assert_equal (Trainer.pocamels trainer) (player_pocamels));
]

let suite =
  "test suite for A6"  >::: List.flatten [
    item_tests;
    game_tests;
    pocamel_tests;
    command_tests;
    trainer_tests;
  ]

let _ = run_test_tt_main suite

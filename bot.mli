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
val determine_move : Battle.t -> Game.t -> string*string
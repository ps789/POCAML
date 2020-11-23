(** The type [object phrase] will be a compoud of a command and its 
 * subsequent details. *)
type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Bag
  | Run
  | Fight
  | Help
  | Switch


(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** 
 * [parse str] parses a player's input into a [command], as follows. The first
 * word (i.e., consecutive sequence of non-space characters) of [str] becomes 
 * the verb. The rest of the words, if any, become the object phrase.
 * Examples: 
 * - [parse "    attack flame thrower   "] is [Attack ["flame"; "thrower"]]
 * - [parse "run"] is [Run]. 
 *
 * Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
 * characters (only ASCII character code 32; not tabs or newlines, etc.).
 *
 * Raises: [Empty] if [str] is the empty string or contains only spaces. 
 *
 * Raises: [Malformed] if the command is malformed.
 *
 * @param str the string to be parsed through a command
 * @return the command, if str is a valid command, that does something with any
 * of the available commands above
*)
val parse : string -> command

(**
 * [cleanSpaceReverse acc] takes in a list of strings [acc], and gets ride of
 * the useless spaces which might throw off the ouput.
 *
 * @param acc the accumulator list to be returned
 * @return a target list in reverse order without any spaces
*)
val cleanSpaceReverse : string list -> string list -> string list
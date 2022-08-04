(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* Problem1 : takes a string list and returns a string list that has only the strings in the argument that start with an uppercase letter *)
fun only_capitals str_list =
    List.filter (fn str => Char.isUpper(String.sub(str, 0))) str_list

(* Problem2 : takes a string list and returns the longest string in the list. returns "" when the list is empty. In the case of a tie, return the sstring closest to the beginning of the list. *)
fun longest_string1 str_list =
    foldl (fn (str, max) => if String.size(str) > String.size(max) then str else max) "" str_list

(* Problem3 : exactly like longest_string1 except returns the string closest to the end of the list in the case of tie. *)
fun longest_string2 str_list =
    foldl (fn (str, max) => if String.size(str) >= String.size(max) then str else max) "" str_list

(* Problem4 : abstraction of ...1 and ...2  *)
fun longest_string_helper f str_list =
    foldl (fn (str, max) => if f(String.size(str), String.size(max)) then str else max) "" str_list

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

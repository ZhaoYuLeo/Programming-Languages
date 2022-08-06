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


(* Problem5 : takes a string list and returns the longest string in the list that begins with an uppercase letter, or "" if there are no such strings *)
val longest_capitalized = longest_string3 o only_capitals


(* Problem6 : takes a string and returns the string that is the same characters in reverse order  *)
val rev_string = String.implode o rev o String.explode


(* Problem7 : takes a function and a list, both are curried and returns the first answer applying each element in the list to the function. Raise NoAnswer when the function returns None for all list elements. *)
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs'
		    | SOME v => v


(* Problem8 : Similar to first_answer. Returns SOME lst which contains all results producted by the function and the list. Returns NONE if the function returns NONE for any element. *)
fun all_answers f xs =
    let fun helper (acc, xs_left) =
	    case xs_left of
		[] => SOME acc
	      | x::xs' => case f x of
			      NONE => NONE
			    | SOME v => helper(acc @ v, xs')
    in helper ([], xs)
    end


(* Problem9 : *)
(* takes a pattern and returns how many Wildcard patterns it contains *)
val count_wildcards = g (fn x => 1) (fn x => 0)

(* takes a pattern and returns teh number of Wildcard patterns it contains plus the sum of the string lengths of all the variables in the variable patterns it contains *)
val count_wild_and_variable_lengths = g (fn x => 1) (fn x => String.size(x))

(* takes a string and a pattern (as a pair) and returns the number of times the string appears as a variable in the pattern *)
fun count_some_var (str, p) = g (fn x => 0) (fn x => if x = str then 1 else 0) p


(* Problem10 : takes a pattern and returns true if and only if all the variables appearing in the pattern are distinct from each other *)
fun check_pat p =
    let fun all_variables_strings p =
	    case p of
	    Variable x => [x]
	  | TupleP ps => List.foldl (fn (p, acc) => (all_variables_strings p) @ acc) [] ps
	  | ConstructorP(_,p) => all_variables_strings p
	  | _ => []
	fun strings_distinct str_list =
	    case str_list of
		[] => true
	      | str::lst' => not( List.exists (fn x => x=str) lst') andalso strings_distinct lst'
    in (strings_distinct o all_variables_strings) p
    end

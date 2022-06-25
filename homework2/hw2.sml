(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* use pattern-matching  *)
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
	     
(* 1a: string list except target string *)
fun all_except_option(except_str, str_list) =
    case str_list of
	[] => NONE
      | str::str_list' => if same_string(except_str, str)
			  then SOME str_list'
			  else case  all_except_option(except_str, str_list') of
				   NONE => NONE
				 | SOME lst => SOME(str::lst)

(* append is too expensive. won't perform better  *)
fun all_except_option_op(except_str, str_list) =
    let fun except(lst, acc) =
	    case lst of
		[] => NONE
	      | s::lst' => if same_string(except_str, s)
			   then SOME (acc @ lst')
			   else except(lst', acc @ [s])
    in except(str_list, [])
    end
	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

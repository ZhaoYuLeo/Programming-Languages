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

fun get_substitutions1(substitutions, name) =
    case substitutions of
	[] => []
      | group::groups => case all_except_option(name, group) of
			     NONE => get_substitutions1(groups, name) 
			   | SOME lst => lst @ get_substitutions1(groups, name)

fun get_substitutions2(substitutions, name) =
    let fun aux(subs, acc) =
	    case subs of
		[] => acc
	      | sub::subs' => case all_except_option(name, sub) of
				     NONE => aux(subs', acc)
				   | SOME lst => aux(subs', acc @ lst)
    in aux(substitutions, [])
    end

(* when i use tail recursion and append, the order is not right *)
fun similar_names(substitutions, {first=first_name, middle=m, last=l}) =
    let fun aux ([]) = []
	  | aux (n::names') = {first=n, last=l, middle=m}::aux(names')
    in aux(first_name::get_substitutions2(substitutions, first_name))
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
 
fun card_color (Spades, _) = Black
  | card_color (Clubs, _) = Black
  | card_color _ = Red

fun card_value (one_card) =
    case one_card of
	(_, Num i) => i
      | (_, Ace) => 11
      | _ => 10

fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | sc::cs' => if sc = c
		   then cs'
		   else sc::remove_card(cs', c, e)
    
fun all_same_color (cards_list) =
    case cards_list of
	[] => true
      | x::[] => true
      | x::y::clt' => card_color(x) = card_color(y) andalso all_same_color(y::clt')
				 
fun sum_cards (cards_list) =
    let fun sum_value(cs, acc) =
	    case cs of
		[] => acc
	      | c::cs' => sum_value(cs', acc + card_value(c))
    in sum_value(cards_list, 0)
    end

(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* (a) string * string list -> string list option *)
fun all_except_option (str : string, strs : string list) =
     let
	fun strInList [] = false
	  | strInList (str2::strs') = same_string(str, str2) orelse strInList(strs')

	val stringinList = strInList strs

	fun removeStrInList [] = []
	  | removeStrInList (str2::strs') = if same_string(str, str2)
					    then removeStrInList(strs')
					    else str2::removeStrInList(strs')

	val strRemovedList = removeStrInList strs
					     
    in if not stringinList then NONE else SOME strRemovedList end
	 
(* (b) string list list * string -> string list *)
fun get_substitutions1 ([], str) = []
  | get_substitutions1 (strs::substitutions', str) =
    let val newStrs = all_except_option(str, strs)
    in
	case newStrs of
	    NONE => get_substitutions1(substitutions', str)
	  | SOME aeoList => aeoList @ get_substitutions1(substitutions', str)
    end

(* (c) string list list * string -> string list *)
fun get_substitutions2 (substitutions, str) =
    let fun aux ([], acc) = acc
	  | aux (strs::substitutions', acc) =
		let val newStrs = all_except_option(str, strs)
		in
		    case newStrs of
			NONE => aux(substitutions', acc)
		      | SOME aeoList => aux(substitutions', acc @ aeoList)
		end
		    
    in aux(substitutions, []) end

(* (d) string list list * {first:string,middle:string,last:string} ->
       {first:string,middle:string,last:string} list *)
fun similar_names (substitutions, fullName) =
    let
	fun retName {first, middle, last} = first
						
	val firstName = retName fullName
	val subNames = get_substitutions2(substitutions, firstName)

	fun retRecName ([], _) = []
	  | retRecName (sub::subNames', {first, middle, last}) =
	    {first=sub, middle=middle, last=last}::retRecName(subNames', fullName)
							     
    in fullName::retRecName(subNames,fullName) end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* (a) card -> color *)
fun card_color (Clubs, _) = Black
      | card_color (Diamonds, _) = Red
      | card_color (Hearts, _) = Red
      | card_color (Spades, _) = Black 

(* (b) card -> int *)		   
fun card_value (_, Num x) = x
      | card_value (_, Ace) = 11
      | card_value _ = 10

fun card_value_challenge (_, Num x) = x
      | card_value (_, Ace) = 1
      | card_value _ = 10

(* (c) card list * card * exception -> card list *)
fun remove_card (cs, c, e) =
    let
	fun isEmpty [] = true
	  | isEmpty (c::cs') = false
				 
	fun c_in_cs [] = raise e
	      | c_in_cs (crd::cs') = (crd = c orelse c_in_cs cs')
							     
	fun ret_cs [] = []
	  | ret_cs (crd::cs') = if c_in_cs cs
				then if crd = c	then cs' else crd::ret_cs(cs')
				else raise e
					   
    in if isEmpty cs then raise e else ret_cs cs end

(* (d) card list -> bool *)
fun all_same_color [] = true
      | all_same_color (_::[]) = true
      | all_same_color (c1::c2::cs') = card_color(c1) = card_color(c2) andalso
				       all_same_color(c2::cs')			
	
(* (e) card list -> int *)
fun sum_cards cs =
    let	fun aux ([], acc) = acc
	  | aux (c::cs', acc) = aux(cs', card_value(c) + acc)			      
    in	aux(cs, 0) end

fun sum_cards_challenge cs =
    let	fun aux ([], acc) = acc
	  | aux (c::cs', acc) = aux(cs', card_value_challenge(c) + acc)		      
    in	aux(cs, 0) end

(* (f) card list * int -> int *)
fun score (cs, goal) =
    let
	val sum = sum_cards cs

	val prelim_score  = if sum > goal then 3 * (sum - goal) else goal - sum
										
    in if all_same_color cs then prelim_score div 2 else prelim_score end

(* (g) card list * move list * int -> int *)
fun officiate (cs, ms, goal) =
    let	fun do_move (ms, heldCards, cs) =
	    if sum_cards heldCards > goal then score(heldCards, goal) else
	    case (ms, heldCards, cs) of
		([], _, _) => score(heldCards, goal)
	      | (_, _, []) => score(heldCards, goal)
	      | (m::ms', _, c::cs') =>
		case m of
		    Discard crd =>
		    let val hldCrds = remove_card (heldCards, crd, IllegalMove)
		    in do_move(ms', hldCrds, cs') end
			
		  | Draw => do_move(ms', c::heldCards, cs')
				  			   
    in do_move(ms,[],cs) end 

(* CHALLENGE PROBLEMS 3 *)

(* (a) card list * int -> int *)
fun score_challenge (cs, goal) =
    let
	val sum = sum_cards_challenge cs

	val prelim_score  = if sum > goal then 3 * (sum - goal) else goal - sum
										
    in if all_same_color cs then prelim_score div 2 else prelim_score end

(* card list * move list * int -> int *)
fun officiate_challenge (cs, ms, goal) =
    let	fun do_move (ms, heldCards, cs) =
	    if sum_cards_challenge heldCards > goal then score_challenge(heldCards, goal)
	    else
		case (ms, heldCards, cs) of
		    ([], _, _) => score_challenge(heldCards, goal)
		  | (_, _, []) => score_challenge(heldCards, goal)
		  | (m::ms', _, c::cs') =>
		    case m of
			Discard crd =>
			let val hldCrds = remove_card (heldCards, crd, IllegalMove)
			in do_move(ms', hldCrds, cs') end
			    
		      | Draw => do_move(ms', c::heldCards, cs')
				  			   
    in do_move(ms,[],cs) end

(* (b) card list * int -> move list *)
fun careful_player (cs, goal) =
    0
    (*let
	val moveList = []

	fun discard_any_to_0 (heldCards, curC) =
	    case heldCards of
		[] => false
	      | c::heldCards =>
		let val hldCrds = remove_card (heldCards, c, IllegalMove)
		in if sum_cards(hldCrds) = 0 then true else  end 
			   
	fun make_move (heldCards, cs) =
	    if score(heldCards, goal) = 0 then moveList
	    else 
    in

    end*)


	

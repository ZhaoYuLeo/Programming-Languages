(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* use pattern-matching  *)
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* Problem1 : using first-name substitutions to come up with alternate names. For example, Fredrick William Smith could also be Fred William Smith or Freddie William Smith. *)

(* Problem1 a : takes a string and a string list. returns NONE if the string is not in the list, else return SOME lst where lst is identical to the argument list except the string is not in it. *)
fun all_except_option(except_str, str_list) =
    case str_list of
	[] => NONE
      | str::str_list' => if same_string(except_str, str)
			  then SOME str_list'
			  else case  all_except_option(except_str, str_list') of
				   NONE => NONE
				 | SOME lst => SOME(str::lst)

(* append is too expensive. won't perform better *)
fun all_except_option_op(except_str, str_list) =
    let fun except(lst, acc) =
	    case lst of
		[] => NONE
	      | s::lst' => if same_string(except_str, s)
			   then SOME (acc @ lst')
			   else except(lst', acc @ [s])
    in except(str_list, [])
    end

(* Problem1 b : takes a string list list of substitutions and a string s returns all strings that are in some list in substitutions that also has s, but s itself should not be in the result. Assume each list in substitutions has no repeats. *)
fun get_substitutions1(substitutions, name) =
    case substitutions of
	[] => []
      | group::groups => case all_except_option(name, group) of
			     NONE => get_substitutions1(groups, name) 
			   | SOME lst => lst @ get_substitutions1(groups, name)

						 
(* Problem1 c : same with get_substitutions1 except it uses a tail-recursive local helper function. *)
fun get_substitutions2(substitutions, name) =
    let fun aux(subs, acc) =
	    case subs of
		[] => acc
	      | sub::subs' => case all_except_option(name, sub) of
				     NONE => aux(subs', acc)
				   | SOME lst => aux(subs', acc @ lst)
    in aux(substitutions, [])
    end


(* Problem1 d : takes a string list list of substitutions and a full name of type. returns all the full names you can produce by substituting for and only for the first name. Assum each list in substitutions has no repeats. *)
(* when i use tail recursion and append, the order is not right *)
fun similar_names(substitutions, {first=first_name, middle=m, last=l}) =
    let fun aux ([]) = []
	  | aux (n::names') = {first=n, last=l, middle=m}::aux(names')
    in aux(first_name::get_substitutions2(substitutions, first_name))
    end

(* Problem2 : tracks the progress of a solitaire card game. a game is played with a card-list and a goal. The player makes a move by either drawing or discarding. The game ends either when the player choose to make no more moves or when the sum of the values of the held-cards is greater than the goal. *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* Problem2 a : takes a card and returns its color. spades and clubs are black, diamonds and hearts are red. *)
fun card_color (Spades, _) = Black
  | card_color (Clubs, _) = Black
  | card_color _ = Red

(* Problem2 b : takes a card and returns its value. numbered cards have their number as the value, aces are 11, everything else is 10. *)
fun card_value (one_card) =
    case one_card of
	(_, Num i) => i
      | (_, Ace) => 11
      | _ => 10

(* Problem2 c : takes a list of cards cs, a card c, and an exception e. returns a list that has all the elements of cs except c. raise exception e if c is not in the list. *)
fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | sc::cs' => if sc = c
		   then cs'
		   else sc::remove_card(cs', c, e)

(* Problem2 d : takes a list of cards and returns true if all the cards in the list are the same color. *)
fun all_same_color (cards_list) =
    case cards_list of
	[] => true
      | x::[] => true
      | x::y::clt' => card_color(x) = card_color(y) andalso all_same_color(y::clt')

(* Problem2 e : takes a list of cards and returns the sum of their value. *)
fun sum_cards (cards_list) =
    let fun sum_value(cs, acc) =
	    case cs of
		[] => acc
	      | c::cs' => sum_value(cs', acc + card_value(c))
    in sum_value(cards_list, 0)
    end

(* Problem2 f : takes the held-cards and the goal and computes the score as described above. *)
fun score (cards_list, goal) =
    let val sum = sum_cards(cards_list)
	val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
    in if all_same_color(cards_list)
       then preliminary_score div 2
       else preliminary_score
    end

(* Problem2 g : takes the card-list, a move list, and the goal and returns the score at the end of this game after processing the moves in the move list on order.  *)

fun process(cards, moves, held_cards, goal, score_fun, sum_cards_fun) =
    let fun helper(cards, moves, held_cards) =
	    case moves of
		[] => score_fun(held_cards, goal)
	      | (Discard card)::moves' => helper(cards, moves', remove_card(held_cards, card, IllegalMove))
	      | Draw::moves' => case cards of
				    [] => score_fun(held_cards, goal)
				  | card::cards' => if sum_cards_fun(card::held_cards) > goal
						    then score_fun(card::held_cards, goal)
						    else helper(cards', moves', card::held_cards)
    in helper(cards, moves, held_cards)
    end


fun officiate (card_list, move_list, goal) = process(card_list, move_list, [], goal, score, sum_cards)



(* Problem3 a : same behavior except each ace can have a value of 1 or 11 and score_challenger should always return the least (i.e., best) possible score. (Note the game-ends-if-sum-exceeds-goal rule should apply only if there is no sum that is less than or equal to the goal.) *)

(* returns the number of Ace in cards list  *)
fun count_ace (cards_list) =
    let fun aux(cards_list, acc) =
	    case cards_list of
		[] => acc
	      | (_, Ace)::cs' => aux(cs', acc + 1)
	      | _::cs' => aux(cs', acc)
    in aux(cards_list, 0)
    end

(* takes the sum of values of the held-cards and the goal returns the preliminary score *)
fun preliminary_score (sum, goal) = if sum > goal then 3 * (sum - goal) else goal - sum


(* always returns the least possible score. *)
fun score_challenge (cards_list, goal) =
    (* try all possible preliminary scores and choose the least one. *)
    let val sum = sum_cards(cards_list)
	val ace_num = count_ace(cards_list)
	fun least_pre_score(sum, ace_num, least) =
	    if ace_num = 0
	    then least
	    else 
 		let val pre_score =  preliminary_score(sum, goal)
		in if pre_score < least
		   then least_pre_score(sum - 10, ace_num - 1, pre_score)
		   else least_pre_score(sum - 10, ace_num - 1, least)
		end
    in if all_same_color(cards_list)
       then least_pre_score(sum - 10, ace_num, preliminary_score(sum, goal)) div 2
       else least_pre_score(sum - 10, ace_num, preliminary_score(sum, goal))
    end

(* the game-ends-if-sum-exceeds-goal rule should apply only if there is no sum that is less than or equal to the goal.  *)
fun least_sum_cards (cards_list) = sum_cards(cards_list) - 10 * count_ace(cards_list)


(* same process but different ways to caculate sum of cards and score. *)
fun officiate_challenge (card_list, move_list, goal ) = process(card_list, move_list, [], goal, score_challenge, least_sum_cards)

(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test11 = all_except_option ("string", ["string"]) = SOME []
val test12 = all_except_option ("string", ["string", "callee", "hybird"]) = SOME ["callee","hybird"]
val test13 = all_except_option ("string", ["digression", "celery", "string", "callee", "hybird"]) = SOME ["digression","celery","callee","hybird"]
val test14 = all_except_option ("string", ["digression", "celery", "string"]) = SOME ["digression","celery"]
val test15 = all_except_option ("string", ["digression", "hybird"]) = NONE

val test21 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test22 = get_substitutions1 ([[],["there"]], "foo") = []
val test23 = get_substitutions1 ([[]], "foo") = []
val test24 = get_substitutions1 ([["there","Fred","foo","Betty","Freddie"]], "foo") = ["there","Fred","Betty","Freddie"]
val test25 = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]

val test31 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test32 = get_substitutions2 ([[],["there"]], "foo") = []
val test33 = get_substitutions2 ([[]], "foo") = []
val test34 = get_substitutions2 ([["there","Fred","foo","Betty","Freddie"]], "foo") = ["there","Fred","Betty","Freddie"]
val test35 = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
						  
val test41 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
val test42 = similar_names ([[]], {first="Fred", last="Smith", middle="W"}) = [{first="Fred",last="Smith",middle="W"}]
val test43 = similar_names ([], {first="Fred", middle=889, last=56}) = [{first="Fred",last=56,middle=889}]

val test51 = card_color (Clubs, Num 2) = Black
val test52 = card_color (Diamonds, King) = Red 
val test53 = card_color (Hearts, Jack) = Red
val test54 = card_color (Spades, Ace) = Black
val test55 = card_color (Clubs, Num 8) = Black

val test61 = card_value (Clubs, Num 2) = 2
val test62 = card_value (Hearts, Num 9) = 9
val test63 = card_value (Diamonds, Ace) = 11
val test64 = card_value (Diamonds, King) = 10
val test65 = card_value (Spades, Jack) = 10

val test71 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test72 = (remove_card ([], (Hearts, Ace), IllegalMove) handle IllegalMove => [(Spades, Ace)]) = [(Spades, Ace)]
val test73 = remove_card ([(Hearts, Num 2), (Spades, Jack), (Hearts, Ace), (Diamonds, Num 1), (Clubs, Queen)], (Hearts, Ace), IllegalMove) = [(Hearts,Num 2),(Spades,Jack),(Diamonds,Num 1),(Clubs,Queen)]
val test74 = (remove_card ([(Hearts, Num 3), (Spades, Jack)], (Hearts, Ace), IllegalMove) handle IllegalMove => []) = []

val test81 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test82 = all_same_color [(Clubs, Ace), (Hearts, Ace)] = false 
val test83 = all_same_color []= true 
val test84 = all_same_color [(Hearts, Ace)]= true 
val test85 = all_same_color [(Spades, Ace), (Hearts, Jack), (Diamonds, King), (Clubs, Num 8)]= false

val test91 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4 
val test92 = sum_cards [(Clubs, Ace),(Spades, King)] = 21 
val test93 = sum_cards [(Clubs, Ace)] = 11 
val test94 = sum_cards [(Clubs, Ace),(Spades, King),(Diamonds, Num 6)] = 27 
val test95 = sum_cards [] = 0

val test101 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test102 = score ([],10) = 5
val test103 = score ([(Hearts, Ace),(Clubs, Num 4)],6) = 27
val test104 = score ([(Hearts, Ace),(Diamonds, Num 4)],6) = 13
val test105 = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2

val test111 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test121 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test131 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             

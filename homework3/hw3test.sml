(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test20 = longest_string1 ["A","bc","C"] = "bc"
val test21 = longest_string1 ["Ac","bc","Cc"] = "Ac"
val test22 = longest_string1 [] = ""
val test23 = longest_string1 ["map", "why", "partial"] = "partial"

val test30 = longest_string2 ["A","bc","C"] = "bc"
val test31 = longest_string2 ["Ac","bc","Cc"] = "Cc"
val test32 = longest_string2 [] = ""
val test33 = longest_string2 ["map", "why", "partial"] = "partial"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4a1 = longest_string3 ["Ac","bc","Cc"] = "Ac"
val test4a2 = longest_string3 [] = ""
val test4a3 = longest_string3 ["map", "why", "partial"] = "partial"

val test4b = longest_string4 ["A","B","C"] = "C"
val test4b1 = longest_string4 ["Ac","bc","Cc"] = "Cc"
val test4b2 = longest_string4 [] = ""
val test4b3 = longest_string4 ["map", "why", "partial"] = "partial"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test51 = longest_capitalized [] = ""
val test52 = longest_capitalized ["About","Basic","Coffe"] = "About"
val test53 = longest_capitalized ["about","Basic","Coffe"] = "Basic"


val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test71 = (first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3]  handle NoAnswer => 0) = 0
val test72 = (first_answer (fn x => if x > 3 then SOME x else NONE) []  handle NoAnswer => 0) = 0
val test73 = first_answer (fn x => if x > 3 then SOME [x] else NONE) [1,2,3,4,5] = [4]

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test81 = all_answers (fn x => if x > 5 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test82 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7] 
val test83 = all_answers (fn x => if x > 1 then SOME [x, 1] else NONE) [2,3,4] = SOME [2,1,3,1,4,1]

val test9a = count_wildcards Wildcard = 1
val test9a1 = count_wildcards (Variable("a")) = 0
val test9a2 = count_wildcards (TupleP[Variable("x"), Wildcard, ConstructorP("str", Wildcard), TupleP[Wildcard]]) = 3

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b1 = count_wild_and_variable_lengths (TupleP[Variable("x"), Variable("acent"), Wildcard, ConstructorP("str", Wildcard), TupleP[Wildcard]]) = 9
val test9b2 = count_wild_and_variable_lengths (ConstructorP("strings", Wildcard)) = 1
val test9b3 = count_wild_and_variable_lengths (Variable("about")) = 5


val test9c = count_some_var ("x", Variable("x")) = 1
val test9c1 = count_some_var ("summer", Variable("summer")) = 1
val test9c2 = count_some_var ("summer", TupleP[Variable("summer"), Wildcard, ConstructorP("strings", Wildcard)]) = 1
val test9c3 = count_some_var ("strings", TupleP[Variable("summer"), Wildcard, ConstructorP("strings", Wildcard)]) = 0

val test10 = check_pat (Variable("x")) = true
val test101 = check_pat (TupleP[Variable("x"), Variable("x")]) = false
val test102 = check_pat Wildcard = true
val test103 = check_pat (TupleP[Variable("x"), Variable("acent"), Wildcard, ConstructorP("str",  Variable("str"))]) = true 


val test110 = match (Const(1), UnitP) = NONE
val test111 = match (Unit, UnitP) = SOME []
val test112 = match (Const 17, Variable("account")) = SOME [("account",Const 17)]
val test113 = match (Const 1, ConstP 17) = NONE
val test114 = match (Tuple [Const 1, Unit], TupleP[Variable "account", Variable "name", Variable "nickname"]) = NONE
val test115 = match (Tuple [Const 1, Constructor("sunny", Unit), Constructor("sun", Unit)], TupleP[Variable "account", Variable "name", Variable "nickname"]) = SOME  [("account",Const 1),("name",Constructor ("sunny",Unit)),("nickname",Constructor ("sun",Unit))] 
val test116 = match (Constructor("sunny", Unit), ConstructorP("sun", UnitP)) = NONE
val test117 = match (Constructor("sunny", Unit), ConstructorP("sunny", ConstP 1)) = NONE
val test118 = match (Constructor("sunny", Const 17), ConstructorP("sunny", ConstP 1)) = NONE
val test119 = match (Constructor("sunny", Const 1), ConstructorP("sunny", ConstP 1)) = SOME [] 
val test1191 = match (Constructor("sunny", Const 1), ConstructorP("sunny", Variable "nickname")) =  SOME [("nickname",Const 1)] 

val test12 = first_match Unit [UnitP] = SOME []
val test121 = first_match Unit [ConstP 17] = NONE
val test122 = first_match Unit [ConstP 17] = NONE
val test123 = first_match Unit [UnitP, Variable("account"), ConstP 1] = SOME []
val test124 = first_match Unit [Variable("account"), UnitP, ConstP 1] = SOME [("account",Unit)] 

val test130 = typecheck_patterns ([], [ConstP 10, Variable "a"]) = SOME IntT
val test131 = typecheck_patterns ([], [TupleP[Variable("x"), Variable("y")], TupleP[Wildcard, Wildcard]]) = SOME TupleT[Anything, Anything]
val test132 = typecheck_patterns ([], [TupleP[Wildcard, Wildcard], TupleP[Wildcard, TupleP[Wildcard, Wildcard]]]) = SOME TupleT[Anything, TupleT[Anything, Anything]]


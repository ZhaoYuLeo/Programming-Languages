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

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []


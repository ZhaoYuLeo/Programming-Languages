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


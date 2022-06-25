(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test11 = all_except_option ("string", ["string"]) = SOME []
val test12 = all_except_option ("string", ["string", "callee", "hybird"]) = SOME ["callee","hybird"]
val test13 = all_except_option ("string", ["digression", "string", "callee", "hybird"]) = SOME ["digression","callee","hybird"]
val test14 = all_except_option ("string", ["digression", "string"]) = SOME ["digression"]
val test15 = all_except_option ("string", ["digression", "hybird"]) = NONE


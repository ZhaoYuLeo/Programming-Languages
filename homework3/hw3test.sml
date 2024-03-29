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

val typecheck_patterns_0 = typecheck_patterns ([], []) = NONE

val typecheck_patterns_1 = typecheck_patterns ([], [TupleP[Variable("x"), Variable("y")], TupleP[Wildcard, Wildcard]]) = SOME (TupleT[Anything, Anything])

val typecheck_patterns_2 = typecheck_patterns ([], [TupleP[Wildcard, Wildcard], TupleP[Wildcard, TupleP[Wildcard, Wildcard]]]) = SOME (TupleT[Anything, TupleT[Anything, Anything]])

val typecheck_patterns_3 = typecheck_patterns ([], [ConstP 10, Wildcard, ConstP 17, Variable "a"]) = SOME IntT

val typecheck_patterns_4 = typecheck_patterns ([], [ConstP 10, Wildcard, UnitP, Variable "a"]) = NONE 

val typecheck_patterns_5 = typecheck_patterns ([], [ConstP 10, Wildcard, ConstructorP("n", Variable "a")]) = NONE 

val typecheck_patterns_6 = typecheck_patterns ([], [UnitP, Wildcard, Variable "a"]) = SOME UnitT 

val typecheck_patterns_7 = typecheck_patterns ([], [TupleP[Wildcard], TupleP[Wildcard, Wildcard]]) = NONE 

val typecheck_patterns_8 = typecheck_patterns ([], [TupleP[ConstP 1], TupleP[UnitP]]) = NONE 

val typecheck_patterns_9 = typecheck_patterns ([], [ConstructorP("n", Variable "a")]) = NONE 

val typecheck_patterns_11 = typecheck_patterns ([("n", "t", TupleT[IntT, Anything])], [ConstructorP("n", TupleP[ConstP 1, Variable "a"])]) = SOME (Datatype "t")



datatype color = Red | Green | Blue

fun b(x) =
   case x of
       (10) => 1
      | a => 3

val t1 = typecheck_patterns([], [ConstP 10, Variable "a"]) = SOME IntT
(*
fun b(x) =
   case x of
      (10) => 1
      | SOME x => 3
      | a => 3
*)
val t2 = typecheck_patterns([("SOME","option",Anything), ("NONE","option",UnitT)], [ConstP 10, Variable "a", ConstructorP("SOME", Variable "x")]) = NONE

fun c(x) =
    case x of
        (a, 10, _) => 1
      | (b, _, 11) => 2
      | _ => 3
		 
val t3 = typecheck_patterns([], [TupleP[Variable "a", ConstP 10, Wildcard], TupleP[Variable "b", Wildcard, ConstP 11], Wildcard]) = SOME (TupleT[Anything, IntT, IntT])

fun f(x) =
   case x of
     Red => 0
     | _ => 1

val t4 = typecheck_patterns([("Red", "color", UnitT), ("Green", "color", UnitT), ("Blue", "color", UnitT)], [ConstructorP("Red", UnitP), Wildcard]) = SOME (Datatype "color")			  


datatype auto =  Sedan of color | Truck of int * color | SUV
	     
fun g(x) = 
   case x of
        Sedan(a) => 1
      | Truck(b,_) => 2
      | _ => 3

val t5 = typecheck_patterns([("Sedan","auto", Datatype "color"),("Truck","auto",TupleT[IntT, Datatype "color"]),("SUV","auto",UnitT)], [ConstructorP("Sedan", Variable "a"), ConstructorP("Truck", TupleP[Variable "b", Wildcard]), Wildcard]) = SOME (Datatype "auto")

datatype 'a list = Empty | List of 'a * 'a list
					   
fun j(x) = 
   case x of
       Empty => 0
     | List(10,Empty) => 1 
     | _ => 3

val t6 = typecheck_patterns([("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])], [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[ConstP 10, ConstructorP("Empty",UnitP)]), Wildcard]) = SOME (Datatype "list")

fun h(x) = 
   case x of
      Empty => 0
    | List(k,_) => 1

val t7 = typecheck_patterns([("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])], [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[Variable "k", Wildcard])]) = SOME (Datatype "list")

fun g(x) = 
   case x of
      Empty => 0
    | List(Sedan(c),_) => 1

val t8 = typecheck_patterns([("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"]),("Sedan","auto", Datatype "color")], [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[ConstructorP("Sedan", Variable "c"), Wildcard])]) = SOME (Datatype "list")

																															    

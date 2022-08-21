type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

(* Problem1 : pass if the grade >= 75 *)
fun pass_or_fail {grade=grade, id=id} =   
    case grade of
	SOME i => if i >= 75 then pass else fail
     | _ => fail

(* Problem2 : true if the grade >= 75 *)
fun has_passed g = (pass_or_fail g = pass)

(* Problem3 : how many list elements have passing (>=75) *)      
fun number_passed gs =
    let fun helper (gs, acc) =
	    case gs of
		[] => acc
	      | x::xs' => helper (xs', if has_passed x then acc + 1 else acc)
    in helper (gs, 0)
    end

(* Problem4 : how many list elements are mislabeled. the first element of the tuple is the label *)
fun number_misgraded labeled_grade =
    let fun helper (lgs, acc) =
	    case lgs of
		[] => acc
	      | (l, g)::xs' => helper (xs', if pass_or_fail g <> l then acc + 1 else acc)
    in helper (labeled_grade, 0)
    end
	
val s1 = {grade = SOME 78, id = 1}
val s2 = {grade = SOME 89, id = 2}
val s3 = {grade = SOME 69, id = 2}
val test1 = pass_or_fail s1 = pass
val test2 = has_passed s1 = true
val test3 = number_passed ([s1, s2, s3]) = 2
val test4 = number_misgraded ([(fail, s1), (pass, s2), (pass, s3)]) = 2


datatype 'a tree = leaf
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

(* Problem5 : returns height of the tree which is the length of the longeset path to a leaf *)
fun tree_height t =
    case t of
	leaf => 0
      | node {value=v, left=lt, right=rt} => Int.max(tree_height lt, tree_height rt) + 1
											   
(* Problem6 : returns the sum of all values in the nodes of an int tree *)
(* how to iterate the tree and sum them up *)			   
fun sum_tree t =
    case t of
	leaf => 0
      | node {value=v, left=lt, right=rt} => v + sum_tree lt + sum_tree rt
	 
(* Problem7 : if the value in the node is prune_me replace it with leaf *)
fun gardener t =
    case t of
	leaf => leaf
      | node {value=v, left=lt, right=rt} => if v = prune_me
					     then leaf
				             else node {value=v, left=lt, right=rt}
		    
val t1 = node {value=1, left=leaf, right=leaf}
val t2 = node {value=2, left=t1, right=leaf}
val t3 = node {value=2, left=t2, right=t1}
val t4 = node {value=prune_me, left=leaf, right=leaf}
val test5 = tree_height t3 = 3
val test6 = sum_tree t3 = 6
val test7 = gardener t4 = leaf

(* Problem8 : Re-implement various functions provided in the SML standard libraries for lists and options. *)
(* Problem8a : returns the last element of l, raise Empty if l is nil *)
fun relast lst =
    case lst of
	[] => raise List.Empty
      | x::[] => x
      | x::xs' => relast xs'

(* Problem8b : returns the first i elements of the list l. It raises Subscript if i < 0 or i > length l. We have take(l, length l) = l. *)
(* keeping the length of list when initializing might be good *)		 
fun retake (lst, i) =
    let fun helper (lst, i) =
	    if i = 0
	    then []
	    else case lst of
		     (* if i can use length, i can move exn outer the helper function and write stop condition inside the branch of case expression which looks more normal. *)
		     [] => raise Subscript
		  | x::xs' => x::helper(xs', i - 1)
				   
    in if i < 0
       then raise Subscript
       else helper (lst, i)
    end
			 
(* Problem8c : returns what is left after dropping the first i elements if the list l. It raises Subscript if i < 0 or i > length. It holds that take(l, i) @ drop(l, i) = l when 0 <= i <= length l. We  *)
fun redrop (lst, i) =
    let fun helper (lst, i) =
	    case lst of
		[] => []
	      | x::xs' => if i = 0 then x::xs' else helper(xs', i - 1)
    in if i < 0 orelse i > (length lst) then raise Subscript else helper(lst, i)
    end


(* Problem8d : returns the list that is the concatenation of all the lists in l in order. concat[l1, l2,...,ln] = l1 @ l2 @ ... @ln *)
fun reconcat lst =
    case lst of
	[] => []
      | x::[] => x
      | x::xs' => x @ reconcat xs'

(* Problem8e : returns v if opt is SOME v; otherwise it returns a *)
fun regetOpt (opt, a) =
    case opt of
	NONE => a
     |  SOME v => v

(* Problem8f : maps NONE to NONE and SOME(v) to v *)
fun rejoin optopt =
    case optopt of
	NONE => NONE
      | SOME v => v	
			       
val test8_a0 = relast [1,2,3] = 3
val test8_a1 = (relast [] handle Empty => 1) = 1

val test8_b0 = retake ([1,2,3], 0) = []
val test8_b1 = retake ([1,2,3], 2) = [1,2]
val test8_b2 = retake ([1,2,3], 3) = [1,2,3]		       
val test8_b3 = (retake ([1,2,3], ~1) handle Subscript => []) = []
val test8_b4 = (retake ([1,2,3], 4) handle Subscript => []) = []
			   
val test8_c0 = redrop ([1,2,3], 0) = [1,2,3]
val test8_c1 = redrop ([1,2,3], 2) = [3]
val test8_c2 = redrop ([1,2,3], 3) = []
val test8_c3 = (redrop ([1,2,3], ~1) handle Subscript => []) = []
val test8_c4 = (redrop ([1,2,3], 4) handle Subscript => []) = []						val test8_c5 = retake ([1,2,3,4,5,6], 3) @ redrop ([1,2,3,4,5,6], 3) = [1,2,3,4,5,6]	   

val test8_d0 = reconcat [[1,2], [3,4], [5,6]] = [1,2,3,4,5,6]
val test8_d1 = reconcat [["s"], ["t"], ["r"]] = ["s", "t", "r"]
val test8_d2 = reconcat [[],[],[]] = []						    

val test8_e0 = regetOpt (SOME 1, 8) = 1
val test8_e1 = regetOpt (NONE, 8) = 8
					
val test8_f0 = rejoin (SOME NONE) = NONE
val test8_f1 = rejoin (SOME (SOME 9)) = SOME 9
val test8_f2 = rejoin NONE = NONE
				   

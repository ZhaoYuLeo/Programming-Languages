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

(* Problem5 : return height of the tree which is the length of the longeset path to a leaf *)
fun tree_height t =
    case t of
	leaf => 0
      | node {value=v, left=lt, right=rt} => Int.max(tree_height lt, tree_height rt) + 1
											   
(* Problem6 : return the sum of all values in the nodes of an int tree *)
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

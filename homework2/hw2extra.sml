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
val test8_c4 = (redrop ([1,2,3], 4) handle Subscript => []) = []
val test8_c5 = retake ([1,2,3,4,5,6], 3) @ redrop ([1,2,3,4,5,6], 3) = [1,2,3,4,5,6]
									   
val test8_d0 = reconcat [[1,2], [3,4], [5,6]] = [1,2,3,4,5,6]
val test8_d1 = reconcat [["s"], ["t"], ["r"]] = ["s", "t", "r"]
val test8_d2 = reconcat [[],[],[]] = []						    

val test8_e0 = regetOpt (SOME 1, 8) = 1
val test8_e1 = regetOpt (NONE, 8) = 8
					
val test8_f0 = rejoin (SOME NONE) = NONE
val test8_f1 = rejoin (SOME (SOME 9)) = SOME 9
val test8_f2 = rejoin NONE = NONE

				 
datatype nat = ZERO | SUCC of nat

(* Problem9 : returns whether the given natural number is positive or zero *)
fun is_positive n =
    case n of
	ZERO => false
      | SUCC _ => true
	      
(* Problem10 : returns the predecessor of the given natural number n; Throw an exception Negative if n is ZERO. *)
exception Negative
	      
fun pred n =
    case n of
	ZERO => raise Negative
      | SUCC pre => pre

(* Problem11 : returns the corresponding int of the given natural number n *)
fun nat_to_int n =
    let fun helper (acc, n) =
	    case n of
		ZERO => acc
	      | SUCC pre => helper (acc + 1, pre)
    in helper(0, n)
    end
	
(* Problem12 : returns the corresponding natural number of the given integer, or throws a Negative exception if the integer was negative *)
fun int_to_nat i =
    let fun helper (acc, i) =
	    if i < 0
	    then raise Negative
	    else if i = 0
	    then acc
	    else helper (SUCC acc, i - 1)
    in helper(ZERO, i)
    end
	
(* Problem13: perform addition *)
fun add (n1, n2) =
    let fun helper (acc, n1) =
	    case n1 of
		ZERO => acc
	      | SUCC pre => helper (SUCC acc, pre)
    in helper(n2, n1)
    end

(* Problem14 : perform substraction. assume n1 is always bigger than n2. throws a Negative exception when n1 is less than n2 which is actually implemented in pred function. *)
fun sub (n1, n2) =
    let fun helper (acc, n2) =
	    case n2 of
		ZERO => acc
	      | SUCC pre => helper (pred acc, pre)
    in helper (n1, n2)
    end

(* Problem15 : perform multiplication. perform better if n1 is the less one. *)
fun mult (n1, n2) =
    let fun helper (acc, n1) =
	    case n1 of
		ZERO => acc
	      | SUCC pre => helper (add(acc, n2), pre)
    in helper (ZERO, n1)
    end

(* Problem16 : returns true when the first argument is less than the second, otherwise false *)
fun less_than (n1, n2) =
    case (n1, n2) of
	(ZERO, SUCC _) => true
      | (SUCC _, ZERO) => false
      | (ZERO, ZERO) => false
      | (SUCC pre_n1, SUCC pre_n2) => less_than (pre_n1, pre_n2)
							    
val n1 = SUCC ZERO
val n2 = SUCC n1
val n3 = SUCC n2
val n4 = SUCC n3
val test9_0 = is_positive ZERO = false
val test9_1 = is_positive (SUCC (SUCC ZERO)) = true

val test10_0 = pred (SUCC (SUCC ZERO)) = (SUCC ZERO)
val test10_1 = (pred ZERO handle Negative => ZERO) = ZERO
						      
val test11_0 = nat_to_int ZERO = 0
val test11_1 = nat_to_int (SUCC (SUCC n4)) = 6
						 
val test12_0 = int_to_nat 0 = ZERO
val test12_1 = int_to_nat 3 = n3
val test12_2 = (int_to_nat ~1 handle Negative => ZERO) = ZERO
							     
val test13_0 = add (ZERO, n3) = n3
val test13_1 = add (n1, n2) = n3
val test13_2 = add (n2, n2) = n4
				  
val test14_0 = sub (n4, n1) = n3
val test14_1 = sub (n2, n2) = ZERO
val test14_2 = sub (n4, n2) = n2
val test14_3 = (sub (n1, n4) handle Negative => ZERO) = ZERO
							    
val test15_0 = mult (ZERO, n4) = ZERO
val test15_1 = mult (n1, n4) = n4
val test15_2 = mult (n2, n2) = n4
				   
val test16_0 = less_than (ZERO, n1) = true
val test16_1 = less_than (n2, n4) = true
val test16_2 = less_than (n3, n1) = false
val test16_3 = less_than (n2, n2) = false

datatype intSet =
  Elems of int list (*list of integers, possibly with duplicates to be ignored*)
| Range of { from : int, to : int }  (* integers from one number to another *)
| Union of intSet * intSet (* union of the two sets *)
| Intersection of intSet * intSet (* intersection of the two sets *)

			       
(* Problem18 : returns whether the set contains a certain element or not *)
fun contains (s, i) =
    case s of
	Elems lst => List.exists (fn x => x = i) lst
     | Range {from, to} => i <= to andalso i >= from
     | Union (s1, s2) => contains (s1, i) orelse contains (s2, i)
     | Intersection (s1, s2) => contains (s1, i) andalso contains (s2, i)

(* Problem19 : returns a list with the set's elements, without deplicates *)
(* lst1 and lst2 are both ordered. returns new ordered list contains all elements in lst1 and lst2 *)
fun merge (lst1, lst2) =
    case (lst1, lst2) of
	([], xs2) => xs2
      | (xs1, []) => xs1
      | (x1::xs1', x2::xs2') => if x1 < x2
				then x1::merge (xs1', x2::xs2')
				else x2::merge (x1::xs1', xs2')

(* mergesort bottom-up *)
fun mergeSort lst =
    let fun sortAdjacency lst =
	    case lst of
		[] => []
	      | x::[] => [x]
	      | x1::x2::xs' => (merge (x1, x2))::(sortAdjacency xs')
	fun repeat lst =
	    case lst of
		[] => []
	      | x::[] => x
	      | xs => repeat(sortAdjacency xs)
    in repeat(List.map (fn x => [x]) lst)
    end

(* returns a list without duplicates and well ordered *)
fun deDuplicateOrdered lst =
    let fun helper (lst) =
	    case lst of
		[] => []
	      | x::[] => [x]
	      | x1::x2::xs' => if x1 = x2
			       then helper(x2::xs')
			       else x1::helper(x2::xs')
    in helper (mergeSort lst)
    end
	
val de_duplicate_ordered_test = deDuplicateOrdered [1,1,6,9,6,7] = [1,6,7,9]
								       
(* returns a list without duplicates and kept the original order from left to right *)
fun deDuplicateL lst =
    let fun helper (lst, acc) =
	    case lst of
		[] => acc
	      | x::xs' => case (List.find (fn i => i = x ) acc) of
			      NONE => helper (xs', acc @ [x])
			    | SOME _ => helper (xs', acc)
    in helper (lst, [])
    end

val de_duplicate_l_test = deDuplicateL [1,1,6,9,6,7] = [1,6,9,7]
	
(* returns a list without duplicates and kept the original order from right to left *)
fun deDuplicateR lst =
    case lst of
	[] => []
      | x::xs' => let val res = deDuplicateR xs'
		  in case (List.find (fn i => i = x ) res) of
			 NONE => x::res
		       | SOME _ => res
		  end

val de_duplicate_r_test = deDuplicateR [1,1,6,9,6,7] = [1,9,6,7]
						 
(* returns list which contains integer from m to  (including n particularly). If n is less than m, returns []. *)
fun range (m, n) =
    if m > n
    then []
    else m::range(m+1, n)

(* return the union of lst1 and lst2, kind of like orelse. which kind of order is needed is really a problem。 maybe we just don‘t care about order. we just need same elements which create the set. *) 
fun union (lst1, lst2) = deDuplicateOrdered (lst1 @ lst2)

(* return the intersection of lst1 and lst2, kind of like andalso *)
fun intersection (lst1, lst2) =
    let
	fun helper (xs1, xs2, acc) =
	    case (xs1, xs2) of
		([], _) => acc
	      | (_, []) => acc
	      | (x1::xs1', x2::xs2') => helper (if x1 < x2
						then (xs1', x2::xs2', acc)
						else if x1 > x2
						then  (x1::xs1', xs2', acc)
						else (xs1', xs2', x1::acc))
    in helper (deDuplicateOrdered lst1,  deDuplicateOrdered lst2, [])
    end

val intersection_test = intersection ([6,5,3,7], [8,7,2,5,3]) = [7,5,3]

(* returns a list with the set's elements, without deplicate *)
fun toList s =
    case s of
	Elems lst => deDuplicateOrdered lst
      | Range {from, to} => range(from, to)
      | Union (s1, s2) => union(toList s1, toList s2)
      | Intersection (s1, s2) => intersection(toList s1, toList s2)

(* Problem17 : determines if the set is empty or not *)
fun isEmpty s =
    case s of
	Elems [] => true
      | Elems _ => false
      | Range {from, to} => from > to (* include to *)
      | Union (s1, s2) => isEmpty s1 andalso isEmpty s2					     
      | Intersection (s1, s2) => isEmpty s1 orelse isEmpty s2 orelse intersection(toList s1, toList s2) = [] (* O? *)

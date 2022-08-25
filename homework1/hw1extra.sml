(* Problem1 : takes a list of numbers and adds them with alternating sign. alternate [1,2,3,4] = 1 - 2 + 3 - 4 = -2 *)
fun alternate(numbers : int list) =
    if null numbers
    then 0
    else hd numbers - alternate(tl numbers)

val test1_0 = alternate [1,2,3,4] = ~2
val test1_1 = alternate [1,2,3] = 2			      
				      
(* Problem2 : takes a non-empty list of numbers, and returns a pair(min, max) of the minimum and maximum of the numbers in the list *)
fun min_max(numbers : int list) =
    case numbers of
	[] => raise Empty
      | [n] => (n, n)
      | n::nb' => let val (min, max) = min_max(nb')
		  in
		      if n < min
		      then (n, max)
		      else if n > max
		      then (min, n)
		      else (min, max)
		  end
		      
val test2_0 = min_max [4,3,6,1,9] = (1, 9)					

(* Problem3 : takes a list of numbers and returns a list of the partial sums of those numbers *)
(* why we need hd, useful for passing as arguments to other functions *)
fun cumsum numbers =
    let fun helper (numbers, sum) =
	    case numbers of
		[] => []
	      | x::xs' => (x + sum)::helper(xs', x + sum)
    in helper(numbers, 0)
    end	

val test3_0 = cumsum [1,4,20] = [1,5,25]
val test3_1 = cumsum [] = []

(* Problem4 : given a string option SOME name returns the string "Hello there, ...!" where the dots would be replaced by name. If it is NONE then replace the dots with "you" *)
fun greeting s = concat["Hello there, ", case s of NONE => "you" | SOME n => n, "!"]

val test4_0 = greeting NONE = "Hello there, you!"
val test4_1 = greeting (SOME "Alice") = "Hello there, Alice!"
val test4_2 = greeting (SOME "Wilson") = "Hello there, Wilson!"

(* Problem5 : given a list of integers and another list of nonnegative integers, repeats the integers in the first list according to the numbers indicated by the second list. *)
fun repeat (intsList, times) =
    case (intsList, times) of
	(_, []) => []
      | ([], _) => []
      | (x::xs', t::ts') => if t > 0
			    then x::repeat (x::xs', (t - 1)::ts')
			    else repeat (xs', ts')

val test5_0 = repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]
val test5_1 = repeat ([1,2], [4,0,3]) = [1,1,1,1]
val test5_2 = repeat ([1], [0]) = []

(* Problem6 : given two "optional" integers, returns SOME of their sum, or returns NONE if at least one of the two arguments is NONE *)
fun addOpt (x1, x2) =
    case (x1, x2) of
	(SOME v1, SOME v2) => SOME(v1 + v2)
      | _ => NONE
					  
val test6_0 = addOpt (NONE, SOME 1) = NONE
val test6_1 = addOpt (SOME 1, NONE) = NONE
val test6_2 = addOpt (SOME 5, SOME 10) = SOME 15
					      
(* Problem7 : given a list of "optional" integers, add those integers that are there. if the list does not contain any SOME is in it or the list is empty, the function should return NONE. *)
fun addAllOpt xs =
    case xs of
	[] => NONE
      | NONE::xs' => addAllOpt xs'
      | (SOME x)::xs' => case addAllOpt xs' of
			     NONE => SOME x
			   | SOME v => SOME (x + v)

val test7_0 = addAllOpt ([SOME 1, NONE, SOME 3]) = SOME 4
val test7_1 = addAllOpt ([SOME 1, SOME 2, SOME 3]) = SOME 6
val test7_2 = addAllOpt ([SOME 1, NONE, NONE]) = SOME 1
val test7_3 = addAllOpt ([NONE, NONE, NONE]) = NONE
val test7_4 = addAllOpt ([]) = NONE
				   
(* Problem8 : given a list of booleans returns true if there is at least one of them that is true, otherwise returns false. return false if the list is empty *)
fun any xs =
    case xs of
	[] => false
      | true::xs' => true
      | false::xs' => any xs'

val test8_0 = any [] = false
val test8_1 = any [false] = false
val test8_2 = any [false, false, false, true] = true
val test8_3 = any [false, true, true, false] = true

(* Problem9 : given a list of booleans returns true if all of them true, otherwise return false. If the list is empty return true because there is no false *)
fun all xs =
    case xs of
	[] => true
      | false::xs' => false
      | true::xs' => all xs'

val test9_0 = all [] = true
val test9_1 = all [true, true, true] = true
val test9_2 = all [true, true, false] = false
val test9_3 = all [false, false, true] = false
val test9_4 = all [false, false, false] = false			   
    
(* Problem10 : given two lists of integers creates consecutive pairs, and stops when one of the lists is empty *)
fun zip (xs1, xs2) =
    case (xs1, xs2) of
	([], _) => []
      | (_, []) => []
      | (x1::xs1', x2::xs2') => (x1, x2)::zip(xs1', xs2')

val test10_0 = zip ([1,2,3], [4,6]) = [(1,4), (2,6)]
val test10_1 = zip ([], []) = []
val test10_2 = zip ([1,2], [3,4,5]) = [(1,3), (2,4)]
val test10_3 = zip ([1,2], ["anny", "bob"]) = [(1, "anny"), (2, "bob")]
						 
					 
(* Problem11 : similar to zip except when one list is empty it starts recycling from its start until the other list completes. *)
fun zipRecycle (lst1, lst2) =
    let fun helper (xs1, xs2) =
	    case (xs1, xs2) of
		(_, []) => []
	      | ([], _) => []
	      | (x1::[], x2::xs2') => (x1, x2)::helper(lst1, xs2')
	      | (x1::xs1', x2::xs2') => (x1, x2)::helper(xs1', xs2')
    in helper(lst1, lst2)
    end
	
val test11_0 = zipRecycle ([1,2,3], [1,2,3,4,5,6,7]) = [(1,1), (2,2), (3,3), (1,4), (2,5), (3,6), (1,7)]
val test11_1 = zipRecycle ([], [1,2,3]) = []
val test11_2 = zipRecycle ([1,2,3], []) = []
val test11_3 = zipRecycle ([1,2], ["a", "b", "c", "d", "e", "f"]) = [(1,"a"),(2,"b"),(1,"c"),(2,"d"),(1,"e"),(2,"f")]
									
	       
(* Problem12 : similar with zip. Returns type (int * int) list option. Return SOME of a list when the original lists have the same lenght, and NONE if they do not. *)									
fun zipOpt (xs1, xs2) =
    case (xs1, xs2) of
	([], []) => SOME []
      | ([], _) => NONE
      | (_, []) => NONE
      | (x1::xs1', x2::xs2') => let val res = zipOpt(xs1', xs2')
				in case res of
				       NONE => NONE
				     | SOME v => SOME ((x1, x2)::v)
				end
		       
val test12_0 = zipOpt ([], []) = SOME []
val test12_1 = zipOpt ([1,2,3], ["a", "b", "c"]) = SOME [(1,"a"),(2,"b"),(3,"c")]
val test12_2 = zipOpt ([1,2,3], [1,2]) = NONE
val test12_3 = zipOpt ([1,2], [1,2,3]) = NONE
					     
(* Problem13 : takes a list of pair(s, i) and a string s2, then goes through the list of pairs looking for the string s2 in the first component. If it finds a match with corresponding number i, then it returns SOME i. If it does not, it returns NONE. *)
fun lookup (pairList, s2) =
    case pairList of
	[] => NONE
      | (s, i)::xs' => if s = s2
		       then SOME i
		       else lookup (xs', s2)

val test13_0 = lookup ([], "a") = NONE
val test13_1 = lookup ([("a", 1), ("b", 3), ("c", 8)], "b") = SOME 3
val test13_2 = lookup ([("a", 5), ("b", 7)], "c") = NONE
val test13_3 = lookup ([("a", 3), ("b", 7), ("a", 13), ("c", 8)], "a") = SOME 3
									      
	
(* Problem14 : given a list of integers create two lists of integers, one containing the non-negative entries, the other containing the negative entries. Relative order must be preserved: All non-negative entries must appear in the same order in which they were on the original list, and similarly for the negative entries. *)									     
fun splitup xs =
    case xs of
	[] => ([], [])
      | x::xs' => let val (non_negatives, negtives) = splitup xs'
		  in if x < 0
		     then (non_negatives, x::negtives)
		     else (x::non_negatives, negtives)
		  end

val test14_0 = splitup [~1, 9, ~4, 0, 1, ~1] = ([9,0,1], [~1,~4,~1])
val test14_1 = splitup [0,0,3,1] = ([0,0,3,1], [])
val test14_2 = splitup [~1,~3,~1,~0] = ([0], [~1,~3,~1])
val test14_3 = splitup [] = ([],[])
				
(* Problem15 : takes an extra "threshold" parameter, and uses that instead of 0 as the separating point for the two resulting lists. *)
fun split (f, xs) =
    case xs of
	[] => ([], [])
      | x::xs' => let val (nosuit, suit) = split (f, xs')
		  in if f(x)
		     then (nosuit, x::suit)
		     else (x::nosuit, suit)
		  end
		      
		  
fun splitAt (xs, x) = split ((fn i => i < x), xs)

val test15_0 = splitAt ([~1, 9, ~4, 0, 1, ~1], 1) = ([9,1], [~1,~4,0,~1])
val test15_1 = splitAt ([0,0,3,1], 0) = ([0,0,3,1], [])
val test15_2 = splitAt ([~1,~3,~1,~0], ~1) = ([~1,~1,0], [~3])
val test15_3 = splitAt ([], 3) = ([],[])

(* Problem16 : given a list of integers determines whether the list is sorted in increasing order *)
fun isSorted xs =
    case xs of
	[] => true
      | x::[] => true
      | x1::x2::xs' => (x1 <= x2) andalso isSorted (x2::xs')

val test16_0 = isSorted [1,2,3,4] = true
val test16_1 = isSorted [] = true
val test16_2 = isSorted [1,2,3,5,4] = false
val test16_3 = isSorted [2,1,3,5,9] = false
val test16_4 = isSorted [1,1,2,2] = true

(* Problem17 : given a list of integers determines whether the list is sorted in either increasing or decreasing order *)
fun isAnySorted xs =
    case xs of
	[] => true
      | x::[] => true
      | x1::x2::[] => true
      | x1::x2::x3::xs' => ((x1 <= x2 andalso x2 <= x3) orelse (x1 >= x2 andalso x2 >= x3)) andalso isAnySorted (x2::x3::xs') (* no reverse pair *)

val test17_0 = isAnySorted [] = true
val test17_1 = isAnySorted [1] = true
val test17_2 = isAnySorted [1,2,3] = true
val test17_3 = isAnySorted [3,2,1] = true
val test17_4 = isAnySorted [1,3,2] = false
	
(* Problem18 : takes two lists of integers that are each sorted from smallest to largest, and merges them into one sorted list. *)
fun sortedMerge (xs1, xs2) =
    case (xs1, xs2) of
	([], []) => []
      | (_, []) => xs1
      | ([], _) => xs2
      | (x1::xs1', x2::xs2') => if x1 < x2
				then x1::sortedMerge (xs1', xs2)
				else x2::sortedMerge (xs1, xs2')
				    
val test18_0 = sortedMerge ([1,4,7], [5,8,9]) = [1,4,5,7,8,9]
val test18_1 = sortedMerge ([], []) = []
val test18_2 = sortedMerge ([1,3,6], [2]) = [1,2,3,6]
val test18_3 = sortedMerge ([8], [0,4,7,10]) = [0,4,7,8,10]
						   
					       
(* Problem19 : takes the first element out, and uses it as the "threshold" for splitAt. It then recursively sorts the two lists produced by splitAt. Finally it brings the two lists together. quicksort, split by the first element. *)
fun qsort xs =
    case xs of
	[] => []
      | x::xs' => let val (big, small) = splitAt (xs', x)
		  in (qsort small) @ (x::(qsort big))
		  end

val test19_0 = qsort ([]) = []
val test19_1 = qsort ([2,1,3,4,9,0]) = [0,1,2,3,4,9]
val test19_2 = qsort [9,8,7,6,5,4,3,2] = [2,3,4,5,6,7,8,9]
					     
(* Problem20 : takes a list of integers and produces two lists by alternating elements between the two lists. *)
fun divide xs =
    case xs of
	[] => ([], [])
      | x::[] => ([x], [])
      | x1::x2::xs' => let val (odd, even) = divide xs'
		       in (x1::odd, x2::even)
		       end

fun divide2 (xs) =
    let fun helper (xs) =
	    case xs of
		[] => ([],[])
	      | x::xs' => let val (odd, even) = divide2 (xs')
			  in (odd, x::even)
			  end
    in case xs of
	   [] => ([],[])
	 | x::xs' => let val (odd, even) = helper (xs')
		     in (x::odd, even)
		     end
    end
				
val test20_0 = divide ([1,2,3,4,5,6,7]) = ([1,3,5,7], [2,4,6])
val test20_1 = divide ([]) = ([],[])
val test20_2 = divide ([1]) = ([1],[])				  
val test20_3 = divide2 ([1,2,3,4,5,6,7]) = ([1,3,5,7], [2,4,6])
val test20_4 = divide2 ([6]) = ([6],[])
				   
(* Problem21 : given the initial list of integers, splits it in two lists using divide, then recursively sorts those two lists, then merges them together with sortedMerge. mergesort, using the index to divide. *)
fun not_so_quick_sort xs =
    case (divide xs) of
	([], x) => x
      | (x, []) => x
      |	(odd, even) => sortedMerge(not_so_quick_sort odd, not_so_quick_sort even)

val test21_0 = not_so_quick_sort ([9,8,7,6,5,4,3,2,1]) = [1,2,3,4,5,6,7,8,9]
val test21_1 = not_so_quick_sort ([]) = []
val test21_2 = not_so_quick_sort ([7,8,4,4,9,1,5,0,6]) = [0,1,4,4,5,6,7,8,9]
	
(* Problem22 : given two numbers k and n it attempts to evenly divide k into n as many times as possible, and returns a pair(d, n2) where d is the number of times while n2 is th resulting n after all those divisions. *)
fun fullDivide (n, k) =
    if k = 0
    then (0,0)
    else if n = 0 orelse k mod n <> 0 
    then (0, k)
    else let val (a, b) = fullDivide (n, k div n)
	 in (a + 1, b)
	 end

val test22_0 = fullDivide (2, 40) = (3,5)
val test22_1 = fullDivide (3, 10) = (0,10) (* 3 does not divide 10 *)
val test22_2 = fullDivide (3, 18) = (2,2)					
val test22_3 = fullDivide (10, 0) = (0,0) (* 0 can never be divided into 10 *)
val test22_4 = fullDivide (0, 10) = (0,10)
				       
(* Problem23 : given a number n returns a list of pairs (d, k) where d is a prime number dividing n and k is the number of times it fits. The pairs should be in increasing order of prime factor, and the process should stop when the divisor considered surpasses the square root of n. *) 
fun factorize n =
    let fun helper (k, n) =
	    if n = 1
	    then []
	    else if k * k > n
	    then [(n, 1)] (* if n can be divided into some numbers, those numbers cannot be all larger than the square root of n. each time we do fullDivide, we get the smaller factor. when we reach this branch, it means that no smaller factor for n has been found before, otherwise n would not exist. *)
	    else case fullDivide (k, n) of
		     (0, x) => helper (k + 1, n)
		   | (a, 1) => [(k, a)]
		   | (a, b) => (k, a)::(helper (k + 1, b)) (* no need to keep k prime. n is not divisible by k while k has one factor and n has lose it. *)
    in helper (2, n)
    end
	
	
val test23_0 = factorize 20 = [(2,2), (5,1)]
val test23_1 = factorize 36 = [(2,2), (3,2)]
val test23_2 = factorize 1 = []
val test23_3 = factorize 35 = [(5,1), (7,1)]
val test23_4 = factorize 49 = [(7,2)]
				 
(* Problem24 : given a factorization of a number n as described in the previous problem computes back the number n. So this should do the opposite of factorize *)
fun multiply n =
    let fun helper (n, acc) =
	    case n of
		[] => acc
	      | (d, k)::xs' => helper (if k < 0
				       then raise Empty (* or something else *)
				       else if k = 0
				       then (xs', acc)
				       else ((d, k - 1)::xs', d * acc))
						
    in helper (n, 1)
    end

val test24_0 = multiply [] = 1
val test24_1 = multiply [(2,2), (3,2)] = 36
val test24_2 = multiply [(2,2), (5,1)] = 20
					     
    
(* Problem25 : given a factorization list result from factorize creats a list all of possible products produced from using some or all of those prime factors no more than the number of times they are available. This should end up being a list of all the divisors of the number n that gave rise to the list. For extra challenge, you recursive process should return the numbers in this order, as opposed to sorting them afterwards. *)
fun all_products factors =
    let val number = multiply factors
	val min = case factors of
		      [] => 1
		    | (min, _)::xs' => min
	val max = number div min
	fun helper (d, acc) =
	    if d = min
	    then d::acc
	    else helper (d - 1, if number mod d = 0 then d::acc else acc)
			      
    in if min = 1 then [1] else 1::helper(max - 1, [max, number])
    end	
    
val test25_0 = all_products ([(2,2), (5,1)]) = [1,2,4,5,10,20] 
val test25_1 = all_products ([(2,2), (3,2)]) = [1,2,3,4,6,9,12,18,36]
val test25_2 = all_products ([(2,3), (3,2), (5,2), (31,2)])
val test25_3 = all_products ([]) = [1]
val test25_4 = all_products ([(2,3), (5,1)]) = [1,2,4,5,8,10,20,40]
						   

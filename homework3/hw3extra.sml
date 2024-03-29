(* Problems1 : compose two functions with "optional" values. if either function returns NONE, then the result is NONE *)
fun compose_opt f g x =
    case g x of
	   NONE => NONE
	| SOME v => f v



(* Problems2 : apply f to x and f again to that result and so on until p x is false *)
fun do_until f p x =
    if p x
    then do_until f p (f(x))
    else x
	     
val remove_factor_two  = do_until (fn x => x div 2) (fn x => x mod 2 <> 1)



(* Problems3 : differece between x/2/2.../2 and x(x - 1)(x - 2 )...1 *)
(* do_until is kind of like iteration. i have to use acc to store the result instead of stack. recursion makes a lot convenience for traversing tree structures *)
fun factorial_c n = #1 (do_until (fn (acc, n) => (acc * n, n - 1)) (fn (acc, n) => n <> 0 ) (1, n))

fun factorial n =
    if n = 0
    then 1
    else n * factorial (n - 1)



(* Problems4 : apply f to x until f x = x. iteration is a special type of recursion. *)
fun fixed_point f x = do_until f (fn x => (f x) <> x) x



(* Problems5 : apply f to each value in pair and returns another pair *)
fun map2 f (a1, a2) = (f a1, f a2)



(* Problems6 : apply f to every element of the list g x and concatenate the results into a single list *)
fun app_all f g x =
    let val inital_list = g x
	fun apply ([]) = []
	  | apply (x::xs') = f x @ apply xs'
    in apply inital_list
    end



(* Problems7 : returns f(xn,...,f(x2, f(x1, init))...) *)
fun foldl f init lst =
    case lst of
	[] => init
     | x::xs' => foldl f (f(x, init)) xs'

(* add up all elements in the given list  *)
fun add_up init lst =
    case lst of
	[] => init
      | x::xs' => add_up (init + x) xs'

(* same with add_up function but implemented by foldl *)
val add_up_c = foldl (fn (n, acc) => n + acc)



(* Problems8 : returns a pair of list. the first part contains elements evaluated to true by f and the second part contains the rest. *)
fun partition f lst =
    case lst of
	[] => ([], [])
      | x::xs' => let val (true_list, false_list) = partition f xs'
		  in if f x
		     then (x::true_list, false_list)
		     else (true_list, x::false_list)
		  end

(* acc store the return value of the stack, calculate in reverse order *)
fun partition_t f lst =
    let fun helper acc lst =
	    case lst of
		[] => acc
	      | x::xs' => helper (if f x
				   then (x::(#1 acc), #2 acc)
				   else (#1 acc, x::(#2 acc))) xs'
    in helper ([], []) lst
    end


   
(* Problems9 : produces a list from a "seed" and a function which given a seed produces SOME of a pair of one element in result list and a new seed, or NONE if it is done seeding *)
fun unfold f seed =
    case f seed of
	NONE => [] (* done seeding *)
     | SOME (result, new_seed) => result::unfold f new_seed

val unfold_eg = unfold (fn n => if n = 0 then NONE else SOME(n, n-1)) 5 = [5, 4, 3, 2, 1]



(* Problems10 : n(n - 1)...1. use unfold to product a target list then use foldl to calculate the final result from the tartget list*)
val factorial_u = (foldl (fn (n, acc) => n * acc) 1) o (unfold (fn n => if n = 0 then NONE else SOME(n, n - 1)))



(* Problems11 : implement map using foldr. consider foldr as a sytax sugar for tail recursion applied f to xi from xn to x1 *)
fun map_r f lst = foldr (fn (n, acc) => (f n)::acc) [] lst
 
fun map_t f lst =
    let fun helper (acc, lst) =
	    case lst of
		[] => acc
	      | x::xs' => helper (acc @ [f x], xs')
    in helper ([], lst)
    end



(* Problems12 : applies f to each element x of lst, from left to right, and returns the list of those x for which f x evaluated to true, in the same order as they occurred in the argument list. using List.foldr *)
fun filter_r f lst = foldr (fn (n, acc) => if f n then n::acc else acc) [] lst



(* Problems13 : implement foldl using foldr *)
fun foldl_r f init lst = foldr f init (foldr (fn (n, acc) => acc @ [n]) [] lst) (* List.rev lst *)



(* Problem14 : define a(polymorphic) type for binary trees where data is at internal nodes but not at leaves *)
			       
datatype 'a binary_trees = Leaf | Node of ('a binary_trees) * 'a * ('a binary_trees)
							   
fun tree_map f tree =
    case tree of
	Leaf => Leaf
     | Node (lt, data, rt) => Node(tree_map f lt, f data, tree_map f rt)

						 
fun tree_fold f init tree =
    case tree of
	Leaf => init
   (* | Node (lt, data, rt) => tree_fold f (tree_fold f (f(data, init)) lt) rt *)
      | Node (lt, data, rt) => f (data, tree_fold f init lt, tree_fold f init rt)


fun tree_filter f tree =
    case tree of
	Leaf => Leaf
      | Node (lt, data, rt) => if f data
			       then Node (tree_filter f lt, data, tree_filter f rt)
			       else Leaf

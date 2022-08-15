(* compose two functions with "optional" values. if either function returns NONE, then the result is NONE *)
fun compose_opt f g x =
    case g x of
	   NONE => NONE
	| SOME v => f v

(* apply f to x and f again to that result and so on until p x is false *)
fun do_until f p x =
    if p x
    then do_until f p (f(x))
    else x
	     
val remove_factor_two  = do_until (fn x => x div 2) (fn x => x mod 2 <> 1)

(* differece between x/2/2.../2 and x(x - 1)(x - 2 )...1 *)
(* do_until is kind of like iteration. i have to use acc to store the result instead of stack. recursion makes a lot convenience for traversing tree structures *)
fun factorial_c n = #1 (do_until (fn (acc, n) => (acc * n, n - 1)) (fn (acc, n) => n <> 0 ) (1, n))

fun factorial n =
    if n = 0
    then 1
    else n * factorial (n - 1)

(* apply f to x until f x = x. iteration is a special type of recursion. *)
fun fixed_point f x = do_until f (fn x => (f x) <> x) x


(* apply f to each value in pair and returns another pair *)
fun map2 f (a1, a2) = (f a1, f a2)


(* apply f to every element of the list g x and concatenate the results into a single list *)
fun app_all f g x =
    let val inital_list = g x
	fun apply ([]) = []
	  | apply (x::xs') = f x @ apply xs'
    in apply inital_list
    end


(* add up all elements in the given list  *)
fun add_up init lst =
    case lst of
	[] => init
      | x::xs' => add_up (init + x) xs'


(* returns f(xn,...,f(x2, f(x1, init))...) *)
fun foldl f init lst =
    case lst of
	[] => init
     | x::xs' => foldl f (f(x, init)) xs'

(* same with add_up function but implemented by foldl *)
val add_up_c = foldl (fn (n, acc) => n + acc)

(* returns a pair of list. the first part contains elements evaluated to true by f and the second part contains the rest. *)
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

   

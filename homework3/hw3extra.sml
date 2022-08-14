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

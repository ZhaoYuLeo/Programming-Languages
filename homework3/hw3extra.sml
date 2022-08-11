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

(* compose two functions with "optional" values. if either function returns NONE, then the result is NONE *)
fun compose_opt f g x =
    case g x of
	   NONE => NONE
	| SOME v => f v

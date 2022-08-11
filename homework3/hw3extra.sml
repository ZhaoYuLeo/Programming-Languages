
fun compose_opt f1 f2 x =
    case f1 x of
	   None => None
	| Some v => f2 v

type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

fun pass_or_fail {grade=grade, id=id} =
    case grade of
	SOME i => if i >= 75 then pass else fail
     | _ => fail

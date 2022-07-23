(* Problem1 : Returns true if the first argument is a date that comes before the second argument, otherwise false  *)
fun is_older(date1 : int * int * int, date2 : int * int * int) =
    if (#1 date1) < (#1 date2)
    then true
    else if (#1 date1) > (#1 date2)
    then false
    else if (#2 date1) < (#2 date2)
    then true
    else if (#2 date1) > (#2 date2)
    then false
    else if (#3 date1) < (#3 date2)
    then true
    else false


fun is_older_compact(date1 : int * int * int, date2 : int * int * int) =
    if (#1 date1) <> (#1 date2)
    then (#1 date1) < (#1 date2)
    else if (#2 date1) <> (#2 date2)
    then (#2 date1) < (#2 date2)
    else (#3 date1) < (#3 date2)


(* Problem2 : Returns how many dates in the list are int the given month *)
fun number_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else
	if (#2 (hd dates))=month
	then 1 + number_in_month(tl dates, month)
	else number_in_month(tl dates, month)


(* Problem3 : Returns the number of dates in the list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. *)
fun number_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then 0   
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

							     
(* Problem4 : Returns a list holding the dates from the argument list of dates that are in the month. The returned list should contain dates in the order they were originally given. *)
fun dates_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
	if (#2 (hd dates))=month
	then (hd dates) :: dates_in_month(tl dates, month)
	else dates_in_month(tl dates, month)


(* Problem5 : Returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. *)
fun dates_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)


(* Problem6 : Returns the nth element of the list where the head of the list is 1st. Your functioin may apply hd or tl to the empty list in this case, which is okay. *)
fun get_nth(stringsList : string list, n : int) =
    if n=1
    then hd stringsList
    else get_nth(tl stringsList, n-1)


(* Problem7 : Returns a string of the form January 20, 2013.  *)
fun date_to_string(date : (int * int * int)) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "Decembe"];
    in  get_nth(months, #2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
    end


(* Problem8 : Returns an int n such that the first n elements of the list add to less than sum, but the first n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in the value; okay for an exception to occur if this in not the case. *)
fun number_before_reaching_sum(sum : int, intList : int list) =
    if null intList
    then 0
    else
	if sum - (hd intList) <= 0
	then 0
	else 1 + number_before_reaching_sum(sum - (hd intList), tl intList)


(* Problem9 : Takes a day of year(i.e. an int between 1 adn 365) and returns what month that day is in(1 for January, 2 for February, etc.) *)
fun what_month(day : int) =
    let val daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in number_before_reaching_sum(day, daysInMonth) + 1
    end


(* Problem10 : Takes two days of the year day1 and day2 and returns an in list [m1,m2,...,mn] where m1 is the month of day1,...,and mn is the month of day day2. *)
fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)


(* Problem11 : Returns NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
fun oldest(dates : (int * int * int) list) =
    if null dates
    then NONE
    else
	let
	    fun oldest_dates(dates : (int * int * int) list) =
		if null (tl dates)
		then hd dates
		else let val oldestDates = oldest_dates(tl dates)
		     in
			 if is_older(hd dates, oldestDates)
			 then hd dates
			 else oldestDates
		     end
	in
	    SOME (oldest_dates(dates))
	end

(* fun sort(xs : int list) = *) 

fun member(x : int, xs : int list) =
    if null xs
    then false
    else
	if x = hd xs
	then true
	else member(x, tl xs)

		   
fun remove_duplicates(xs : int list) =
    if null xs
    then []
    else
	let val tl_ans = remove_duplicates(tl xs)
	in
	    if member(hd xs, tl_ans)
	    then tl_ans
	    else (hd xs :: tl_ans)
	end


(* Problem12 : Same behavior except having a month in the second argument multiple times has no more effect than having it once. Remove duplicates. *)
(* so easy to change the function behavior *)
fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
    number_in_months(dates, remove_duplicates months)


fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) =
    dates_in_months(dates, remove_duplicates months)    

(* same behavior with get_nth but takes int list as argument *)
fun get_nth_int(intsList : int list, n : int) =
    if n=1
    then hd intsList
    else get_nth_int(tl intsList, n-1)


(* Takes a year and returns an int list which consists of the days of 12 months. *)
fun days_in_month(year : int) =
    if year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
    then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]


(* Problem13 : Returns whether the given date describes a real date in the common era. *)
fun reasonale_date(date : (int * int * int)) =
    let val daysInMonth = days_in_month(#1 date)
	val maxDaysOfMonth = get_nth_int(daysInMonth, #2 date)
    in
	#1 date > 0 andalso #2 date > 0 andalso #2 date < 13 andalso #3 date > 0 andalso #3 date < maxDaysOfMonth + 1
    end

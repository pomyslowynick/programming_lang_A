fun is_older (d1: int * int * int, d2: int * int * int) =
    if (#1 d1) > (#1 d2)
    then false
    else
	if (#1 d1) < (#1 d2)
	then true
	else
	    if (#2 d1) > (#2 d2)
	    then false
	    else
		if (#2 d1) < (#2 d2)
		then true
		else
		    if (#3 d1) >= (#3 d2)
		    then false
		    else true


fun number_in_month (ds: (int * int * int) list, m: int) =
    if null ds
    then 0
    else
	let val cur = hd(ds)
	in
	    if (#2 cur) = m 
	    then 1 + number_in_month(tl(ds), m)
	    else number_in_month(tl(ds), m)
	end

fun number_in_months (ds: (int * int * int) list, ms: int list) =
    if null ms
    then 0
    else number_in_month(ds, hd(ms)) + number_in_months(ds, tl(ms))

fun dates_in_month (ds: (int * int * int) list, m: int) =
    if null ds
    then []
    else
	let val cur = hd(ds)
	in
	    if (#2 cur) = m
	    then cur :: dates_in_month(tl(ds), m)
	    else dates_in_month(tl(ds), m)
	end
	    
fun dates_in_months (ds: (int * int * int) list, ms: int list) =
    if null ms
    then []
    else
	let val cur = dates_in_month(ds, hd(ms))
	in
	    if null cur
	    then dates_in_months(ds, tl(ms))
	    else cur @  dates_in_months(ds, tl(ms))
	end

fun get_nth (strs: string list, n: int) =
    if n = 1
    then hd(strs)
    else get_nth(tl(strs), n-1)

fun date_to_string (date: int * int * int) =
    let 
		val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
		val year = Int.toString(#1 date)
		val int_month = #2 date
		val month = get_nth(months, int_month)
		val day = Int.toString(#3 date)
    in
		month ^ " " ^ day ^ ", " ^ year
    end
val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

fun number_before_reaching_sum (sum: int, ls: int list) =
    let val cur = hd(ls)
    in
		if cur >= sum
		then 0
		else 1 + number_before_reaching_sum(sum-cur, tl(ls))
    end	
		
fun what_month (day: int) =
    let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		val pre = number_before_reaching_sum(day, days)
    in
	pre + 1
    end

fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)
					    
fun oldest (ds: (int * int * int) list) =
    if null ds
    then NONE
    else
	let
	    fun oldest_nonempty (ds: (int * int * int) list) =
		if null (tl ds)
		then hd(ds)
		else
		    let val tl_ans = oldest_nonempty(tl(ds))
		    in
			if is_older(hd(ds), tl_ans)
			then hd(ds)
			else tl_ans
		    end
	in
	    SOME (oldest_nonempty ds)
	end
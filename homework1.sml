
val dates_list = [(1990,2,31),(2000,2,01),(1865,3,8),(1666,1,21),(1242,2,1)]

fun is_older (date1 : int * int * int, date2 : int * int * int) = 
    if #1 date1 > #1 date2
    then false
    else if #1 date1 = #1 date2 andalso #2 date1 > #2 date2
    then false
    else if (#1 date1 = #1 date2 andalso #2 date1 = #2 date2) andalso #3 date1 > #3 date2 
    then false
    else if (#1 date1 = #1 date2 andalso #2 date1 = #2 date2) andalso #3 date1 = #3 date2 
    then false
    else true


val test1 = is_older ((1,2,1991),(2,3,4))

fun number_in_month (dates_li : (int * int * int) list, month_num : int) = 
    if null dates_li
    then 0
    else 
        let val month_count =  number_in_month(tl dates_li, month_num)
        in 
            if #2 (hd dates_li) = month_num
            then month_count + 1
            else month_count
        end
        
val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2)

fun number_in_months(dates_li2 : (int * int * int) list, months_li : int list) = 
    if null dates_li2
    then 0
    else if null months_li
    then 0
    else
        let val occurences_month = number_in_months(dates_li2, tl months_li)
        in 
            if number_in_month(dates_li2, hd months_li) > 0
            then occurences_month + 1
            else occurences_month
        end


val test3 = number_in_months([(2012,2,28),(2013,12,1),(2011,1,31),(2011,4,28)],[2,3,4])

fun dates_in_month(dates_li3 : (int * int * int) list, month_num2 : int) = 
    if null dates_li3
    then []
    else 
        let val dates_result = dates_in_month(tl dates_li3, month_num2)
        in
            if #2 (hd dates_li3) = month_num2
            then (hd dates_li3)::dates_result
            else dates_result
        end 


val test4 = dates_in_month([(2012,2,28),(2013,12,1)],12)

fun dates_in_months(dates_list : (int * int * int) list, months_list : int list) = 
    if null dates_list
    then []
    else if null months_list
    then []
    else
        let val new_dates_result = dates_in_months(dates_list, tl months_list)
            val current_result = dates_in_month(dates_list, hd months_list)
        in
            if null current_result
            then new_dates_result
            else current_result@new_dates_result
        end



val test5 = dates_in_months ([(2012,5,28),(2013,12,1),(2011,3,31),(2011,4,28),(2011,3,12)],[5,3,12])

fun get_nth(slist : string list, indexn : int) =
    if indexn = 1
    then hd slist
    else get_nth(tl slist, indexn - 1)


val test6 = get_nth (["hi", "there", "how", "are", "you"], 2)


fun date_to_string(int_date : int * int * int) =
    let 
        val list_months = ["January", "February", "March", "April",
            "May", "June", "July", "August", "September", "October", "November", "December"]
        val string_day = Int.toString ((#3 int_date))
        val string_month = get_nth(list_months, #2 int_date) 
        val string_year = Int.toString ((#1 int_date))
    in 
        string_month ^ " " ^ string_day ^ ", " ^ string_year
    end
            

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

fun number_before_reaching_sum(sum : int, int_list : int list) = 
    if sum <= hd int_list
    then 0
    else
        let val some_r = number_before_reaching_sum(sum - (hd int_list), tl int_list)
        in
            some_r + 1
        end

fun number_before_reaching_sum2(sum : int, int_list : int list) = 
    if sum <= hd int_list
    then 0
    else 1 + number_before_reaching_sum2(sum - hd int_list, tl int_list)


val test8 = number_before_reaching_sum (10, [10,1,2,3,2,1,5])


fun what_month(day_of_year : int) =
    let val days_in_months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in 
        1 + number_before_reaching_sum2(day_of_year, days_in_months)
    end

val test9 = what_month 70

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
        let val result_list = month_range(day1 + 1, day2)
        in
            what_month(day1)::result_list
        end

val test10 = month_range (31, 34) = [1,2,2,2]


fun oldest(list_of_dates : (int * int * int) list) = 
    if null list_of_dates
    then NONE
    else if null (tl list_of_dates)
    then NONE
    else
        let val result = oldest(tl list_of_dates)
        in
            if isSome result andalso is_older(valOf result, hd list_of_dates)
            then result
            else SOME (hd dates)
        end

fun oldest1 (dates : (int * int * int) list) =
    if null dates
    then NONE
    else let 
             val ans = oldest(tl dates)
        in 
        if isSome ans andalso is_older(valOf ans, hd dates)
            then ans
            else SOME (hd dates)
        end 

val test11 = oldest1([(5,5,2),(5,10,2),(5,2,2),(5,12,2)])
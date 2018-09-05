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

fun oldest(list_of_dates : (int * int * int) list) = 
    if null list_of_dates
    then NONE
    else if null (tl list_of_dates)
    then NONE
    else
        let val result = oldest(tl list_of_dates)
        in
            if is_older(hd list_of_dates, (hd(tl list_of_dates)))
            then SOME (hd list_of_dates)
            else SOME (hd(tl list_of_dates))
        end

val test11 = oldest([(5,5,2),(5,10,2),(5,2,2),(5,12,2)])
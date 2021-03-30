(* Homework #1 - codgician *)

fun is_older (x : int * int * int, y : int * int * int) = 
    if (#1 x) <> (#1 y) 
    then (#1 x) < (#1 y)
    else if (#2 x) <> (#2 y)
    then (#2 x) < (#2 y)
    else (#3 x) < (#3 y)

fun number_in_month (xs : (int * int * int) list, y : int) = 
    if null xs
    then 0
    else if #2 (hd xs) = y 
    then 1 + number_in_month(tl xs, y)
    else number_in_month(tl xs, y)

fun number_in_months (xs : (int * int * int) list, ys : int list) = 
    if null ys
    then 0
    else number_in_month(xs, hd ys) + number_in_months(xs, tl ys);

fun dates_in_month (xs : (int * int * int) list, y : int) =
    if null xs
    then []
    else if #2 (hd xs) = y
    then (hd xs) :: dates_in_month(tl xs, y)
    else dates_in_month(tl xs, y)

fun dates_in_months (xs : (int * int * int) list, ys : int list) = 
    if null ys
    then []
    else dates_in_month(xs, hd ys) @ dates_in_months(xs, tl ys)

fun get_nth (xs : string list, y : int) = 
    if y = 1
    then hd xs
    else get_nth (tl xs, y - 1)

fun date_to_string (x : int * int * int) = 
    let val months = ["January", "February", "March", "April", "May", "June", "July", 
                        "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 x) ^ " " ^ Int.toString (#3 x) ^ ", " ^ Int.toString (#1 x)
    end

fun number_before_reaching_sum (sum : int, xs : int list) = 
    if sum - hd xs <= 0
    then 0
    else 1 + number_before_reaching_sum(sum - hd xs, tl xs)

fun what_month (x : int) = 
    let val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(x, days) + 1
    end

fun month_range (l : int, r : int) =
    if l > r
    then []
    else List.tabulate(r - l + 1, fn x => what_month(x + l))

fun oldest (xs : (int * int * int) list) = 
    if null xs
    then NONE
    else 
        let fun oldest_nonempty (xs : (int * int * int) list) = 
            if null (tl xs)
            then hd xs
            else 
                let val tl_ans = oldest_nonempty(tl xs)
                in
                    if is_older(hd xs, tl_ans)
                    then hd xs
                    else tl_ans
                end
        in
            SOME(oldest_nonempty(xs))
        end

(* Challenges *)

fun remove_dup_sorted (xs : int list) = 
    if null xs
    then []
    else if null (tl xs)
    then [hd xs]
    else if hd xs <> hd (tl xs)
    then (hd xs) :: remove_dup_sorted(tl xs)
    else remove_dup_sorted(tl xs)

fun remove_dup (xs : int list) = 
    remove_dup_sorted (ListMergeSort.sort (fn(x, y) => x > y) xs)
    
fun number_in_months_challenge (xs : (int * int * int) list, ys : int list) = 
    number_in_months(xs, remove_dup(ys))
    
fun dates_in_months_challenge (xs : (int * int * int) list, ys : int list) = 
    dates_in_months(xs, remove_dup(ys))

fun reasonable_date (x : int * int * int) = 
    if (#1 x) <= 0
    then false
    else if (#2 x) > 12 orelse (#2 x) < 1
    then false
    else if (#3 x) < 1
    then false
    else
        let 
            fun is_leap (x : int) =
                (x mod 400 = 0) orelse (x mod 4 = 0 andalso x mod 100 <> 0)
            fun get_nth_int (xs : int list, y : int) = 
                if y = 1
                then hd xs
                else get_nth_int(tl xs, y - 1)
            val days = 
                if is_leap(#1 x)
                then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        in
            (#3 x) <= get_nth_int(days, #2 x) 
        end

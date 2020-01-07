fun is_older(date1 : int * int * int, date2 : int * int * int) =
    let
        val y1 = #1 date1
        val m1 = #2 date1
        val d1 = #3 date1
        val y2 = #1 date2
        val m2 = #2 date2
        val d2 = #3 date2
    in
        y1 < y2 orelse (y1 = y2 andalso m1 < m2)
        orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)
    end


fun number_in_month (lst : (int * int * int) list, goal : int) =
  if null lst
  then 0
  else if (#2 (hd lst)) = goal
  then 1 + number_in_month (tl lst, goal)
  else number_in_month (tl lst, goal)


fun number_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month (dates, hd months) + number_in_months (dates, tl months)

fun dates_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then []
  else if (#2 (hd dates)) = month
  then (hd dates) :: dates_in_month ((tl dates), month)
  else dates_in_month ((tl dates), month)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then []
  else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)

fun get_nth (strings: string list, n : int) =
  if n = 1
  then hd strings
  else get_nth(tl strings, n - 1)

fun date_to_string (date : (int * int * int)) =
  let
    val months = ["January", "February", "March", "April",
      "May", "June", "July", "August", "September",
      "October", "November", "December"]
    val month = get_nth(months, (#2 date))
    val day = Int.toString (#3 date)
    val year = Int.toString (#1 date)
  in
    month ^ " " ^ day ^ ", " ^ year
  end

fun number_before_reaching_sum (goal : int, nums : int list) =
  if (hd nums) >= goal
  then 0
  else 1 + number_before_reaching_sum (goal - (hd nums), tl nums)

fun what_month (day : int) =
  let
    val days = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days) + 1
  end

fun month_range (day1 : int, day2: int) =
  if day1 > day2 then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates : (int * int * int) list) =
  if null dates then NONE
  else
    let
      fun find_oldest (oldest_seen : (int * int * int), xs : (int * int * int) list) =
        if null xs then SOME oldest_seen
        else
          if is_older(oldest_seen, hd xs)
          then find_oldest(oldest_seen, tl xs)
          else find_oldest(hd xs, tl xs)

    in
      find_oldest(hd dates, tl dates)
    end
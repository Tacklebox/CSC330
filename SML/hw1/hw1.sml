(*  Assignment #1: Maxwell A. Borden *)

type DATE = (int * int * int)
exception InvalidParameter

fun is_older(d1: DATE, d2: DATE): bool =
  if #1 d1 < #1 d2 then true
  else if #1 d1 > #1 d2 then false
  else if #2 d1 < #2 d2 then true
  else if #2 d1 > #2 d2 then false
  else if #3 d1 < #3 d2 then true
  else false;
fun number_in_month(dates: DATE list, month: int): int = if null dates then 0
  else if (#2 (hd dates)) = month then 1 + number_in_month((tl dates), month)
  else number_in_month((tl dates), month);

fun number_in_months(dates: DATE list, months: int list): int =
  if null months then 0
  else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))

fun dates_in_month(dates: DATE list, month: int): DATE list =
  if null dates then []
  else if (#2 (hd dates)) = month then (hd dates)::dates_in_month((tl dates), month)
  else dates_in_month((tl dates), month);

fun dates_in_months(dates: DATE list, months: int list): DATE list =
  if null months then []
  else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months));

fun get_nth(strings: string list, n: int): string =
  if n = 0 then raise InvalidParameter
  else if null strings then raise InvalidParameter
  else if n = 1 then (hd strings)
  else get_nth((tl strings), n - 1);

fun date_to_string(date: DATE): string =
  let
    val months: string list = ["January", "February", "March", "April", "May",
    "June", "July", "August", "September", "October", "November", "December"];
    val monthString: string = get_nth(months, #2 date);
    val dayString: string = Int.toString(#3 date);
    val yearString: string = Int.toString(#1 date);
  in
    monthString ^ " " ^ dayString ^ ", " ^ yearString
  end

fun number_before_reaching_sum(sum: int, input: int list): int =
  let fun recursive_sum(current_val: int, index: int, input: int list): int =
    if (hd input) + current_val < sum then recursive_sum(current_val + (hd
    input), index + 1, (tl input))
    else index;
  in
    recursive_sum(0, 0, input)
  end

fun what_month(dayOfYear: int): int =
  let
    val monthDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  in
    number_before_reaching_sum(dayOfYear, monthDays) + 1
  end

fun month_range(day1: int, day2: int): int list =
  if day1 > day2 then []
  else what_month(day1)::month_range(day1 + 1, day2);

fun oldest(dates: DATE list): DATE option =
  let fun recursive_oldest(oldest: DATE, dates: DATE list): DATE =
    if null dates then oldest
    else if is_older((hd dates), oldest) then recursive_oldest((hd dates), (tl dates))
    else recursive_oldest(oldest, (tl dates));
  in
    if null dates then NONE
    else SOME (recursive_oldest((hd dates), (tl dates)))
  end

fun reasonable_date(date: DATE): bool =
  let
    fun get_nth(ints: int list, n: int): int =
      if n = 0 then raise InvalidParameter
      else if null ints then raise InvalidParameter
      else if n = 1 then (hd ints)
      else get_nth((tl ints), n - 1);
    val year = (#1 date);
    val month = (#2 date);
    val day = (#3 date);
    val leapYear = year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
    val monthDays = if leapYear then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                    else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  in
    if year < 1 then false
    else if month < 1 orelse month > 12 then false
    else if day < 1 orelse day > get_nth(monthDays, month) then false
    else true
  end

(*BEGIN TEST BLOCK*)
(* val feb28_2012 = (2012, 2, 28); *)
(* val feb12_2011 = (2011, 2, 12); *)
(* val dec1_2013 = (2013,  12,  1); *)
(* val march31_2011 = (2011,  3,  31); *)
(* val april28_2011 = ( 2011,  4,  28); *)
(* val june1_2013 = (2013,  6,  1); *)

(* val test1 = is_older(( 1,  2,  3),( 2,  3,  4)) = true; *)
(* val test1a = is_older(feb28_2012,dec1_2013) = true; *)
(* val test1b = is_older(dec1_2013, feb28_2012) = false; *) (* val test1c = is_older(dec1_2013, dec1_2013) = false; *) (* val test2 = number_in_month([feb28_2012,dec1_2013],2) = 1; *)
(* val test2a = number_in_month([feb28_2012,dec1_2013],3) = 0; *)
(* val test2b = number_in_month([feb28_2012,dec1_2013,march31_2011,april28_2011],3) = 1; *)
(* val test2b = number_in_month([feb28_2012,dec1_2013,feb12_2011,march31_2011,april28_2011],2) = 2; *)

(* val test3a = number_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[3,4]) = 2; *)
(* val test3a = number_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[2,3,4]) = 3; *)

(* val test4  = dates_in_month([feb28_2012,dec1_2013],2) = [feb28_2012]; *)
(* val test4a = dates_in_month([feb28_2012,dec1_2013],12) = [dec1_2013]; *)
(* val test4b = dates_in_month([feb28_2012,dec1_2013],3) = []; *)
(* val test4c = dates_in_month([feb28_2012,feb12_2011,dec1_2013],2) = [feb28_2012,feb12_2011]; *)

(* val test5a = dates_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[2,3,4]) = [feb28_2012,march31_2011,april28_2011]; *)

(* val test5d = dates_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[5,7]) = []; *)

(* val test6 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"; *)
(* val test6a = get_nth(["hi", "there", "how", "are", "you"], 7) = "never" handle InvalidParameter => true; *)
(* val test6b = get_nth(["hi", "there", "how", "are", "you"], 0) = "never" handle InvalidParameter => true; *)
(* val test6c = get_nth([], 0) = "never" handle InvalidParameter => true; *)

(* val test7 = date_to_string(june1_2013) = "June 1, 2013"; *)

(* val test7a = date_to_string(april28_2011) = "April 28, 2011"; *)

(* val test8 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3 *)
(* val test8a = number_before_reaching_sum(10, [11,1,2,3,4,5]) = 0 *)
(* val test8b = number_before_reaching_sum(12, [11,1,2,3,4,5]) = 1 *)
(* val test8c = number_before_reaching_sum(1, [1,2,3,4,5]) = 0; *)
(* val test8d = number_before_reaching_sum(6, [1,2,3,4,5]) = 2; *)

(* val test9  = what_month(70) = 3; *)
(* val test9a = what_month(31) = 1; *)
(* val test9b = what_month(32) = 2; *)
(* val test9c = what_month(360) = 12; *)
(* val test91 = what_month(70) = 3; *)
(* val test92 = what_month(30) = 1; *)
(* val test93 = what_month(1) = 1; *)
(* val test94 = what_month(32) = 2; *)
(* val test95 = what_month(365) = 12; *)
(* val test96 = what_month(364) = 12; *)

(* val test10 = month_range(31, 34) = [1,2,2,2]; *)
(* val test10a = month_range(360, 365) = [12,12,12,12,12,12]; *)
(* val test10b = month_range(31,31 + 28 +1) = [1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3]; *)
(* val test10c = month_range(35, 34) = []; *)
(* val test10d = month_range(35, 35) = [2]; *)
(* val test10e = month_range(31+29, 31+29) = [3]; *)

(* val test11 = oldest([feb28_2012,march31_2011,april28_2011]) = SOME march31_2011; *)
(* val test11a = oldest([april28_2011]) = SOME april28_2011; *)
(* val test11b = oldest([]) = NONE; *)

(* val test12 = reasonable_date( 2014,  12,  31); *)
(* val test12a = not (reasonable_date ( 2015,  2,  29)); *)
(*END TEST BLOCK*)

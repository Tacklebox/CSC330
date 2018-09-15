(*  Assignment #1 *)
(* This file is where your solutions go *)

type DATE = (int * int * int)
exception InvalidParameter

(*BEGIN TEST BLOCK*)
val feb28_2012 = (2012, 2, 28);
val feb12_2011 = (2011, 2, 12);
val dec1_2013 = (2013,  12,  1);
val march31_2011 = (2011,  3,  31);
val april28_2011 = ( 2011,  4,  28);
val june1_2013 = (2013,  6,  1);
(*END TEST BLOCK*)


fun is_older(d1: DATE, d2: DATE): bool =
    if #1 d1 < #1 d2 then true else
    if #1 d1 > #1 d2 then false else
    if #2 d1 < #2 d2 then true else
    if #2 d1 > #2 d2 then false else
    if #3 d1 < #3 d2 then true else false

(*BEGIN TEST BLOCK*)
val test1 = is_older(( 1,  2,  3),( 2,  3,  4)) = true;
val test1a = is_older(feb28_2012,dec1_2013) = true;
val test1b = is_older(dec1_2013, feb28_2012) = false;
val test1c = is_older(dec1_2013, dec1_2013) = false;
(*END TEST BLOCK*)

fun number_in_month(dates: DATE list, month: int): int = 
  if null dates then 0 else
  if (#2 (hd dates)) = month then 1 + number_in_month((tl dates), month) else number_in_month((tl dates), month)

(*BEGIN TEST BLOCK*)
val test2 = number_in_month([feb28_2012,dec1_2013],2) = 1;
val test2a = number_in_month([feb28_2012,dec1_2013],3) = 0;
val test2b = number_in_month([feb28_2012,dec1_2013,march31_2011,april28_2011],3) = 1;
val test2b = number_in_month([feb28_2012,dec1_2013,feb12_2011,march31_2011,april28_2011],2) = 2;
(*END TEST BLOCK*)

fun number_in_months(dates: DATE list, months: int list): int =
if null months then 0 else
number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))

(*BEGIN TEST BLOCK*)
val test3a = number_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[3,4]) = 2;
val test3a = number_in_months([feb28_2012,dec1_2013,march31_2011,april28_2011],[2,3,4]) = 3;
(*END TEST BLOCK*)
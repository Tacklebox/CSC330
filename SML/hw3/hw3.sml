(* Assign 03 Provided Code *)

(*  Version 1.0 *)

exception NoAnswer

datatype pattern = Wildcard
     | Variable of string
     | UnitP
     | ConstP of int
     | TupleP of pattern list
     | ConstructorP of string * pattern

datatype valu = Const of int
        | Unit
        | Tuple of valu list
        | Constructor of string * valu

(* Description of g:

*)

fun g f1 f2 p =
    let
  val r = g f1 f2
    in
  case p of
      Wildcard          => f1 ()
    | Variable x        => f2 x
    | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
    | ConstructorP(_,p) => r p
    | _                 => 0
    end


(**** put all your code after this line ****)

(* Question 1
 * Write a function only_capitals that takes a string list and returns a string
 * list that has only the strings in the argument that start with an uppercase
 * letter. Assume all strings have at least 1 character. Use List.filter,
 * Char.isUpper, and String.sub to make a 1-2 line solution. *)
fun only_capitals(xs: string list): string list =
  List.filter (fn x => Char.isUpper(String.sub(x,0))) xs

(* Question 2
 * Write a function longest_string1 that takes a string list and returns the
 * longest string in the list. If the list is empty, return "". In the case of a
 * tie, return the string closest to the beginning of the list. Use foldl,
 * String.size, and no recursion (other than the implementation of foldl is
 * recursive). *)
fun longest_string1(xs: string list): string =
  foldl (fn (x,ls) => if String.size(x) <= String.size(ls) then ls else x) "" xs

(* Question 3
 * Write a function longest_string2 that is exactly like longest_string1 except
 * in the case of ties it returns the string closest to the end of the list.
 * Your solution should be almost an exact copy of longest_string1. Still use
 * foldl and String.size. *)
fun longest_string2(xs: string list): string =
  foldl (fn (x,ls) => if String.size(x) < String.size(ls) then ls else x) "" xs

(* Question 4
 * Write functions longest_string_helper, longest_string3, and longest_string4
 * such that:
  • longest_string3 has the same behavior as longest_string1 and longest_string4
    has the same behavior as longest_string2.
  • longest_string_helper has type (int * int -> bool) -> string list -> string
    (notice the currying). This function will look a lot like longest_string1 and
    longest_string2 but is more general because it takes a function as an argument.
  • longest_string3 and longest_string4 are defined with val-bindings and
    partial applications of longest_string_helper. *)
fun longest_string_helper(f: int * int -> bool): string list -> string =
  foldl (fn (x,ls) => if f (String.size(x), String.size(ls)) then ls else x) ""

val longest_string3: string list -> string = longest_string_helper(fn (x,y) => x <= y)
val longest_string4: string list -> string = longest_string_helper(fn (x,y) => x < y)

(* Question 5
 * Write a function longest_capitalized that takes a string list and returns the
 * longest string in the list that begins with an uppercase letter (or "" if
 * there are no such strings). Use a val-binding and the ML library’s o operator
 * for composing functions. Resolve ties like in problem 2. *)
val longest_capitalized = longest_string1 o only_capitals

(* Question 6
 * Write a function rev_string that takes a string and returns the string that
 * is the same characters in reverse order. Use ML’s o operator, the library
 * function rev for reversing lists, and two library functions in the String
 * module (browse the module documentation to find the most useful functions.) *)
val rev_string = String.implode o List.rev o String.explode

(* Question 7
 * Write a function first_answer that has type (’a -> ’b option) -> ’a list -> ’b.
 * Notice that the 2 arguments are curried. The first argument should be applied
 * to elements of the second argument in order, until the first time it returns
 * SOME v for some v and then v is the result of the call to first_answer. If
 * the first argument returns NONE for all list elements, then first_answer
 * should raise the exception NoAnswer . Hints: Sample solution is 5 lines and
 * does nothing fancy. *)
fun first_answer f xs =
  case List.mapPartial f xs of
    [] => raise NoAnswer
  | x::_ => x

(* Question 8
 * Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b
 * list option (notice the 2 arguments are curried). The first argument should
 * be applied to elements of the second argument. If it returns NONE for any
 * element, then the result for all_answers is NONE . Else the calls to the
 * first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and
 * the result of all_answers is SOME lst where lst is lst1, lst2, ..., lstn
 * appended together (the order in the result list should be preserved).
 * Hints: The sample solution is 8 lines. It uses a helper function with an
 * accumulator and uses @ . Note all_answers f [] should evaluate to SOME [] .
 *)
fun all_answers f xs =
  let
    fun noneProp (NONE, _) = NONE
      | noneProp (_, NONE) = NONE
      | noneProp (SOME a, SOME b) = SOME (a @ b);
    fun applyf (x,y) = (f(x),y);
  in
    foldl (noneProp o applyf) (SOME []) xs
  end

(* Question 9 *)

(* Part A *)
(* g is a function that takes 3 curried parameters: f1, f2, and p. it computes the
 * bindings that are produced when evaluating a mini-version of SML *)

(* Part B *)
fun count_wildcards p =
  g (fn _ => 1) (fn _ => 0) p

(* Part C *)
fun count_wild_and_variable_lengths p =
  g (fn _ => 1) (fn var => String.size var) p

(* Part D *)
fun count_some_var(s, p) =
  g (fn _ => 0) (fn var => if var = s then 1 else 0) p

(* Question 10 *)
fun check_pat p =
  let
    fun get_all_variables p =
      case p of
           Variable x        => [x]
         | ConstructorP(_,p) => get_all_variables(p)
         | TupleP ps         =>
             List.foldl (fn (p, acc) => get_all_variables(p) @ acc) [] ps
         | _                 => [];
    fun set xs = foldl (fn (x, acc) => if List.exists (fn x_acc => x = x_acc) acc
                                    then acc else x::acc) [] xs
    val all_variables = get_all_variables(p);
  in
      List.length(set(all_variables)) = List.length(all_variables)
  end

(* Question 11 *)
fun match (_, Wildcard)        = SOME []
  | match (Unit, UnitP)        = SOME []
  | match (_, UnitP)           = NONE
  | match (v, Variable s)      = SOME [(s, v)]
  | match (Const v, ConstP vP) = if v = vP then SOME [] else NONE
  | match (_, ConstP _)        = NONE
  | match (Constructor(s2,v), ConstructorP(s1,p)) =
    if s1 = s2 then match(v,p) else NONE
  | match (_, ConstructorP(_,_)) = NONE
  | match (Tuple vs, TupleP ps) =
  if List.length(vs) <> List.length(ps) then NONE
  else all_answers match (List.rev (ListPair.zip(vs,ps)))
  | match (_, TupleP _) = NONE

(* Question 12 *)


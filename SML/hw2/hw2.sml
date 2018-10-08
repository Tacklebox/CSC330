(* if you use this function to compare two strings (returns true if the same
   string), then you avoid some warning regarding polymorphic comparison  *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for Part 1 here *)

fun all_except_option(target, xs) =
  case xs of
       [] => NONE
     | x::xs' =>
         (case same_string(target, x) of
              true => SOME xs'
            | false => (case all_except_option(target, xs') of
                            NONE => NONE
                          | SOME xs'' => SOME (x::xs'')))

fun get_substitutions1(sll, t) =
  case sll of
       [] => []
     | sl::sll' => 
         (case all_except_option(t, sl) of
               NONE => get_substitutions1(sll', t)
             | SOME sl' => sl' @ get_substitutions1(sll', t))

fun get_substitutions2(sll, t) =
  let fun get_substitutions2_acc(sll, acc) =
    case sll of
         [] => acc
       | sl::sll' =>
           (case all_except_option(t, sl) of
                 NONE => get_substitutions2_acc(sll', acc)
               | SOME sl' => get_substitutions2_acc(sll',  acc @ sl'))
  in
    get_substitutions2_acc(sll, [])
  end

fun similar_names(sll, {first=first, middle=middle, last=last}) =
  let fun similar_names_acc(name_list, acc) =
    case name_list of
         [] => acc
       | name::name_list' => similar_names_acc(name_list', acc @ [{first=name,
         middle=middle, last=last}])
  in
    similar_names_acc(get_substitutions2(sll, first), [{first=first,
    middle=middle, last=last}])
  end

(************************************************************************)
(* Game  *)

(* you may assume that Num is always used with valid values 2, 3, ..., 10 *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

fun card_color(c) =
  case c of
       (Clubs, _) => Black
     | (Spades, _) => Black
     | (Diamonds, _) => Red
     | (Hearts, _) => Red

fun card_value(c) =
  case c of
       (_, Jack) => 10
     | (_, Queen) => 10
     | (_, King) => 10
     | (_, Ace) => 11
     | (_, Num num) => num

fun remove_card(cs, c, e) =
  let
    fun same_card(x: card, y: card) = (x = y);
    fun all_except_option_card(c, cs) =
      case cs of
           [] => NONE
         | x::cs' =>
             (case same_card(c, x) of
                   true => SOME cs'
                 | false => (case all_except_option_card(c, cs') of
                                  NONE => NONE
                                | SOME cs'' => SOME (x::cs'')))
  in
    case all_except_option_card(c, cs) of
         NONE => raise e
       | SOME cs' => cs'
  end

fun all_same_color(cl) =
  case cl of
       [] => true
     | _::[] => true
     | top_card::next_card::cl' => card_color(top_card) = card_color(next_card)
     andalso all_same_color(next_card::cl')

fun sum_cards(cs) =
  let fun sum_cards_tr(cs, acc) =
    case cs of
         [] => acc
       | c::cs' => sum_cards_tr(cs', acc + card_value(c))
  in
    sum_cards_tr(cs, 0)
  end

fun score(hand, goal) =
  let
    val hand_sum = sum_cards(hand);
    val pre_score = case hand_sum > goal of
         true => (2 * (hand_sum - goal))
       | false => (goal - hand_sum);
  in
    case all_same_color(hand) of
         true => Int.div(pre_score, 2)
       | false => pre_score
  end
                             
fun officiate(cl, ml, g) =
  let fun officiate_tr(cl, ml, h) =
    case ml of
         [] => score(h, g)
       | m::ml' => case m of
                        Discard c => officiate_tr(cl, ml', remove_card(h, c, IllegalMove))
                      | Draw => case cl of
                                     [] => score(h, g)
                                   | c::cl' => case sum_cards(c::h) > g of
                                                    true => score(c::h, g)
                                                  | false => officiate_tr(cl', ml', c::h)
  in
    officiate_tr(cl, ml, [])
  end
(************************************************************************)
(* tests *)

val test1_0=all_except_option("1",["2","3","4","1"]) = SOME ["2","3","4"];

val test2_0=get_substitutions1([["Maxwell","m"],["Max","m"],["Maximillion","M"]], "m") = ["Maxwell","Max"];

val test3_0=get_substitutions2([["Maxwell","m"],["Max","m"],["Maximillion","M"]], "m") = ["Maxwell","Max"];

val test4_0=similar_names([ ["Maxwell", "Max"], ["Maximillion", "Maxime", "Maxine"] ],
 {first="Maxwell", middle = "Alexander", last="Borden"}) = [{first="Maxwell",
  middle = "Alexander", last="Borden"},{first="Max", middle = "Alexander", last="Borden"}];

val test5_0=card_color(Clubs, Num 2) = Black

val test6_0=card_value(Clubs, Num 2) = 2;

exception notFound

val test_cards = [(Spades, Ace), (Clubs, Num 2), (Hearts, Queen)];

val test7_0 = remove_card(test_cards, (Clubs, Num 2), notFound) = [(Spades, Ace), (Hearts, Queen)];

val cards1 = [(Clubs, Ace), (Diamonds, Num 10), (Spades, Num 4), (Clubs, Num 4)];
val cards2 = [];
val cards3 = [(Clubs, Ace), (Diamonds, Num 10), (Spades, Num 5), (Clubs, Num 9)];
val cards4 = [(Clubs, Ace), (Clubs, Num 10), (Clubs, Num 5), (Clubs, Num 2)];
val cards5 = [(Diamonds, Ace), (Diamonds, Num 10), (Diamonds, Queen), (Diamonds, Jack), (Diamonds,King)];

val test8_0 = all_same_color([(Clubs, Num 2), (Spades, Ace), (Hearts, Queen)]) = false;

val test9_0 = sum_cards([(Clubs, Num 2), (Spades, Ace), (Hearts, Queen)]) = 23;

val test10_0 = score([(Clubs, Num 2), (Spades, Ace), (Hearts, Queen)], 23) = 0;

val test11_0 = officiate([(Clubs, Num 2), (Spades, Ace), (Hearts, Queen)], [Draw,Draw,Draw],24)=1;

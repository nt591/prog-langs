(* DanAce  Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

fun all_except_option (str, xs) =
  let
    fun filter_strs (strs) = case strs of
      [] => []
      | y::ys => if same_string(y, str) then ys else y :: filter_strs(ys);
    val ans = filter_strs(xs)
  in
    if ans = xs then NONE else SOME ans
  end


fun get_substitutions1 (str_list : string list list, str : string) =
  let
    fun filter_strs (xs : string list) = case all_except_option(str, xs) of
      NONE => []
      | SOME lst => lst;
    fun get_subs (strs: string list list) = case strs of
      [] => []
      | x :: xs => filter_strs(x) @ get_subs(xs);
  in
    get_subs(str_list)
  end

fun get_substitutions2 (str_list : string list list, str: string) =
  let
    fun filter_strs (xs : string list) = case all_except_option(str, xs) of
      NONE => []
      | SOME lst => lst;
    fun get_subs (xs : string list list, acc : string list) =
      case xs of
        [] => acc
        | y :: ys => get_subs(ys, acc @ filter_strs(y));
  in
    get_subs(str_list, [])
  end

type full_name = { first : string, middle :string, last : string }

fun similar_names (subs : string list list, user_name: full_name) =
  let
    val { first, middle, last } = user_name;
    fun filter_strs (xs : string list) = case all_except_option(first, xs) of
         NONE => []
        | SOME lst => lst;

    fun get_subs (name_list : string list list, acc: string list) =
      case name_list of
        [] => acc
        | y :: ys => get_subs(ys, acc @ filter_strs(y));

    val filtered_names = get_subs(subs, []);
    fun make_names_from_subs (name_subs : string list, people : full_name list)
      =
      case name_subs of
        [] => people
        | y::ys => make_names_from_subs(ys, { first=y, last=last,
        middle=middle } :: people);
  in
    make_names_from_subs(filtered_names, [])
  end

fun card_color (suit, rank) =
  case suit of
    Diamonds => Red
    | Hearts => Red
    | Spades => Black
    | Clubs => Black

fun card_value (s, rank) =
  case rank of
    Num i => i
    | Ace => 11
    | _  => 10

fun remove_card (cards : card list, card : card, e) =
  let
    fun reduce_list (cs) =
      case cs of
        [] => raise e
        | x::xs => if x = card then xs else x :: reduce_list(xs);
  in
    reduce_list(cards)
  end

fun all_same_color (cards: card list) =
  case cards of
    [] => true
    | _ :: [] => true
    | fst :: ( snd :: rest ) => (card_color fst) = (card_color snd) andalso
    all_same_color (snd :: rest)


fun sum_cards (cards : card list) =
  let
    fun count (cs, acc) =
      case cs of
        [] => acc
        | x :: xs => count(cs, acc + (card_value x))
  in
    count(cards, 0)
  end

fun score (cards : card list, goal : int) =
  let
    val card_sum = sum_cards(cards);
    val prelim_score = if card_sum > goal then 3 * (card_sum - goal) else (goal
    - card_sum);
  in
    if (all_same_color cards) then prelim_score div 2 else prelim_score
  end

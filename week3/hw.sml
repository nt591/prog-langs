(* Coursera Programming Languages, Homework 3, Provided Code *)

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter (fn x => Char.isUpper (String.sub (x, 0)))

val longest_string1 = foldl (fn (acc, el) => if (String.size acc) >= (String.size el) then acc else el) ""

val longest_string2 = foldl (fn (acc, el) => if (String.size acc) > (String.size el) then acc else el) ""

fun longest_string_helper f  = foldl f ""

val longest_string3 = longest_string_helper (fn (acc, el) => if (String.size acc) >= (String.size el) then acc else el)

val longest_string4 = longest_string_helper (fn (acc, el) => if (String.size acc) > (String.size el) then acc else el)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
  case xs of
    [] => raise NoAnswer
    | y::ys =>
        case f y of
          NONE => first_answer f ys
          | SOME v => v

fun all_answers f lst =
    let
	fun helper remaining acc =
	    case (remaining, acc) of
		([], _) => acc
	      | (x :: xs, SOME v) => (case f x of
					  NONE => NONE
					| SOME xv => helper xs (SOME (xv @ v)))
	      | _ => NONE
    in
	helper lst (SOME [])
    end

val count_wildcards = g (fn x => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn x => 1) (fn s => String.size s)

fun count_some_var (str, pat) = g (fn x => 0) (fn x => if x = str then 1 else 0) pat

fun check_pat pat =
	let
		fun get_vars ps acc = case ps of
			Variable x => x :: acc
			| TupleP pss => foldl (fn (a, p) => get_vars p a) acc
			| ConstructorP(_, p) => get_vars p acc
			| _ => acc

		fun has_dup xs = case xs of
			[] => false
			| y::ys => if (List.exists (fn x => x = y) ys) then true else has_dup(ys)
	in
		not (has_dup (get_vars pat []))
	end

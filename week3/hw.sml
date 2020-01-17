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

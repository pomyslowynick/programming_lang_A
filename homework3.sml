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

fun only_capitals(xs) =
    List.filter (fn y => Char.isUpper(String.sub (y, 0))) xs

val test1 = only_capitals [] = ["Adela","C"]

fun longest_string1 xs =
    case xs of
        [] => ""
        | xs' => foldl (fn (x,y) => if String.size x > String.size y then x else y) "" xs'

val test2 = longest_string1 ["A","b","Cc"] 

fun longest_string2 xs = 
    case xs of
        [] => ""
        | xs' => foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" xs'

val test3 = longest_string2 ["Aaa","bcc","Ccc"]

fun longest_string_helper f xs =
    case xs of
            [] => ""
            | xs' => foldl (fn(x,y) => if f(String.size x,String.size y) then x else y) "" xs'

fun longest_string3 xs =
    let val partial = longest_string_helper(fn(x,y) => x > y)
    in partial xs
    end

fun longest_string4 xs = 
    let val partial = longest_string_helper((fn(x,y) => x >= y))
    in partial xs
    end

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

fun longest_capitalized xs = 
    let val capitalized = longest_string1 o only_capitals
    in capitalized xs
    end


val test5 = longest_capitalized [] 

fun rev_string str =
    let val reverse = implode o rev o explode
    in reverse str
    end


val test6 = rev_string "Homer Simpson" 

fun first_answer some_fn xs =
    case xs of
        [] => raise NoAnswer
        | x::xs' => (case some_fn x of
                    SOME x =>  x
                    | NONE => first_answer some_fn xs')

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,1,52,1,2,1] 

fun all_answers some_fn xs =
    let fun aux some_fn accum xs=
            case xs of
                [] => SOME accum
                | x::xs' => (case some_fn x of
                            NONE => NONE
                            | SOME x => aux some_fn (accum @ x) xs')
    in aux some_fn [] xs
    end


val test8 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6] 

(* val test101 = fn => g (fn () => 1) (fn x => 0) *)
fun count_wildcards p = 
    g (fn v => 1) (fn v => 0) p 


val newss = count_wildcards(TupleP [ConstP 1])
val test9a = count_wildcards (TupleP[Wildcard,Wildcard])

fun count_wild_and_variable_lengths p =
    g (fn x => 1) (fn x => String.size x) p

val test9b = count_wild_and_variable_lengths (TupleP[Variable "aaaaaaa", Wildcard,Wildcard])

fun count_some_var (str,p) =
    g (fn x => 0) (fn x => if x = str then 1 else 0 ) p

val test9c = count_some_var ("x", TupleP[Variable("x"), Variable("x"), Variable("x"), Variable("x")])

fun check_pat p =   
    let val list_str = fn accu => g (fn () => []) (fn x => accu @ x) p;
        fun check_distinct strs =
            case strs of
                [] => true
                | x::xs => List.foldl (fn x y => List.exists x y ) [] xs

    in check_distinct(list_str)
    end
val test10 = check_pat (TupleP[Variable("x"), Variable("x"), Variable("x"), Variable("x")]) = true
(* 






val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME [] 
*)

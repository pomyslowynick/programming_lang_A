(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(some_str, some_str_lst) = 
    case some_str_lst of
        [] => NONE
        | (x::xs) => if same_string(x, some_str)
                    then SOME xs
                    else  case all_except_option(some_str,xs) of
                            NONE => NONE
                            | SOME xs' => SOME (x::xs')

fun get_substitutions1(substitutions, s) =
    case substitutions of 
        [] => []
        | (x::xs) => case all_except_option(s, x) of 
                            NONE => get_substitutions1(xs,s)
                            | SOME x => x @ get_substitutions1(xs,s)

fun get_substitutions2(substitutions, s) =
    let fun aux(substitutions, s, accum) =
        case substitutions of
        [] => accum
        | x::xs => case all_except_option(s,x) of
                        NONE => aux(xs,s,accum)
                        | SOME x => aux(xs,s,x@accum)
    in
        aux(substitutions,s,[])
    end

fun more_names(names_lst,full_name) =
    case (names_lst,full_name) of
        ([],y) => []
        | (x::xs,{first=a,middle=b,last=c}) => [{first=x,middle=b,last=c}] @ more_names(xs,full_name)
(* *)
fun similar_names(substitutions, full_name) =
    let fun aux(substitutions, full_name, accum) =
        case full_name of 
            {first=a,middle=b,last=c} => case get_substitutions2(substitutions,a) of
                                            [] => [{first=a,middle=b,last=c}]
                                            | xs => more_names(a::xs,full_name)
            
    in
        aux(substitutions, full_name,[])
    end
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(card, num) =
    case card of
        Clubs => Black
        |Spades => Black
        | Hearts => Red
        | Diamonds => Red

fun card_value(card, num) =
    case num of 
        Num x => x
        | Ace => 11
        | _ => 10

fun remove_card(cs,c,e) =
    let val remov_check = []
    in
        case cs of
                [] => raise e
                | x::xs =>  case x = c of
                            true => xs
                            | false => x::remove_card(xs,c,e)
    end

fun all_same_color(list_cards) =
    case list_cards of 
        [] => true
        | _::[] => true
        | x::(y::xs) => (card_color(x) = card_color(y) andalso all_same_color(y::xs))

fun sum_cards(list_cards) =
    let fun aux(cards, accum) =
            case cards of
                [] => accum
                | x::xs => aux(xs, accum + card_value(x))
    in 
        aux(list_cards, 0)
    end

fun score(card_list,goal) = 
    let val sum_hand = sum_cards(card_list)
        val is_same_color = all_same_color(card_list)
    in
    case card_list of 
        [] => goal
        | xs => case (sum_hand > goal) of
                    true => (case is_same_color of
                                true => 3 * (sum_hand - goal) div 2
                                |false => 3 * (sum_hand - goal))
                    | false => (case is_same_color of
                                false => (goal - sum_hand)
                                | true => (goal - sum_hand) div 2)
    end

fun officiate(card_list, move_list, goal) =
    let fun aux(held_cards, move_list_aux, card_list_aux, goal_aux) =
            case move_list_aux of 
                [] => score(held_cards, goal_aux)
                | Discard x::xs => aux(remove_card(held_cards, x, IllegalMove), xs, card_list_aux, goal_aux)
                | Draw z::zs => case card_list_aux of
                              [] => score(held_cards, goal_aux)
                              | x::xs => case score(held_cards, goal_aux) > goal_aux of
                                        true => score(held_cards, goal_aux)
                                        | false => aux(x::held_cards, zs, remove_card(x), goal_aux)

    in
        aux([], move_list, card_list, goal)
    end

(* test *)
val test1 = all_except_option ("string", ["string"])
val tst102 = all_except_option("Fred",["Freddie","Fred","F"])
val testest = all_except_option("makaron",["maka","buraki","makaron","ser","gunwo","cukierki","pasta"])
val test2 = get_substitutions1([["maka","buraki"],["makaron","ser","gunwo"],["makaron","cukierki","pasta"]],"makaron")
val test2_1 = get_substitutions1 ([["foo"],["there","foo"]], "foo") 
val tst2_2 = get_substitutions2([["Fredrick","Fred"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
"Fred")
val test2_3 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff")
val test3_1 = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],
"Jeff")
val test3 = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") 

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"},
	     {first="F", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}]
val test4_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})
val test5 = card_color (Clubs, Num 2) = Black
val test6 = card_value (Clubs, Num 2) = 2
val test7 = remove_card ([(Clubs, Num 2),(Clubs, Num 2),(Clubs, Num 2),(Hearts, Ace),(Clubs, Num 2)], (Clubs, Num 2), IllegalMove)
val test8 = all_same_color [(Diamonds, Num 2),(Hearts, Ace), (Hearts, Ace)] = true
val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2), (Hearts, Ace), (Spades, Num 7)]
val test10 = score ([(Hearts, Num 2),(Clubs, Num 4), (Clubs, Num 2),(Clubs, Num 2), (Hearts, Ace), (Spades, Num 7)],10)
val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
(* 





val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true) *)
(* tests *)







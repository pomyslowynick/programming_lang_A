(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (_, []) = NONE
|   all_except_option (x, head :: tail) =
        if same_string (x, head)
        then SOME tail
        else case all_except_option (x, tail) of
                NONE => NONE
            |   SOME res => SOME (head :: res)

fun get_substitutions1 ([], _) = []
|   get_substitutions1 (head :: tail, str) =
        case all_except_option (str, head) of
                NONE => get_substitutions1 (tail, str)
            |   SOME res => res @ get_substitutions1 (tail, str)

fun get_substitutions2 (lst, str) =
    let fun aux ([], _, acc) = acc
        |   aux (head :: tail, str, acc) =
                case all_except_option(str, head) of
                    NONE => aux (tail, str, acc)
                |   SOME res => aux (tail, str, acc @ res)
                    (* acc gets copied every time to maintain same older as get_substitutions1, but has worse performance *)
    in
        aux (lst, str, [])
    end

fun similar_names (subs, full_name) =
    let
        val {first=first, middle=middle, last=last} = full_name
        fun make_names ([]) = []
        |   make_names (head :: tail) = {first=head, middle=middle, last=last} :: make_names tail
    in
        full_name :: make_names (get_substitutions2(subs, first))
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
fun card_color (suit, _) =
    case suit of
        Clubs => Black
    |   Spades => Black
    |   Diamonds => Red
    |   Hearts => Red

fun card_value (_, rank) =
    case rank of
        Num i => i
    |   Ace => 11
    |   Jack => 10
    |   Queen => 10
    |   King => 10

fun remove_card ([], c, e) = raise e
|   remove_card (head :: tail, c, e) =
    if (head = c)
    then tail
    else head :: remove_card (tail, c, e)

fun all_same_color ([]) = true
|   all_same_color ([_]) = true
|   all_same_color (card1 :: card2 :: tail) =
        if card_color card1 = card_color card2
        then all_same_color (card2 :: tail)
        else false

fun sum_cards (cards) =
    let fun sum ([], acc) = acc
        |   sum (head :: tail, acc) = sum (tail, acc + card_value head)
    in
        sum (cards, 0)
    end

fun score (cards, goal) =
    let
        val sum = sum_cards cards
        val prelim_score = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        if all_same_color cards
        then prelim_score div 2
        else prelim_score
    end

fun officiate (cards, moves, goal) =
    let fun process (deck_cards, held_cards, moves, goal) =
            case (moves, deck_cards) of
                ([], _) => score (held_cards, goal)
            |   (Discard card :: moves_tail, _) =>
                    process (deck_cards, remove_card (held_cards, card, IllegalMove), moves_tail, goal)
            |   (Draw :: moves_tail, []) => score (held_cards, goal)
            |   (Draw :: moves_tail, card :: deck_tail) =>
                    let val new_hand = card::held_cards
                    in
                        if sum_cards new_hand > goal
                        then score (new_hand, goal)
                        else process (deck_tail, new_hand, moves_tail, goal)
                    end

    in
        process (cards, [], moves, goal)
    end

    val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Freddie", last="Smith", middle="W"},
	     {first="F", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}]
val test4_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"})
val test5 = card_color (Clubs, Num 2) = Black
val test6 = card_value (Clubs, Num 2) = 2
val test11_2 = officiate([(Hearts, Num 2),(Clubs, Num 4),(Diamonds, Num 2),(Hearts, Ace)],[Draw, Draw, Draw, Draw, Discard(Clubs, Num 4)], 20)
val test7 = remove_card ([(Clubs, Num 2),(Clubs, Num 2),(Clubs, Num 2),(Hearts, Ace),(Clubs, Num 2)], (Clubs, Num 2), IllegalMove)
val test8 = all_same_color [(Diamonds, Num 2),(Hearts, Ace), (Hearts, Ace)] = true
val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2), (Hearts, Ace), (Spades, Num 7)]
val test10 = score ([(Hearts, Num 2),(Clubs, Num 4), (Clubs, Num 2),(Clubs, Num 2), (Hearts, Ace), (Spades, Num 7)],10)
val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15)

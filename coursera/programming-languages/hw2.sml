(* Homework #2 - codgician *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (x, xs) =
   case xs of
        [] => NONE
      | x'::xs' =>  if same_string (x, x')
                    then SOME (xs')
                    else case all_except_option (x, xs') of
                            NONE => NONE
                          | SOME xs'' => SOME (x'::xs'')

fun get_substitutions1 (xss, x) =
    case xss of
        [] => []
      | xs::xss' => let
                        val xss'' = get_substitutions1 (xss', x)
                    in
                        case all_except_option (x, xs) of
                            NONE => xss''
                          | SOME xs' => xs' @ xss''
                    end

fun get_substitutions2 (xss, x) =
    let
        fun helper (xss, ans) =
            case xss of
                [] => ans
              | xs::xss' => case all_except_option (x, xs) of
                                NONE => helper (xss', ans)
                              | SOME xs' => helper (xss', ans @ xs')
    in
        helper (xss, [])
    end

fun similar_names (xss, {first = fst, middle = mid, last = lst}) =
    let 
        val subs = (fst)::get_substitutions2 (xss, fst)
        fun helper (firsts, ans) =
            case firsts of
                [] => ans
              | x::xs => helper (xs,
                        {first = x, middle = mid, last = lst}::ans)
    in
        helper (subs, [])
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
      | Spades => Black
      |  _ => Red

fun card_value (_, rank) =
   case rank of
      Num x => x
   |  Ace => 11
   |  _ => 10

fun remove_card (cards, card, e) =
    case cards of
        [] => raise e
      | c::cs => if c = card
                 then cs
                 else c::remove_card (cs, c, e)

fun all_same_color (cards) =
    case cards of
        c1::c2::cs => (card_color (c1) = card_color (c2))
                        andalso all_same_color (c2::cs)
      | _ => true

fun sum_cards (cards) =
    let
        fun helper (cards, acc) =
            case cards of
                [] => acc
              | c::cs => helper (cs, card_value (c) + acc)
    in
        helper (cards, 0)
    end

fun score (cards, goal) =
    let
        val sum = sum_cards (cards)
        val pre_score = if sum > goal
                        then 3 * (sum - goal)
                        else goal - sum
    in
        if all_same_color (cards)
        then pre_score div 2
        else pre_score
    end

fun officiate (cards, moves, goal) =
    let
        fun helper (cards, moves, held) =
            if sum_cards (held) > goal
            then score (held, goal)
            else case (moves, cards) of
                    (Discard c::ms, _) => helper (cards, ms,
                                        remove_card (held, c, IllegalMove))
                  | (Draw::ms, c::cs) => helper (cs, ms, c::held)
                  | _ => score (held, goal)
    in
        helper (cards, moves, [])
    end

(* Challenges *)

fun count_ace (cards, acc) =
    case cards of
        [] => acc
      | (_, Ace)::cs => count_ace (cs, 1 + acc)
      | _::cs => count_ace (cs, acc)

fun score_challenge (cards, goal) =
    let
        fun pre_score (sum, goal) =
            if sum > goal
            then 3 * (sum - goal)
            else goal - sum
        fun min_pre_score (sum, ace_num, goal) =
            if ace_num = 0
            then pre_score (sum, goal)
            else Int.min (pre_score (sum, goal),
                        min_pre_score (sum - 10, ace_num - 1, goal))
        val cur_pre_score = min_pre_score (sum_cards (cards),
                            count_ace (cards, 0), goal)
    in
        if all_same_color (cards)
        then cur_pre_score div 2
        else cur_pre_score
    end

fun officiate_challenge (cards, moves, goal) =
    let
        fun min_sum_cards (cards) =
            sum_cards (cards) - 10 * count_ace (cards, 0)
        fun helper (cards, moves, held) =
            if min_sum_cards (held) > goal
            then score_challenge (held, goal)
            else case (moves, cards) of
                    (Discard c::ms, _) => helper (cards, ms,
                                        remove_card (held, c, IllegalMove))
                  | (Draw::ms, c::cs) => helper (cs, ms, c::held)
                  | _ => score_challenge (held, goal)
    in
        helper (cards, moves, [])
    end

fun careful_player (cards, goal) =
    let
        fun reverse (xs, ans) =
            case xs of
                [] => ans
              | x::xs => reverse (xs, x::ans)
        fun find_card (cards, value) =
            case cards of
                [] => NONE
              | c::cs => if card_value (c) = value
                         then SOME c
                         else find_card (cs, value)
        fun helper (cards, held, moves) =
            if score (held, goal) = 0
            then reverse (moves, [])
            else case cards of
                    [] => reverse (Draw::moves, [])
                  | c::cs =>
                    let
                        val held_sum = sum_cards (held)
                        val expected_value =
                            held_sum + card_value (c) - goal
                    in
                        if held_sum + 10 < goal
                        then helper (cs, c::held, Draw::moves)
                        else case find_card (held, expected_value) of
                                NONE => reverse (moves, [])
                              | SOME dc => reverse (Draw::Discard dc::moves, [])
                    end
    in
        helper (cards, [], [])
    end

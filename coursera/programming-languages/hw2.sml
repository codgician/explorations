(* Homework 2 - codgician *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (x, xs) = 
   case xs of
      [] => NONE
   |  x'::xs' => if same_string (x, x')
                then SOME (xs') 
                else case all_except_option (x, xs') of
                        NONE => NONE
                     |  SOME xs'' => SOME (x'::xs'')

fun get_substitutions1 (xss, x) = 
   case xss of
      [] => []
   |  xs::xss' => let 
                     val xss'' = get_substitutions1 (xss', x) 
                  in 
                     case all_except_option (x, xs) of
                        NONE => xss''
                     |  SOME xs' => xs' @ xss''
                  end

fun get_substitutions2 (xss, x) = 
   let fun helper (xss, ans) =
      case xss of
         [] => ans
      |  xs::xss' => case all_except_option (x, xs) of
                        NONE => helper (xss', ans)
                     |  SOME xs' => helper (xss', ans @ xs')
   in 
      helper (xss, []) 
   end

fun similar_names (xss, {first = fst, middle = mid, last = lst}) =
      let 
         val subs = (fst)::get_substitutions2 (xss, fst)
         fun helper (firsts, ans) = 
            case firsts of
               [] => ans
            |  x::xs => helper (xs, 
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
fun card_color (c) =
   case c of
      (Clubs, _) => Black
   |  (Spades, _) => Black
   |  _ => Red

fun card_value (c) = 
   case c of
      (_, Num x) => x
   |  (_, Ace) => 11
   |  _ => 10

fun remove_card (cards, card, e) =
   case cards of
      [] => raise e
   |  c::cs => if c = card
               then cs
               else c::remove_card (cs, c, e)  

fun all_same_color (cards) = 
   case cards of
      c1::c2::cs => (card_color (c1) = card_color (c2)) 
                     andalso all_same_color (c2::cs)
   |  _ => true 

fun sum_cards (cards) = 
   let
      fun helper (cards, acc) = 
         case cards of
            [] => acc
         |  c::cs => helper (cs, card_value (c) + acc)
   in
      helper (cards, 0)
   end

fun score (cards, goal) =
   let 
      val sum = sum_cards (cards)
      val pre_score =   if sum > goal
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
         case moves of
            [] => score (held, goal)
         |  m::ms => case m of
                        Draw => (case cards of
                                    [] => score (held, goal)
                                 |  c::cs => if sum_cards (c::held) > goal
                                             then score (c::held, goal)
                                             else helper (cs, ms, c::held))
                     |  Discard c => helper (cards, ms, 
                                    remove_card (held, c, IllegalMove))
   in
      helper (cards, moves, [])
   end

(* To-do: Challenges *)

(* Homework #3 - codgician *)

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
        Wildcard            => f1 ()
      | Variable x          => f2 x
      | TupleP ps           => List.foldl (fn (p,i) => (r p) + i) 0 ps
      | ConstructorP (_,p)  => r p
      | _                   => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
             | UnitT
             | IntT
             | TupleT of typ list
             | Datatype of string

(**** you can put all your code here ****)

fun only_capitals s =
    List.filter (fn s => Char.isUpper (String.sub (s, 0))) s

val longest_string1 =
    List.foldl (fn (x, a) => if size x > size a then x else a) ""

val longest_string2 =
    List.foldl (fn (x, a) => if size x >= size a then x else a) ""

fun longest_string_helper f =
    List.foldl (fn (x, a) => if f (size x, size a) then x else a) ""

val longest_string3 = longest_string_helper op>

val longest_string4 = longest_string_helper op>=

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f xs =
    case List.mapPartial f xs of
        [] => raise NoAnswer
      |	x::_ => x

fun all_answers f = List.foldl (fn (x, a) =>
    case (x, a) of
        (SOME x, SOME a) => SOME (a @ x)
      | _ => NONE
    ) (SOME []) o (List.map f)

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) String.size

fun count_some_var (x, p) = g (fn _ => 0) (fn x' => if x' = x then 1 else 0) p

val check_pat =
    let
        fun extract p =
            case p of
                Variable s => [s]
              | TupleP ps => List.foldl (fn (p, a) => extract p @ a) [] ps
              | ConstructorP (_, p) => extract p
              | _ => []
        fun check_sorted [] = true
          | check_sorted (x::xs) = #1 (List.foldl (fn (x, (ans, last)) =>
                                (x <> last andalso ans, x)) (true, x) xs)
    in
        check_sorted o ListMergeSort.sort op> o extract
    end

fun match (v, p) =
    case (v, p) of
        (_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const v, ConstP x) => 
            if v = x then SOME [] else NONE
      | (Tuple vs, TupleP ps) =>
            if length vs = length ps
            then (all_answers match o ListPair.zip) (vs, ps)
            else NONE
      | (Constructor (s, v), ConstructorP (s', p)) =>
            if s = s' then match (v, p) else NONE
      | _ => NONE

fun first_match v ps = SOME (first_answer (fn p => match (v, p)) ps)
                       handle NoAnswer => NONE

(* Challenges *)

fun typecheck_patterns (datatypes, patterns) =
    let
        fun opt_bind f (SOME x) = f x
          | opt_bind f NONE = NONE
        (* all_answers': slightly modified version of all_answers,
            where f returns 'a option instead of 'a list option *)
        fun all_answers' f =
            Option.map List.rev
            o List.foldl (fn (x, a) =>
                case (x, a) of
                    (SOME x, SOME a) => SOME (x::a)
                    | _ => NONE
            ) (SOME []) o (List.map f)
        (* type_matches (t1, t2): check if t1 and t2 matches *)
        fun type_matches (Anything, _) = true
          | type_matches (_, Anything) = true
          | type_matches (TupleT ts1, TupleT ts2) =
                (List.foldl (fn (x, a) => x andalso a) true
                o List.map type_matches o ListPair.zip) (ts1, ts2)
          | type_matches (t1, t2) = t1 = t2
        (* common (t1, t2): get the common type of types t1 and t2 *)
        fun common (Anything, t) = SOME t
          | common (t, Anything) = SOME t
          | common (TupleT ts1, TupleT ts2) =
                if length ts1 = length ts2
                then (Option.map TupleT o all_answers' common
                    o ListPair.zip) (ts1, ts2)
                else NONE
          | common (t1, t2) = if t1 = t2 then SOME t1 else NONE
        (* get_type p: get the most lenient type of pattern p *)
        fun get_type p =
            case p of
                Wildcard => SOME Anything
              | Variable s => SOME Anything
              | UnitP => SOME UnitT
              | ConstP x => SOME IntT
              | TupleP ps => (Option.map TupleT o all_answers' get_type) ps
              | ConstructorP (s, p) =>
                    case (List.filter (fn (c, _, _) => c = s) datatypes, get_type p) of
                        ((c, dt, at)::_, SOME t) =>
                            if type_matches (at, t)
                            then SOME (Datatype dt)
                            else NONE
                      | _ => NONE
    in
        (opt_bind (List.foldl (fn (x, a) =>
            opt_bind (fn x' => common (x, x')) a) (SOME Anything))
        o (all_answers' get_type)) patterns
    end

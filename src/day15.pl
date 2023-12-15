:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(lambda)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(aoc).

solve(15) :-
    phrase_from_file(parse(Seq), "input/day15.txt"),
    maplist(step_chars, Seq, Chrs),
    maplist(hash, Chrs, Hashes),
    sum(Hashes, #=, Sol1),
    format("part 1: ~d~n", [Sol1]),

    boxes_init(B0),
    foldl(step_apply, Seq, B0, B),
    boxes_power(B, Sol2),
    format("part 2: ~d~n", [Sol2]).

hash(S, H) :- hash(S, 0, H).
hash([], H, H).
hash([X|T], H0, H) :-
    char_code(X, XC),
    H1 #= (17 * (H0 + XC) rem 256),
    hash(T, H1, H).

boxes_init(B) :- empty_assoc(B).

step_apply(eq(L, X), B0, B) :-
    hash(L, Cd),
    (   get_assoc(Cd, B0, Box0)
    ->  (   append([Hd, [lens(L,_)], Tl], Box0)
        ->  append([Hd, [lens(L,X)], Tl], Box)
        ;   append([Box0, [lens(L,X)]], Box)
        )
    ;   Box = [lens(L,X)]
    ),
    put_assoc(Cd, B0, Box, B).
step_apply(rm(L), B0, B) :-
    hash(L, Cd),
    (   get_assoc(Cd, B0, Box0)
    ->  (   select(lens(L,_), Box0, Box)
        ->  true
        ;   Box = Box0
        ),
        put_assoc(Cd, B0, Box, B)
    ;   B = B0
    ).

boxes_power(Boxes, Power) :-
    assoc_to_list(Boxes, Pairs),
    maplist(box_power, Pairs, Ps),
    sum(Ps, #=, Power).
box_power(Cd-Box, Power) :-
    PBox #= Cd + 1,
    foldl(lenses_power(PBox), Box, f(1,0), f(_,Power)).
lenses_power(PBox, lens(_, FLen), f(PSlot0, C0), f(PSlot, C)) :-
    P #= PBox * PSlot0 * FLen,
    PSlot #= PSlot0 + 1,
    C #= C0 + P.

parse([S|Ss]) -->
    parse_step(S), ",", parse(Ss).
parse([S]) -->
    parse_step(S), "\n".
parse_step(eq(L, X)) -->
    parse_label(L), "=", [XCh],
    {   char_type(XCh, decimal_digit),
        number_chars(X, [XCh])
    }.
parse_step(rm(L)) -->
    parse_label(L), "-".
parse_label([H|T]) --> [H], { [H] \= "," }, parse_label(T).
parse_label([]) --> [].

step_chars(eq(L, X), Ch) :- number_chars(X, XCh), append([L, "=", XCh], Ch).
step_chars(rm(L), Ch) :- append([L, "-"], Ch).

:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(aoc).

solve(1, X) :-
    phrase_from_file(parse(Cards), "input/day04.txt"),
    maplist(card_points, Cards, Points),
    sum_list(Points, X).
solve(2, X) :-
    phrase_from_file(parse(Cards), "input/day04.txt"),
    length(Cards, Len),
    list_init(1, Len, Copies0),
    foldl(card_copies_fold, Cards, Copies0, Copies),
    sum_list(Copies, X).

card_points(card(_, W, Ns), P) :- card_points_(Ns, W, 0, P).
card_points_([], _, P, P).
card_points_([N | Ns], W, Acc0, P) :-
    memberchk(N, W),
    Acc #= max(2 * Acc0, 1),
    card_points_(Ns, W, Acc, P).
card_points_([N | Ns], W, Acc, P) :-
    \+memberchk(N, W),
    card_points_(Ns, W, Acc, P).

card_copies_fold(Card, Copies0, Copies) :-
    card_wincount(Card, Count),
    card_id(Card, ID),
    nth1(ID, Copies0, Add),
    copies_add_fold(Count, ID, Add, Copies0, Copies).

copies_add_fold(0, _, _, Copies, Copies).
copies_add_fold(N0, ID0, Add, Copies0, Copies) :-
    N0 #> 0,
    ID #= ID0 + N0,
    copies_id_add(Copies0, ID, Add, Copies1),
    N #= N0 - 1,
    copies_add_fold(N, ID0, Add, Copies1, Copies).

copies_id_add(Copies0, ID, Add, Copies) :-
    nth1(ID, Copies0, Copy0, Copies1),
    Copy #= Copy0 + Add,
    nth1(ID, Copies, Copy, Copies1).

list_init(Elt, Len, Ls) :- list_init_(Elt, Len, [], Ls).
list_init_(_, 0, Ls, Ls).
list_init_(Elt, Len0, Acc, Ls) :-
    Len0 #> 0,
    Len #= Len0 - 1,
    list_init_(Elt, Len, [Elt | Acc], Ls).

card_id(card(ID, _, _), ID).

card_wincount(card(_, W, Ns), C) :- card_wincount_(Ns, W, 0, C).
card_wincount_([], _, C, C).
card_wincount_([N | Ns], W, Acc0, C) :-
    (   memberchk(N, W)   -> Acc #= Acc0 + 1
    ;   \+memberchk(N, W) -> Acc #= Acc0
    ),
    card_wincount_(Ns, W, Acc, C).

parse([]) --> [].
parse([card(ID, W, N) | Rest]) -->
    "Card", ws, parse_number(ID), ": ",
    parse_numbers(W), " | ", parse_numbers(N),
    "\n", parse(Rest).
parse_numbers([]) --> [].
parse_numbers([N | Ns]) -->
    ws, parse_number(N), parse_numbers(Ns).

parse_number(N) --> digits(Ch), { number_chars(N, Ch) }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).
digit(D) --> [D], { char_type(D, decimal_digit) }.
ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].
%Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53

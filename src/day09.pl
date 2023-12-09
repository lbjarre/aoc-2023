:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(library(dif)).
:- use_module(aoc).

solve(1, X) :-
    phrase_from_file(parse(Hs), "input/day09.txt"),
    maplist(predict_next, Hs, Ns),
    sum(Ns, #=, X).
solve(2, X) :-
    phrase_from_file(parse(Hs), "input/day09.txt"),
    maplist(predict_prev, Hs, Ns),
    sum(Ns, #=, X).

predict_next(Hs, N) :-
    all_deltas(Hs, Ds),
    maplist(list_last, Ds, D),
    foldl(add, D, 0, N).
predict_prev(Hs, N) :-
    all_deltas(Hs, Ds),
    maplist(list_first, Ds, D),
    foldl(sub, D, 0, N).
add(D, V0, V) :- V #= V0 + D.
sub(D, V0, V) :- V #= D - V0.

all_deltas(Ls, Deltas) :- all_deltas(Ls, [Ls], Deltas).
all_deltas(Ls, Acc, Deltas) :-
    list_deltas(Ls, Ds),
    (   all_zeros(Ds)
    ->  Deltas = Acc
    ;   all_deltas(Ds, [Ds | Acc], Deltas)
    ).

list_deltas(Ls, Ds) :- list_deltas(Ls, [], Ds).
list_deltas([_], Acc, Ds) :- reverse(Acc, Ds).
list_deltas([V1, V2 | T], Acc, Ds) :-
    D #= V2 - V1,
    list_deltas([V2 | T], [D | Acc], Ds).

all_zeros([0]).
all_zeros([0 | T]) :- all_zeros(T).

list_last(Ls, X) :- reverse(Ls, [X|_]).
list_first([X|_], X).

parse([]) --> [].
parse([H|T]) --> parse_line(H), "\n", parse(T).
parse_line([H|T]) --> parse_number(H), " ", parse_line(T).
parse_line([H])   --> parse_number(H).
negation(-1) --> "-".
negation(1) --> "".
parse_number(N) -->
    negation(Neg), digits(Ch),
    { number_chars(N0, Ch), N #= Neg * N0 }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).
digit(D) --> [D], { char_type(D, decimal_digit) }.

:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(aoc).

solve(13) :-
    phrase_from_file(parse(Gs), "input/day13.txt"),
    maplist(reflection(used), Gs, Rs),
    sum(Rs, #=, Sol1),
    format("part 1: ~d~n", [Sol1]),
    maplist(reflection(avail), Gs, Rs2),
    sum(Rs2, #=, Sol2),
    format("part 2: ~d~n", [Sol2]).

reflection(Flip, G, N) :- N #= 100 * X,     reflection_length(Flip, G, X).
reflection(Flip, G, N) :- transpose(G, GT), reflection_length(Flip, GT, N).

reflection_length(Flip, G, N) :-
    nonempty(R1), nonempty(R2), append(R1, R2, G),
    reverse(R1, RR1),
    eql(Flip, RR1, R2),
    length(R1, N).

eql(Flip, [R|R1], [R|R2]) :- eql(Flip, R1, R2).
eql(avail, [X1|R1], [X2|R2]) :- one_dif(avail, X1, X2), eql(used, R1, R2).
eql(used, [], _).
eql(used, _, []).

one_dif(Flip, [X|Xs], [X|Ys]) :- one_dif(Flip, Xs, Ys).
one_dif(avail, [X|Xs], [Y|Ys]) :- X \= Y, one_dif(used, Xs, Ys).
one_dif(used, [], []).

parse([G|Gs]) --> parse_grid(G), "\n", parse(Gs).
parse([G]) --> parse_grid(G).
parse_grid([R|Rs]) --> parse_row(R), { nonempty(R) }, parse_grid(Rs).
parse_grid([]) --> [].
parse_row([C|Cs]) --> parse_cell(C), parse_row(Cs).
parse_row([]) --> "\n".
parse_cell(C) --> [C], { C \= '\n' }.

nonempty([_|_]).

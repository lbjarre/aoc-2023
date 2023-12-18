:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(library(time)).
:- use_module(aoc).

solve(18) :-
    phrase_from_file(parse(Instr), "input/day18.txt"),
    area(Instr, S1),
    format("part 1: ~d~n", [S1]),
    maplist(correct_instr, Instr, Corrected),
    area(Corrected, S2),
    format("part 2: ~d~n", [S2]).

% Area calculation using Pick's theorem
% https://en.wikipedia.org/wiki/Pick%27s_theorem
%
% Area = #Interior + (#Perimiter / 2) + 1
%
area(Instr, S) :-
    instructions_points(Instr, Points),
    shoelace(Points, Interior),
    instructions_perimiter(Instr, Perimiter),
    S #= Interior + (Perimiter div 2) + 1.

% Interior area calculation using the Shoelace formula
% https://en.wikipedia.org/wiki/Shoelace_formula
shoelace(Points, Area) :-
    shoelace_(Points, 0, Shoelace),
    Area #= abs(Shoelace) div 2.
shoelace_([[X0,Y0], [X1,Y1] | Rest], Acc0, Area) :-
    Acc #= Acc0 + (X0 * Y1) - (Y0 * X1),
    shoelace_([[X1,Y1]|Rest], Acc, Area).
shoelace_([_], Area, Area).

instructions_points(Instr, Points) :-
    foldl(instructions_points_, Instr, st([0,0], [[0,0]]), st(_, Points)).
instructions_points_(i(Dir, Count, _), st(Pos0, Points0), st(Pos, [Pos|Points0])) :-
    walk(Dir, Pos0, Count, Pos).
instructions_points_(i(Dir, Count), st(Pos0, Points0), st(Pos, [Pos|Points0])) :-
    walk(Dir, Pos0, Count, Pos).

walk([Dx,Dy], [X0,Y0], N, [X,Y]) :-
    X #= X0 + N * Dx,
    Y #= Y0 + N * Dy.

instructions_perimiter(Instr, Perim) :-
    maplist(counts_, Instr, Counts),
    sum(Counts, #=, Perim).
counts_(i(_, Count, _), Count).
counts_(i(_, Count), Count).

correct_instr(i(_, _, Hex), i(Dir, C)) :-
    length(CCh, 5),
    append(CCh, [DCh], Hex),
    hex_direction_char(Dir, DCh),
    hex_number(CCh, C).

hex_direction_char([1,0], '0').
hex_direction_char([0,1], '1').
hex_direction_char([-1,0], '2').
hex_direction_char([0,-1], '3').

hex_number(Hex, N) :-
    reverse(Hex, Rev),
    hex_number_(Rev, 0, 0, N).
hex_number_([], _, N, N).
hex_number_([H|T], Pl0, A0, N) :-
    hex_digit_(H, Digit),
    A #= A0 + Digit * (16 ^ Pl0),
    Pl #= Pl0 + 1,
    hex_number_(T, Pl, A, N).
hex_digit_('0', 0).
hex_digit_('1', 1).
hex_digit_('2', 2).
hex_digit_('3', 3).
hex_digit_('4', 4).
hex_digit_('5', 5).
hex_digit_('6', 6).
hex_digit_('7', 7).
hex_digit_('8', 8).
hex_digit_('9', 9).
hex_digit_('a', 10).
hex_digit_('b', 11).
hex_digit_('c', 12).
hex_digit_('d', 13).
hex_digit_('e', 14).
hex_digit_('f', 15).

direction_char([0,-1], 'U').
direction_char([0,1], 'D').
direction_char([-1,0], 'L').
direction_char([1,0], 'R').

parse([]) --> [].
parse([I|Rest]) --> instruction(I), "\n", parse(Rest).
instruction(i(Dir, N, Hex)) -->
    [DirCh], { direction_char(Dir, DirCh) },
    " ", digits(NCh), " ", { number_chars(N, NCh) },
    "(#", seq(Hex), ")", { length(Hex, 6) }.

digit(D) --> [D], { char_type(D, decimal_digit) }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).

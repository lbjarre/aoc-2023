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
    part2(Instr, S2),
    format("part 2: ~d~n", [S2]).
    % part1(Instr, S1),
    % format("part 1: ~d~n", [S1]).
part1(Instr, S) :-
    dig_perimiter(Instr, st(_, Perimiter)),
    % starting point picked at random, don't know if there is a better way of
    % finding a point that is known to be inside the perimiter.
    flood_fill(Perimiter, [0,-1], Filled),
    maplist(assoc_size, [Perimiter, Filled], Sizes),
    sum(Sizes, #=, S).
part2(Instr, S) :-
    maplist(correct_instr, Instr, InstrCorr),
    dig_perimiter(InstrCorr, st(_, Perimiter)),
    assoc_size(Perimiter, S).

dig_perimiter(Instr, St) :-
    empty_assoc(Digged0),
    foldl(dig_perimiter_, Instr, st([0,0], Digged0), St).
dig_perimiter_(i(Dir, Count, _), st(Pos0, Digged0), st(Pos, Digged)) :-
    walk(Dir, Pos0, Count, Positions),
    foldl(fold_assoc(1), Positions, Digged0, Digged),
    reverse(Positions, [Pos|_]).
dig_perimiter_(i(Dir, Count), st(Pos0, Digged0), st(Pos, Digged)) :-
    walk(Dir, Pos0, Count, Positions),
    foldl(fold_assoc(1), Positions, Digged0, Digged),
    reverse(Positions, [Pos|_]).

fold_assoc(V, K, A0, A) :-
    put_assoc(K, A0, V, A).
assoc_size(A, S) :-
    assoc_to_list(A, L), length(L, S).

flood_fill(Perimiter, Start, Filled) :-
    empty_assoc(Filled0),
    flood_fill(Perimiter, Start, Filled0, Filled).
flood_fill(Perimiter, Pos, Filled0, Filled) :-
    (   (   get_assoc(Pos, Perimiter, _)
        ;   get_assoc(Pos, Filled0, _)
        )
    ->  Filled = Filled0
    ;   put_assoc(Pos, Filled0, 1, Filled1),
        foldl(flood_fill_dir(Perimiter, Pos), [[0,-1],[0,1],[-1,0],[1,0]], Filled1, Filled)
    ).
flood_fill_dir(Perimiter, Pos0, Dir, Filled0, Filled) :-
    dir_pos_next(Dir, Pos0, Pos),
    flood_fill(Perimiter, Pos, Filled0, Filled).

walk(_, _, 0, []).
walk(Dir, Pos0, N0, [Pos|Rest]) :-
    N0 #> 0,
    N #= N0 - 1,
    dir_pos_next(Dir, Pos0, Pos),
    walk(Dir, Pos, N, Rest).
dir_pos_next([Dx,Dy], [X0,Y0], [X,Y]) :-
    X #= X0 + Dx,
    Y #= Y0 + Dy.

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

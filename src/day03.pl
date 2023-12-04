:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(aoc).

solve(1, X) :-
    phrase_from_file(parse_grid(Grid), "input/day03.txt"),
    findall(N, grid_number(Grid, N), Numbers),
    list_include(call(grid_is_partnum, Grid), Numbers, PartNums),
    maplist(num_number, PartNums, Ns),
    sum_list(Ns, X).
solve(2, X) :-
    phrase_from_file(parse_grid(Grid), "input/day03.txt"),
    grid_items(Grid, Numbers, Symbols),
    maplist(call(gear_ratio, Numbers), Symbols, Gears),
    sum_list(Gears, X).

grid_is_partnum(Grid, num(Start, Len, _)) :-
    grid_number(Grid, num(Start, Len, _)),
    start_len_coord(Start, Len, Co),
    coord_neighbor(Co, CoN),
    grid_coord_cell(Grid, CoN, Cell),
    is_symbol(Cell).

num_number(num(_, _, Num), Num).

grid_number(Grid, num(C-R, Len, Num)) :-
    nth0(R, Grid, Row),
    append(Leading, Rest, Row),
    length(Leading, C),
    \+list_last_pred(Leading, is_digit),
    append(Chars, Tail, Rest),
    length(Chars, Len),
    \+list_first_pred(Tail, is_digit),
    phrase(parse_number(Num), Chars).

list_first_pred([H | _], Pred) :- call(Pred, H).
list_last_pred(Ls, Pred) :- reverse(Ls, Rev), list_first_pred(Rev, Pred).

coord_neighbor(C0-R0, C-R) :-
    (   C #= C0 - 1, R #= R0
    ;   C #= C0 + 1, R #= R0
    ;   C #= C0    , R #= R0 - 1
    ;   C #= C0    , R #= R0 + 1
    ;   C #= C0 + 1, R #= R0 + 1
    ;   C #= C0 + 1, R #= R0 - 1
    ;   C #= C0 - 1, R #= R0 - 1
    ;   C #= C0 - 1, R #= R0 + 1
    ).

start_len_coord(C0-R, N, C-R) :-
    N #> 0, C #>= C0, C0 + N #> C.

grid_coord_cell(Grid, C-R, Cell) :-
    C #>= 0, R #>= 0,
    nth0(R, Grid, Row),
    nth0(C, Row, Cell).

parse_grid([]) --> [].
parse_grid([Row | Rows]) --> parse_line(Row), "\n", parse_grid(Rows).
parse_line([]) --> [].
parse_line([H | T]) --> [H], { H \= '\n' }, parse_line(T).

parse_number(N) --> digits(Ch), { number_chars(N, Ch) }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).
digit(D) --> [D], { is_digit(D) }.

is_digit(D) :- char_type(D, decimal_digit).
is_dot('.').
is_symbol(S) :- \+is_digit(S), \+is_dot(S).
incr(N0, N) :- N #= N0 + 1.
nonempty([_|_]).

grid_items(Grid, Numbers, Symbols) :- grid_items(Grid, 0, Numbers, Symbols).
grid_items([], _, [], []).
grid_items([Row | T], R0, Numbers, Symbols) :-
    row_items(Row, R0, 0, RNumbers, RSymbols),
    R #= R0 + 1,
    grid_items(T, R, TNumbers, TSymbols),
    append(RNumbers, TNumbers, Numbers),
    append(RSymbols, TSymbols, Symbols).
row_items([], _, _, [], []).
row_items(Ls, R, C0, [num(R, C0, N) | Rest], Symbols):-
    phrase(parse_number(N), Ls, Tail),
    \+list_first_pred(Tail, is_digit),
    number_len(N, NLen),
    C #= C0 + NLen,
    row_items(Tail, R, C, Rest, Symbols).
row_items(['.' | Tail], R, C0, Numbers, Symbols) :-
    C #= C0 + 1,
    row_items(Tail, R, C, Numbers, Symbols).
row_items([H | Tail], R, C0, Numbers, [sym(R, C0, H) | Rest]) :-
    is_symbol(H),
    C #= C0 + 1,
    row_items(Tail, R, C, Numbers, Rest).

gear_ratio(Numbers, Sym, Ratio) :-
    (   Sym = sym(_, _, '*'),
        list_include(call(number_is_adjacent, Sym), Numbers, Adj),
        Adj = [num(_,_,A), num(_,_,B)],
        Ratio #= A * B
    ;   Ratio #= 0
    ).

number_is_adjacent(sym(Ys, Xs, _), num(Yr, Xn0, Num)) :-
    adjacent(Ys, Xs, Yr, Xr),
    number_len(Num, L),
    Xr #>= Xn0,
    Xr #< Xn0 + L.

adjacent(Y, X, Ys, Xs) :-
    Ys #=< Y + 1,
    Ys #>= Y - 1,
    Xs #=< X + 1,
    Xs #>= X - 1.

num_coords(num(R, C, Num), Coords) :-
    number_len(Num, L),
    num_coords_(R, C, L, Coords).
num_coords_(_, _, 0, []).
num_coords_(R, C0, L0, [R-C0 | Rest]) :-
    L0 #>= 0,
    L #= L0 - 1,
    C #= C0 + 1,
    num_coords_(R, C, L, Rest).

number_len(N, L) :- number_chars(N, Ch), length(Ch, L).

% [cell(0, 0, dot), cell(1, 0, s('$')), cell(2, 0, d('2'))]

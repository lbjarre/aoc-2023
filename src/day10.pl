:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(library(dif)).
:- use_module(aoc).

solve(1, S) :-
    phrase_from_file(parse(0, 0, G), "input/day10.txt"),
    maplist(assert_cell, G),
    assert_grid_size(G),
    cell(X, Y, start),
    loop(X, Y, Loop),
    length(Loop, N),
    S #= N // 2.
solve(2, C) :-
    phrase_from_file(parse(0, 0, G), "input/day10.txt"),
    maplist(assert_cell, G),
    assert_grid_size(G),
    cell(X, Y, start),
    loop(X, Y, Loop),
    assert_loop(Loop),
    chk(C).

loop(X, Y, Loop) :- loop(X, Y, [], Loop), !.
loop(X, Y, Loop, Loop) :- cell(X, Y, start), nonempty(Loop), !.
loop(X, Y, [], Loop) :-
    neighbor(X, Y, Xc, Yc),
    loop(Xc, Yc, [[X,Y]], Loop).
loop(X, Y, [P|Acc], Loop) :-
    neighbor(X, Y, Xc, Yc),
    dif(P, [Xc,Yc]),
    loop(Xc, Yc, [[X,Y], P | Acc], Loop).

parse(_, _, []) --> [].
parse(_, Y0, M) -->
    "\n", { incr(Y0, Y) }, parse(0, Y, M).
parse(X0, Y, [c(X0, Y, ground) | M]) -->
    ".", { incr(X0, X) }, parse(X, Y, M).
parse(X0, Y, [c(X0, Y, start) | M]) -->
    "S", { incr(X0, X) }, parse(X, Y, M).
parse(X0, Y, [c(X0, Y, P) | M]) -->
    [C], { pipe_char(P, C), incr(X0, X) }, parse(X, Y, M).

isloop(C, y) :- loop(C).
isloop(C, n) :- \+loop(C).

chk(X) :-
    grid_coords(Cs),
    maplist(line_loop_intersections, Cs, Ints),
    maplist(count_enclosed, Cs, Ints, Counts),
    sum(Counts, #=, X).

count_enclosed(Cs, Ints, Count) :- count_enclosed(Cs, Ints, 0, Count).
count_enclosed([], [], C, C).
count_enclosed([Pos|PosT], [Int|IntT], C0, C) :-
    (   odd(Int), isloop(Pos, n)
    ->  incr(C0, C1)
    ;   C1 #= C0
    ),
    count_enclosed(PosT, IntT, C1, C).

line_loop_intersections(L, I) :- line_loop_intersections(L, s(0, no_seg), I).
line_loop_intersections([], _, []).
line_loop_intersections([P|T], s(C0, no_seg), Int) :-
    (   isloop(P, n)
    ->  C #= C0, S = s(C, no_seg)
    ;   P=[X,Y], cell(X, Y, p(D, e))
    ->  incr(C0, C), S = s(C, in_seg(D))
    ;   incr(C0, C), S = s(C, no_seg)
    ),
    line_loop_intersections(T, S, Int0),
    Int = [C|Int0].
line_loop_intersections([P|T], s(C0, in_seg(D)), Int) :-
    isloop(P, y), P=[X,Y], cell(X, Y, p(Dend, w)),
    (   Dend = e
    ->  C #= C0, S = s(C, in_seg(D))
    ;   dif(Dend, D)
    ->  C #= C0, S = s(C, no_seg)
    ;   incr(C0, C), S = s(C, no_seg)
    ),
    line_loop_intersections(T, S, Int0),
    Int = [C|Int0].

list_fill(Ls, N, V) :- length(Ls, N), phrase(fill(V), Ls).
fill(_) --> [].
fill(V) --> [V], fill(V).

list_count(Ls, N) :- length(Ls, N), phrase(count(0), Ls).
count(_) --> [].
count(N) --> [N], { incr(N, Nn) }, count(Nn).

grid_coords(C) :-
    grid_size(GX_, GY_), incr(GX_, GX), incr(GY_, GY),
    list_count(Xs, GX), list_count(Ys, GY),
    maplist(grid_coords_row(Xs), Ys, C).
grid_coords_row(Xs, Y, Row) :- maplist(coord(Y), Xs, Row).
coord(Y, X, [X,Y]).

assert_cell(c(X, Y, p(D1, D2))) :-
    assertz(cell(X, Y, p(D1, D2))),
    assertz(connects(X, Y, D1)),
    assertz(connects(X, Y, D2)).
assert_cell(c(X, Y, ground)) :-
    assertz(cell(X, Y, ground)).
assert_cell(c(X, Y, start)) :-
    assertz(cell(X, Y, start)),
    maplist(\D^(assertz(connects(X, Y, D))), [n, s, e, w]).
assert_grid_size(G) :-
    maplist(cell_x, G, Xs), list_max(Xs, MaxX),
    maplist(cell_y, G, Ys), list_max(Ys, MaxY),
    assertz(grid_size(MaxX, MaxY)).
assert_loop(Loop) :-
    maplist(assert_loop_segments, Loop),
    assert_loop_connections(Loop),
    Loop = [P0|_], reverse(Loop, [Start,P1|_]),
    assertz(loop_connection(Start, P0)),
    assert_start_shape(Start, P0, P1).
assert_loop_segments([X,Y]) :-
    assertz(loop([X,Y])).
assert_loop_connections([_]).
assert_loop_connections([A,B|T]) :-
    assertz(loop_connection(A, B)),
    assert_loop_connections([B|T]).
assert_start_shape([XS,YS], [X1,Y1], [X2,Y2]) :-
    DX1 #= XS - X1, DY1 #= YS - Y1, delta_direction([DX1,DY1], D1),
    DX2 #= XS - X2, DY2 #= YS - Y2, delta_direction([DX2,DY2], D2),
    assertz(cell(XS, YS, p(D1,D2))),
    assertz(cell(XS, YS, p(D2,D1))).
delta_direction([-1,0], e).
delta_direction([1,0], w).
delta_direction([0,-1], s).
delta_direction([0,1], n).

cell_x(c(X,_,_), X).
cell_y(c(_,Y,_), Y).

neighbor(X, Y, Xc, Y) :- decr(X, Xc), connects(X, Y, w), connects(Xc, Y, e).
neighbor(X, Y, Xc, Y) :- incr(X, Xc), connects(X, Y, e), connects(Xc, Y, w).
neighbor(X, Y, X, Yc) :- decr(Y, Yc), connects(X, Y, n), connects(X, Yc, s).
neighbor(X, Y, X, Yc) :- incr(Y, Yc), connects(X, Y, s), connects(X, Yc, n).

pipe_char(p(n, s), '|').
pipe_char(p(e, w), '-').
pipe_char(p(n, e), 'L').
pipe_char(p(n, w), 'J').
pipe_char(p(s, w), '7').
pipe_char(p(s, e), 'F').
pipe_connects(p(_, D), D).
pipe_connects(p(D, _), D).
pipe_connects(start, _).

incr(X0, X) :- X #= X0 + 1.
decr(X0, X) :- X #= X0 - 1.
nonempty([_|_]).
even(N) :- 0 #= (N mod 2).
odd(N) :- 1 #= (N mod 2).


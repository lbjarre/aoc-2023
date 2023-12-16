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


solve(16) :-
    phrase_from_file(parse(G), "input/day16.txt"),
    p1(G, S1), format("part 1: ~d~n", [S1]),
    p2(G, S2), format("part 2: ~q~n", [S2]).

p1(G, S) :-
    energize(G, beam([0,0], e), S, _).

p2(G, S) :-
    entrypoints(G, Entries),
    foldl(p2_fold(G), Entries, st([], 0), st(_, S)).
p2_fold(G, beam(P,D), st(Exits0, Max0), st(Exits, Max)) :-
    (   dir_reverse(D, Rev), ord_memberchk(beam(P,Rev), Exits0)
    ->  format("~q: skipped~n", [beam(P,D)]),
        Exits = Exits0, Max = Max0
    ;   format("~q: shoot~n", [beam(P,D)]),
        energize(G, beam(P,D), En, Exits1),
        Max #= max(En, Max0),
        ord_union(Exits0, Exits1, Exits)
    ).

entrypoints(grid(_, [XB,YB]), Es) :-
    range(XB, Xr),
    range(YB, Yr),
    XMax #= XB - 1,
    YMax #= YB - 1,
    maplist(\Y^newbeam(0,Y,e), Yr, West),
    maplist(\Y^newbeam(XMax,Y,w), Yr, East),
    maplist(\X^newbeam(X,0,s), Xr, North),
    maplist(\X^newbeam(X,YMax,n), Xr, South),
    append([West,North,East,South], Es).
newbeam(X,Y,D,beam([X,Y],D)).

:- dynamic(energized/2).
:- dynamic(exit_beam/2).

energize(G, Start, Energized, Exits) :-
    retractall(energized(_,_)),
    retractall(exit_beam(_,_)),
    shoot_beams(G, [Start]),
    findall(P, energized(P,_), PosWithDup),
    list_to_set(PosWithDup, Positions),
    length(Positions, Energized),
    findall(beam(P,D), exit_beam(P,D), Exits).

shoot_beams(G, [beam(P,D)|Beams]) :-
    (   energized(P, D)
    ->  shoot_beams(G, Beams)
    ;   grid_get(G, P, Cell)
    ->  assertz(energized(P, D)),
        beam_next(beam(P,D), Cell, NewBeams),
        append([Beams, NewBeams], AllBeams),
        shoot_beams(G, AllBeams)
    ;   pos_dir_next(Exit, D, P),
        assertz(exit_beam(Exit, D)),
        shoot_beams(G, Beams)
    ).
shoot_beams(_, []).

beam_next(beam(P0, D0), Cell, [beam(P, D)]) :-
    dir_cell_next(D0, Cell, [D]),
    pos_dir_next(P0, D, P).
beam_next(beam(P0, D0), Cell, [beam(P1, D1), beam(P2, D2)]) :-
    dir_cell_next(D0, Cell, [D1, D2]),
    pos_dir_next(P0, D1, P1),
    pos_dir_next(P0, D2, P2).

dir_cell_next(e, '\\', [s]).
dir_cell_next(s, '\\', [e]).
dir_cell_next(w, '\\', [n]).
dir_cell_next(n, '\\', [w]).
dir_cell_next(e, '/', [n]).
dir_cell_next(n, '/', [e]).
dir_cell_next(w, '/', [s]).
dir_cell_next(s, '/', [w]).
dir_cell_next(s, '-', [e,w]).
dir_cell_next(n, '-', [e,w]).
dir_cell_next(e, '-', [e]).
dir_cell_next(w, '-', [w]).
dir_cell_next(e, '|', [n,s]).
dir_cell_next(w, '|', [n,s]).
dir_cell_next(n, '|', [n]).
dir_cell_next(s, '|', [s]).
dir_cell_next(D, '.', [D]).

pos_dir_next([X,Y], n, [X,Z]) :- Z #= Y - 1.
pos_dir_next([X,Y], s, [X,Z]) :- Z #= Y + 1.
pos_dir_next([X,Y], w, [Z,Y]) :- Z #= X - 1.
pos_dir_next([X,Y], e, [Z,Y]) :- Z #= X + 1.

dir_reverse(n, s).
dir_reverse(s, n).
dir_reverse(e, w).
dir_reverse(w, e).

grid_get(grid(Cells, [XB,YB]), [X,Y], C) :-
    X #>= 0, X #< XB,
    Y #>= 0, Y #< YB,
    Idx #= X + XB * Y,
    nth0(Idx, Cells, C).
grid_inbound(grid(_, [XB,YB]), [X,Y]) :-
    X #>= 0, X #< XB,
    Y #>= 0, Y #< YB.
range(To, R) :-
    X #>= 0, X #< To,
    findall(X, label([X]), R).

parse(grid(Cells, [XB,YB])) -->
    parse_lines(Lines, XB),
    { append(Lines, Cells), length(Lines, YB) }.
parse_lines([Line|Rest], XB) -->
    parse_line(Line),
    { length(Line, XB) },
    parse_lines(Rest, XB).
parse_lines([], _) --> [].
parse_line([H|T]) --> [H], { H \= '\n' }, parse_line(T).
parse_line([]) --> "\n".


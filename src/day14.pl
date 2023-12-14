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

solve(14) :-
    phrase_from_file(parse(G), "input/day14.txt"),
    tilt_north(G, Tilted),
    load(Tilted, Sol1),
    format("part 1: ~d~n", [Sol1]),

    cycle_loop(G, loop(Start, End, G0)),
    LoopLen #= End - Start,
    Remaining #= 1000000000 - End,
    LoopsLeft #= Remaining rem LoopLen,
    range(LoopsLeft, R),
    foldl(\_^spin_cycle, R, G0, GFinal),
    load(GFinal, Sol2),
    format("part 2: ~d~n", [Sol2]).

n_cycle(0, G, G).
n_cycle(N0, G0, G) :-
    N0 #> 0,
    ( 0 #= N0 mod 10 -> format("~d~n", [N0]) ; true ),
    N #= N0 - 1,
    spin_cycle(G0, G1),
    n_cycle(N, G1, G).

cycle_loop(G0, Loop) :-
    empty_assoc(Cache),
    cycle_loop(Cache, 1, G0, Loop).
cycle_loop(Cache, It, G0, Loop) :-
    spin_cycle(G0, G),
    (   get_assoc(G, Cache, Start)
    ->  Loop = loop(Start, It, G)
    ;   put_assoc(G, Cache, It, CacheN),
        ItN #= It + 1,
        cycle_loop(CacheN, ItN, G, Loop)
    ).

spin_cycle(G0, G) :-
    transpose(G0, G1), maplist(tilt(start), G1, G2),
    transpose(G2, G3), maplist(tilt(start), G3, G4),
    transpose(G4, G5), maplist(tilt(end), G5, G6),
    transpose(G6, G7), maplist(tilt(end), G7, G).

tilt_north(G0, G) :-
    transpose(G0, GT),
    maplist(tilt(start), GT, Tilted),
    transpose(Tilted, G).

tilt(Dir, Ls, Tilted) :-
    append([Group, "#", Tail], Ls),
    \+memberchk('#', Group),
    separate(Group, Empty, Rocks),
    order(Dir, Empty, Rocks, GroupTilted),
    tilt(Dir, Tail, TiltedTail),
    append([GroupTilted, "#", TiltedTail], Tilted).
tilt(Dir, Ls, Tilted) :-
    \+memberchk('#', Ls),
    separate(Ls, Empty, Rocks),
    order(Dir, Empty, Rocks, Tilted).

order(start, Empty, Rocks, Group) :- append([Rocks, Empty], Group).
order(end,   Empty, Rocks, Group) :- append([Empty, Rocks], Group).

separate(['.'|T], ['.'|Empty], Rocks) :- separate(T, Empty, Rocks).
separate(['O'|T], Empty, ['O'|Rocks]) :- separate(T, Empty, Rocks).
separate([], [], []).

load(G, N) :-
    maplist(count('O'), G, RockCount),
    reverse(RockCount, RockCountRev),
    length(RockCountRev, Len),
    range(Len, Range),
    maplist(mult, Range, RockCountRev, Loads),
    sum(Loads, #=, N).
mult(X, Y, P) :- P #= X * Y.

count(Elt, Ls, C) :-
    foldl(count_(Elt), Ls, 0, C).
count_(X, X, C0, C) :- C #= C0 + 1.
count_(X, Y, C, C) :- X \= Y.

range(To, R) :-
    length(R, To),
    phrase(range_(1), R).
range_(N0) --> [N0], { N #= N0 + 1}, range_(N).
range_(_) --> [].

parse([]) --> [].
parse([R|Rs]) --> line(R), "\n", parse(Rs).
line([X|Xs]) --> [X], { X \= '\n' }, line(Xs).
line([]) --> [].

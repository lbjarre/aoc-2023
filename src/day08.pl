:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(library(dif)).
:- use_module(library(arithmetic)).
:- use_module(aoc).

solve(1, X) :-
    phrase_from_file(parse(Ins, Map), "input/day08.txt"),
    walk(Map, Ins, Ins, "AAA", 0, X).
solve(2, X) :-
    phrase_from_file(parse(Ins, Map), "input/day08.txt"),
    assert_map(Map),
    assertz(instructions(Ins)),
    findall(S, (next(l, S, _), ghost_start(S)), S0),
    maplist(find_ghost_cycle, S0, [C|Cs]),
    foldl(lcm, Cs, C, X).

assert_map([]).
assert_map([map(S, L, R) | T]) :-
    assertz(next(l, S, L)),
    assertz(next(r, S, R)),
    assert_map(T).

walk(_, _, _, "ZZZ", X, X).
walk(M, IAll, [I | T], C0, A0, X) :-
    map_next(M, I, C0, C),
    A #= A0 + 1,
    walk(M, IAll, T, C, A, X).
walk(M, IAll, [], C, A, X) :- walk(M, IAll, IAll, C, A, X).

find_ghost_cycle(S0, N) :- find_ghost_cycle_(state([], S0, 0), N).
find_ghost_cycle_(state(_, S, N), N) :- ghost_goal(S).
find_ghost_cycle_(state([], S0, N0), N) :-
    instructions(I),
    find_ghost_cycle_(state(I, S0, N0), N).
find_ghost_cycle_(state([I | IT], S0, N0), N) :-
    next(I, S0, S1),
    N1 #= N0 + 1,
    find_ghost_cycle_(state(IT, S1, N1), N).

ghost_start([_,_,'A']).
ghost_goal([_,_,'Z']).

map_next([map(S0, S, _) | _], l, S0, S).
map_next([map(S0, _, S) | _], r, S0, S).
map_next([map(R, _, _) | T], I, S0, S) :- dif(R, S0), map_next(T, I, S0, S).
map_next([], _, S, _) :- throw(error(no_next_room, S)).

parse(LRs, Map) --> parse_lrs(LRs), "\n", parse_map(Map).
parse_lrs([]) --> "\n".
parse_lrs([l|T]) --> "L", parse_lrs(T).
parse_lrs([r|T]) --> "R", parse_lrs(T).
parse_map([]) --> [].
parse_map([map(C, L, R) | Rest]) -->
    parse_room(C), " = (", parse_room(L), ", ", parse_room(R), ")\n",
    parse_map(Rest).
parse_room(Room) --> seq(Room), { length(Room, 3) }.

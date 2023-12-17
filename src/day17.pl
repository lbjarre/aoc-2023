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

solve(17) :-
    phrase_from_file(parse(G), "sample"),
    dijkstra(g(G, 1, 3), S1),
    format("part 1: ~d~n", [S1]),
    dijkstra(g(G, 4, 10), S2),
    format("part 2: ~d~n", [S2]).

dijkstra(G, Dist) :-
    Start = st([0,0], [0,0], 0),
    heap_empty(Q0),
    heap_push(Q0, 0, Start, Q),
    empty_assoc(D0),
    put_assoc(Start, D0, 0, D),
    dijkstra_(G, Q, D, Dist).

dijkstra_(G, Queue0, Dists0, Dist) :-
    heap_pop(Queue0, _, St0, Queue1),
    get_assoc(St0, Dists0, Cost0),
    (   goal(G, St0)
    ->  Dist = Cost0
    ;   findall(
            [Cost,St],
            (next_state(G, St0, ImCost, St), Cost #= Cost0 + ImCost),
            NextStates
        ),
        foldl(fold_next_states, NextStates, fold(Dists0, Queue1), fold(Dists, Queue)),
        dijkstra_(G, Queue, Dists, Dist)
    ).

fold_next_states([Cost,St], fold(Dist0, Queue0), fold(Dist, Queue)) :-
    (   get_assoc(St, Dist0, CurrCost)
    ->  (   CurrCost #> Cost
        ->  put_assoc(St, Dist0, Cost, Dist),
            heap_push(Queue0, Cost, St, Queue)
        ;   Queue = Queue0,
            Dist = Dist0
        )
    ;   put_assoc(St, Dist0, Cost, Dist),
        heap_push(Queue0, Cost, St, Queue)
    ).

goal(g(grid(_, [XB,YB]),_,_), st([X,Y],_,_)) :-
    X #= XB - 1,
    Y #= YB - 1.

next_state(g(G, Min, Max), st(Pos0, Dir0, Count0), Cost, st(Pos, Dir, Count)) :-
    direction(Dir),
    dir_reverse(Dir0, Rev), Dir \= Rev,
    ( Count0 #= Max -> Dir0 \= Dir ; true ),
    (   Dir0 \= Dir
    ->  Steps #= Min, Count #= Min
    ;   Steps #= 1, Count #= Count0 + 1
    ),
    accum_n(dxdy(Dir), Steps, Pos0, Positions),
    maplist(grid_inbound(G), Positions),
    maplist(grid_get(G), Positions, Costs),
    sum(Costs, #=, Cost),
    reverse(Positions, [Pos|_]).
direction([-1,0]).
direction([1,0]).
direction([0,-1]).
direction([0,1]).
dir_reverse([DX,DY], [RX,RY]) :-
    RX #= -1 * DX, RY #= -1 * DY.
dxdy([DX,DY], [X0, Y0], [X,Y]) :-
    X #= X0 + DX, Y #= Y0 + DY.
accum_n(_, 0, _, []).
accum_n(Pred, N0, S0, [S|Ls]) :-
    N0 #> 0,
    N #= N0 - 1,
    call(Pred, S0, S),
    accum_n(Pred, N, S, Ls).

grid_get(grid(Cells, [XB,YB]), [X,Y], C) :-
    X #>= 0, X #< XB,
    Y #>= 0, Y #< YB,
    Idx #= X + XB * Y,
    nth0(Idx, Cells, C).
grid_inbound(grid(_, [XB,YB]), [X,Y]) :-
    X #>= 0, X #< XB,
    Y #>= 0, Y #< YB.

parse(grid(Cells, [XB,YB])) -->
    parse_lines(Lines, XB),
    { append(Lines, Cells), length(Lines, YB) }.
parse_lines([Line|Rest], XB) -->
    parse_line(Line),
    { length(Line, XB) },
    parse_lines(Rest, XB).
parse_lines([], _) --> [].
parse_line([H|T]) --> [C], { C \= '\n', number_chars(H, [C]) }, parse_line(T).
parse_line([]) --> "\n".

% Simplified heap implementation from SWI.
heap_empty(nil).
heap_push(Q0, P, X, Q) :-
    heap_meld(Q0, t(X,P,[]), Q).
heap_pop(t(X,P,Sub), P, X, Q) :-
    heap_pairing(Sub, Q).
heap_meld(nil, Q, Q) :- !.
heap_meld(Q, nil, Q) :- !.
heap_meld(L, R, Q) :-
    L = t(X,Px,SubL),
    R = t(Y,Py,SubR),
    (   Px #< Py
    ->  Q = t(X,Px,[R|SubL])
    ;   Q = t(Y,Py,[L|SubR])
    ).
heap_pairing([], nil).
heap_pairing([Q], Q) :- !.
heap_pairing([Q0,Q1|Qs], Q) :-
    heap_meld(Q0, Q1, Q2),
    heap_pairing(Qs, Q3),
    heap_meld(Q2, Q3, Q).

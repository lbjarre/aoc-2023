:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(library(dif)).
:- use_module(aoc).

solve(11) :-
    phrase_from_file(parse(0, 0, B, G), "input/day11.txt"),
    empty_rows(G, B, Xempty),
    empty_cols(G, B, Yempty),
    pairs(G, Pairs),
    format("calc dists~n", []),
    maplist(dist(Xempty, Yempty, 1), Pairs, Dists),
    format("dists done~n", []),
    sum_list(Dists, Sol),
    format("part1: ~d~n", [Sol]),
    maplist(dist(Xempty, Yempty, 999999), Pairs, Dists2),
    format("dists done~n", []),
    sum_list(Dists2, Sol2),
    format("part2: ~d~n", [Sol2]).

dist(Xempty, Yempty, Factor, [A, B], D) :-
    xrange(A, B, Xrange),
    findall(X, (X #= Xrange, X #= Xempty, indomain(X)), Xintr),
    length(Xintr, NXintr),
    dx(A, B, Dx),
    Xdist #= Dx + (Factor * NXintr),
    yrange(A, B, Yrange),
    findall(Y, (Y #= Yrange, Y #= Yempty, indomain(Y)), Yintr),
    length(Yintr, NYintr),
    dy(A, B, Dy),
    Ydist #= Dy + (Factor * NYintr),
    D #= Xdist + Ydist.

empty_rows(G, B, Xs) :-
    all_x(B, Xs),
    maplist(fst, G, GXs),
    maplist((#\=)(Xs), GXs).
empty_cols(G, B, Ys) :-
    all_y(B, Ys),
    maplist(snd, G, GYs),
    maplist((#\=)(Ys), GYs).

pairs(Ls, P) :- pairs(Ls, [], P).
pairs([], P, P).
pairs([H|T], Acc, P) :-
    maplist(add_pair(H), T, Acc0),
    append([Acc, Acc0], Acc1),
    pairs(T, Acc1, P).
add_pair(X1, X2, [X1,X2]).

parse(_, Y, B, []) --> { snd(B, Y) }, [].
parse(X, Y0, B, G) --> "\n", { fst(B, X), incr(Y0, Y) }, parse(0, Y, B, G).
parse(X0, Y, B, [[X0, Y] | G]) --> "#", { incr(X0, X) }, parse(X, Y, B, G).
parse(X0, Y, B, G) --> ".", { incr(X0, X) }, parse(X, Y, B, G).

all_x([XB, _], X) :- decr(XB, XM), X in 0..XM.
all_y([_, YB], Y) :- decr(YB, YM), Y in 0..YM.

all(X, Xs) :- findall(X, label([X]), Xs).

xrange([X1,_],[X2,_], X) :- Xmin #= min(X1,X2), Xmax #= max(X1,X2), X in Xmin..Xmax.
yrange([_,Y1],[_,Y2], Y) :- Ymin #= min(Y1,Y2), Ymax #= max(Y1,Y2), Y in Ymin..Ymax.

dx([X1,_], [X2,_], DX) :- DX #= abs(X1-X2).
dy([_,Y1], [_,Y2], DY) :- DY #= abs(Y1-Y2).

incr(X0, X) :- X #= X0 + 1.
decr(X0, X) :- X #= X0 - 1.
fst([X,_], X).
snd([_,Y], Y).

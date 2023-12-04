:- module(aoc, [list_include/3]).

list_include(_, [], []).
list_include(Pred, [H | T], Incl) :-
    list_include(Pred, T, TIncl),
    (   call(Pred, H) -> Incl = [H | TIncl]
    ;   Incl = TIncl
    ).

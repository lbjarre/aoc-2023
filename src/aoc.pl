:- module(aoc, [list_include/3]).

:- meta_predicate list_include(1, ?, ?).

list_include(_, [], []).
list_include(Pred, [H | T], Incl) :-
    list_include(Pred, T, TIncl),
    (   call(Pred, H) -> Incl = [H | TIncl]
    ;   Incl = TIncl
    ).

:- meta_predicate pred_partition(1, ?, ?, ?).
pred_partition(_, [], [], []).
pred_partition(Pred, [H|T], True, False) :-
    list_include(Pred, T, True0, False0),
    (   call(Pred, H)
    ->  True = [H|True0], False = False0
    ;   True = True0, False = [H|False0]
    ).

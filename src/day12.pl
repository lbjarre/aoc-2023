:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(aoc).

solve(12) :-
    phrase_from_file(parse(Rs), "input/day12.txt"),
    maplist(permutations(1), Rs, Perms1), sum_list(Perms1, Sol1),
    format("part 1: ~d~n", [Sol1]),
    maplist(permutations(5), Rs, Perms2), sum_list(Perms2, Sol2),
    format("part 2: ~d~n", [Sol2]).

permutations(Folds, [S0,G0], N) :-
    phrase(unfold(S0, "?", Folds), S),
    phrase(unfold(G0, [], Folds), G),
    empty_assoc(Cache),
    dp(Cache, S, none, G, _, N).

parse([]) --> [].
parse([[R, N] | Rest]) -->
    parse_records(R), " ", parse_numbers(N), "\n", parse(Rest).
parse_records([]) --> [].
parse_records(['?'|R]) --> "?", parse_records(R).
parse_records(['.'|R]) --> ".", parse_records(R).
parse_records(['#'|R]) --> "#", parse_records(R).
parse_numbers([N]) --> parse_number(N).
parse_numbers([N|R]) --> parse_number(N), ",", parse_numbers(R).
parse_number(N) --> digits(Ch), { number_chars(N, Ch) }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).
digit(D) --> [D], { char_type(D, decimal_digit) }.

unfold(S, _, 1) --> S.
unfold(S, J, N) --> { N #> 1, M #= N - 1 }, S, J, unfold(S, J, M).

dp(C, [], none, [], C, 1).
dp(C, [], some(N), [N], C, 1) :- !.
dp(C, [], _, _, C, 0) :- !.
dp(C, [_|_], some(_), [], C, 0) :- !.
dp(C0, S, Curr, Rem, C, N) :-
    cache_key(S, Curr, Rem, Key),
    (   get_assoc(Key, C0, N)
    ->  C = C0
    ;   dp_(C0, S, Curr, Rem, C1, N),
        put_assoc(Key, C1, N, C)
    ).
dp_(C0, ['.'|_], some(X),  [R0|_],  C0, 0) :- X #\= R0.
dp_(C0, ['.'|S], none,     Rem,     C,  N) :-             dp(C0, S, none,    Rem, C, N).
dp_(C0, ['.'|S], some(X),  [X|Rem], C,  N) :-             dp(C0, S, none,    Rem, C, N).
dp_(C0, ['#'|S], some(X0), Rem,     C,  N) :- incr(X0,X), dp(C0, S, some(X), Rem, C, N).
dp_(C0, ['#'|S], none,     Rem,     C,  N) :-             dp(C0, S, some(1), Rem, C, N).
dp_(C0, ['?'|S], some(X0), Rem, C, N) :-
    incr(X0,X), dp(C0, S, some(X), Rem, C1, N1),
    (   Rem = [X0|Rem0]
    ->  dp(C1, S, none, Rem0, C, N2),
        N #= N1 + N2
    ;   N #= N1, C = C1
    ).
dp_(C0, ['?'|S], none, Rem, C, N) :-
    dp(C0, S, some(1), Rem, C1, N1),
    dp(C1, S, none,    Rem, C,  N2),
    N #= N1 + N2.

cache_key(S, Curr, Rem, key(SC, W, RC)) :-
    length(S, SC), length(Rem, RC),
    option_or(Curr, 0, W).
option_or(some(X), _, X).
option_or(none, X, X).
incr(X0, X) :- X #= X0 + 1.

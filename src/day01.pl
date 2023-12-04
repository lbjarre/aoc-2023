:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).

solve(1, X) :-
    phrase_from_file(parse(Ns), "input/day01.txt"),
    sum_list(Ns, X).
solve(2, X) :-
    phrase_from_file(parse2(Ns), "input/day01.txt"),
    sum_list(Ns, X).

digits_firstlast(Ds, N) :-
    list_first(Ds, First),
    list_last(Ds, Last),
    number_chars(N, [First, Last]).
list_first([X | _], X).
list_last(Ls, X) :- reverse(Ls, [X | _]).

parse([]) --> [].
parse([N | Rest]) -->
    alphanums(Line), "\n",
    {   first_digit(Line, First),
        reverse(Line, Rev),
        first_digit(Rev, Last),
        number_chars(N, [First, Last])
    },
    parse(Rest).

parse2([]) --> [].
parse2([N | Rest]) -->
    alphanums(Line), "\n",
    { line_firstlast_2(Line, N) },
    parse2(Rest).

line_firstlast_2(Line, N) :-
    first_digit_or_literal(Line, First),
    reverse(Line, Rev),
    first_digit_or_reverse_literal(Rev, Last),
    number_chars(N, [First, Last]).

first_digit([H | T], D) :-
    ( digit(H) -> D = H
    ; first_digit(T, D)
    ).

first_digit_or_literal(Ls, D) :-
    Ls = [H | T],
    (   digit(H) -> D = H
    ;   line_startswith_literal(Ls, D)
    ;   \+line_startswith_literal(Ls, _),
        first_digit_or_literal(T, D)
    ).
first_digit_or_reverse_literal(Ls, D) :-
    Ls = [H | T],
    (   digit(H) -> D = H
    ;   line_startswith_reverse_literal(Ls, D)
    ;   \+line_startswith_reverse_literal(Ls, _),
        first_digit_or_reverse_literal(T, D)
    ).

line_startswith_literal(Ls, D) :-
    append(Head, _, Ls),
    literal_digit(Head, D).

line_startswith_reverse_literal(Ls, D) :-
    append(Head, _, Ls),
    reverse(Head, Rev),
    literal_digit(Rev, D).

literal_digit("one", '1').
literal_digit("two", '2').
literal_digit("three", '3').
literal_digit("four", '4').
literal_digit("five", '5').
literal_digit("six", '6').
literal_digit("seven", '7').
literal_digit("eight", '8').
literal_digit("nine", '9').

digit(D) :- char_type(D, decimal_digit).

alphanums([A|As]) --> alphanum(A), alphanums(As).
alphanums([A]) --> alphanum(A).
alphanum(A) --> [A], { char_type(A, alphanumeric) }.

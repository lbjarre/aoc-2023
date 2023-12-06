:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(aoc).

solve(1, X) :-
    phrase_from_file(parse(Rs), "input/day06.txt"),
    maplist(race_holdtime_win_count, Rs, Counts),
    foldl(mult, Counts, 1, X).
solve(2, X) :-
    phrase_from_file(parse(Rs0), "input/day06.txt"),
    races_fix_kerning(Rs0, Rs),
    race_holdtime_win_count(Rs, X).

mult(A, B, M) :- M #= A * B.

race_time(race(Time, _), Time).
race_dist(race(_, Dist), Dist).
races_fix_kerning(Rs0, race(Time, Dist)) :-
    maplist(race_time, Rs0, Times), numbers_strjoin(Times, Time),
    maplist(race_dist, Rs0, Dists), numbers_strjoin(Dists, Dist).
numbers_strjoin(Ns, N) :- numbers_strjoin(Ns, [], N).
numbers_strjoin([], Ch, N) :- number_chars(N, Ch).
numbers_strjoin([N0 | Rest], Ch0, N) :-
    number_chars(N0, Ch1),
    append(Ch0, Ch1, Ch),
    numbers_strjoin(Rest, Ch, N).

race_holdtime_win_count(race(Time, Record), Count) :-
    Hold in 0..Time,
    Hold * (Time - Hold) #> Record,
    fd_size(Hold, Count).

parse(Races) -->
    "Time:", ws, parse_numbers(Times),
    "Distance:", ws, parse_numbers(Distances),
    { maplist(time_dist_race, Times, Distances, Races) }.
parse_numbers([]) --> "\n".
parse_numbers([N | Rest]) --> parse_number(N), ws, parse_numbers(Rest).
time_dist_race(Time, Dist, race(Time, Dist)).

parse_number(N) --> digits(Ch), { number_chars(N, Ch) }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).
digit(D) --> [D], { char_type(D, decimal_digit) }.
ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].

:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(aoc).

solve(1, X) :-
    phrase_from_file(parse(Gs), "input/day02.txt"),
    list_include(possible_game, Gs, Possible),
    maplist(game_id, Possible, IDs),
    sum_list(IDs, X).

solve(2, X) :-
    phrase_from_file(parse(Gs), "input/day02.txt"),
    maplist(game_cubepower, Gs, Powers),
    sum_list(Powers, X).

game_id(game(ID, _), ID).

% only 12 red cubes, 13 green cubes, and 14 blue cubes
possible_game(Game) :-
    game_color_max(Game, red, RMax),
    game_color_max(Game, green, GMax),
    game_color_max(Game, blue, BMax),
    RMax #=< 12,
    GMax #=< 13,
    BMax #=< 14.

game_cubepower(Game, Power) :-
    game_color_max(Game, red, RMax),
    game_color_max(Game, green, GMax),
    game_color_max(Game, blue, BMax),
    Power #= RMax * GMax * BMax.

game_color_max(game(_, Draws), Color, M) :-
    maplist(\D^draw_color_count(D, Color), Draws, Counts),
    foldl(max, Counts, 0, M).
draw_color_count([N-Color | _], Color, N).
draw_color_count([_-C | Rest], Color, N) :-
    C \= Color,
    draw_color_count(Rest, Color, N).
draw_color_count([], _, 0).

max(A, B, M) :-
    ( A #> B -> M #= A
    ; M #= B
    ).

parse([]) --> [].
parse([Game | Rest]) --> parse_game(Game), "\n", parse(Rest).
parse_game(game(ID, Draws)) --> "Game ",  parse_number(ID), ": ", parse_draws(Draws).
parse_draws([Draw | Rest]) --> parse_draw(Draw), "; ", parse_draws(Rest).
parse_draws([Draw])        --> parse_draw(Draw).
parse_draw([Count | Rest]) --> parse_count(Count), ", ", parse_draw(Rest).
parse_draw([Count])        --> parse_count(Count).
parse_count(N-Color) --> parse_number(N), " ", parse_color(Color).
parse_color(blue)  --> "blue".
parse_color(green) --> "green".
parse_color(red)   --> "red".
parse_number(N) --> digits(Ch), { number_chars(N, Ch) }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).
digit(D) --> [D], { char_type(D, decimal_digit) }.

:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(arithmetic)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(library(time)).
:- use_module(aoc).

solve(20) :-
    parse(S0, Conn),
    part1(Conn, S0, Sol1),
    format("part 1: ~d~n", [Sol1]),
    part2(Conn, S0, Sol2),
    format("part 2: ~d~n", [Sol2]).

part1(Conn, S0, Sol) :-
    part1_(Conn, S0, 1000, 0, 0, Sol).
part1_(_, _, 0, H, L, Sol) :-
    Sol #= H * L.
part1_(Conn, S0, N0, H0, L0, Sol) :-
    N0 #> 0,
    N #= N0 - 1,
    push_button(Conn, S0, S, xupd_part1, counts(H0, L0), counts(H, L)),
    part1_(Conn, S, N, H, L, Sol).

xupd_part1(pulse(_,_,low), counts(H, L0), counts(H, L)) :-
    L #= L0 + 1.
xupd_part1(pulse(_,_,high), counts(H0, L), counts(H, L)) :-
    H #= H0 + 1.


part2(Conn, S0, Sol) :-
    gen_assoc(RxPre, Conn, ["rx"]),
    gen_assoc(RxPre, S0, conj(_)),
    find_input_activations(Conn, S0, RxPre, 0, [], X),
    foldl(lcm, X, 1, Sol).
find_input_activations(_, S, RxPre, _, X, X) :-
    get_assoc(RxPre, S, conj(St)),
    same_length(St, X),
    !.
find_input_activations(Conn, S0, RxPre, N0, X0, X) :-
    push_button(Conn, S0, S, xupd_part2, name(RxPre), Y),
    N #= N0 + 1,

    (   Y = activated(_)
    ->  X1 = [N|X0]
    ;   X1 = X0
    ),

    find_input_activations(Conn, S, RxPre, N, X1, X).

xupd_part2(pulse(From,To,high), name(To), activated(From)) :- !.
xupd_part2(_, name(N), name(N)).
xupd_part2(_, activated(N), activated(N)).

push_button(Conn, S0, S, XUpd, X0, X) :-
    pulse_([pulse("button","broadcaster",low)], Conn, S0, S, XUpd, X0, X).

pulse_([], _, S, S, _, X, X).
pulse_([pulse(From,To,Sig)|Q0], Conn, S0, S, XUpd, X0, X) :-
    call(XUpd, pulse(From,To,Sig), X0, X1),

    (   get_assoc(To, S0, Mod0)
    ->  mod_update(From, Sig, Mod0, Mod, Out),
        put_assoc(To, S0, Mod, S1)
    ;   S1 = S0, Out = none
    ),

    new_pulses(Conn, To, Out, Pulses),
    append([Q0, Pulses], Q),

    pulse_(Q, Conn, S1, S, XUpd, X1, X).

new_pulses(_, _, none, []).
new_pulses(Conn, From, some(Sig), Pulses) :-
    get_assoc(From, Conn, Dsts),
    maplist(new_pulse(From, Sig), Dsts, Pulses).
new_pulse(From, Sig, To, pulse(From,To,Sig)).

mod_update(_, Sig, broadcaster, broadcaster, some(Sig)).
mod_update(_, high, flipflop(St), flipflop(St), none).
mod_update(_, low, flipflop(S0), flipflop(S), some(Out)) :-
    flipflop_toggle(S0, S),
    flipflop_output(S, Out).
mod_update(From, Sig, conj(S0), conj(S), some(Out)) :-
    conj_state_update(From, Sig, S0, S),
    conj_state_output(S, Out).
flipflop_toggle(on, off).
flipflop_toggle(off, on).
flipflop_output(on, high).
flipflop_output(off, low).
conj_state_update(From, Sig, [[From,_]|T], [[From,Sig]|T]) :- !.
conj_state_update(From, Sig, [H|T0], [H|T]) :- conj_state_update(From, Sig, T0, T).
conj_state_output(S, low) :- conj_state_all_high(S), !.
conj_state_output(_, high).
conj_state_all_high([[_,high]|T]) :- conj_state_all_high(T).
conj_state_all_high([]).

parse(Mods, Conn) :-
    phrase_from_file(parse_(ModList), "input/day20.txt"),
    empty_assoc(C0),
    init_connections(ModList, C0, Conn),
    empty_assoc(S0),
    init_states(Conn, ModList, S0, Mods).

init_connections([[Name,_,Dsts]|Rest], C0, Conn) :-
    put_assoc(Name, C0, Dsts, C1),
    init_connections(Rest, C1, Conn).
init_connections([], C, C).

init_states(Conn, [[Name,flipflop,_]|Rest], M0, M):-
    put_assoc(Name, M0, flipflop(off), M1),
    init_states(Conn, Rest, M1, M).
init_states(Conn, [[Name,conj,_]|Rest], M0, M) :-
    findall([I,low], (gen_assoc(I, Conn, Dsts), memberchk(Name, Dsts)), Inputs),
    put_assoc(Name, M0, conj(Inputs), M1),
    init_states(Conn, Rest, M1, M).
init_states(Conn, [[Name,broadcaster,_]|Rest], M0, M) :-
    put_assoc(Name, M0, broadcaster, M1),
    init_states(Conn, Rest, M1, M).
init_states(_, [], M, M).

parse_([M|Mods]) -->
    parse_module(M), "\n",
    parse_(Mods).
parse_([]) --> [].
parse_module([Name, flipflop, Dsts]) -->
    "%", parse_name(Name), " -> ", parse_destinations(Dsts).
parse_module([Name, conj, Dsts]) -->
    "&", parse_name(Name), " -> ", parse_destinations(Dsts).
parse_module(["broadcaster", broadcaster, Dsts]) -->
    "broadcaster -> ", parse_destinations(Dsts).
parse_name([H|T]) -->
    [H], { [H] \= " " }, parse_name(T).
parse_name([])  --> [].
parse_destinations([Dst|Dsts]) -->
    parse_name(Dst), ", ", parse_destinations(Dsts).
parse_destinations([Dst]) -->
    parse_name(Dst).


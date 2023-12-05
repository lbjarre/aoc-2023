:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(aoc).

solve(1, X) :-
    phrase_from_file(parse(almanac(Seeds, Ms)), "input/day05.txt"),
    maplist(single_range, Seeds, RSeeds),
    foldl(collapse_layer, Ms, RSeeds, Ls),
    maplist(range_dst, Ls, Dsts),
    list_min(Dsts, X).
solve(2, X) :-
    phrase_from_file(parse(almanac(Seeds, Maps)), "input/day05.txt"),
    seeds_ranges(Seeds, RSeeds),
    foldl(collapse_layer, Maps, RSeeds, Ls),
    maplist(range_dst, Ls, Dsts),
    list_min(Dsts, X).

seeds_ranges([], []).
seeds_ranges([S, N | Rest], [range(S, S, N) | Rs]) :- seeds_ranges(Rest, Rs).
single_range(S, range(S, S, 1)).
range_dst(range(Dst, _, _), Dst).

collapse_layer(MDst, MSrc, M) :-
    maplist(collapse_map(MDst), MSrc, Ms),
    append(Ms, M).

% Src0,L0         +--------+
%                 \         \
% Dst0,L0          +--------+
% SOvr,LOvr        |        |
% Src1,L1       +--+--------+--+
%               \  \        \   \
% Dst1,L1        +--+--------+--+
collapse_map([range(Dst1, Src1, L1) | _], range(Dst0, Src0, L0), [range(Dst, Src0, LOvr)]) :-
    range_split(r(Dst0, L0), r(Src1, L1), none, some(LOvr), none),
    Dst #= Dst1 + Dst0 - Src1.
% Src0,L0      +--+--------+
%              \  \         \
% Dst0,L0       +--+--------+
% SOvr,LOvr        |        |
% Src1,L1          +--------+--+
%                  \        \   \
% Dst1,L1           +--------+--+
collapse_map([range(Dst1, Src1, L1) | Rest], range(Dst0, Src0, L0), Rs) :-
    range_split(r(Dst0, L0), r(Src1, L1), some(LBef), some(LOvr), none),
    collapse_map(Rest, range(Dst0, Src0, LBef), RsBefore),
    Src #= Src0 + LBef,
    Rs = [range(Dst1, Src, LOvr) | RsBefore].
% Src0,L0         +--------+--+
%                 \         \  \
% Dst0,L0          +--------+--+
% SOvr,LOvr        |        |
% Src1,L1       +--+--------+
%               \  \        \
% Dst1,L1        +--+--------+
collapse_map([range(Dst1, Src1, L1) | Rest], range(Dst0, Src0, L0), Rs) :-
    range_split(r(Dst0, L0), r(Src1, L1), none, some(LOvr), some(LAft)),
    SrcAft #= Src0 + LOvr,
    DstAft #= Dst0 + LOvr,
    collapse_map(Rest, range(DstAft, SrcAft, LAft), RsAfter),
    Dst #= Dst1 + (Dst0 - Src1),
    Rs = [range(Dst, Src0, LOvr) | RsAfter].
% Src0,L0      +--+--------+--+
%              \  \         \  \
% Dst0,L0       +--+--------+--+
% SOvr,LOvr        |        |
% Src1,L1          +--------+
%                  \        \
% Dst1,L1           +--------+
collapse_map([range(Dst1, Src1, L1) | Rest], range(Dst0, Src0, L0), Rs) :-
    range_split(r(Dst0, L0), r(Src1, L1), some(LBef), some(LOvr), some(LAft)),
    collapse_map(Rest, range(Dst0, Src0, LBef), RsBefore),
    SrcAft #= Src0 + LOvr,
    DstAft #= Dst0 + LOvr,
    collapse_map(Rest, range(DstAft, SrcAft, LAft), RsAfter),
    SrcOvr #= Src0 + LBef,
    append([RsBefore, [range(Dst1, SrcOvr, LOvr)], RsAfter], Rs).
% No overlap.
collapse_map([range(_, Src1, L1) | Rest], range(Dst0, Src0, L0), Rs) :-
    range_split(r(Dst0, L0), r(Src1, L1), _, none, _),
    collapse_map(Rest, range(Dst0, Src0, L0), Rs).
% Reach end of ranges, leave range untouched.
collapse_map([], R, [R]).

range_split(r(S0, L0), r(S1, L1), Before, Overlap, After) :-
    SOvr #= max(S0, S1),
    EOvr #= min(S0 + L0, S1 + L1),
    LOvr #= max(0, EOvr - SOvr),
    LBef #= max(0, SOvr - S0),
    LAft #= max(0, S0 + L0 - EOvr),
    maplist(maybe_length, [LBef, LOvr, LAft], [Before, Overlap, After]).
maybe_length(L, some(L)) :- L #> 0.
maybe_length(0, none).

parse(almanac(Seeds, Maps)) -->
    "seeds: ", parse_seeds(Seeds), "\n",
    parse_maps(Maps).

parse_seeds([Seed | Rest]) --> parse_number(Seed), " ", parse_seeds(Rest).
parse_seeds([Seed]) --> parse_number(Seed), "\n".

parse_maps([]) --> [].
parse_maps([Ranges | Rest]) -->
    ..., " map:\n",
    parse_ranges(Ranges),
    parse_maps(Rest).

parse_ranges([]) --> "\n".
parse_ranges([range(Dst, Src, L) | Rest]) -->
    parse_number(Dst), " ", parse_number(Src), " ", parse_number(L), "\n",
    parse_ranges(Rest).

parse_number(N) --> digits(Ch), { number_chars(N, Ch) }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).
digit(D) --> [D], { char_type(D, decimal_digit) }.

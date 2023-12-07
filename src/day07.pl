:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(library(dif)).
:- use_module(aoc).

solve(1, X) :-
    phrase_from_file(parse(Gs), "input/day07.txt"),
    predqsort(compare_games, Gs, Srt),
    with_ranks(Srt, WithRanks),
    foldl(winnings, WithRanks, 0, X).
solve(2, X) :-
    phrase_from_file(parse(Gs0), "input/day07.txt"),
    maplist(game_jokerhand, Gs0, Gs),
    predqsort(compare_games, Gs, Srt0),
    maplist(game_jokerhand, Srt, Srt0),
    with_ranks(Srt, WithRanks),
    foldl(winnings, WithRanks, 0, X).

winnings(game(R, _, B), W0, W) :- W #= (R*B) + W0.

with_ranks(Games, WRanks) :- with_ranks(Games, 1, WRanks).
with_ranks([], _, []).
with_ranks([game(H, B) | Rest], R, [game(R, H, B) | Rs]) :- Rnext #= R + 1, with_ranks(Rest, Rnext, Rs).

hand_type(Hand, Type) :- hand_card_counts(Hand, CCs), ccs_type(CCs, Type).

game_jokerhand(game(H, B), game_joker(H, JR, B)) :- best_hand(H, JR).

best_hand(Original, Best) :-
    findall(H, replace_jokers(Original, H), Hands),
    pred_list_max(compare_hand_type, Hands, Best).

replace_jokers([], []).
replace_jokers(['J' | H0], [R | R0]) :-
    card(R), dif(R, 'J'),
    replace_jokers(H0, R0).
replace_jokers([C | H0], [C | R0]) :-
    dif(C, 'J'),
    replace_jokers(H0, R0).

compare_hand_type(Ord, H1, H2) :-
    hand_type(H1, T1), hand_type(H2, T2),
    compare_type(Ord, T1, T2).

ccs_type([cc(5, _)], five_of_a_kind).
ccs_type([cc(4, _), cc(1, _)], four_of_a_kind).
ccs_type([cc(3, _), cc(2, _)], full_house).
ccs_type([cc(3, _), cc(1, _), cc(1, _)], three_of_a_kind).
ccs_type([cc(2, _), cc(2, _), cc(1, _)], two_pair).
ccs_type([cc(2, _), cc(1, _), cc(1, _), cc(1, _)], one_pair).
ccs_type([cc(1, _), cc(1, _), cc(1, _), cc(1, _), cc(1, _)], high_card).

hand_card_counts(Hand, CCs) :-
    hand_card_counts(Hand, [], CCs).
hand_card_counts([], Acc, CCs) :-
    sort(Acc, CCs0), reverse(CCs0, CCs).
hand_card_counts([H |  T], Acc0, CCs) :-
    (   select(cc(C0, H), Acc0, Acc1)
    ->  Acc = [cc(C, H) | Acc1], C #= C0 + 1
    ;   Acc = [cc(1, H) | Acc0]
    ),
    hand_card_counts(T, Acc, CCs).

compare_games(Ord, game(H1, _), game(H2, _)) :- compare_hands(Ord, H1, H2).
compare_games(Ord, game_joker(H1, JR1, _), game_joker(H2, JR2, _)) :-
    hand_type(JR1, T1), hand_type(JR2, T2),
    compare_type(O, T1, T2),
    (   equals(O)
    ->  compare_cards(joker, Ord, H1, H2)
    ;   Ord = O
    ).
compare_hands(Ord, H1, H2) :-
    hand_type(H1, T1), hand_type(H2, T2),
    compare_type(O, T1, T2),
    (   equals(O)
    ->  compare_cards(Ord, H1, H2)
    ;   Ord = O
    ).
compare_cards(=, [], []).
compare_cards(Ord, [H1|T1], [H2|T2]) :-
    compare_card(O, H1, H2),
    (   equals(O)
    ->  compare_cards(Ord, T1, T2)
    ;   Ord = O
    ).
compare_cards(joker, =, [], []).
compare_cards(joker, Ord, [H1|T1], [H2|T2]) :-
    compare_card(joker, O, H1, H2),
    (   equals(O)
    ->  compare_cards(joker, Ord, T1, T2)
    ;   Ord = O
    ).

equals(=).

compare_type(Ord, T1, T2) :-
    type_ord(T1, O1), type_ord(T2, O2),
    compare(Ord, O1, O2).
type_ord(five_of_a_kind, 7).
type_ord(four_of_a_kind, 6).
type_ord(full_house, 5).
type_ord(three_of_a_kind, 4).
type_ord(two_pair, 3).
type_ord(one_pair, 2).
type_ord(high_card, 1).

compare_card(joker, Ord, C1, C2) :-
    card_ord(joker, C1, O1), card_ord(joker, C2, O2),
    compare(Ord, O1, O2).
compare_card(Ord, C1, C2) :-
    card_ord(C1, O1), card_ord(C2, O2),
    compare(Ord, O1, O2).

card_ord('A', 14).
card_ord('K', 13).
card_ord('Q', 12).
card_ord('J', 11).
card_ord('T', 10).
card_ord('9', 9).
card_ord('8', 8).
card_ord('7', 7).
card_ord('6', 6).
card_ord('5', 5).
card_ord('4', 4).
card_ord('3', 3).
card_ord('2', 2).
card_ord(joker, 'A', 14).
card_ord(joker, 'K', 13).
card_ord(joker, 'Q', 12).
card_ord(joker, 'T', 10).
card_ord(joker, '9', 9).
card_ord(joker, '8', 8).
card_ord(joker, '7', 7).
card_ord(joker, '6', 6).
card_ord(joker, '5', 5).
card_ord(joker, '4', 4).
card_ord(joker, '3', 3).
card_ord(joker, '2', 2).
card_ord(joker, 'J', 1).
card(C) :- card_ord(C, _).

predqsort(_, [], []).
predqsort(Pred, [X | L], S) :-
    partition(Pred, X, L, L1, L2),
    predqsort(Pred, L1, S1),
    predqsort(Pred, L2, S2),
    append(S1, [X|S2], S).

partition(_, _, [], [], []).
partition(P, X, [Y|L], K, [Y|M]) :-
    call(P, <, X, Y),
    partition(P, X, L, K, M).
partition(P, X, [Y|L], [Y|K], M) :-
    call(P, Ord, X, Y), dif(Ord, <),
    partition(P, X, L, K, M).

pred_list_max(P, [M0 | Ls], Max) :- pred_list_max(P, Ls, M0, Max).
pred_list_max(_, [], M, M).
pred_list_max(P, [X | T], M0, M) :-
    (   call(P, >, X, M0)
    ->  pred_list_max(P, T, X, M)
    ;   pred_list_max(P, T, M0, M)
    ).

parse([]) --> [].
parse([game(Hand, Bid) | Rest]) -->
    seq(Hand),
    { length(Hand, 5), maplist(card, Hand) },
    " ", parse_number(Bid), "\n",
    parse(Rest).
parse_number(N) --> digits(Ch), { number_chars(N, Ch) }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).
digit(D) --> [D], { char_type(D, decimal_digit) }.

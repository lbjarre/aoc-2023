:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(clpz)).
:- use_module(library(lambda)).
:- use_module(library(time)).
:- use_module(aoc).

solve(19) :-
    phrase_from_file(parse(Wfs, Pts), "input/day19.txt"),

    maplist(part_rating_score(Wfs), Pts, Scores),
    sum(Scores, #=, S1),
    format("part 1: ~d~n", [S1]),

    wfs_tree(Wfs, Tree0),
    tree_prune_tautology(Tree0, Tree1),
    tree_accept_expr(Tree1, Expr0),
    simplify_expr(Expr0, Expr1),
    expr_clpz(Expr1, Vars),
    Vars ins 1..4000,
    format("~q~n", [Vars]),
    fd_dom(Vars, Dom).

tree_clpz(Part, t(cond(Cat,Op,Cmp),T,F)) :-
    tree_clpz(PT, T), part_cat(PT, Cat, VarT),
    tree_clpz(PF, F), part_cat(PF, Cat, VarF),
    part_cat(Part, Cat, Var),
    (op_cmp(Op, Var, Cmp) #/\ VarT) #\/ (op_neg(Op, Var, Cmp) #/\ VarF).
tree_clpz(_, leaf(accept)) :- 1 #= 1.
tree_clpz(_, leaf(reject)) :- 1 #= 0.

expr_clpz(cond(x, >, Cmp), [X,_,_,_]) :- X #> Cmp.
expr_clpz(cond(x, <, Cmp), [X,_,_,_]) :- X #< Cmp.
expr_clpz(cond(m, >, Cmp), [_,M,_,_]) :- M #> Cmp.
expr_clpz(cond(m, <, Cmp), [_,M,_,_]) :- M #< Cmp.
expr_clpz(cond(a, >, Cmp), [_,_,A,_]) :- A #> Cmp.
expr_clpz(cond(a, <, Cmp), [_,_,A,_]) :- A #< Cmp.
expr_clpz(cond(s, >, Cmp), [_,_,_,S]) :- S #> Cmp.
expr_clpz(cond(s, <, Cmp), [_,_,_,S]) :- S #< Cmp.
expr_clpz(not(cond(x, >, Cmp)), [X,_,_,_]) :- X #=< Cmp.
expr_clpz(not(cond(x, <, Cmp)), [X,_,_,_]) :- X #>= Cmp.
expr_clpz(not(cond(m, >, Cmp)), [_,M,_,_]) :- M #=< Cmp.
expr_clpz(not(cond(m, <, Cmp)), [_,M,_,_]) :- M #>= Cmp.
expr_clpz(not(cond(a, >, Cmp)), [_,_,A,_]) :- A #=< Cmp.
expr_clpz(not(cond(a, <, Cmp)), [_,_,A,_]) :- A #>= Cmp.
expr_clpz(not(cond(s, >, Cmp)), [_,_,_,S]) :- S #=< Cmp.
expr_clpz(not(cond(s, <, Cmp)), [_,_,_,S]) :- S #>= Cmp.

expr_clpz(and(E1,E2), [X,M,A,S]) :-
    expr_clpz(E1, [X1,M1,A1,S1]),
    expr_clpz(E2, [X2,M2,A2,S2]),
    X #= X1 #/\ X #= X2,
    M #= M1 #/\ M #= M2,
    A #= A1 #/\ A #= A2,
    S #= S1 #/\ S #= S2.
expr_clpz(or(E1,E2), [X,M,A,S]) :-
    expr_clpz(E1, [X1,M1,A1,S1]),
    expr_clpz(E2, [X2,M2,A2,S2]),
    X #= X1 #\/ X #= X2,
    M #= M1 #\/ M #= M2,
    A #= A1 #\/ A #= A2,
    S #= S1 #\/ S #= S2.

tree_accept_expr(t(Cond,T,F), or(and(Cond,TExpr), and(not(Cond),FExpr))) :-
    tree_accept_expr(T, TExpr),
    tree_accept_expr(F, FExpr).
tree_accept_expr(leaf(accept), b(t)).
tree_accept_expr(leaf(reject), b(f)).

simplify_expr(or(E1,E2), Expr) :-
    simplify_expr(E1,SE1),
    simplify_expr(E2,SE2),
    simplify_expr_(or(SE1,SE2), Expr).
simplify_expr(and(E1,E2), Expr) :-
    simplify_expr(E1,SE1),
    simplify_expr(E2,SE2),
    simplify_expr_(and(SE1,SE2), Expr).
simplify_expr(not(E), not(E)).
simplify_expr(b(B), b(B)).
simplify_expr(cond(Cat,Op,Cmp),cond(Cat,Op,Cmp)).

simplify_expr_(or(b(t),_), b(t)) :- !.
simplify_expr_(or(_,b(t)), b(t)) :- !.
simplify_expr_(or(b(f),E), E) :- !.
simplify_expr_(or(E,b(f)), E) :- !.
simplify_expr_(or(E1,E2), or(E1,E2)).
simplify_expr_(and(b(f),_), b(f)) :- !.
simplify_expr_(and(_,b(f)), b(f)) :- !.
simplify_expr_(and(b(t),E), E) :- !.
simplify_expr_(and(E,b(t)), E) :- !.
simplify_expr_(and(E1,E2), and(E1,E2)).
simplify_expr_(cond(Cat,Op,Cmp), cond(Cat,Op,Cmp)).

wfs_tree(Wfs, Tree) :-
    wfs_tree_target(Wfs, cont("in"), Tree).
wfs_tree_target(_, final(accept), leaf(accept)).
wfs_tree_target(_, final(reject), leaf(reject)).
wfs_tree_target(Wfs, cont(Name), T) :-
    memberchk(wf(Name, Rules), Wfs),
    wfs_tree_rules(Wfs, Rules, T).
wfs_tree_rules(Wfs, [cond(Cat,Op,V,Tgt)|Rules], t(cond(Cat,Op,V),T,F)) :-
    wfs_tree_target(Wfs, Tgt, T),
    wfs_tree_rules(Wfs, Rules, F).
wfs_tree_rules(Wfs, [uncond(Tgt)|_], Tree) :-
    wfs_tree_target(Wfs, Tgt, Tree).

tree_reachable(leaf(L), [L]).
tree_reachable(t(_,T,F), R) :-
    tree_reachable(T, TR),
    tree_reachable(F, FR),
    append([TR,FR], Rall),
    list_to_set(Rall, R).
tree_prune_tautology(t(C,T,F), Pruned) :-
    (   tree_reachable(t(C,T,F), [L])
    ->  Pruned = leaf(L)
    ;   tree_prune_tautology(T, TPruned),
        tree_prune_tautology(F, FPruned),
        Pruned = t(C,TPruned,FPruned)
    ).
tree_prune_tautology(leaf(C), leaf(C)).

tree_pp(Tree) :- tree_pp(Tree, "").
tree_pp(t(Cond,T,F), Prefix) :-
    format("~s~q~n", [Prefix, Cond]),
    tree_pp(T, ['|'|Prefix]),
    tree_pp(F, ['|'|Prefix]).
tree_pp(leaf(Leaf), Prefix) :-
    format("~s~q~n", [Prefix, Leaf]).

part_rating_score(Wfs, Part, Score) :-
    part_eval(Wfs, Part, Outcome),
    part_outcome_score(Part, Outcome, Score).
part_outcome_score(_, reject, 0).
part_outcome_score(Part, accept, S) :- part_total(Part, S).

part_eval(Wfs, Part, Outcome) :-
    part_eval(Wfs, Part, "in", Outcome).
part_eval(Wfs, Part, Name, Outcome) :-
    memberchk(wf(Name, Rules), Wfs),
    rules_eval(Rules, Part, Eval),
    (   Eval = final(Outcome)
    ;   Eval = cont(NameNext),
        part_eval(Wfs, Part, NameNext, Outcome)
    ).
rules_eval([R|Rs], Part, Target) :-
    rule_eval(R, Part, Eval),
    rules_eval_(Rs, Part, Eval, Target).
rules_eval_(_, _, some(T), T).
rules_eval_(Rs, Part, none, Target) :-
    rules_eval(Rs, Part, Target).
rule_eval(cond(Cat, Op, Value, Tgt), Part, Target) :-
    part_cat(Part, Cat, V),
    (   op_cmp(Op, V, Value)
    ->  Target = some(Tgt)
    ;   Target = none
    ).
rule_eval(uncond(Tgt), _, some(Tgt)).

part_cat(part(X,_,_,_), x, X).
part_cat(part(_,M,_,_), m, M).
part_cat(part(_,_,A,_), a, A).
part_cat(part(_,_,_,S), s, S).

part_total(part(X,M,A,S), T) :- T #= X + M + A + S.

op_cmp(>, A, B) :- A #> B.
op_cmp(<, A, B) :- A #< B.
op_neg(>, A, B) :- A #=< B.
op_neg(<, A, B) :- A #>= B.

parse(Workflows, Parts) --> parse_workflows(Workflows), "\n", parse_parts(Parts).

parse_workflows([W|Ws]) --> parse_workflow(W), "\n", parse_workflows(Ws).
parse_workflows([]) --> [].

parse_workflow(wf(Name, Rules)) --> workflow_name(Name), "{", workflow_rules(Rules), "}".
workflow_name([H|T]) --> [H], { [H] \= "{", char_type(H, lower) }, workflow_name(T).
workflow_name([]) --> [].
workflow_rules([R|Rs]) --> workflow_rule(R), ",", workflow_rules(Rs).
workflow_rules([R]) --> workflow_rule(R).
workflow_rule(cond(Cat, Op, Value, Tgt)) -->
    category(Cat), operator(Op), number_(Value), ":", target(Tgt).
workflow_rule(uncond(Tgt)) --> target(Tgt).

parse_parts([P|Ps]) --> parse_part(P), "\n", parse_parts(Ps).
parse_parts([]) --> [].
parse_part(part(X,M,A,S)) -->
    "{x=", number_(X), ",m=", number_(M), ",a=", number_(A), ",s=", number_(S), "}".

category(x) --> "x".
category(m) --> "m".
category(a) --> "a".
category(s) --> "s".
operator(>) --> ">".
operator(<) --> "<".
target(final(reject)) --> "R".
target(final(accept)) --> "A".
target(cont(Name)) --> workflow_name(Name).

number_(N) --> digits(Ds), { number_chars(N, Ds) }.
digits([D|Ds]) --> digit(D), digits(Ds).
digits([D]) --> digit(D).
digit(D) --> [D], { char_type(D, decimal_digit) }.


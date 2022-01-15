%% -*- prolog -*-

key_random([], []).
key_random([V|L0], [K-V|L]) :- random(K), key_random(L0, L).

pair_values([], []).
pair_values([_-V|L0], [V|L]) :- pair_values(L0, L).

permute_list(L0, L) :-
    randomize,
    permute_list_(L0, L).
permute_list_(L0, L) :-
    key_random(L0, Keyed0),
    keysort(Keyed0, Keyed),
    pair_values(Keyed, L).

maybe :-
    randomize,
    random(X),
    X < 0.5.

probably(P) :-
    randomize,
    random(X),
    X < P.

random_member(X, L) :-
    permute_list(L, Lr),
    member(X, Lr).

%% faster for large lists:
%%
%%random_member(X, L) :-
%%    repeat,
%%    length(L, N),
%%    random(I),
%%    J is 1+floor(I*N),
%%    nth(J, L, X).

replace(X, Y, L0, L) :-
    replace(X, Y, L0, L, []).

replace(_, _, [], L, L1) :-
    !, reverse(L1, L).

replace(X, Y, [X|L0], L, L1) :-
    !, replace(X, Y, L0, L, [Y|L1]).
                              
replace(X, Y, [Z|L0], L, L1) :-
    !, replace(X, Y, L0, L, [Z|L1]).

replace_any([], _, L, L).
replace_any([X|Xs], Y, L0, L) :-
    replace(X, Y, L0, L1),
    replace_any(Xs, Y, L1, L).
